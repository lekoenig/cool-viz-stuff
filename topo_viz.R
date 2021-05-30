## Play around with terrainr package and TNM data
## LE Koenig

# Load packages
library(ggplot2)       # create plots
library(terrainr)      # interface with TNM to access elevation data
library(sf)            # work with geospatial data
library(raster)        # work with raster data
library(progressr)     # show progress bars
library(scico)         # color palettes


handlers("progress")
#terrainr vignette: https://cran.r-project.org/web/packages/terrainr/vignettes/overview.html
# CDI workshop tutorial: https://github.com/mikemahoney218/2021-05-28-CDI-terrainr.git

####################################################
##             Work through vignette              ##
####################################################

## some point data (e.g. from field collection)
mt_elbert_points <- data.frame(
  lat = runif(100, min = 39.11144, max = 39.12416),
  lng = runif(100, min = -106.4534, max = -106.437)
)
mt_elbert_points_sf <- st_as_sf(mt_elbert_points,coords=c("lng","lat"),crs=4326)

## access elev data and orthoimagery (note shorthand for services):
# can also specify service, e.g. "3DEPElevation", "USGSNAIPPlus"
with_progress(
  output_files <- get_tiles(mt_elbert_points_sf,
                            output_prefix = tempfile(),
                            services = c("elevation", "ortho"))
)

# inspect output (output_files is a list of file paths):
output_files

## make some plots:
raster::plot(raster::raster(output_files[[1]]))
raster::plotRGB(raster::brick(output_files[[2]]), scale = 1)

elevation_raster <- raster::raster(output_files[[1]])
elevation_df <- as.data.frame(elevation_raster, xy = TRUE)
elevation_df <- setNames(elevation_df, c("x", "y", "elevation"))

ortho_raster <- raster::stack(output_files[[2]])
ortho_df <- as.data.frame(ortho_raster, xy = TRUE)
ortho_df <- setNames(ortho_df, c("x", "y", "red", "green", "blue"))

# interfaces with ggplot, too:
ggplot() + 
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = elevation)) + 
  scale_fill_distiller(palette = "BrBG") + 
  coord_sf(crs = 4326)

ggplot()+geom_contour(data=elevation_df,aes(x=x,y=y,z=elevation,color=..level..),bins=45) + 
  scale_color_viridis_c(option="A") +
  theme_void()+theme(panel.background = element_rect(fill="black"),legend.position="none")

ggplot(elevation_df, aes(x, y)) +
  geom_raster(aes(fill = elevation), interpolate = TRUE) +
  scale_color_viridis_c(option="A")

# geom_spatial_rgb gives you some options:
  # data frame:
  ggplot() + 
    geom_spatial_rgb(data = ortho_df,
                     # Required aesthetics r/g/b specify color bands:
                     aes(x = x, y = y, r = red, g = green, b = blue)) + 
    coord_sf(crs = 4326)

# RasterStack objects:
  ggplot() + geom_spatial_rgb(data = ortho_raster,aes(x = x, y = y, r = red, g = green, b = blue)) + 
             coord_sf(crs = 4326)

# path to file read by raster::stack
ggplot() + geom_spatial_rgb(data = output_files[[2]],aes(x = x, y = y, r = red, g = green, b = blue)) + 
           coord_sf(crs = 4326)

# use orthoimagery as basemaps:
ggplot() + geom_spatial_rgb(data = output_files[[2]],aes(x = x, y = y, r = red, g = green, b = blue)) + 
           geom_sf(data = mt_elbert_points_sf)


# bring into unity to visualize in 3D:
mt_elbert_overlay <- vector_to_overlay(mt_elbert_points_sf,
                                       output_files[[2]],
                                       size = 15,
                                       color = "red")
ortho_with_points <- combine_overlays(
  # Overlays are stacked in order, with the first file specified on the bottom
  output_files[[2]],
  mt_elbert_overlay,
  output_file = tempfile(fileext = ".png")
)
img <- magick::image_read(ortho_with_points)
plot(img) # note that you can work with much larger rasters encompassing many tiles, see vignette 

# restore original georeferencing:
georef_overlay <- georeference_overlay(
  ortho_with_points,
  output_files[[2]],
)

grey <- raster(georef_overlay)
plot(grey)

try <- georeference_overlay(
  overlay_file = output_files[[2]],
  reference_raster = output_files[[1]],
  output_file = tempfile(fileext = ".tif")
)
gdalUtils::gdalinfo(try)

try2 <- raster(x = try)
plot(try2)

# now w/ ggplot:
try2_df <- as.data.frame(try2, xy = TRUE)
names(try2_df) <- c("x","y","elev")
ggplot() + geom_raster(data = try2_df , aes(x = x, y = y,fill=elev)) +
  scale_fill_viridis_c() +
  coord_quickmap()


####################################################
##                      Now try!                  ##
####################################################

## How 'bout the White Mountains/Hubbard Brook area? Pawtuckaway?
hb <- data.frame(lat = 43.9503, lng =-71.775)
mtw <- data.frame(lat=44.2702778,lng=-71.3033333) # mt washington
pwt <- data.frame(lat=43.10194,lng= -71.18111) # pawtuckaway
# convert to sf object and pad with bbox:
pt_sf <- st_as_sf(pwt, coords = c("lng","lat"), crs = 4326)
pt_bbox <- set_bbox_side_length(pt_sf, 9, "km")

# download 5 m tiles:
with_progress(
  output_tiles <- get_tiles(pt_bbox, 
                            output_prefix = tempfile(),
                            services = c("elevation","contours"),
                            resolution = 5)
)

# visualize orthoimagery (add "ortho" to services above to create plot below):
#ggplot() + 
#  geom_spatial_rgb(data = output_tiles$ortho,
#                   mapping = aes(x, y, r = red, g = green, b = blue)) + 
#  geom_sf(data = hb_sf, shape = 4, color = "red") + 
#  coord_sf(crs = 4326) + 
#  theme_void()

# stack elevation and contours together and convert to data frame:
elevation <- raster(output_tiles$elevation)
contours <- stack(output_tiles$contours)

contour_stack <- stack(elevation, contours)
contour_df <- as.data.frame(contour_stack, xy = TRUE)
names(contour_df) <- c("x", "y", "z", "r", "g", "b", "a") # longitude, latitude, elevation, red, green, blue, alpha

# use alpha (transparency) to filter data to just include contour lines
contour_lines <- contour_df[contour_df$a != 0, ]

## Make a beautiful plot!
ggplot(data = contour_lines)+
  geom_raster(mapping = aes(x, y, fill = z))+
  theme_void()+
  theme(plot.background = element_rect(fill="black"),
        plot.title = element_text(color="white"),
        legend.position = "none",
        legend.text = element_text(color="white"),
        title = element_text(size=7.5))+
  coord_sf(crs = 4326)+
  scale_fill_scico(palette="imola", direction = 1)+
  ggtitle("   Pawtuckaway State Park, NH")


