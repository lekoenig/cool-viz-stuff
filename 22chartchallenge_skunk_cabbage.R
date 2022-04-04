library(tidyverse) 
library(rgbif)    # interface with GBIF API 
library(ggthemes)
library(cowplot)
library(tigris)   # get state polygons
library(sf)
library(tmap)

# Download eastern skunk cabbage observations

# define species name
spp_name <- c("Symplocarpus foetidus")

# download GBIF occurrence data/iNaturalist observations
gbif_data <- occ_data(scientificName = spp_name, 
                      hasCoordinate = TRUE, 
                      limit = 50000)

# filter GBIF data based on time interval (2018+) and spatial domain (Northeastern U.S.)
data <- gbif_data$data %>%
  filter(countryCode == "US",
         !is.na(dateIdentified),
         year > 2018,
         stateProvince %in% 
           c("New York", "Massachusetts","Connecticut","Rhode Island",
             "New Hampshire","Vermont","Maine","New Jersey","Pennsylvania")) %>%
  mutate(Date = as.Date(dateIdentified),
         doy = lubridate::yday(Date)) 

# plot distribution of observation counts
# barcode plot colored by number of observations
strip_plot <- data %>% 
    group_by(year,species,doy) %>%
    summarize(n = n(),
              .groups = "drop") %>%
    ggplot() + 
    geom_tile(aes(y = species,x = doy, fill = n),alpha=.8) + 
    scale_fill_viridis_c(option="B")+
    scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335), 
                       labels = c("J","F","M","A","M","J","J","A","S","O","N","D"))+
    labs(y="", x = "day of year") +
    facet_wrap(~year,ncol=1) +
    theme_void() +
    theme(legend.position = "none",
          axis.ticks=element_blank(),
          axis.text=element_text(size = 4, color = "gray30"),
          axis.text.y = element_blank(),
          strip.text.x = element_text(hjust = 0.05,size = 5, color = "gray30"),
          plot.margin = margin(0.1,0.1,0.5,0.1, "cm"))

# find observations ~March - May and convert to sf object
sites_spring <- data %>%
  select(decimalLongitude,decimalLatitude) %>%
  sf::st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=4326) 

# download states polygons  
states <- tigris::states(year = 2020,progress_bar=FALSE) %>%
  filter(STUSPS %in% c("ME","VT","NH",
                       "PA","NJ","NY",
                       "CT","MA","RI"))

# create map
map_plot <- tm_shape(states) +
  tm_polygons(col = "gray95",lwd = 0.4) +
  tm_shape(sites_spring) +
  tm_dots(size = 0.008, 
          col = "darkmagenta",
          alpha = 0.1) +
  tm_credits("Data source: iNaturalist", 
             position = c(0.65,0.05), just = "right",
             size = .35, fontface = "italic", col = "gray30") +
  tm_credits("Eastern skunk cabbage observations\n2019 - 2022",
             position = c(0,0.85),
             size = 0.37, col = "gray30") +
  tm_layout(frame = FALSE)

# Stitch plots together and save
tm_map <- tmap_grob(map_plot)

png(file = "./22chartchallenge_skunk_cabbage.png",   
    width = 3.55, height = 2,units = "in", res = 400)
plot_grid(NULL,tm_map,NULL,strip_plot,
          rel_widths = c(0.2,1.1,0.2,1.5), 
          rel_heights = c(1,0.9,1,2),
          nrow = 1)
dev.off()



