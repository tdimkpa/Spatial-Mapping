# ------------------------------------------------------------------
# Portability note: install.packages() calls were removed from this
# script. Install the packages listed in the README before running,
# then run the script from this folder (paths are relative).
# ------------------------------------------------------------------

############################ Lab 5 - Geoprocessing #############################

#### Set-up Working Environment 
# Install packages

# Load library
library(tidyverse)
library(sf)
library(ggspatial)
library(classInt)
library(RColorBrewer)
library(osrm)
library(patchwork)

# Set working directory 


#### Part 1 - Buffers ----
# Read-in data 
schools <- st_read("schools.shp")
vape <- st_zm(st_read("vaping.shp"))

# Base map
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = schools,
          color = "black",
          size = 1) +
  geom_sf(data = vape,
          color = "darkred",
          shape = 18,
          size = 1) 

# Create buffer 
buffer <- st_buffer(schools, dist = 1000) 

# Map it!
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = schools,
          color = "black",
          size = 1) + 
  geom_sf(data = buffer,
          color = "black",
          fill = "yellow",
          alpha = .2,
          size = 1.5) +
  geom_sf(data = vape,
          color = "darkred",
          shape = 18,
          size = 1) 

# Select only vape shows that intersect with the school buffer
vape |> 
  st_intersection(buffer) -> sel_vape

# Map it again!
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = schools,
          color = "black",
          size = 1) + 
  geom_sf(data = buffer,
          color = "black",
          fill = "yellow",
          alpha = .2,
          size = 1.5) +
  geom_sf(data = sel_vape,
          color = "darkred",
          shape = 18,
          size = 1) 

# Spatial joins with vape shows
vape_in_buffer <- st_join(vape, buffer,
                          join = st_within,
                          left = FALSE)

# Count number of vape shops per buffer
vape_in_buffer |> 
    group_by(ID.y,) |>  
    summarize(vape_count = n()) |> 
    st_drop_geometry() -> buffer_n

# Join that information to school layer
schools |> 
  left_join(buffer_n, by = c("ID" = "ID.y")) |> 
  replace_na(list(vape_count = 0)) |> 
  filter(vape_count > 0)-> school_join


# Map it!
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = school_join,
          aes(size = vape_count),
          color = "black",
          alpha = .6,) +
  scale_size_continuous(range = c(1, 7)) +
  geom_sf(data = sel_vape,
          color = "darkred",
          shape = 18,
          size = 1) 

# Add final elements 
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = school_join,
          aes(size = vape_count),
          color = "black",
          alpha = .5) +
  scale_size_continuous(range = c(1, 7)) +
  geom_sf(data = sel_vape,
          color = "darkred",
          shape = 18,
          size = 1.5) +
  geom_sf_text(data = school_join |>  
               filter(vape_count >= 7),
               aes(label = ID),
               size = 2.5,
               color = "white",
               fontface = "bold") +
  theme_void() +
  labs(title = "Number of Vape Shops within 1,000ft of School Grounds",
       subtitle = "Trenton, New Jersey 2018",
       caption = "Tobechi Dimkpa \nSources: NJ DOE \nDate:Feb 19 2025",
       size = "No. of Vape Shops") +
  theme(plot.title = element_text(size = 13,
                                  family  = "Avenir"),
        plot.subtitle = element_text(size = 11,
                                     family  = "Avenir",
                                     face = "italic"),
        legend.position = c(.8,.4),
        plot.caption = element_text(colour = "grey50",
                                    size = 8)) + 
  annotation_north_arrow(height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(0.2, "in"), 
                         pad_y = unit(0.15, "in"),
                         style = north_arrow_minimal) +
  annotation_scale(style = "ticks",
                   pad_x = unit(0.4, "in"),
                   pad_y = unit(.2, "in"),
                   unit_category = "imperial",
                   width_hint = .1) -> map1

ggsave(map1,
       filename = "trenton_schools.jpg",
       width = 7,
       height = 4)

#### Part 2 - Distance to Nearest Hub
# Read-in data
vac <- st_read("NYC_COVID19_vacc_sites.shp")
zip <- st_read("MODZCTA_2010.shp")

st_crs(zip)
# Change projection
zip <- st_transform(zip, st_crs(vac))

# Calculate centroids of the ZIP polygons
zip_centroids <- st_centroid(zip)

# Base map
ggplot() +
  geom_sf(data = zip,
          fill = "white",
          color = "black") +
  geom_sf(data = zip_centroids,
          color = "black",
          size = .7,
          alpha = .5)

# Calculate the distance from each ZIP centroid to the nearest vaccination site
distances <- st_distance(zip_centroids, vac)
View(distances)

# Add the minimum distance to each ZIP area using dplyr's mutate function
zip <- zip |> 
         mutate(distance = apply(distances, 1, min))

# Dissolve NYC
nyc <- st_union(zip)
plot(nyc)

summary(zip$distance)
hist(zip$distance)
jenks_breaks <- classIntervals(zip$distance, n = 5, style = "jenks")$brks

# Map it!
ggplot() +
  geom_sf(data = zip,
          aes(fill = distance),
          color = NA) +
  geom_sf(data = vac,
          color = "black",
          size = .7,
          alpha = .5) +
  scale_fill_fermenter(type = "seq",
                       direction = 1,
                       palette = "YlOrBr",
                       breaks = jenks_breaks,
                       guide = "colorbar",
                       labels = c("0", "1.6k","3.4k", "5.8k", "9.2k","14.6")) +
  geom_sf(data = nyc,
          color = "black",
          fill = NA) +
  labs(title = "Distance to Nearest COVID-19 Vaccination Site",
       subtitle = "New York City, 2021",
       caption = "Author: Tobechi Dimkpa \n Source: NYC DOH \n Date:Feb 19 2025",
       fill = "Distance") +
  theme_void() +
  theme(plot.title = element_text(size = 13,
                                  family  = "Avenir"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 11,
                                     family  = "Avenir",
                                     face = "italic"),
        legend.position = c(0.3, 0.57),
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 15,
                                    hjust = .9)) + 
  annotation_north_arrow(height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(0.6, "in"), 
                         pad_y = unit(0, "in"),
                         style = north_arrow_minimal) +
  annotation_scale(style = "ticks",
                   pad_x = unit(1, "in"),
                   pad_y = unit(.05, "in"),
                   unit_category = "imperial",
                   width_hint = .2) -> map2

ggsave(map2,
       filename = "distance2vac_centers.jpg",
       width = 5,
       height = 6)

### Part 3 - Isochrones buffers 
# Read-in data
mailman <- st_read("mailman_school.shp")

# Walking 
iso_walk <- osrmIsochrone(loc = mailman, 
              breaks = seq(from = 0, to = 40, by = 10),
              osrm.profile = "foot")
str(iso_walk)

# Create a new variables with time ranges 
iso_walk <- iso_walk  |> 
                mutate(times = factor(paste(isomin, "to", isomax, "min")))

# Map it!
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = iso_walk,
          aes(fill = times),
          color = "white",
          linewidth = .1,
          alpha = .3) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(data = mailman,
          shape = 18,
          color = "red",
          size = 3) +
  labs(title = "40-Minute Walk",
       fill = "") +
  theme_void() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.key.size = unit(.4, 'cm')) -> walk

# Car
iso_car <- osrmIsochrone(loc = mailman, 
                         breaks = seq(from = 0, to = 40, by = 10))

iso_car <- iso_car  |> 
           mutate(times = factor(paste(isomin, "to", isomax, "min")))

ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = iso_car,
          aes(fill = times),
          color = "white",
          linewidth = .1,
          alpha = .3) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(data = mailman,
          shape = 18,
          color = "red",
          size = 3) +
  labs(title = "40-Minute Car Ride",
       fill = "") +
  theme_void() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.key.size = unit(.4, 'cm')) -> car      


#  Bike
so_bike <- osrmIsochrone(loc = mailman, 
                        breaks = seq(from = 0, to = 40, by = 10),
                        osrm.profile = "bike")

iso_bike <- so_bike  |> 
               mutate(times = factor(paste(isomin, "to", isomax, "min")))
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = iso_bike,
          aes(fill = times),
          color = "white",
          linewidth = .1,
          alpha = .3) +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(data = mailman,
          shape = 18,
          color = "red",
          size = 3) +
  labs(title = "40-Minute Bike Ride",
       fill = "") +
  theme_void() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.key.size = unit(.1, 'cm')) -> bike      

car + bike + walk



  












