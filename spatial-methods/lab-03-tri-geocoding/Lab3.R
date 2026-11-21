# ------------------------------------------------------------------
# Portability note: install.packages() calls were removed from this
# script. Install the packages listed in the README before running,
# then run the script from this folder (paths are relative).
# ------------------------------------------------------------------

############### Lab 3: Geocoding and Symbolizing Point data  ###################

#### Setting Up your R Environment ----
# Step 0: install packages 

# Step 1: load packages 
library(tidyverse)
library(sf)
library(RColorBrewer)
library(classInt)
library(ggspatial)
library(tidygeocoder)
library(patchwork)

# Step 2: Set working directory 



#### Changing Coordinate Reference Systems/Projections ----

# Read in US shapefile
states <- st_read("US_states.shp")
st_crs(states)

ggplot() +
  geom_sf(data = states) +
  labs(title = "EPSG: 4326 - WGS 84" ) -> WGS84

# Project to EPSG: 3857 Pseudo Mercator 
states_3857 <- st_transform(states,
                            crs = 3857)
st_crs(states_3857)

ggplot() +
  geom_sf(data = states_3857) +
  labs(title = "EPSG: 3857 - Pseudo Mercator" ) -> Mercator


# Project to EPSG:5070 - Albers Conic
states_5070 <- st_transform(states,
                            crs = 5070)

ggplot() +
  geom_sf(data = states_5070) +
  labs(title = "EPSG: 5070 - Albers Conic" ) -> Albers


WGS84 / Mercator / Albers

#### Geocoding ----
# Read in csv
TRI <- read_csv("tri_2020_nj.csv")

# Set your seed for reproducibility
set.seed(710)

# Data management 
TRI |> 
  select(facility_id, fac_name, st_address, city, state, zipcode) |> 
  sample_n(10) |> 
  mutate(address = paste(st_address, city, state, zipcode, sep = ", ")) -> TRI_sample
  
# Geocode selected addresses 
TRI_sample |> 
  geocode(address = address, method = "osm") -> TRI_sample_geocoded

# Making the coordinates "spatial" 
TRI_sample_sf <- TRI_sample_geocoded |> 
                    drop_na(lat, long) |> 
                    st_as_sf(coords = c("long", "lat"), 
                            crs = 4269)

# Plotting coordinates
ggplot() +
  geom_sf(data = TRI_sample_sf)


#### Plotting coordinates for all TRIs ----
TRI <- st_as_sf(TRI, 
                coords = c("long", "lat"), 
                crs = 4269)

# Putting everything together
NJ <- st_read("County_Boundaries_of_NJ.shp")
st_crs(NJ)
st_crs(TRI)

# Transform TRI to the same CRS as NJ
TRI_prj <- st_transform(TRI, st_crs(NJ))
st_crs(TRI_prj)

# Adjust transparency
ggplot() +
  geom_sf(data = NJ,
          fill = "grey80",
          color = "white",
          linewidth = .2) +
  geom_sf(data = TRI_prj,
          color ="#6E260E",
          alpha = .3)

# Make the size of the points proportional to total emission
ggplot() +
  geom_sf(data = NJ,
          fill = "grey80",
          color = "white",
          linewidth = .4) +
  geom_sf(data = TRI_prj,
          aes(size = total_release),
          color ="#6E260E",
          linewidth = .2,
          alpha = .5) +
  scale_size_continuous(range = c(.3, 10)) # Adjust the range values to control the point size


# Add final elements 
ggplot() +
  geom_sf(data = NJ,
          fill = "grey80",
          color = "white",
          linewidth = .4) +
  geom_sf(data = TRI_prj,
          aes(size = total_release),
          color ="#6E260E",
          linewidth = .2,
          alpha = .5) +
  scale_size_continuous(range = c(.3, 10)) +
  labs(title = "Amount of Toxic Release of Industrial Facilities",
       subtitle = "New Jersey, FY2020",
       caption = "Author: Tobechi Dimkpa \nSources: TRI - EPA \nDate:Feb 5 2025") +
  theme_void() + 
  theme(plot.title = element_text(size = 13,
                                  family  = "Avenir"),
        plot.subtitle = element_text(size = 11,
                                     family  = "Avenir",
                                     face = "italic"),
        legend.position = "none",
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 35)) + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(.28, "in"), 
                         pad_y = unit(0.8, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(pad_x = unit(0.08, "in"),
                   pad_y = unit(.6, "in"),
                   unit_category = "imperial")

# Add final elements 
ggplot() +
  geom_sf(data = NJ,
          fill = "grey80",
          color = "white",
          linewidth = .4) +
  geom_sf(data = TRI_prj,
          aes(size = total_release),
          color ="#6E260E",
          linewidth = .2,
          alpha = .5) +
  scale_size_continuous(range = c(.3, 10)) +
  labs(title = "Amount of Toxic Release of Industrial Facilities",
       subtitle = "New Jersey, FY2020",
       caption = "Author: Tobechi Dimkpa \nSources: TRI - EPA \nDate:Feb 5 2025") +
  theme_void() + 
  theme(plot.title = element_text(size = 13,
                                  family  = "Avenir"),
        plot.subtitle = element_text(size = 11,
                                     family  = "Avenir",
                                     face = "italic"),
        legend.position = "none",
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 35)) + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(.28, "in"), 
                         pad_y = unit(0.8, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(pad_x = unit(0.08, "in"),
                   pad_y = unit(.6, "in"),
                   unit_category = "imperial") -> map1 
ggsave(map1,
       filename = "NJ_TRI.jpg",
       width = 5,
       height = 7)

#### Spatial join ----
 st_join(NJ,TRI_prj,
         join = st_intersects,
         left = TRUE) -> NJ_TRI_join
 
  
# Summarize total amount of emissions per country
NJ_TRI_join %>% 
    group_by(COUNTY) %>% 
    summarize(total_emissions = sum(total_release, na.rm = TRUE)) -> NJ_TRI_total

summary(NJ_TRI_total$total_emissions)

# Base map
ggplot() +
  geom_sf(data = NJ_TRI_total,
          aes(fill = total_emissions),
          color = "black",
          linewidth = .2) + 
  scale_fill_fermenter() +
  geom_sf(data = TRI_prj,
          size = 1,
          color ="black",
          alpha = .5) 


# Add our color palette and breaks 
display.brewer.all(type = "seq")

# Natural breaks (Jenks)
jenks_breaks <- classIntervals(NJ_TRI_total$total_emissions, n = 4, style = "jenks")$brks

# Update our map
ggplot() +
  geom_sf(data = NJ_TRI_total,
          aes(fill = total_emissions),
          color = "black",
          linewidth = .2) + 
  scale_fill_fermenter(direction = 1,
                       palette = "YlOrRd",
                       breaks = jenks_breaks,
                       labels = c("0", "385k", "1m", "2.4m", "3.3m"),
                       guide = "colorbar") +
  geom_sf(data = TRI_prj,
          size = 1,
          color ="black",
          alpha = .5) +
  labs(title = "Total Pounds of Toxic Emissions",
       subtitle = "Couties in New Jersey, FY2021",
       fill = "Total lbs",
       caption = "Author: Tobechi Dimkpa \nSources: TRI - EPA \nDate:Feb 5 2025") 
                       

# Add final elements 
ggplot() +
  geom_sf(data = NJ_TRI_total,
          aes(fill = total_emissions),
          color = "black",
          linewidth = .2) + 
  scale_fill_fermenter(direction = 1,
                       palette = "YlOrRd",
                       breaks = jenks_breaks,
                       labels = c("0", "385k", "1m", "2.4m", "3.3m"),
                       guide = "colorbar") +
  geom_sf(data = TRI_prj,
          size = 1,
          color ="black",
          alpha = .5) +
  labs(title = "Total Pounds of Toxic Emissions per County",
       subtitle = " New Jersey, FY2021",
       fill = "Total lbs",
       caption = "Author: Tobechi Dimkpa \nSources: TRI - EPA \nDate:Feb 5 2025") +
  theme_void() +
  theme(plot.title = element_text(size = 13,
                                  family  = "Avenir"),
        plot.subtitle = element_text(size = 11,
                                     family  = "Avenir",
                                     face = "italic"),
        legend.title = element_text(size = 10),
        legend.position = c(0.2, 0.55),
        legend.key.size = unit(.5, 'cm'),
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 38)) + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(.28, "in"), 
                         pad_y = unit(0.8, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(pad_x = unit(0.08, "in"),
                   pad_y = unit(.6, "in"),
                   unit_category = "imperial") -> map2

ggsave(map2,
       filename = "tot_emissions_county.jpg",
       width = 5,
       height = 7)

















