# ------------------------------------------------------------------
# Portability note: install.packages() calls were removed from this
# script. Install the packages listed in the README before running,
# then run the script from this folder (paths are relative).
# ------------------------------------------------------------------

################## Lab 4: Spatial Point Pattern Analysis #######################

#### Setting Up your R Environment ----
# Step 1: Install packages

# Step 2: Load packages
library(tidyverse)
library(sf)
library(sfhotspot)
library(RColorBrewer)
library(classInt)
library(ggspatial)

# Step 3: Set working directory 


# Read in your data
soho <- st_read("soho.shp")
deaths <- st_read("cholera_death_cases.shp")

# Base map
ggplot() +
  geom_sf(data = soho,
          fill = NA,
          color = "darkred",
          linewidth = .5,
          linetype = 2) + 
  geom_sf(data = deaths,
          size = 1,
          alpha = .5)

# Add a reference map with {ggspatial}
ggplot() +
   annotation_map_tile(type = "cartolight", zoomin = 0) + # adds a reference map
   geom_sf(data = soho,
           fill = NA,
           color = "darkred",
           linewidth = .5,
           linetype = 2) + 
   geom_sf(data = deaths,
           size = 1,
           alpha = .5)




#### Kernel Density Maps with {sfhotspot} ----
deaths |> 
    hotspot_kde(cell_size = 10,
                grid_type = "hex",
                bandwidth_adjust = .5) |> 
    st_intersection(soho) -> kde

summary(kde)
hist(kde$kde)

# Add our color palette and breaks 
display.brewer.all(type = "div")

# Natural breaks (Jenks)
jenks_breaks <- classIntervals(kde$kde, n = 5, style = "jenks")$brks

# Map it
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = kde,
          aes(fill = kde),
          col = NA,
          alpha = .7) +
  scale_fill_fermenter(type = "div",
                       palette = "Spectral",
                       breaks = jenks_breaks) +
  theme_void() 


# Add Neighborhood limits and points 
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = soho,
          fill = NA,
          color = "darkred",
          linewidth = .5,
          linetype = 3) + 
  geom_sf(data = kde,
          aes(fill = kde),
          col = NA,
          alpha = .7) +
  scale_fill_fermenter(type = "div",
                       palette = "Spectral",
                       breaks = jenks_breaks,
                       labels = c("0", "3.3", "8.1", "13.7", "19.5", "28"),
                       guide = guide_colorbar(barwidth = 1,
                                              barheight = 6)) +
  geom_sf(data = deaths,
          size = 0.5,
          shape = 5,
          alpha = .5) + 
  theme_void() 

# Add final elements 
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = soho,
          fill = NA,
          color = "darkred",
          linewidth = .5,
          linetype = 3) + 
  geom_sf(data = kde,
          aes(fill = kde),
          col = NA,
          alpha = .7) +
  scale_fill_fermenter(type = "div",
                       palette = "Spectral",
                       breaks = jenks_breaks,
                       labels = c("0", "3.3", "8.1", "13.7", "19.5", "28"),
                       guide = guide_colorbar(barwidth = 1,
                                              barheight = 7)) +
  geom_sf(data = deaths,
          size = 0.5,
          shape = 5,
          alpha = .5) + 
  theme_void() +
  labs(title = "Kernel Density of Cholera Deaths in Soho, 1854",
       subtitle = "Kernel Density Estimates",
       caption = "Tobechi Dimkpa \nSources: John Snow (1854) \nDate: February 12,2025",
       fill = "Density") + 
  theme(plot.title = element_text(size = 13,
                                  family  = "Avenir"),
        plot.subtitle = element_text(size = 11,
                                     family  = "Avenir",
                                     face = "italic"),
        legend.position = c(0.8, 0.7),
        plot.caption = element_text(colour = "grey20",
                                    size = 8,
                                    vjust = 25)) + 
  annotation_north_arrow(height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(0.1, "in"), 
                         pad_y = unit(0.15, "in"),
                         style = north_arrow_minimal) +
  annotation_scale(style = "ticks",
                   pad_x = unit(0.4, "in"),
                   pad_y = unit(.2, "in"),
                   unit_category = "imperial",
                   width_hint = .2) 
  
  
# Export and submit as part of Assignment 4
ggsave(plot = last_plot(),
       filename = "kde_cholera.jpg",
       width = 6,
       height = 5)


#### Hotspot analysis with {sfhotspot} ----
deaths |> 
  hotspot_gistar(cell_size = 17,
                 grid_type = "hex") -> hotspot


# filter out non-significant gi* values 
deaths |> 
   hotspot_gistar(cell_size = 10,
                  grid_type = "hex") |> 
   filter(gistar > 0 & pvalue < 0.05)  -> hotspot


# Base-map 
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = hotspot,
          fill = "red",
          col = "blue",
          alpha = .7) +
  geom_sf(data = soho,
          fill = NA,
          color = "darkred",
          linewidth = .5,
          linetype = 2) +
  theme_void() 

# filter out non-significant gi* values 
deaths |> 
  hotspot_gistar(cell_size = 10,
                 grid_type = "hex") |> 
  filter(gistar > 0 & pvalue < 0.05) |> 
  st_union() -> hotspot

# Base-map 
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = hotspot,
          fill = "red",
          col = "blue",
          alpha = .7) +
  geom_sf(data = soho,
          fill = NA,
          color = "darkred",
          linewidth = .5,
          linetype = 2) +
  theme_void() 

# Map it 
ggplot() +
   annotation_map_tile(type = "cartolight", zoomin = 0) +
   geom_sf(data = hotspot,
           fill = "red",
           col = NA,
           alpha = .7) +
   geom_sf(data = soho,
           fill = NA,
           color = "darkred",
           linewidth = .5,
           linetype = 2) +
   theme_void() +
   labs(title = "Cholera Death Hot Spots in Soho, 1854",
        subtitle = "Statistically Significant Clusters of Mortality",
        caption = "Tobechi Dimkpa \nSources: John Snow (1854) \nDate: February 12,2025") +
        theme(plot.title = element_text(size = 13,
                                       family  = "Avenir"),
             plot.subtitle = element_text(size = 11,
                                          family  = "Avenir",
                                          face = "italic"),
             legend.position = "none",
             plot.caption = element_text(colour = "grey20",
                                         size = 8,
                                         vjust = 25)) + 
        annotation_north_arrow(height = unit(.8, "cm"),
                               width = unit(.6, "cm"),
                               pad_x = unit(0.1, "in"), 
                               pad_y = unit(0.15, "in"),
                               style = north_arrow_minimal) +
        annotation_scale(style = "ticks",
                         pad_x = unit(0.4, "in"),
                         pad_y = unit(.2, "in"),
                         unit_category = "imperial",
                         width_hint = .2) 
    
# Export and submit as part of assignment 4
ggsave(plot = last_plot(),
       filename = "hotspot.jpg",
       width = 6,
       height = 5)


