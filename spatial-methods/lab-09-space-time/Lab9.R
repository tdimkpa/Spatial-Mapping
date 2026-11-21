# ------------------------------------------------------------------
# Portability note: install.packages() calls were removed from this
# script. Install the packages listed in the README before running,
# then run the script from this folder (paths are relative).
# ------------------------------------------------------------------

######################### Lab 9: Space-time Analysis ###########################

#### Set up your working environment ----
# Install packages

# Load packages
library(tidyverse)
library(sf)
library(sfhotspot)
library(gganimate)
library(ggspatial)
library(classInt)
library(RColorBrewer)
library(gifski)

# Set working directory
# NOTE: the original setwd() call was removed for portability.
# Run this script from this project folder; all file paths below are relative.


#### Animated map ----

# Read in your data
pa <- st_read("PA_county.shp")
head(pa)

# Change format from "wide" to 
pa  |>  
  pivot_longer(cols = disabl_14:disabl_20,
               names_to = "year",
               names_prefix = "disabl_",
               values_to = "perc_dis") -> pa_long

# Color palette 
display.brewer.all(type = "seq")

# Setting our breaks 
nbreaks <- classIntervals(pa_long$perc_dis, n = 5, style = "jenks")$brks


# Base map
# Animated map
ggplot() +
  geom_sf(data = pa_long,
          aes(fill = perc_dis),
          color = NA,
          linewidth = .2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       breaks = nbreaks,
                       guide = "legend") +
  theme_void() +
  labs(title = "Disability Status Across Counties in Pennsylvania",
       fill = "Percent") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 18,
                                     face = "italic"),
        legend.title = element_text(size = 12),
        legend.position = c(0.96, 0.38),
        legend.key.size = unit(.6, 'cm')) 


# Add animation
ggplot() +
  geom_sf(data = pa_long,
          aes(fill = perc_dis),
          color = NA,
          linewidth = .2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       breaks = nbreaks,
                       guide = "legend") +
  theme_void() +
  labs(title = "Disability Status Across Counties in Pennsylvania",
       fill = "Percent") +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18,
                                     face = "italic"),
        legend.title = element_text(size = 12),
        legend.position = c(0.96, 0.38),
        legend.key.size = unit(.6, 'cm'),
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 11,
                                    hjust = .95)) +
  transition_manual(year) +
  labs(subtitle = "Year: 20{current_frame}")


# Make adjustments to animation and export it
ggplot() +
  geom_sf(data = pa_long,
          aes(fill = perc_dis),
          color = NA,
          linewidth = .2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       breaks = nbreaks,
                       guide = "legend") +
  theme_void() +
  labs(title = "Disability Status Across Counties in Pennsylvania",
       fill = "Percent") +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18,
                                     face = "italic",
                                     color = "grey30"),
        legend.title = element_text(size = 12),
        legend.position = c(0.96, 0.38),
        legend.key.size = unit(.6, 'cm'),
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 11,
                                    hjust = .95)) +
  transition_manual(year) +
  labs(subtitle = "Year: 20{current_frame}") -> animation

animate(animation, 
        nframes = 4,
        fps = 1, 
        renderer = gifski_renderer("Disability_Status_Pennsylvania.gif"),
        width = 800, 
        height = 600) 

#### Changes in Spatial Distribution over Time ----

# Read-in data 
WH <- st_read("WH.shp")  # Washington  Heights
load("robs.RData") # robberies


# Spatial distribution for all robberies
# KDE analysis 
robs |> 
  hotspot_kde(cell_size = 150,
              bandwidth_adjust = .8,
              grid_type = "hex") |> 
  st_as_sf() |> 
  st_intersection(WH) -> kde


ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = kde,
          aes(fill = kde,
              alpha = kde),
          col = NA,
          alpha = .8) +
  geom_sf(data = WH,
          color = "black",
          fill = NA) +
  scale_fill_distiller(palette = "Spectral",
                       direction = -1) +
  scale_alpha_continuous(range = c(0.01, 1),
                         trans = "sqrt") +
  theme_void()


# Hot spots 
robs |> 
  hotspot_gistar(cell_size = 300,
                 grid_type = "hex") |> 
  filter(gistar > 0 & pvalue < 0.05) |> 
  st_union()  -> hotspot

ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = hotspot,
          fill = "red",
          col = NA,
          alpha = .6) + 
  geom_sf(data = WH,
          color = "black",
          fill = NA) +
  theme_void()

# Space over time 
# Create time variables
robs <- robs  |>  
  mutate(time  = hms(cmplnt_fr_tm),
         hour = hour(time),
         shift = case_when(between(hour, 6,13) ~"06:00 to 13:59",
                           between(hour, 14, 21) ~"14:00 to 21:59",
                           hour >= 22 | hour < 6  ~ "22:00 to 05:59",
                            TRUE ~ NA_character_))

# KDE by shift 
kde_by_shift <- robs |> 
  group_by(shift) |> 
  group_modify(
    ~ hotspot_kde(.x, cell_size = 250,
                  grid_type = "hex",
                  quiet = TRUE)) |> 
  ungroup() |> 
  st_as_sf() |> 
  st_intersection(WH)

ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  geom_sf(aes(fill = kde,
              alpha = kde), 
          data = kde_by_shift, 
          alpha = 0.75, 
          colour = NA) +
  geom_sf(data = WH, 
          colour = "black", 
          fill = NA) +
  facet_wrap(vars(shift)) +
  scale_fill_distiller(breaks = range(pull(kde_by_shift, kde)),
                       palette = "Spectral",
                       labels = c("lower", "higher"),
                       direction = -1) +
  labs(fill = "Density of robberies") +
  scale_alpha_continuous(range = c(0.01, 1),
                         trans = "sqrt") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.align = 1)


# Top 20 riskiest places
kde_shift_highest <- kde_by_shift |> 
                       group_by(shift) |> 
                       slice_max(order_by = kde, n = 20)

# Base map
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  geom_sf(data = kde_shift_highest,
          aes(fill = kde), 
          alpha = 0.75, 
          colour = NA,
          fill = "red2") +
  geom_sf(data = WH, 
          colour = "grey33", 
          fill = NA) +
  facet_wrap(vars(shift)) +
  theme_void() 


# Add final elements 
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  geom_sf(data = kde_shift_highest,
          aes(fill = kde), 
          alpha = 0.75, 
          colour = NA,
          fill = "red2") +
  geom_sf(data = WH, 
          colour = "grey33", 
          fill = NA) +
  facet_wrap(vars(shift)) +
  labs(title = "Robbery Hotspots in Washington Heights, 2023",
       subtitle = "Statistically Significant Hot Spots by Police Shifts",
       caption = "Author: Tobechi Dimkpa \n Source: NYPD \n Date:April 2 2025") +
  theme_void() +
  theme(plot.title = element_text(size = 13),
        plot.title.position = "panel",
        plot.subtitle = element_text(size = 11,
                                     face = "italic"),
        strip.text = element_text(size = 10, 
                                  face = "bold",
                                  color = "blue",
                                  margin = margin(t = 10)),
        plot.caption = element_text(colour = "black",
                                    size = 8,
                                    vjust = 20,
                                    hjust = .99)) -> map1 

# Export your map and submit for HW9
ggsave(map1,
       filename = "cluster_by_shift.jpg",
       width = 7,
       height = 6)




