# ------------------------------------------------------------------
# Portability note: install.packages() calls were removed from this
# script. Install the packages listed in the README before running,
# then run the script from this folder (paths are relative).
# ------------------------------------------------------------------

############################ Lab 6 - IDW Interpolation #############################

# Install packages

# Load library
library(tidyverse)
library(sf)
library(gstat)
library(ggspatial)
library(RColorBrewer)
library(classInt)


# Set working directory 


# Read-in shp 
states <- st_read("US_states.shp")

# Read-in ozone readings
oz <- st_read("ozone_1990.csv") |> 
        mutate(mean = as.numeric(mean))

# Make it spatial 
oz_sf <- st_as_sf(oz, 
                coords = c("Longitude", "Latitude"), 
                crs = 4269)

# Change projections for all layers
states <- st_transform(states,
                         crs = 5070)

oz_sf <- st_transform(oz_sf,
                         crs = 5070)

# Dissolve this layer to use to clip
states |> 
  st_union() -> US

# Base map 
ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = oz_sf,
          aes(color = mean)) +
  scale_color_continuous(type = "viridis")


# Select monitors within the lower-48
oz_sf |> 
  st_filter(states) -> sel_oz_sf

# Map it again
ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = sel_oz_sf,
          aes(color = mean)) +
  scale_color_continuous(type = "viridis")


# Create a regular grid for interpolation
grid <- US  |> 
  st_bbox() |>  
  st_make_grid(cellsize = 40000, square = TRUE) |>  
  st_as_sf() |>
  st_transform(crs = st_crs(sel_oz_sf))


# Map it!
ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = grid,
          fill = NA) 


idw_output <- idw(formula = mean ~ 1, locations = sel_oz_sf, newdata = grid, idp = 2.0)


# Round it
idw_output |> 
  mutate(var1.pred = round(var1.pred, 4)) -> idw_output

# Color palette 
display.brewer.all(type = "div")

# Breaks
hist(idw_output$var1.pred)
fisher_breaks <- classIntervals(idw_output$var1.pred, n = 5, style = "fisher")$brks


# Map it!
ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = idw_output,
          aes(fill = var1.pred),
          color = NA,
          alpha = .9) +
  scale_fill_fermenter(direction = -1,
                       palette = "Spectral",
                       breaks = fisher_breaks)

# clip it
idw_output |> 
  st_intersection(US) -> idw_clip

# Map it!
ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = idw_clip,
          aes(fill = var1.pred),
          color = NA,
          alpha = .9) +
  scale_fill_fermenter(direction = -1,
                       palette = "Spectral",
                       breaks = fisher_breaks)


# Map it again!
ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = idw_clip,
          aes(fill = var1.pred),
          color = NA,
          alpha = .8) +
  scale_fill_fermenter(direction = -1,
                       palette = "Spectral",
                       breaks = fisher_breaks,
                       guide = "legend") +
  geom_sf(data = states,
          fill = NA,
          color = "black",
          linewidth = .2) +
  theme_void() +
  labs(title = "Ground-Level Ozone Concentration in the Mainland United States (1990)",
       subtitle = "Interpolated ozone levels based on EPA monitoring data",
       caption = "Tobechi Dimkpa \nSources: EPA \nDate:Feb 26 2025",
       fill = "Ozone Level") +
  theme(plot.title = element_text(size = 14,
                                  family  = "Roboto"),
        plot.subtitle = element_text(size = 12,
                                     family = "Roboto",
                                     face = "italic"),
        legend.position = c(.96,.47),
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 23),
        legend.title = element_text(size = 8,
                                    family  = "Roboto"),
        legend.text = element_text(size=6),
        legend.key.size = unit(.43, "cm")) + 
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

# Export it and submit as part HW 6
ggsave(map1,
       filename = "us_ozone.jpg",
       width = 8,
       height = 4)


#### Join Interpolated values to Counties ----

counties <- st_read("US_Counties.shp")

# Change projections 
counties <- st_transform(counties,
                       crs = 5070)

ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = idw_clip,
          aes(fill = var1.pred),
          color = NA,
          alpha = .9) +
  scale_fill_fermenter(direction = -1,
                       palette = "Spectral",
                       breaks = fisher_breaks) +
  geom_sf(data = counties,
          fill = NA)


# Spatial Join
counties_int <- st_intersection(counties, idw_output)

# Average Ozone by county
county_avg_ozone <- counties_int |> 
                   group_by(FIPS) |>  
                   summarize(mean_ozone = mean(var1.pred))

# Natural Break Jenks 
fisher_breaks2 <- classIntervals(county_avg_ozone$mean_ozone, n = 5, style = "fisher")$brks

# Map it!
ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = county_avg_ozone,
          aes(fill = mean_ozone),
          color = NA,
          alpha = .9) +
  scale_fill_fermenter(direction = -1,
                       palette = "Spectral",
                       breaks = fisher_breaks2) 

# Add final elements
ggplot() +
  geom_sf(data = US,
          fill = "white",
          color = "black") +
  geom_sf(data = county_avg_ozone,
          aes(fill = mean_ozone),
          color = NA,
          alpha = .9) +
  scale_fill_fermenter(direction = -1,
                       palette = "Spectral",
                       breaks = fisher_breaks2) +
  theme_void() +
  labs(title = "Average Ground-Level Ozone Concentration by U.S. County (1990)",
       subtitle = "County-level averages based on interpolated EPA ozone monitoring data",
       caption = "Tobechi Dimkpa \nSources: EPA \nDate:Feb 26 2025",
       fill = "Ozone Level") +
  theme(plot.title = element_text(size = 14,
                                  family  = "Roboto"),
        plot.subtitle = element_text(size = 12,
                                     family = "Roboto",
                                     face = "italic"),
        legend.position = c(.96,.47),
        plot.caption = element_text(colour = "grey50",
                                    size = 8,
                                    vjust = 23),
        legend.title = element_text(size = 8,
                                    family  = "Roboto"),
        legend.text = element_text(size=6),
        legend.key.size = unit(.43, "cm")) + 
  annotation_north_arrow(height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(0.2, "in"), 
                         pad_y = unit(0.15, "in"),
                         style = north_arrow_minimal) +
  annotation_scale(style = "ticks",
                   pad_x = unit(0.4, "in"),
                   pad_y = unit(.2, "in"),
                   unit_category = "imperial",
                   width_hint = .1) -> map2

# Export it and submit as part HW 6
ggsave(map2,
       filename = "us_ozone2.jpg",
       width = 8,
       height = 4)

