# ------------------------------------------------------------------
# Portability note: install.packages() calls were removed from this
# script. Install the packages listed in the README before running,
# then run the script from this folder (paths are relative).
# ------------------------------------------------------------------

#################### Lab 1: Making Census Data Spatial ########################


#### Set up your working environment ----

# Install packages 

# Load packages
library(tidycensus)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(classInt)
library(ggspatial)

# Set working directory 




#### {tidycensus} to get census data ----

# Set US Census API key 
# Census API key: set your own key as the CENSUS_API_KEY environment variable.
# Get a free key at https://api.census.gov/data/key_signup.html
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Obtain list of all variables in acs5/subject 
census_data <- load_variables(year = 2020, 
                              dataset = "acs5/subject",
                              cache = TRUE)  # if TRUE, the function will cache the variable metadata locally
                              

# Explore the metadata
View(census_data)

# or search directly from here
census_data |> 
  filter(name %in% c("S2701_C01_001", "S2701_C02_001")) 


# Define your query
nj_data <- get_acs(state = "nj",
                   year = 2020,
                   geography = "county",
                   variables = c("S2701_C01_001E",
                                 "S2701_C02_001E"),
                   geometry = TRUE,  # adds the spatial geometry (shapefiles) to the data
                   survey = "acs5",
                   output = "wide",  # changes format of data to wide    
                   progress_bar = FALSE)

head(nj_data)
class(nj_data)


# Data management with {dplyr}
nj_data |> 
     rename(total_pop = S2701_C01_001E,
            no_insured = S2701_C02_001E) |> 
     mutate(pct_insured = round((no_insured/total_pop)*100,1),
            county = str_remove(NAME, "County, New Jersey")) -> nj_insured

head(nj_insured)

summary(nj_insured$pct_insured)


# Saving your data in shapfiles with the {sf} package 
st_write(nj_insured,
         "NJ_insured.shp")


#### Mapping with {ggplot2} ----

# Base map 
ggplot() +
  geom_sf(data = nj_insured)

# Adjust colors
ggplot() +
  geom_sf(data = nj_insured,
          fill = "salmon",  # adjust fill color
          color = "black",  # adjust the stroke color
          linewidth = 0.2,  # stroke width 
          alpha = .6)       # transparency 

# Apply a theme
ggplot() +
  geom_sf(data = nj_insured,
          fill = "salmon",  
          color = "black",  
          linewidth = 0.2,  
          alpha = .6) +
  theme_void()

# Choropleth map 
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured), # fill color based on `pct_insured`
          color = "black",
          linewidth = 0.2) +
  theme_void()


# Explore color scales
display.brewer.all(type = "seq")

# Adding a color palette with `scale_fill_fermenter()`
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured),
          color = "black",
          linewidth = 0.2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues") +
  theme_void()


# Data classification 
breaks <- classIntervals(nj_insured$pct_insured, n = 4, style = "quantile")$brks


# Add quantile breaks 
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured),
          color = "black",
          linewidth = 0.2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       guide = "legend",  # you can also use "colorbar"
                       breaks = breaks) +  # breaks the distribution as specified above
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverse legend order
  theme_void()

# Label our counties
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured),
          color = "black",
          linewidth = 0.2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       guide = "legend",  
                       breaks = breaks) +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  geom_sf_text(data = nj_insured, aes(label = county)) + # label based on county name 
  theme_void()

# Select the bottom 25% of the distribution 
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured),
          color = "black",
          linewidth = 0.2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       guide = "legend",
                       breaks = breaks) +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  geom_sf_text(data = nj_insured |>   # pass data to a filter()
               filter(pct_insured < 92.30),
               aes(label = county),
               size = 2,              # adjust the size
               color = "black",       # adjust the color
               fontface = "bold") +   # adjust the font face
  theme_void()


#### Add final elements to map ----

# Titles and subtitles 
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured),
          color = "black",
          linewidth = 0.2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       guide = "legend",
                       breaks = breaks) +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  geom_sf_text(data = nj_insured |>   
                 filter(pct_insured < 92.30),
               aes(label = county),
               size = 2,            
               color = "black",       
               fontface = "bold") +   
  theme_void() +
  labs(title = "Percent of Population Who are Insured",   # add our title, subtitles, and source 
       subtitle = "New Jersey Counties, 2020",
       caption = "Author: Tobechi Dimkpa \n Source: US Census \n Date:Jan 22 2025",
       fill = "% Insured") +
  theme_void() 


# Scale bar and north arrow 
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured),
          color = "black",
          linewidth = 0.2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       guide = "legend",
                       breaks = breaks) +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  geom_sf_text(data = nj_insured |>   
                 filter(pct_insured < 92.30),
               aes(label = county),
               size = 2,            
               color = "black",       
               fontface = "bold") +   
  theme_void() +
  labs(title = "Percent of Population Who are Insured",  # add our title, subtitles, and source 
       subtitle = "New Jersey Counties, 2020",
       caption = "Author: Tobechi Dimkpa \n Source: US Census \n Date:Jan 22 2025",
       fill = "% Insured") +
  annotation_scale(pad_x = unit(0.4, "in"), # adjust x position
                   pad_y = unit(0.1, "in")) + # adjust y position  
  annotation_north_arrow(location = "bl", which_north = "true", # north arrow and adjust size and position 
                         height = unit(.8, "cm"), # adjust size
                         width = unit(1, "cm"),   # adjust size
                         pad_x = unit(0, "in"), # adjust x position
                         pad_y = unit(0, "in"), # adjust y position
                         style = north_arrow_fancy_orienteering) 


# Adjust text and position of text
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured),
          color = "black",
          linewidth = 0.2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       guide = "legend",
                       breaks = breaks) +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  geom_sf_text(data = nj_insured |>   
                 filter(pct_insured < 92.30),
               aes(label = county),
               size = 2,            
               color = "black",       
               fontface = "bold") +   
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_void() +
  labs(title = "Percent of Population Who are Insured",  
       subtitle = "New Jersey Counties, 2020",
       caption = "Author: Tobechi Dimkpa \n Source: US Census \n Date:Jan 22 2025",
       fill = "% Insured") +
  annotation_scale(pad_x = unit(0.35, "in"), 
                   pad_y = unit(0.1, "in")) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(.8, "cm"), 
                         width = unit(1, "cm"),  
                         pad_x = unit(0, "in"), 
                         pad_y = unit(0, "in"), 
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(face = "bold", # adjustments to our title 
                                  size = 11,
                                  family  = "sans"),
        plot.title.position = "plot",  # changing title position 
        plot.subtitle = element_text(face = "italic",  # adjustments to our title
                                     size = 10,
                                     family  = "sans",
                                     color = "grey25"),
        plot.caption = element_text(colour = "grey50", # adjustments to our caption
                                    size = 6,
                                    vjust = 19),
        legend.title = element_text(size = 8),  # adjusting legend title 
        legend.key.size = unit(.4, 'cm'),  # adjusting size of legend elements
        legend.position = c(0.15, 0.53)) # adjust legend position 

# Export map
ggplot() +
  geom_sf(data = nj_insured,
          aes(fill = pct_insured),
          color = "black",
          linewidth = 0.2) +
  scale_fill_fermenter(direction = 1,
                       palette = "Blues",
                       guide = "legend",
                       breaks = breaks) +  
  guides(fill = guide_legend(reverse = TRUE)) + 
  geom_sf_text(data = nj_insured |>   
                 filter(pct_insured < 92.30),
               aes(label = county),
               size = 2,            
               color = "black",       
               fontface = "bold") +   
  theme_void() +
  labs(title = "Percent of Population Who are Insured",  
       subtitle = "New Jersey Counties, 2020",
       caption = "Author: Tobechi Dimkpa \n Source: US Census \n Date:Jan 22 2025",
       fill = "% Insured") +
  annotation_scale(pad_x = unit(0.35, "in"), 
                   pad_y = unit(0.1, "in")) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(.8, "cm"), 
                         width = unit(1, "cm"),  
                         pad_x = unit(0, "in"), 
                         pad_y = unit(0, "in"), 
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(face = "bold", 
                                  size = 11,
                                  family  = "sans"),
        plot.title.position = "plot",  #
        plot.subtitle = element_text(face = "italic",  
                                     size = 8,
                                     family  = "sans",
                                     color = "grey25"),
        plot.caption = element_text(colour = "grey50",
                                    size = 6,
                                    vjust = 19),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(.4, 'cm'),
        legend.position = c(0.15, 0.53)) -> map

ggsave("NJ_insured.jpg",
       map,
       width = 4.5,
       height = 5.5,
       units = "in")

























































