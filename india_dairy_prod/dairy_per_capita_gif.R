library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)
library(gifski)
library(RColorBrewer)

# Attempt to create animated gif of milk per capita consumption
# With years as the time variable

# Load the data
data <- read_csv('./india_dairy_prod/data/per-capita-milk-consumption.csv')
world <- ne_countries(scale = "medium", returnclass = "sf")

colnames(data) <- c("Entity", "Code", "Year", "Value")

# Join the data to the world map
map_data <- world %>%
  left_join(data, by = c("iso_a3" = "Code"))

# Create the animated map

animated_map <- ggplot(map_data, aes(fill = Value)) +
  geom_sf(aes(fill = Value),    # Fill countries by some metric
          color = "black",      # Set outline color
          size = 0.2) + 
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), na.value = "grey90") +
  theme_minimal() +
  labs(title = "Milk per Capita: {frame_time}",
       fill = "Kg/year") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank()) +
  transition_time(Year) +
  ease_aes('linear')

anim <- animate(animated_map, 
                renderer = gifski_renderer(), 
                width = 1600,
                height = 1000, 
                nframes = length(unique(data$Year))*2, 
                fps = 10)

# Save the GIF
anim_save("milk_per_capita_animation.gif", animation = anim)
