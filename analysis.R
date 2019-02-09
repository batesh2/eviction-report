# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

# Load in your data
eviction_notices <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = F)
# Compute some values of interest and store them in variables for the report

# How many evictions were there?
total_evictions <- nrow(eviction_notices) 
# Create a table (data frame) of evictions by zip code (sort descending)
by_zipcode <- eviction_notices %>% 
  group_by(Eviction.Notice.Source.Zipcode) %>% 
  count() %>% 
  arrange(-n)
# Create a plot of the number of evictions each month in the dataset
by_month <- eviction_notices %>% 
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>% 
  mutate(month = floor_date(date, unit="month")) %>% 
  group_by(month) %>% 
  count()

# Store plot in a variable
date_plot <- ggplot(data = by_month) +
  geom_col(mapping = aes(x = month, y = n))
# Map evictions in 2017 

# Format the lat/long variables, filter to 2017
evictions_2017 <- eviction_notices %>% 
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>% 
  filter(format(date, "%Y") == "2017") %>% 
  separate(Location, c("lat", "long"), ",") %>% 
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)),
    long = as.numeric(gsub("\\)", "", long))
    )
# Create a maptile background
map_plot <- qmplot(
  data = evictions_2017,
  x = long,
  y = lat,
  geom = "blank",
  maptype = "toner-background",
  darken = .7,
  legend = "topright"
)

# Add a layer of points on top of the map tiles
evictions_plot <- map_plot +
  geom_point(mapping = aes(x = long, y = lat), color = "red", alpha = .3) +
  labs(title = "Evictions in San Francisco, 2017") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm"))

