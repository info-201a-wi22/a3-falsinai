
# Installing and loading packages 

library(tidyverse)
library(dplyr)
library(maps)
library(mapproj)
library(ggplot2)
library(plotly)
library(patchwork)

# setting directory 
#setwd("~/_Code/a3-falsinai")

# Load incarceration_trends.csv data

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
view(incarceration_trends)

# Variable: Latinx Jail Population 
# What is the most recent average value of Latinx jail population across all counties? 
recent_average_latinx_pop <- incarceration_trends %>%
  filter(year == max(year, na.rm = T)) %>% 
  summarize(mean_latinx_jail = mean(latinx_jail_pop, na.rm = T))

# From the dataset, extract the year that has the highest Latinx jail population
year_highest_latinx_jail_pop <- incarceration_trends %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = T)) %>% 
  pull (year)

# From the dataset, extract the state with the highest Latinx jail population 
place_highest_latinx_jail_pop <- incarceration_trends %>% 
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = T)) %>%
  select(state, county_name)

# What is the total Latinx jail population in Washington in the most recent year?
total_wa <- incarceration_trends %>%
  filter(state == "WA") %>%
  filter(year == max(year, na.rm = T)) %>%
  summarize(total_latinx_jail = sum(latinx_jail_pop, na.rm = T))
  
# What are the top five counties with the highest Latinx jail population 
top_5_places <- incarceration_trends %>%
  group_by(county_name) %>% 
  summarize(total_in_each_county = sum(latinx_jail_pop, na.rm = T)) %>% 
  arrange(desc(total_in_each_county)) %>%
  slice(1:5)
top_5_chart <- top_5_places %>% 
  left_join(incarceration_trends, by = "county_name", na.rm = T) %>%
  filter(latinx_jail_pop != "NA")

# Trends over time chart 
latinx_jail_pop_over_time_in_top_5 <- ggplot(data = top_5_chart) +
  geom_point(mapping = aes(x= year, y= latinx_jail_pop, color = county_name)) +
  geom_smooth(mapping = aes(x = year, y = latinx_jail_pop, color = county_name),
              se = FALSE) +
  labs(x = "Year", y = "Latinx Jail Population", title = "Top 5 Counties With The Highest Latinx Jail Population Over Time") +
    scale_color_discrete("County Name")
  
# Variable comparison charts
# Make a data frame for Latinx and White pop
two_variables_data <- data.frame(incarceration_trends$latinx_jail_pop,
                                 incarceration_trends$white_jail_pop)
#Graph the two variables
variable_comparison_chart <-  ggplot(data = two_variables_data) + 
  geom_point(mapping = aes(x= incarceration_trends.latinx_jail_pop,
                           y = incarceration_trends.white_jail_pop)) +
  labs(x= "Latinx Jail Pop", y = "White Jail pop", title = "Latinx Jail Population VS White Jail Population")


# Map
latinx_jail_data <- incarceration_trends %>%
  filter(year == max(year, na.rm = T))

# Join 'incarceration_trends' dataset with the map_data using the map_data function
join_map <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Merge map data and incarceration data

merge_map <- join_map %>%
  left_join(latinx_jail_data, by = "fips") %>% 
  filter(latinx_jail_pop != "NA")


# Structuring blank theme 
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )
# Creating map for Latinx in jail 'Latinx_jail_map'
latinx_jail_map <- ggplot(merge_map) +
  geom_polygon(
    mapping = aes(x= long, y= lat, group = group, fill = latinx_jail_pop),
    color = "gray", size = 0.3) +
  coord_map () +
  scale_fill_continuous(limits = c(0,max(merge_map$latinx_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Latinx Jail Population in the U.S.")

