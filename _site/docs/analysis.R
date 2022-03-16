
# Installing and loading packages 

library(tidyverse)
library(dplyr)
library(maps)
library(mapproj)
library(ggplot2)
library(plotly)
library(patchwork)

# setting directory 
setwd("~/_Code/a3-falsinai")

# Load incarceration_trends.csv data

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
# view(incarceration_trends)

# Variable: Female Jail Population 
# What is the most recent average value of female jail population across all counties? 
recent_average_female_jail_pop <- incarceration_trends %>%
  filter(year == max(year, na.rm = T)) %>% 
  summarize(mean_female_jail = mean(female_jail_pop, na.rm = T))

# From the dataset, extract the year that has the highest female jail population
year_highest_female_jail_pop <- incarceration_trends %>%
  filter(female_jail_pop == max(female_jail_pop, na.rm = T)) %>% 
  pull (year)

# From the dataset, extract the state with the highest female jail population 
place_highest_female_jail_pop <- incarceration_trends %>% 
  filter(female_jail_pop == max(female_jail_pop, na.rm = T)) %>%
  select(state, county_name)

# What is the total female jail population in Washington in the most recent year?
total_wa <- incarceration_trends %>%
  filter(state == "WA") %>%
  filter(year == max(year, na.rm = T)) %>%
  summarize(total_female_jail = sum(female_jail_pop, na.rm = T))
  
# What are the top five counties with the highest female jail population 
top_5_places <- incarceration_trends %>%
  group_by(county_name) %>% 
  summarize(total_in_each_county = sum(female_jail_pop, na.rm = T)) %>% 
  arrange(desc(total_in_each_county)) %>%
  slice(1:5)
top_5_chart <- top_5_places %>% 
  left_join(incarceration_trends, by = "county_name", na.rm = T) %>%
  filter(female_jail_pop != "NA")

# Trends over time chart 
female_jail_pop_over_time_in_top_5 <- ggplot(data = top_5_chart) +
  geom_point(mapping = aes(x= year, y= female_jail_pop, color = county_name)) +
  geom_smooth(mapping = aes(x = year, y = female_jail_pop, color = county_name),
              se = FALSE) +
  labs(x = "Year", y = "Female Jail Population", title = "Top 5 Counties With The Highest 
       Female Jail Population Over Time") +
    scale_color_discrete("County Name")
  
# Variable comparison charts
# Make a data frame for female and male pop
two_variables_data <- data.frame(incarceration_trends$female_jail_pop,
                                 incarceration_trends$male_jail_pop)
#Graph the two variables
variable_comparison_chart <-  ggplot(data = two_variables_data) + 
  geom_point(mapping = aes(x= incarceration_trends.female_jail_pop,
                           y = incarceration_trends.male_jail_pop)) +
  labs(x= "Female Jail Pop", y = "Male Jail pop", title = "Female Jail Population VS Male Jail Population")


# Map
female_jail_data <- incarceration_trends %>%
  filter(year == max(year, na.rm = T))

# Join 'incarceration_trends' dataset with the map_data using the map_data function
join_map <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Merge map data and incarceration data

merge_map <- join_map %>%
  left_join(female_jail_data, by = "fips") %>% 
  filter(female_jail_pop != "NA")


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
# Creating map for female in jail 'female_jail_map'
female_jail_map <- ggplot(merge_map) +
  geom_polygon(
    mapping = aes(x= long, y= lat, group = group, fill = female_jail_pop),
    color = "gray", size = 0.3) +
  coord_map () +
  scale_fill_continuous(limits = c(0,max(merge_map$female_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Female Jail Population in the U.S.")

