
library(tidyverse)
library(dplyr)
library(maps)
library(mapproj)
#library(ggplot2)
#library(plotly)

setwd("~/_Code/a3-falsinai")

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
view(incarceration_trends)

incarceration_trends_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")
view(incarceration_trends_jurisdiction)


#map



national_incar <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

recent_info <- national_incar %>%
  filter(year == max(year))

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(recent_info, by = "fips") %>%
  filter(state == "WA")

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

incarceration_map <- ggplot(map_data) +
  geom_polygon(mapping = aes(x = long, y= lat, group = group, fill = total_jail_pop),
               color = "gray", size = 0.3) + coord_map() +
  scale_fill_continuous("Female jail Population", limits = c(0, max(map_data$female_prison_pop)),
                        na.value = "white", low = "yellow", high = "red" ) +
  ggtitle("Incarcerated Female Across Washington State") +
  theme(plot.title = element_text(size = 15, face = "bold"))

#plot(incarceration_map)

# trends over time
mean_female_jail_pop <- mean(data$female_jail_pop, na.rm = T) *10 
mean_male_jail_pop <- mean(data$male_jail_pop, na.rm = T) *10
mean_female_juvenille_jail_pop <- mean(data$female_juvenille_jail_pop, na.rm = T) *10
mean_male_juvenille_jail_pop <- mean(data$male_juvenille_jail_pop, na.rm = T) *10

chart_trends <- ggplot(data = data) + 
  geom_line(mapping = aes(y = female_jail_pop, x= year, color = "Female jail population trend")) +
  geom_hline(yintercept = mean_male_jail_pop, color ="blue") +
  geom_hline(yintercept = mean_female_jail_pop, color ="black") +
  labs(title = "Female vs Male Jail population")




