library(tidyverse)
library(maps)
library(maproj)


national_incar <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

recent_info <- national_incar %>%
  filter(year == max(year))

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(recent_info, by = "fips") %>%
  filter(state == "WA")

blanl_theme <- theme_bw() +
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

