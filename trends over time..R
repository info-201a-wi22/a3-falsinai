
library(ggplot2)
library(tidyverse)
#library(dplyr)

incarceration_trends_over_time <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

mean_female_jail_pop <- mean(data$female_jail_pop, na.rm = T) *10 
mean_male_jail_pop <- mean(data$male_jail_pop, na.rm = T) *10
mean_female_juvenille_jail_pop <- mean(data$female_juvenille_jail_pop, na.rm = T) *10
mean_male_juvenille_jail_pop <- mean(data$male_juvenille_jail_pop, na.rm = T) *10

chart_trends <- ggplot(data = data) + 
  geom_line(mapping = aes(y = female_jail_pop, x= year, color = "Female jail population trend")) +
  geom_hline(yintercept = mean_male_jail_pop, color ="blue") +
  geom_hline(yintercept = mean_female_jail_pop, color ="black") +
    labs(title = "Female vs Male Jail population")


    
