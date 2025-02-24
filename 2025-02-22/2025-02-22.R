## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE, 
                      results="hide", 
                      warning=FALSE, 
                      message=FALSE)

r = getOption("repos")
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
# 
# install.packages("tidytuesdayR")
# install.packages("skimr")
# install.packages("DataExplorer")
# install.packages("ggplot2")
# install.packages("ggExtra")
# install.packages("dplyr")
# install.packages("scales")
# install.packages("ggpubr")
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("quarto")
# install.packages("maps")

## -----------------------------------------------------------------------------
library(skimr)
library(DataExplorer)
library(ggplot2)
library(ggExtra)
library(dplyr)
library(scales)
library(ggpubr)
library(tidyverse)
library(tidytext)
library(quarto)
library(maps)

## -----------------------------------------------------------------------------
agencies <- tidytuesdayR::tt_load(2025, week = 7)$agencies

## -----------------------------------------------------------------------------
# DataExplorer::create_report(agencies)

## -----------------------------------------------------------------------------
# histogram of latitudes
lats <- ggplot(agencies, aes(x = latitude)) +
  geom_histogram(aes(y = ..count..), 
                 color = "black", 
                 fill = "red",
                 bins = 50,
                 alpha = 0.5) +
  geom_density(aes(y = ..density..),
               alpha = 0.4, 
               fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(latitude, na.rm = TRUE)),
             color = "blue",
             linetype = "solid") +
  labs(title = "Frequency of Latitudes",
       x = "Latitude",
       y = "Number of Agencies") +
  theme_classic() +
  scale_x_continuous(limits = c(20, 55))

lats

# histogram of longitudes
longs <- ggplot(agencies, aes(x = longitude)) +
  geom_histogram(aes(y = ..count..),
                 color = "black", 
                 fill = "blue",
                 bins = 50,
                 alpha = 0.5) +
  geom_density(aes(y = ..density..),
               alpha = 0.4,
               fill = "#66A1FF") +
  geom_vline(aes(xintercept = mean(longitude, na.rm = TRUE)),
             color = "red",
             linetype = "solid",
             ) +
  scale_x_continuous(limits = c(-130, -60)) +
  labs(title = "Frequency of Longitudes",
       x = "Longitude",
       y = "Number of Agencies") +
  theme_classic() 

longs

## -----------------------------------------------------------------------------
long_state_sum <- agencies %>%  
  group_by(state_abbr, is_nibrs) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = is_nibrs, values_from = count) %>% 
  rename(True = "TRUE") %>% 
  rename(False = "FALSE") 

state_sum_fct <- agencies %>% group_by(state_abbr, is_nibrs) %>% 
  summarise(count = n()) %>%
  ungroup() %>% 
  mutate(state_abbr = reorder_within(state_abbr, count, is_nibrs))

state_sum <- agencies %>% group_by(state_abbr, is_nibrs) %>% 
  summarise(count = n()) %>%
  ungroup() 
  
# faceted bar graphs showing NIBRS status
ggplot(data = state_sum_fct, aes(x = reorder(state_abbr, +count), y = count, fill = is_nibrs)) +
  facet_wrap(~is_nibrs, scales = "free_y") +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(
    title = "Bar Chart of States Count",
    ylab = "Frequency",
    fill = "States"
  )

# side by side bars
ggplot(data = state_sum, aes(x = reorder(state_abbr, +count), y = count, fill = is_nibrs)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(
    title = "Bar Chart of States Count",
    ylab = "Frequency",
    fill = "States"
  )

# stacked bar
ggplot(data = state_sum, aes(x = reorder(state_abbr, +count), y = count, fill = factor(is_nibrs))) +
  geom_bar(position = "stack",
           stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(
    title = "Bar Chart of States Count",
    xlab = "Frequency",
    fill = "NIBRS Status"
  )


## -----------------------------------------------------------------------------
# install.packages("usmap")
library(usmap)

# create data frame for mapping
usmap_df <- agencies %>% group_by(state_abbr) %>% 
  summarise(value = n()) %>% 
  rename("state" = "state_abbr")

# plot all states of the U.S. to create an empty map
plot_usmap(data = usmap_df, values = "value", regions = "states", color = "red") +
  scale_fill_gradient2(low = "white", high = "red", name = "Number of Agencies", labels = scales::comma, breaks = c(250, 500, 750, 1000, 1250, 1500), limits = c(0, 1600))
  labs(title = "Map of the U.S.",
       subtitle = "Distribution of FBI Agencies") +
  theme(legend.position = "right",
        legend.key.spacing.y = 5)

## -----------------------------------------------------------------------------
# subset USA data using maps library
world_map <- map_data("world")
usa_map <- subset(world_map, world_map$region == "USA")

# create a base plot with ggplot2
base_plot <- ggplot() +
  coord_fixed() +
  xlab("") +
  ylab("")

# add map to base plot
base_world_messy <- base_plot +
  geom_polygon(data = usa_map,
               aes(x = long, y = lat, group = group),
               colour = "light green", 
               fill = "light green") +
  xlim(c(-175, -60))

base_world_messy

# strip and clean world map

