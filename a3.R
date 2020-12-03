library(tidyverse)
library(ggplot2)
library(maps)
library(mapproj)
# load data in

data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Introduction + Summary Information

# Where is my aapi_pop_15to64 the highest / lowest in what states?

aapi <- data %>%
  select(state, year, aapi_pop_15to64) %>%
  filter(!is.na(aapi_pop_15to64)) %>%
  group_by(state) %>%
  summarise(total = sum(aapi_pop_15to64))

# aapi_pop_15to64 highest state
aapi_highest <- aapi %>%
  filter(total == max(total)) %>%
  pull(state)

# aapi_pop_15to64 lowest state
aapi_lowest <- aapi %>%
  filter(total == min(total)) %>%
  pull(state)

# find the native_pop_15to64  in each state
native <- data %>%
  select(state, year, native_pop_15to64) %>%
  filter(!is.na(native_pop_15to64)) %>%
  group_by(state) %>%
  summarise(total = sum(native_pop_15to64))

# which state has the highest native_pop_15to64
highest_pop <- native %>%
  filter(total == max(total)) %>%
  pull(state)

# which state has the lowest native_pop_15to64
lowest_pop <- native %>%
  filter(total == min(total)) %>%
  pull(state)

# which county has the highest metro area
total_metro_area <- data %>%
  group_by(county_name) %>%
  summarise(total_metro_area = sum(metro_area), .groups = "drop")

clear_na <- na.omit(total_metro_area)

highest_metro_area <- max(clear_na$total_metro_area)

location_most_area <-
  clear_na[clear_na$total_metro_area ==
    highest_metro_area, "county_name"] %>%
  pull(county_name)

# A paragraph of summary information,
# citing at least 5 values calculated from the data


# Trends over time chart

# create a new data frame of Black criminal population in CA
trend_df <- data %>%
  rename(
    Black = "black_jail_pop",
    White = "white_jail_pop"
  ) %>%
  select(year, state, Black, White) %>%
  filter(!is.na(Black)) %>%
  filter(!is.na(White)) %>%
  group_by(year, state) %>%
  summarise(total_black = sum(Black), total_white = sum(White)) %>%
  filter(state == "CA") %>%
  filter(year > 2008)

# create a line chart
color <- c("Total Black population in Jail" = "darkred", 
           "Total White population in Jail" = "blue")

chart_1 <- ggplot(trend_df, aes(x = year)) +
  geom_line(aes(y = total_black, color = "Total Black population in Jail")) +
  geom_line(aes(y = total_white, color = "Total White population in Jail")) +
  labs(
    title = "Black vs White Criminal population in CA",
    x = "Year",
    y = "Population",
    color = "Race"
  ) +
  scale_color_manual(values = c("darkred", "blue"))

# Variable Comparison Chart

# create a new data frame
comparison_df <- data %>%
  rename(
    jail_pop = "total_jail_pop",
    male_juvenile = "male_juvenile_jail_pop"
  ) %>%
  select(state, jail_pop, male_juvenile) %>%
  filter(!is.na(jail_pop)) %>%
  filter(!is.na(male_juvenile)) %>%
  group_by(state) %>%
  summarise(
    total_jail = sum(jail_pop),
    total_male_juvenile = sum(male_juvenile)
  )

comparison_df <- mutate(comparison_df,
  male_juv_rate =
    round((total_jail / total_male_juvenile), 3)
)

# create a bar plot
chart_2 <- ggplot(comparison_df, aes(x = state, y = male_juv_rate)) +
  geom_bar(stat = "identity", aes(fill = male_juv_rate)) +
  scale_fill_gradient(name = "Male Juvenile Rate") +
  labs(
    title = "How Male Juvenile criminal population related to each state",
    x = "State",
    y = "Male Juvenile Rate in each state (%)")


# Map

# filter dataset
total_aapi_jail <- data %>%
  select(year, fips, state, total_pop_15to64, aapi_pop_15to64) %>%
  mutate(aapi_jail_rate = total_pop_15to64 / aapi_pop_15to64) %>%
  filter(year == 2018)

# create df of map data with fips code
county_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# merge map data with total_aapi_jail df
map_data <- county_shape %>%
  left_join(total_aapi_jail, by = "fips") %>%
  filter(state == "NY")

# create a blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# create a map for NY
aapi_pop_ny <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = aapi_jail_rate),
    color = "white",
    size = .3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$aapi_jail_rate)),
    na.value = "white"
  ) +
  blank_theme +
  labs(
    title = "Asian American / Pacific Islander Jail Population in NY",
    fill = "Asian American / Pacific Islander Jail Population"
  )
