library(readr)
library("tidyverse")
library("tidyr")
library(ggplot2)
library("plyr")
library(usdata)
library("dplyr")

data <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Summary INFO
#The total population in prison 2018,
#ratio of black prison to prison population in 2001, 
#ratio of Asian prison to prison population in 2001 
# ratio of white prison to prison population in 2001 



#ratio of black prison to prison population in 2001, 
black_prison_2001 <- data %>%
  select(year, black_prison_pop) %>%
  filter(year == 2001)%>%
  summarise(black_prison_total = sum(black_prison_pop, na.rm = TRUE))%>%
  pull(black_prison_total)
#total population of prison in 2001
total_prison_2001 <- data %>%
  select(year, total_prison_pop, black_prison_pop ) %>%
  filter (year == 2001) %>%
  summarise(total_prison = sum(total_prison_pop, na.rm = TRUE)) %>%
  pull(total_prison)

ratio_black_to_total_2001 <- black_prison_2001 / total_prison_2001
# ratio of white prison to prison population in 2001 
white_prison_2001 <- data %>%
  select(year, white_prison_pop) %>%
  filter(year == 2001) %>%
  summarise(white_prison_total = sum(white_prison_pop, na.rm = TRUE)) %>%
  pull(white_prison_total)

total_prison_2001 <- data %>%
  select(year, total_prison_pop, black_prison_pop ) %>%
  filter (year == 2001) %>%
  summarise(total_prison = sum(total_prison_pop, na.rm = TRUE)) %>%
  pull(total_prison)

ratio_white_to_total_2001 <- white_prison_2001 /total_prison_2001

#ratio of Asian prison to prison population in 2001 
asian_prison_2001 <- data %>%
  select(year, aapi_prison_pop) %>%
  filter(year == 2001 ) %>%
  summarise(asian_prison_total = sum(aapi_prison_pop, na.rm = TRUE)) %>%
  pull(asian_prison_total)

ratio_asian_to_total_2001 <-asian_prison_2001 /total_prison_2001


# County that contain the most black prison in 2001
highest_black_county <- data %>%
  select(year, black_prison_pop, county_name) %>%
  filter(year == 2001) %>%
  filter(black_prison_pop == max(black_prison_pop, na.rm = TRUE)) %>%
  pull(county_name)

# County that contain the most white prison in 2001
highest_white_county <- data %>%
  select(year, white_prison_pop, county_name) %>%
  filter(year == 2001) %>%
  filter(white_prison_pop == max(white_prison_pop, na.rm = TRUE)) %>%
  pull(county_name)

# County that contain the most asian prison in 2001
highest_asian_county <- data %>%
  select(year, aapi_prison_pop, county_name) %>%
  filter(year == 2001) %>%
  filter(aapi_prison_pop == max(aapi_prison_pop, na.rm = TRUE)) %>%
  pull(county_name)


  

