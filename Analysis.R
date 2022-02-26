library(readr)
library("tidyverse")
library("tidyr")
library(ggplot2)
library("plyr")
library(usdata)
library("dplyr")
library("plotly", warn.conflicts = FALSE)
library("leaflet", warn.conflicts = FALSE)
library("maps")

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


#Trend over time 
#Shows the increase or decrease of prison in different kind of race over the time periods from 2001 to 2018

black_prison <- data %>%
  select(year, black_prison_pop) %>%
  group_by(year) %>%
  filter(year > 2000)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(black = sum(black_prison_pop, na.rm = TRUE)) 

latin_prison <- data %>%
  select(year, latinx_prison_pop) %>%
  group_by(year) %>%
  filter(year > 2000)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(latinx = sum(latinx_prison_pop, na.rm = TRUE))

  
asian_prison <- data %>%
  select(year, aapi_prison_pop) %>%
  group_by(year) %>%
  filter(year > 2000)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(asian = sum(aapi_prison_pop, na.rm = TRUE))

white_prison <- data %>%
  select(year, white_prison_pop) %>%
  group_by(year) %>%
  filter(year > 2000)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(white = sum(white_prison_pop, na.rm = TRUE))

other_prison <- data %>%
  select(year, other_race_prison_pop) %>%
  group_by(year) %>%
  filter(year > 2000)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(other = sum(other_race_prison_pop, na.rm = TRUE))

total_prison <- data %>%
  select(year, total_prison_pop) %>%
  group_by(year) %>%
  filter(year > 2000)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(total = sum(total_prison_pop, na.rm = TRUE))

native_prison <- data %>%
  select(year, native_prison_pop) %>%
  group_by(year) %>%
  filter(year > 2000)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(native = sum(native_prison_pop, na.rm = TRUE))
  
combined <- left_join(total_prison, black_prison, by="year")
combined <- left_join(combined, asian_prison, by = "year")
combined <- left_join(combined, latin_prison, by = "year")
combined <- left_join(combined, native_prison, by = "year")
combined <- left_join(combined, white_prison, by = "year")
combined <- left_join(combined, other_prison, by = "year")

chart_data <- combined %>%
  gather(key = compare, value = population, native, total, other, white, asian, latinx, black  )


trend_data <- plot_ly(
  data = chart_data,
  x = ~year,
  y = ~population,
  color = ~compare,
  type = "scatter",
  mode = "markers"
) %>% layout(
  title = "Prison Population by race from year 2000 to 2016",
  xaxis = list(title = "Year"),
  yaxis = list(title = "Prison population"),
  legend = list(title = list(text = "<b> Demographic </b>"))
)
trend_data


# compares two variables between white prison population to white population

white_pop_total <- data %>%
  select(year, white_pop_15to64) %>%
  group_by(year) %>%
  filter(year > 1990)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(white_pop_total = sum(white_pop_15to64, na.rm = TRUE))

white_prison_total <- data %>%
  select(year, white_prison_pop) %>%
  group_by(year) %>%
  filter(year > 1990)%>%
  filter(year < 2017)%>%
  group_by(year) %>%
  summarise(white_prison_total = sum(white_prison_pop, na.rm = TRUE))


Compare <- left_join(white_prison_total, white_pop_total, by = "year")

double_chart_data <- Compare %>%
  gather(key = compare, value = population, white_pop_total,white_prison_total)

two_chart <- plot_ly(
  data = double_chart_data,
  x = ~year,
  y = ~population,
  color = ~compare,
  type = "scatter",
  mode = "lines"
) %>% layout(
  title = "White American populaton compare to white prison",
  xaxis = list(title = "Year"),
  yaxis = list(title = "population")
)
two_chart



# Map the black prison by county from 1990 - 2017 
black_prison_county <- data %>%
  select(year, county_name, black_prison_pop) %>%
  group_by(county_name)%>%
  filter(year > 1990)%>%
  filter(year < 2017)%>%
  group_by(county_name) %>%
summarise(black_prison_pop = sum(black_prison_pop, na.rm =TRUE))






