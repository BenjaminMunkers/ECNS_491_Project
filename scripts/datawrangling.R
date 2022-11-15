library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)

# setting directory to data folder
wd = str_replace(getwd(), "scripts", "Data")
setwd(wd)

### Financial data
# loading in nasdaq data and selecting close price and date
nasdaq = read_csv("^IXIC.csv")
nasdaq = nasdaq %>%
  select(Date, Close)

# loading in dow jones data
dow = read_csv("dowjones.csv")

### Weather data
# loading in Seattle weather data
seattle = read_csv("seattle 2020-10-12 to 2022-10-12.csv")
# Seattle had temp in C instead of F
for(i in 1:nrow(seattle)){
  seattle[i,8] = seattle[i,8]*9/5 + 32
}

# loading in Chicago weather data
chicago = read_csv("Chicago 2020-10-12 to 2022-10-12.csv")

# loading in New York City weather data
nyc = read_csv("New York City 2020-10-12 to 2022-10-12.csv")

### Selecting weather variables
# Seattle
seattle = seattle %>%
  select(datetime, feelslike, precip, precipcover, cloudcover)
colnames(seattle) = c("datetime", "seattle_temp", "seattle_precip", "seattle_precipcover", "seattle_cloudcover")

# Chicago
chicago = chicago %>%
  select(datetime, feelslike, precip, precipcover, cloudcover)
colnames(chicago) = c("datetime", "chicago_temp", "chicago_precip", "chicago_precipcover", "chicago_cloudcover")
# NYC
nyc = nyc %>%
  select(datetime, feelslike, precip, precipcover, cloudcover)
colnames(nyc) = c("datetime", "nyc_temp", "nyc_precip", "nyc_precipcover", "nyc_cloudcover")

### creating weather df
weather = seattle %>% 
  left_join(chicago, by = "datetime")

weather = weather %>%
  left_join(nyc, by = "datetime")

# avg temp variable
weather = weather %>%
  mutate(
    temp = mean(seattle_temp, chicago_temp, nyc_temp)
  )

weather$temp = mean(weather$seattle_temp, weather$chicago_temp, weather$nyc_temp)

### creating financial df
# date variables
nasdaq$Date = ymd(nasdaq$Date)
dow$date = mdy(dow$date)

# column names
colnames(nasdaq) = c("datetime", "nasdaq")
colnames(dow) = c("datetime", "dow_jones")

# merging finance
finance = nasdaq %>%
  left_join(dow, by = "datetime")

### merging weather and finance into one df
# only care about weather observations on trading days
df = finance %>%
  left_join(weather, by = "datetime")

### create seasonal variables for 2021
fall = vector(mode = "numeric", length = nrow(df))
df$fall = fall
summer = vector(mode = "numeric", length = nrow(df))
df$summer = summer
spring = vector(mode = "numeric", length = nrow(df))
df$spring = spring
winter = vector(mode = "numeric", length = nrow(df))
df$winter = winter

for (i in 1:nrow(df)) {
  if (df$datetime[i] >= ymd("2021-09-22") && df$datetime[i] < ymd("2021-12-21")) {
    df$fall[i] = 1
  }
  if (df$datetime[i] >= ymd("2021-12-21") && df$datetime[i] < ymd("2022-03-20")) {
    df$winter[i] = 1
  }
  if (df$datetime[i] >= ymd("2021-03-20") && df$datetime[i] < ymd("2021-06-20")) {
    df$spring[i] = 1
  }
  if (df$datetime[i] > ymd("2021-06-20") && df$datetime[i] < ymd("2021-09-22")) {
    df$summer[i] = 1
  }
}

### saving dataset
save(df, file = "finalcleaned.RData")
