rm(list = ls())
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(ggplot2)
library(patchwork)

# changing the wd to knit the document
# set working directory to this file location by going to Session > Set Working Directory > To Source File Location
# wd = paste(getwd(), "/Data/", sep = "")
wd = str_remove(getwd(), "/scripts")

# loading in data frame
load(paste(wd, "/Data/", "finalcleaned.RData", sep = ""))

### making visualizations
#initial temp 
temp_plot1 = ggplot(data = df, aes(x = datetime)) +
  geom_line(aes(y = seattle_temp, color = "Seattle")) +
  geom_line(aes(y = chicago_temp, color = "Chicago")) +
  geom_line(aes(y = nyc_temp, color = "NYC")) +
  geom_smooth(aes(y = nyc_temp),span = 0.3, color = "steelblue") +
  geom_smooth(aes(y = chicago_temp),span = 0.3, color = "darkred") +
  geom_smooth(aes(y = seattle_temp),span = 0.3, color = "magenta") +
  xlab("Date") +
  ylab("Temperature (F)") + 
  scale_color_manual(name = "City", values = c("Seattle" = "magenta", 
                                               "Chicago" = "darkred",
                                               "NYC" = "steelblue")) + 
  ggtitle("Tempurature of US Financial Centers")
#cloud cover
cloud_plot1 = ggplot(data = df, aes(x = datetime)) +
  geom_line(aes(y = seattle_cloudcover, color = "Seattle"), alpha = 0.3) +
  geom_line(aes(y = chicago_cloudcover, color = "Chicago"), alpha = 0.3) +
  geom_line(aes(y = nyc_cloudcover, color = "NYC"), alpha = 0.3) + 
  geom_smooth(aes(y = nyc_cloudcover),span = 0.3, color = "steelblue") +
  geom_smooth(aes(y = chicago_cloudcover),span = 0.3, color = "darkred") +
  geom_smooth(aes(y = seattle_cloudcover),span = 0.3, color = "magenta") +
  xlab("Date") +
  ylab("Percent Cloud Cover") +
  scale_color_manual(name = "City", values = c("Seattle" = "magenta", 
                                               "Chicago" = "darkred",
                                               "NYC" = "steelblue")) + 
  ggtitle("Cloud Cover over US Financial Centers")
#and precipitation coverage graphs
precip_plot1 = ggplot(data = df, aes(x = datetime)) +
  geom_line(aes(y = seattle_precipcover), color = "magenta") +
  geom_line(aes(y = chicago_precipcover), color = "darkred") +
  geom_line(aes(y = nyc_precipcover), color = "steelblue") +
  geom_smooth(aes(y = nyc_precipcover),span = 0.3, color = "steelblue") +
  geom_smooth(aes(y = chicago_precipcover),span = 0.3, color = "darkred") +
  geom_smooth(aes(y = seattle_precipcover),span = 0.3, color = "magenta") +
  xlab("Date") +
  ylab("Percent Precipitation Cover")
#Finds the change in temperature for each city and percent daily change in stock price
df = df %>% 
  mutate(chicago_delta_temp = chicago_temp - lag(chicago_temp)) %>% 
  mutate(nyc_delta_temp = nyc_temp - lag(nyc_temp)) %>%
  mutate(seattle_delta_temp = seattle_temp - lag(seattle_temp)) %>% 
  mutate(dow_jones_delta = 1 - lag(dow_jones)/dow_jones) %>% 
  mutate(nasdaq_delta = 1 - lag(nasdaq)/nasdaq)

#sets precipitation to has it or doesn't
for(i in length(df[,1])){
  df$seattle_gotprecip = ifelse(df$seattle_precip > 0,1,0)
  df$chicago_gotprecip = ifelse(df$chicago_precip > 0,1,0)
  df$nyc_gotprecip = ifelse(df$nyc_precip > 0,1,0)
}
#finds on average how often it rains
precip_chi = mean(df$chicago_gotprecip)
precip_sea = mean(df$seattle_gotprecip) 
precip_nyc = mean(df$nyc_gotprecip)
#initiallizes the colums to be used for a moving average of temp and cloud cover
df$chicago_macloud = 0
df$chicago_matemp = 0
df$chicago_maprecip = 0
df$seattle_macloud = 0
df$seattle_matemp = 0
df$seattle_maprecip = 0
df$nyc_macloud = 0
df$nyc_matemp = 0
df$nyc_maprecip = 0
i = 12
# moving average of temp and cloud cover to better see trend
for(i in 12:nrow(df)){
  j = i-11
  df$chicago_macloud[i] = mean(df$chicago_cloudcover[j:i])
  df$chicago_matemp[i] = mean(df$chicago_temp[j:i])
  df$seattle_macloud[i] = mean(df$seattle_cloudcover[j:i])
  df$seattle_matemp[i] = mean(df$seattle_temp[j:i])
  df$nyc_macloud[i] = mean(df$nyc_cloudcover[j:i])
  df$nyc_matemp[i] = mean(df$nyc_temp[j:i])
  df$seattle_maprecip[i] = mean(df$seattle_precipcover[j:i])
  df$nyc_maprecip[i] = mean(df$nyc_precipcover[j:i])
  df$chicago_maprecip[i] = mean(df$chicago_precipcover[j:i])
}
#solves for the moving average of the two variables 
df1 = df
for(i in 1:11){df1 = df1[-1,]}
# regraphes with the moving average
# for temp
ma_temp_plot = ggplot(data = df1, aes(x = datetime)) +
  geom_line(aes(y = seattle_matemp), color = "magenta") +
  geom_line(aes(y = chicago_matemp), color = "darkred") +
  geom_line(aes(y = nyc_matemp), color = "steelblue") + 
  ggtitle("Mean Average of Tempurature") + 
  labs(subtitle = "Over Time") + 
  xlab("Date") + 
  ylab("Degrees Fahrenheit")
# precipitaion cover
ma_cloud_plot = ggplot(data = df1, aes(x = datetime)) +
  geom_line(aes(y = seattle_macloud), color = "magenta") +
  geom_line(aes(y = chicago_macloud), color = "darkred") +
  geom_line(aes(y = nyc_macloud), color = "steelblue") + 
  ggtitle("Mean Average of Precipitaion Cover") + 
  labs(subtitle = "Over Time") + 
  xlab("Date") + 
  ylab("Percent Percipitation Cover")
# and for cloud cover
ma_cloud_plot = ggplot(data = df1, aes(x = datetime)) +
  geom_line(aes(y = seattle_macloud), color = "magenta") +
  geom_line(aes(y = chicago_macloud), color = "darkred") +
  geom_line(aes(y = nyc_macloud), color = "steelblue") + 
  ggtitle("Mean Average of Cloud Cover") + 
  labs(subtitle = "Over Time") + 
  xlab("Date") + 
  ylab("Percent Cloud Cover")

### graphing indices 
# dow jones
dow_plot = ggplot(data = df1, aes(x = datetime)) + 
  geom_line(aes(y = dow_jones), color = "green") + 
  ggtitle("Dow Jones Index") + 
  labs(subtitle = "Over Time") + 
  xlab("Date") + 
  ylab("US Dollars")

# nasdaq
ndaq_plot = ggplot(data = df1, aes(x = datetime)) + 
  geom_line(aes(y = nasdaq), color = "green") + 
  ggtitle("NASDAQ Index") + 
  labs(subtitle = "Over Time") + 
  xlab("Date") + 
  ylab("US Dollars")

### exploring uncharted territory
# comparing dow plot to ma temp plot
ma_temp_plot + dow_plot

# comparing ndaq plot to ma temp plot
ma_temp_plot + ndaq_plot

## graphing based on season
# separating data by season
df_fall = df %>%
  filter(df$fall == 1)

df_spring = df %>%
  filter(df$spring == 1)
df_winter = df %>%
  filter(df$winter == 1)
df_summer = df %>%
  filter(df$summer == 1)

# seasonal comparisons
# spring temp on Dow Jones
dow_plot_spring = ggplot(data = df_spring, aes(x = datetime)) + 
  geom_line(aes(y = dow_jones), color = "green") + 
  ggtitle("Dow Jones Index") + 
  labs(subtitle = "Spring 2021") + 
  xlab("Date") + 
  ylab("US Dollars")

ma_temp_plot_spring = ggplot(data = df_spring, aes(x = datetime)) +
  geom_line(aes(y = seattle_matemp), color = "darkmagenta") +
  geom_line(aes(y = chicago_matemp), color = "darkred") +
  geom_line(aes(y = nyc_matemp), color = "steelblue") + 
  ggtitle("Mean Average of Tempurature") + 
  labs(subtitle = "Spring 2021") + 
  xlab("Date") + 
  ylab("Degrees Fahrenheit")

# fall temp on Dow Jones
dow_plot_fall = ggplot(data = df_fall, aes(x = datetime)) + 
  geom_line(aes(y = dow_jones), color = "green") + 
  ggtitle("Dow Jones Index") + 
  labs(subtitle = "Fall 2021") + 
  xlab("Date") + 
  ylab("US Dollars")

ma_temp_plot_fall = ggplot(data = df_fall, aes(x = datetime)) +
  geom_line(aes(y = seattle_matemp), color = "darkmagenta") +
  geom_line(aes(y = chicago_matemp), color = "darkred") +
  geom_line(aes(y = nyc_matemp), color = "steelblue") + 
  ggtitle("Mean Average of Tempurature") + 
  labs(subtitle = "Fall 2021") + 
  xlab("Date") + 
  ylab("Degrees Fahrenheit")

# New graphs on daily changes
dow_delta = ggplot(data = df, aes(x = datetime)) + 
  geom_line(aes(y = dow_jones_delta), color = "purple") +
  xlab("Date") +
  ylab("Daily Change")
nasdaq_delta = ggplot(data = df, aes(x = datetime)) + 
  geom_line(aes(y = nasdaq_delta), color = "purple") +
  xlab("Date") +
  ylab("Daily Change") +
  geom_smooth(aes(y = nasdaq_delta),method = lm, se = TRUE)

cut_off = quantile(df$dow_jones_delta, na.rm = T, probs = .75)

