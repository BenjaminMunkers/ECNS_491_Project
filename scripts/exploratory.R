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
  geom_line(aes(y = seattle_temp), color = "magenta") +
  geom_line(aes(y = chicago_temp), color = "darkred") +
  geom_line(aes(y = nyc_temp), color = "steelblue")
#cloud cover
cloud_plot1 = ggplot(data = df, aes(x = datetime)) +
  geom_line(aes(y = seattle_cloudcover), color = "magenta") +
  geom_line(aes(y = chicago_cloudcover), color = "darkred") +
  geom_line(aes(y = nyc_cloudcover), color = "steelblue")
#and precipitation coverage graphs
precip_plot1 = ggplot(data = df, aes(x = datetime)) +
  geom_line(aes(y = seattle_precipcover), color = "magenta") +
  geom_line(aes(y = chicago_precipcover), color = "darkred") +
  geom_line(aes(y = nyc_precipcover), color = "steelblue")
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
df$seattle_macloud = 0
df$seattle_matemp = 0
df$nyc_macloud = 0
df$nyc_matemp = 0
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

