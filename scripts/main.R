### Main Script to run other scripts from

source("datawrangling.R")

wd = str_replace(getwd(), "Data", "scripts")
setwd(wd)

source("exploratory.R")
