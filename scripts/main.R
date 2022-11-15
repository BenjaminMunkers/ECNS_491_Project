### Main Script to run other scripts from
# set wd to scripts folder
setwd(paste(getwd(), "/scripts", sep = ""))
source("datawrangling.R")

wd = str_replace(getwd(), "Data", "scripts")
setwd(wd)

source("exploratory.R")
