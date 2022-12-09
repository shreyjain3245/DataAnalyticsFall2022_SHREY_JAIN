library(labelled)
library(dplyr)
library(lubridate)
#find your working directory for R
getwd()

#change your working directory to the location where your files are
#  (make sure you put the folder path for your new directory between the 
#   quotation marks)
setwd("C:/Users/Shrey Jain/Documents/Study/Data Analytics/DataAnalyticsFall2022_SHREY_JAIN/Final project/Data/Weather")

#loading weather data in CSV format
b<-read.csv("GHCND_NY_Central_Park_20080101_20161231.csv",header=TRUE)

#labelling different variables
var_label(b$AWND) <- "Average Daily Wind Speed (mi/h)"
var_label(b$FMTM) <- "Time of Fastest 1-minute wind (HHMM)"
var_label(b$PGTM) <- "Peak Gust Time (HHMM)"
var_label(b$PRCP) <- "Precipitation (in)"
var_label(b$SNOW) <- "Snowfall (in)"
var_label(b$SNWD) <- "Snow depth (in)"
var_label(b$TMAX) <- "Max Temp (F)"
var_label(b$TMIN) <- "Min Temp (F)"
var_label(b$WDF2) <- "Direction of fastest 2-minute wind (degrees)"
var_label(b$WDF5) <- "Direction of fastest 5-second wind (degrees)"
var_label(b$WSF2) <- "Fastest 2-minute wind speed (mph)"
var_label(b$WSF5) <- "Fastest 5-second wind speed (mph)"
var_label(b$WT01) <- "Fog"
var_label(b$WT02) <- "Heavy Fog"
var_label(b$WT03) <- "Thunder"
var_label(b$WT04) <- "Ice pellets"
var_label(b$WT05) <- "Hail"
var_label(b$WT06) <- "Glaze or rime"
var_label(b$WT07) <- "Dust or sand"
var_label(b$WT08) <- "Smoke or haze"
var_label(b$WT09) <- "Blowing or drifting snow"
var_label(b$WT11) <- "High or damaging winds"
var_label(b$WT13) <- "Mist"
var_label(b$WT14) <- "Drizzle"
var_label(b$WT16) <- "Rain"
var_label(b$WT17) <- "Freezing Rain"
var_label(b$WT18) <- "Snow"
var_label(b$WT19) <- "Unknown Source of Precipitation"
var_label(b$WT22) <- "Freezing Fog"

#replace blanks with zeros for binary variables
wts <- grep('^WT', names(b), value = TRUE) #creates a new vector of all the binary variables related to weather type
b[wts][is.na(b[wts])] <- 0 #replaces all of the "NA" values in those binary variables with 0s

#create date variable for merging
a$date_m <- as.Date(a$incident_dt, "%m/%d/%Y")

#reformat date variable in weather data from character to date
b$date_m <- as.Date(b$DATE, "%m/%d/%Y")

#create month category
b$month <- floor_date(b$date_m, "month") #uses the floor_date command within dplyr

#create new dataframe that stores total precipitation by month
g <- b %>% #specifies the b data frame as the reference data frame
  group_by(month) %>%
  summarise(precip_total = sum(PRCP))

#plot that new dataframe by month and year
ggplot(g, aes(month, precip_total)) +
  geom_line() +
  xlab("Year") + ylab("Total Rainfall (inches)")

#merge data at the incident level
merged_data <- left_join(a,b,by="date_m")