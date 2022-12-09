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

merged_data

setwd("C:/Users/Shrey Jain/Documents/Study/Data Analytics/DataAnalyticsFall2022_SHREY_JAIN/Final project/Data/EMS")
ems_data <- readRDS("training_final.RDS")
dim(ems_data)
ems_data_without_na <- na.omit(ems_data)
dim(ems_data_without_na)
# remove na in r - remove rows - na.omit function / option
a<-na.omit(a)
dim(a)
#Filtered the merged_data according to borough
merged_data_manhattan <- merged_data[merged_data$borough=="MANHATTAN", ] 
View(merged_data_manhattan)

#Removing columns with most NA values
merged_data_manhattan <- subset(merged_data_manhattan, select= -c(TAVG, TSUN))
View(merged_data_manhattan)


hist(merged_data_manhattan$incident_response_seconds_qy,seq(0, 40000, 1000.0), main='Incident Response Time', col = "blue", xlab = "Response time (in seconds)", ylab = "Frequency")
boxplot(merged_data_manhattan$incident_response_seconds_qy, main='Incident Response Time', col = "blue")

#Removing Outliers
# Finding outliers with interquartile range
iqr <- IQR(merged_data_manhattan$incident_response_seconds_qy, na.rm=TRUE)
iqr
lowerInnerFence <- quantile(merged_data_manhattan$incident_response_seconds_qy, 0.25,na.rm=TRUE) - 1.5 * iqr
lowerInnerFence
upperInnerFence <- quantile(merged_data_manhattan$incident_response_seconds_qy, 0.75, na.rm=TRUE) + 1.5 * iqr
upperInnerFence
dim(merged_data_manhattan)
final_dataset_without_outliers <- merged_data_manhattan[merged_data_manhattan$incident_response_seconds_qy <= upperInnerFence,]
View(final_dataset_without_outliers)
dim(final_dataset_without_outliers)

#Plotting the data to check for outliers
hist(final_dataset_without_outliers$incident_response_seconds_qy, prob=T, main='Incident Response Time', col = "green", xlab = "Response time (in seconds)", ylab = "Frequency")
boxplot(final_dataset_without_outliers$incident_response_seconds_qy, main='Incident Response Time')

#Modelling
final_dataset_without_outliers <- final_dataset_without_outliers[final_dataset_without_outliers$zipcode=="10019" | final_dataset_without_outliers$zipcode=="10023" | final_dataset_without_outliers$zipcode=="10024" | final_dataset_without_outliers$zipcode=="10025", ]
#remove from EMS dataset
final_dataset_without_outliers <- subset(final_dataset_without_outliers, select= -c(cad_incident_id,borough, incident_dispatch_area, incident_dt, first_assign_dt, first_act_dt, first_on_scene_dt, first_to_hosp_dt, first_hosp_arrival_dt, incident_close_dt, incident_year ))
#remove from weather dataset
final_dataset_without_outliers <- subset(final_dataset_without_outliers, select= -c(dow, date_m, STATION, NAME, DATE))
View(final_dataset_without_outliers)


final_dataset_without_outliers$valid_incident_rspns_time_indc[final_dataset_without_outliers$valid_incident_rspns_time_indc == 'Y'] <- 1
final_dataset_without_outliers$valid_incident_rspns_time_indc[final_dataset_without_outliers$valid_incident_rspns_time_indc == 'N'] <- 0


final_dataset_without_outliers$valid_dispatch_rspns_time_indc[final_dataset_without_outliers$valid_dispatch_rspns_time_indc == 'Y'] <- 1
final_dataset_without_outliers$valid_dispatch_rspns_time_indc[final_dataset_without_outliers$valid_dispatch_rspns_time_indc == 'N'] <- 0


final_dataset_without_outliers$held_indicator[final_dataset_without_outliers$held_indicator == 'Y'] <- 1
final_dataset_without_outliers$held_indicator[final_dataset_without_outliers$held_indicator == 'N'] <- 0

final_dataset_without_outliers$reopen_indicator[final_dataset_without_outliers$reopen_indicator == 'Y'] <- 1
final_dataset_without_outliers$reopen_indicator[final_dataset_without_outliers$reopen_indicator == 'N'] <- 0

final_dataset_without_outliers$special_event_indicator[final_dataset_without_outliers$special_event_indicator == 'Y'] <- 1
final_dataset_without_outliers$special_event_indicator[final_dataset_without_outliers$special_event_indicator == 'N'] <- 0

final_dataset_without_outliers$standby_indicator[final_dataset_without_outliers$standby_indicator == 'Y'] <- 1
final_dataset_without_outliers$standby_indicator[final_dataset_without_outliers$standby_indicator == 'N'] <- 0


final_dataset_without_outliers$transfer_indicator[final_dataset_without_outliers$transfer_indicator == 'Y'] <- 1
final_dataset_without_outliers$transfer_indicator[final_dataset_without_outliers$transfer_indicator == 'N'] <- 0

sapply(final_dataset_without_outliers, class)
dim(final_dataset_without_outliers)

final_dataset_without_outliers <- na.omit(final_dataset_without_outliers)
library(dplyr)

final_dataset_without_outliers <- select(final_dataset_without_outliers, -WT03)
final_dataset_without_outliers <- select(final_dataset_without_outliers, -WT04)
final_dataset_without_outliers <- select(final_dataset_without_outliers, -valid_incident_rspns_time_indc)
final_dataset_without_outliers <- select(final_dataset_without_outliers, -valid_dispatch_rspns_time_indc)
final_dataset_without_outliers <- select(final_dataset_without_outliers, -WT14)
sapply(lapply(final_dataset_without_outliers, unique), length)

View(final_dataset_without_outliers)

model_linear <- lm(incident_response_seconds_qy~., data = final_dataset_without_outliers)
summary(model_linear)

#Install Package
install.packages("e1071")

#Load Library
library(e1071)

#Regression with SVM
modelsvm = svm(incident_response_seconds_qy~.,final_dataset_without_outliers)

#Predict using SVM regression
predYsvm = predict(modelsvm, data)

#Overlay SVM Predictions on Scatter Plot
points(data$X, predYsvm, col = "red", pch=16)