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