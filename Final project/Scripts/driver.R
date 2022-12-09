ems <- a
weather <- b
final <- merged_data

#NA Removal
dim(ems)
ems_no_na <- na.omit(ems)
dim(ems_no_na)
#Data Filtering as we only have Manhattan NYC weather data
final_filtered <- final[final$borough=="MANHATTAN", ] 
#Removing two columns that have the most NA values
final_filtered <- subset(final_filtered, select= -c(TSUN, TAVG))

#Outlier Removal - IQR
IQR(final_filtered$incident_response_seconds_qy, na.rm=TRUE)
Tmin = fivenum(final_filtered$incident_response_seconds_qy)[2] - (1.5*IQR(final_filtered$incident_response_seconds_qy, na.rm = TRUE))
Tmin
Tmax = fivenum(final_filtered$incident_response_seconds_qy)[4] + (1.5*IQR(final_filtered$incident_response_seconds_qy, na.rm = TRUE))
Tmax
# Find outlier count
length(final_filtered$incident_response_seconds_qy[which(final_filtered$incident_response_seconds_qy < Tmin | final_filtered$incident_response_seconds_qy > Tmax)])
# Remove outlier
final_filtered[final_filtered$incident_response_seconds_qy > Tmin & final_filtered$incident_response_seconds_qy < Tmax,]
dim(final_filtered)

final_no_outlier <- final_filtered[final_filtered$incident_response_seconds_qy <= Tmax,]
dim(final_no_outlier)

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

#Linear Regression
model_linear <- lm(incident_response_seconds_qy~., data = final_dataset_without_outliers)
summary(model_linear)
sqrt(mean(model_linear$residuals^2))

#Install Package
install.packages("e1071")

#Load Library
library(e1071)

final_dataset_without_outliers$PRCP
#Regression with SVM
modelsvm = svm(incident_response_seconds_qy~SNOW+PRCP,final_dataset_without_outliers)

#Predict using SVM regression
predYsvm = predict(modelsvm, data)

#Overlay SVM Predictions on Scatter Plot
points(data$X, predYsvm, col = "red", pch=16)