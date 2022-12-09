library(dplyr)
#Install Package
install.packages("e1071")
install.packages("Metrics")
#Load Library
library(e1071)


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
hist(final_filtered$incident_response_seconds_qy, prob=T, main='Incident Response Time', col = "blue", xlab = "Response time (in seconds)", ylab = "Frequency")
boxplot(final_filtered$incident_response_seconds_qy, main='Incident Response Time')

hist(final_no_outlier$incident_response_seconds_qy, prob=T, main='Incident Response Time', col = "green", xlab = "Response time (in seconds)", ylab = "Frequency")
boxplot(final_no_outlier$incident_response_seconds_qy, main='Incident Response Time')

#Modelling
final_no_outlier <- final_no_outlier[final_no_outlier$zipcode=="10019" | final_no_outlier$zipcode=="10023" | final_no_outlier$zipcode=="10024" | final_no_outlier$zipcode=="10025", ]
#remove from EMS dataset
final_no_outlier <- subset(final_no_outlier, select= -c(cad_incident_id,borough, incident_dispatch_area, incident_dt, first_assign_dt, first_act_dt, first_on_scene_dt, first_to_hosp_dt, first_hosp_arrival_dt, incident_close_dt, incident_year ))
#remove from weather dataset
final_no_outlier <- subset(final_no_outlier, select= -c(dow, date_m, STATION, NAME, DATE))

final_no_outlier$valid_incident_rspns_time_indc[final_no_outlier$valid_incident_rspns_time_indc == 'Y'] <- 1
final_no_outlier$valid_incident_rspns_time_indc[final_no_outlier$valid_incident_rspns_time_indc == 'N'] <- 0


final_no_outlier$valid_dispatch_rspns_time_indc[final_no_outlier$valid_dispatch_rspns_time_indc == 'Y'] <- 1
final_no_outlier$valid_dispatch_rspns_time_indc[final_no_outlier$valid_dispatch_rspns_time_indc == 'N'] <- 0


final_no_outlier$held_indicator[final_no_outlier$held_indicator == 'Y'] <- 1
final_no_outlier$held_indicator[final_no_outlier$held_indicator == 'N'] <- 0

final_no_outlier$reopen_indicator[final_no_outlier$reopen_indicator == 'Y'] <- 1
final_no_outlier$reopen_indicator[final_no_outlier$reopen_indicator == 'N'] <- 0

final_no_outlier$special_event_indicator[final_no_outlier$special_event_indicator == 'Y'] <- 1
final_no_outlier$special_event_indicator[final_no_outlier$special_event_indicator == 'N'] <- 0

final_no_outlier$standby_indicator[final_no_outlier$standby_indicator == 'Y'] <- 1
final_no_outlier$standby_indicator[final_no_outlier$standby_indicator == 'N'] <- 0


final_no_outlier$transfer_indicator[final_no_outlier$transfer_indicator == 'Y'] <- 1
final_no_outlier$transfer_indicator[final_no_outlier$transfer_indicator == 'N'] <- 0

sapply(final_no_outlier, class)
dim(final_no_outlier)
final_no_outlier <- na.omit(final_no_outlier)
dim(final_no_outlier)

final_no_outlier <- select(final_no_outlier, -WT03)
final_no_outlier <- select(final_no_outlier, -WT04)
final_no_outlier <- select(final_no_outlier, -valid_incident_rspns_time_indc)
final_no_outlier <- select(final_no_outlier, -valid_dispatch_rspns_time_indc)
final_no_outlier <- select(final_no_outlier, -WT14)
sapply(lapply(final_no_outlier, unique), length)

#Linear Regression
set.seed(7)
model_linear <- lm(incident_response_seconds_qy~., data = final_no_outlier)
summary(model_linear)
sqrt(mean(model_linear$residuals^2))
#5.84613e-12

#Regression with SVM
set.seed(7)
train <- sample(dim(final_no_outlier)[1], 18000)
modelsvm = svm(incident_response_seconds_qy~dispatch_response_seconds_qy+incident_travel_tm_seconds_qy, subset = train, data = final_no_outlier)

#Predict using SVM regression
predYsvm = predict(modelsvm, final_no_outlier)

#Overlay SVM Predictions on Scatter Plot
plot(final_no_outlier$dispatch_response_seconds_qy, main = "Scatter Plot")
points(final_no_outlier$dispatch_response_seconds_qy, predYsvm, col = "red", pch=16)
summary(modelsvm)

library(Metrics)
RMSEsvm=rmse(predYsvm,final_no_outlier$dispatch_response_seconds_qy)
RMSEsvm

### Import libraries
library(randomForest)
library(ggplot2)

set.seed(4543)
rf.fit <- randomForest(incident_response_seconds_qy~SNOW+PRCP, data=final_no_outlier, keep.forest=FALSE, importance=TRUE)
rf.fit
