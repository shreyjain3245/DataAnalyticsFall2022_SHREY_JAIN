# Load Libraries
library(randomForest)
library(class)

# Data set 1: Absenteeism At Work
# Set directory and read absenteeism at work data
setwd("C:/Users/Shrey Jain/Documents/Study/Data Analytics/DataAnalyticsFall2022_SHREY_JAIN/Lab/DataAnalytics_A7_SHREY_JAIN/")
dataset1 <- read.csv('Absenteeism_at_work.csv', sep=';')
View(dataset1)


# Question 1: Exploratory Data Analysis
#IQR
range <- IQR(dataset1$Absenteeism.time.in.hours)
range
Tmin = fivenum(dataset1$Absenteeism.time.in.hours)[2] - (1.5*range)
Tmax = fivenum(dataset1$Absenteeism.time.in.hours)[4] + (1.5*range)
Tmin
Tmax
# Finding outlier
length(dataset1$Absenteeism.time.in.hours[which(dataset1$Absenteeism.time.in.hours <= Tmin | dataset1$Absenteeism.time.in.hours >= Tmax)])
# Removing outlier
dataset1_without_outlier <- dataset1[dataset1$Absenteeism.time.in.hours >= Tmin & dataset1$Absenteeism.time.in.hours <= Tmax,]
# Histograms
hist(dataset1$Absenteeism.time.in.hours, main="Absenteeism Time Histogram", xlab="Absenteeism Time in hours", col="blue", breaks = 20)
hist(dataset1_without_outlier$Absenteeism.time.in.hours, main="Absenteeism Time Histogram", xlab="Absenteeism Time in hours", col="green", breaks = 20)
# Boxplots
boxplot(dataset1$Absenteeism.time.in.hours, main='Absenteeism Time Boxplot')
boxplot(dataset1_without_outlier$Absenteeism.time.in.hours, main='Absenteeism Time Boxplot')


# Question 2: Model Development
# Linear regression
training_dataset1 <- sample(dim(dataset1)[1], 0.7*dim(dataset1)[1])
training_dataset2 <- sample(dim(dataset1_without_outlier)[1], 0.7*dim(dataset1_without_outlier)[1])
linear_model1 <- lm(dataset1$Absenteeism.time.in.hours~., data=dataset1, subset = training_dataset1)
linear_model2 <- lm(dataset1_without_outlier$Absenteeism.time.in.hours~., data=dataset1_without_outlier, subset = training_dataset2)
summary(linear_model1)
summary(linear_model2)
sqrt(mean((dataset1$Absenteeism.time.in.hours-predict(linear_model1, dataset1))[-training_dataset1]^2))
sqrt(mean((dataset1_without_outlier$Absenteeism.time.in.hours-predict(linear_model2, dataset1_without_outlier))[-training_dataset2]^2))

# Random Forest
training_dataset3 <- dataset1_without_outlier[training_dataset2,]
random_forest_model <- randomForest(Absenteeism.time.in.hours~., data=training_dataset3)
test_x <- dataset1_without_outlier[-training_dataset2, -dim(dataset1_without_outlier)[2]]
test_y <- dataset1_without_outlier[-training_dataset2, dim(dataset1_without_outlier)[2]]
random_forest_rms <- sqrt(mean((test_y - predict(random_forest_model, test_x))^2))
random_forest_rms
plot(sqrt(random_forest_model$mse), main = "Root mean square error plot using absenteeism dataset for random forest", xlab = "Number of trees", ylab = "RMS")

# KNN regression model
training_dataset4 <- dataset1_without_outlier[training_dataset2,]
train_x <- dataset1_without_outlier[training_dataset2,]
test_x <- dataset1_without_outlier[-training_dataset2,]
knn_model <- knn(train=train_x, test=test_x, k=sqrt(nrow(dataset1_without_outlier)), cl = train_x$Absenteeism.time.in.hours)
knn_rms <- mean(train_x$Absenteeism.time.in.hours != knn_model)  
knn_rms
