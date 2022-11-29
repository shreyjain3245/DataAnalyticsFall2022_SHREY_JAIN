# Load Libraries
library(randomForest)
library(class)
library(ggplot2)

# Data set 2: Obesity levels based on eating
# Set directory and read absenteeism at work data
setwd("C:/Users/Shrey Jain/Documents/Study/Data Analytics/DataAnalyticsFall2022_SHREY_JAIN/Lab/DataAnalytics_A7_SHREY_JAIN/")
dataset2 <- read.csv('ObesityDataSet_raw_and_data_sinthetic.csv')
View(dataset2)


# Question 1: Exploratory Data Analysis
summary(dataset2$NObeyesdad)
unique(dataset2$NObeyesdad)
ggplot(dataset2, aes(x=reorder(dataset2$NObeyesdad, dataset2$NObeyesdad, function(x)-length(x)))) + geom_bar(fill='green') +  labs(x='NObeyesdad', y='Count')
ggplot(dataset2, aes(x=reorder(dataset2$Gender, dataset2$Gender, function(x)-length(x)))) + geom_bar(fill='blue') +  labs(x='Gender', y='Count')

# Question 2: Model Development
# Logistic regression
training_dataset2 <- sample(dim(dataset2)[1], 0.7*dim(dataset2)[1])
training_dataset2 <- sample(dim(dataset2_without_outlier)[1], 0.7*dim(dataset2_without_outlier)[1])
linear_model1 <- lm(dataset2$Absenteeism.time.in.hours~., data=dataset2, subset = training_dataset2)
linear_model2 <- lm(dataset2_without_outlier$Absenteeism.time.in.hours~., data=dataset2_without_outlier, subset = training_dataset2)
summary(linear_model1)
summary(linear_model2)
sqrt(mean((dataset2$Absenteeism.time.in.hours-predict(linear_model1, dataset2))[-training_dataset2]^2))
sqrt(mean((dataset2_without_outlier$Absenteeism.time.in.hours-predict(linear_model2, dataset2_without_outlier))[-training_dataset2]^2))

# Random Forest
training_dataset3 <- dataset2_without_outlier[training_dataset2,]
random_forest_model <- randomForest(Absenteeism.time.in.hours~., data=training_dataset3)
test_x <- dataset2_without_outlier[-training_dataset2, -dim(dataset2_without_outlier)[2]]
test_y <- dataset2_without_outlier[-training_dataset2, dim(dataset2_without_outlier)[2]]
random_forest_rms <- sqrt(mean((test_y - predict(random_forest_model, test_x))^2))
random_forest_rms
plot(sqrt(random_forest_model$mse), main = "Root mean square error plot using absenteeism dataset for random forest", xlab = "Number of trees", ylab = "RMS")

# KNN regression model
training_dataset4 <- dataset2_without_outlier[training_dataset2,]
train_x <- dataset2_without_outlier[training_dataset2,]
test_x <- dataset2_without_outlier[-training_dataset2,]
knn_model <- knn(train=train_x, test=test_x, k=sqrt(nrow(dataset2_without_outlier)), cl = train_x$Absenteeism.time.in.hours)
knn_rms <- mean(train_x$Absenteeism.time.in.hours != knn_model)  
knn_rms
