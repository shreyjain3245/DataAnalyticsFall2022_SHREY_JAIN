# Assignment 4


# Importing required libraries
library(ggplot2)
library(rpart)
library(rpart.plot)
library(maptree)
library(dplyr)
# Set directory and read sales Sales data
setwd("C:/Users/Shrey Jain/Documents/Study/Data Analytics/DataAnalyticsFall2022_SHREY_JAIN/Lab/DataAnalytics_A4_SHREY_JAIN/")
sales <- read.csv("nyc_Citywide_Annualized_Calendar_Sales_Update.csv")
View(sales)
# Data Pre-processing
# sales <- sales[sales$ZIP.CODE > 11200 & sales$ZIP.CODE < 11257,] - Identify unique values of BOROUGH - Used zip codes as mentioned on the web
# unique(sales$BOROUGH) - It seems BOROUGH = 3 also means BROOKLYN
sales <- sales[sales$BOROUGH == "BROOKLYN" | sales$BOROUGH == "3",]
head(sales)
# Remove NAs
sales <- sales[sales$GROSS.SQUARE.FEET > 0 & !is.na(sales$GROSS.SQUARE.FEET),]
sales <- sales[sales$LAND.SQUARE.FEET > 0 & !is.na(sales$LAND.SQUARE.FEET),]
sales <- sales[sales$SALE.PRICE > 0 & !is.na(sales$SALE.PRICE),]
# Removing commas
sales$GROSS.SQUARE.FEET <- gsub(",","",as.character(sales$GROSS.SQUARE.FEET))
sales$LAND.SQUARE.FEET <- gsub(",","",as.character(sales$LAND.SQUARE.FEET))
# Converting value as numeric
sales$GROSS.SQUARE.FEET <- as.numeric(sales$GROSS.SQUARE.FEET)
sales$LAND.SQUARE.FEET <- as.numeric(sales$LAND.SQUARE.FEET)



# Question 1 (A)
# Exploratory Data Analysis (Checking Five nums and plotting histogram)
summary(sales$SALE.PRICE)
hist(sales$SALE.PRICE, xlim=c(0, 2000000), breaks=10000, main="Sale Price Histogram", xlab="Sale Price", col="blue")
summary(sales$LAND.SQUARE.FEET)
hist(sales$LAND.SQUARE.FEET, xlim=c(0, 10000), breaks=10000, main="Land Sq Ft Histogram", xlab="Land Sq Ft", col="green")
summary(sales$GROSS.SQUARE.FEET)
hist(sales$GROSS.SQUARE.FEET, xlim=c(0, 10000), breaks=10000, main="Gross Sq Ft Histogram", xlab="Gross Sq Ft", col="red")



# Question 1 (B)
#IQR
IQR(sales$SALE.PRICE)
Tmin = fivenum(sales$SALE.PRICE)[2] - (1.5*IQR(sales$SALE.PRICE))
Tmax = fivenum(sales$SALE.PRICE)[4] + (1.5*IQR(sales$SALE.PRICE))
# Find outlier
length(sales$SALE.PRICE[which(sales$SALE.PRICE < Tmin | sales$SALE.PRICE > Tmax)])
# Remove outlier
sales$SALE.PRICE[which(sales$SALE.PRICE > Tmin & sales$SALE.PRICE < Tmax)]
# Similarly for other two columns
IQR(sales$LAND.SQUARE.FEET)
Tmin = fivenum(sales$LAND.SQUARE.FEET)[2] - (1.5*IQR(sales$LAND.SQUARE.FEET))
Tmax = fivenum(sales$LAND.SQUARE.FEET)[4] + (1.5*IQR(sales$LAND.SQUARE.FEET))
IQR(sales$GROSS.SQUARE.FEET)
Tmin = fivenum(sales$GROSS.SQUARE.FEET)[2] - (1.5*IQR(sales$GROSS.SQUARE.FEET))
Tmax = fivenum(sales$GROSS.SQUARE.FEET)[4] + (1.5*IQR(sales$GROSS.SQUARE.FEET))

# Cooks Distance
cooks_outliers <- data.frame(sales$SALE.PRICE, sales$LAND.SQUARE.FEET, sales$GROSS.SQUARE.FEET)
colnames(cooks_outliers) <- c("Sale_Price", "Land_Sq_Ft", "Gross_Sq_Ft")
cooks_outliers <- na.omit(cooks_outliers)

model1 <- lm(Sale_Price~Land_Sq_Ft+Gross_Sq_Ft, data=cooks_outliers)
summary(model1)
plot(model1, pch=9, col="blue", which=c(4))
cooksD <- cooks.distance(model1)

influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential
names_of_influential <- names(influential)
length(names_of_influential)
outliers <- cooks_outliers[names_of_influential,]
data_Without_outliers <- cooks_outliers %>% anti_join(outliers)

model2 <- lm(Sale_Price~Land_Sq_Ft+Gross_Sq_Ft, data=data_Without_outliers)
summary(model1)
plot(model2, pch=9, col="green", which=c(4))
cooksD2 <- cooks.distance(model2)

total_outliers <- length(cooksD) - length(cooksD2)
total_outliers



# Question 1 (C) Multivariate Regression
#First sample
training_dataset <- sample(dim(sales)[1], 1800)
model3_linear <- lm(SALE.PRICE~GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data=sales, subset = training_dataset)
summary(model3_linear)
plot(SALE.PRICE~GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data=sales)
abline(model3_linear, col="green")

model3_quad <- lm(SALE.PRICE~poly(GROSS.SQUARE.FEET,2,raw=TRUE) + poly(LAND.SQUARE.FEET,2,raw=TRUE), data = sales, subset = training_dataset)
summary(model3_quad)
abline(model3_quad, col="blue")

model3_cube <- lm(SALE.PRICE~poly(GROSS.SQUARE.FEET,3,raw=TRUE) + poly(LAND.SQUARE.FEET,3,raw=TRUE), data = sales, subset = training_dataset)
summary(model3_cube)
abline(model3_cube, col="red")

# Second sample
training_dataset2 <- sample(dim(sales)[1], 1800)
model4_linear <- lm(SALE.PRICE~GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data=sales, subset = training_dataset2)
summary(model4_linear)
plot(SALE.PRICE~GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data=sales)
abline(model4_linear, col="green")

model4_quad <- lm(SALE.PRICE~poly(GROSS.SQUARE.FEET,2,raw=TRUE) + poly(LAND.SQUARE.FEET,2,raw=TRUE), data = sales, subset = training_dataset2)
summary(model4_quad)
abline(model4_quad, col="blue")

model4_cube <- lm(SALE.PRICE~poly(GROSS.SQUARE.FEET,3,raw=TRUE) + poly(LAND.SQUARE.FEET,3,raw=TRUE), data = sales, subset = training_dataset2)
summary(model4_cube)
abline(model4_cube, col="red")

# Third sample
training_dataset3 <- sample(dim(sales)[1], 1800)
model5_linear <- lm(SALE.PRICE~GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data=sales, subset = training_dataset3)
summary(model5_linear)
plot(SALE.PRICE~GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data=sales)
abline(model5_linear, col="green")

model5_quad <- lm(SALE.PRICE~poly(GROSS.SQUARE.FEET,2,raw=TRUE) + poly(LAND.SQUARE.FEET,2,raw=TRUE), data = sales, subset = training_dataset3)
summary(model5_quad)
abline(model5_quad, col="blue")

model5_cube <- lm(SALE.PRICE~poly(GROSS.SQUARE.FEET,3,raw=TRUE) + poly(LAND.SQUARE.FEET,3,raw=TRUE), data = sales, subset = training_dataset3)
summary(model5_cube)
abline(model5_cube, col="red")



# Question 1 (D) # Decision tree
decision_tree_data <- data.frame(sales$SALE.PRICE, sales$LAND.SQUARE.FEET)
colnames(decision_tree_data) <- c("Sale_Price", "Land_Sq_Ft")
decision_tree_data <- na.omit(decision_tree_data)
# Data split into training and testing
help(sample)
samples <- sample(150, 120)
train <- decision_tree_data[samples, ]
test <- decision_tree_data[-samples, ]

dtree <- rpart(Sale_Price~., train, method="class")
rpart.plot(dtree, box.palette = "grey")
prp(dtree, faclen = 2)
draw.tree(dtree,cex=1)



# Question 2 (A) 
model_quad <- lm(SALE.PRICE~poly(GROSS.SQUARE.FEET,2,raw=TRUE) + poly(LAND.SQUARE.FEET,2,raw=TRUE), data = sales)
plot(SALE.PRICE~GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data=sales)
abline(model_quad, col="blue")

model_quad_2 <- lm(Sale_Price~poly(Gross_Sq_Ft,2,raw=TRUE) + poly(Land_Sq_Ft,2,raw=TRUE), data = data_Without_outliers)
plot(Sale_Price~Gross_Sq_Ft+Land_Sq_Ft, data=data_Without_outliers)
abline(model_quad_2, col="green")

# Question 2 (B)
summary(model_quad_2)