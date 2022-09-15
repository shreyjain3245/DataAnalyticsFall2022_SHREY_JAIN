install.packages("MASS")
install.packages("ISLR")
install.packages("ISLR2")

library(MASS)
attach(Boston)

?Boston
help(Boston)
head(Boston)
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston)
summary(Boston$crim)

library(ISLR)
data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median(Auto$weight)


help(read.csv)
data <- read.csv(file.choose(), header = T)
data

View(data)
dim(data)

summary(data$EPI)
boxplot(data$EPI)
fivenum(data$EPI, na.rm = TRUE)
stem(data$EPI)
hist(data$EPI)

matrix(1:12,byrow = TRUE, ncol = 6)



help(factor)
