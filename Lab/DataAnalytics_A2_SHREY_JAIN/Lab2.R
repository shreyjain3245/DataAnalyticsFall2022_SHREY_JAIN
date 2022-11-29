# Lab2 Par1a: Measure of Central Tendency

EPI_data <- read.csv("2010EPI_data.csv", skip = 1, header = TRUE)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
# Generate central tendency values & boxplots
EPI <- EPI[!is.na(EPI)]
DALY <- DALY[!is.na(DALY)]
summary(EPI)
summary(DALY)
boxplot(EPI)
boxplot(DALY)
# Generate central tendency values & boxplots
NOX_pt <- NOX_pt[!is.na(NOX_pt)]
SO2_pt <- SO2_pt[!is.na(SO2_pt)]
summary(NOX_pt)
summary(SO2_pt)
OZONE_pt <- OZONE_pt[!is.na(OZONE_pt)]
boxplot(OZONE_pt)
WQI_pt <- WQI_pt[!is.na(WQI_pt)]
boxplot(WQI_pt)
# Generate central tendency values & boxplots
summary(CLIMATE)
summary(AGRICULTURE)
boxplot(FISHERIES)
boxplot(NMVOC_pt)
# Generate the boxplot
boxplot(ENVHEALTH, ECOSYSTEM)
# Generate the qqplot
qqplot(ENVHEALTH, ECOSYSTEM)

# Lab2 Par1b: Regression Exercises
EPI_data <- read.csv("EPI_data.csv")
attach(EPI_data)
# 
boxplot(ENVHEALTH, DALY, AIR_H, WATER_H)
lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)
pENV <- predict(lmENVH, NEW, interval = "prediction")
cENV <- predict(lmENVH, NEW, interval = "confidence")


# Lab2 Par1a: Measure of Central Tendency

EPI_data <- read.csv("2010EPI_data.csv", skip = 1, header = TRUE)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
# Generate central tendency values & boxplots
EPI <- EPI[!is.na(EPI)]
DALY <- DALY[!is.na(DALY)]
summary(EPI)
summary(DALY)
boxplot(EPI)
boxplot(DALY)
# Generate central tendency values & boxplots
NOX_pt <- NOX_pt[!is.na(NOX_pt)]
SO2_pt <- SO2_pt[!is.na(SO2_pt)]
summary(NOX_pt)
summary(SO2_pt)
OZONE_pt <- OZONE_pt[!is.na(OZONE_pt)]
boxplot(OZONE_pt)
WQI_pt <- WQI_pt[!is.na(WQI_pt)]
boxplot(WQI_pt)
# Generate central tendency values & boxplots
summary(CLIMATE)
summary(AGRICULTURE)
boxplot(FISHERIES)
boxplot(NMVOC_pt)
# Generate the boxplot
boxplot(ENVHEALTH, ECOSYSTEM)
# Generate the qqplot
qqplot(ENVHEALTH, ECOSYSTEM)

# Lab2 Par1b: Regression Exercises
EPI_data <- read.csv("EPI_data.csv")
attach(EPI_data)
# 
boxplot(ENVHEALTH, DALY, AIR_H, WATER_H)
lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)
pENV <- predict(lmENVH, NEW, interval = "prediction")
cENV <- predict(lmENVH, NEW, interval = "confidence")