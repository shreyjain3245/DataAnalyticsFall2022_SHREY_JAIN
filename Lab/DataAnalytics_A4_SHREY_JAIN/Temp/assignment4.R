# load packages
library(ggplot2)
library(rpart)
library(rpart.plot)
library(maptree)

# load data
nyc <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update.csv")
View(nyc)

# clean up data
nyc <- nyc[nyc$BOROUGH == "MANHATTAN" | nyc$BOROUGH == "1",]
nyc$GROSS.SQUARE.FEET <- gsub(",","",as.character(nyc$GROSS.SQUARE.FEET))
nyc$GROSS.SQUARE.FEET <- as.numeric(nyc$GROSS.SQUARE.FEET)
nyc$LAND.SQUARE.FEET <- gsub(",","",as.character(nyc$LAND.SQUARE.FEET))
nyc$LAND.SQUARE.FEET <- as.numeric(nyc$LAND.SQUARE.FEET)
nyc <- nyc[nyc$GROSS.SQUARE.FEET > 0 & !is.na(nyc$GROSS.SQUARE.FEET),]
nyc <- nyc[nyc$LAND.SQUARE.FEET > 0 & !is.na(nyc$LAND.SQUARE.FEET),]
nyc <- nyc[nyc$YEAR.BUILT > 0 & !is.na(nyc$YEAR.BUILT),]
nyc <- nyc[nyc$SALE.PRICE > 0 & !is.na(nyc$SALE.PRICE),]

# no scientific notation
options(scipen = 100)

### QUESTION 1

# eda
summary(nyc$SALE.PRICE)
hist(nyc$SALE.PRICE, xlim=c(0, 2000000), breaks=10000, main="Sale Price Histogram", xlab="Sale Price")

summary(nyc$GROSS.SQUARE.FEET)
hist(nyc$GROSS.SQUARE.FEET, xlim=c(0, 40000), breaks=100000, main="Gross Sq Ft Histogram", xlab="Gross Sq Ft")

summary(nyc$YEAR.BUILT)
hist(nyc$YEAR.BUILT, breaks=100, main="Year Built Histogram", xlab="Year Built")

# cooks distance
outliers <- data.frame(nyc$SALE.PRICE, nyc$GROSS.SQUARE.FEET, nyc$YEAR.BUILT)
colnames(outliers) <- c("SalePrice", "GrossSquareFeet", "YearBuilt")
outliers <- na.omit(outliers)

lin_reg <- lm(SalePrice~GrossSquareFeet+YearBuilt, data=outliers)
summary(lin_reg)
cooks <- cooks.distance(lin_reg)
influential <- cooks[cooks > (3 * mean(cooks))]
plot(lin_reg, pch=18, col="purple", which=c(4))

# multivariate regression
multi <- data.frame(nyc$SALE.PRICE, nyc$GROSS.SQUARE.FEET, nyc$LAND.SQUARE.FEET)
colnames(multi) <- c("SalePrice", "GrossSquareFeet", "LandSquareFeet")
multi <- na.omit(multi)

sale_gross <- lm(SalePrice~GrossSquareFeet, data=multi)
summary(sale_gross)
plot(SalePrice~GrossSquareFeet, data=multi)
abline(sale_gross, col="green")

sale_land <- lm(SalePrice~LandSquareFeet, data=multi)
summary(sale_land)
plot(SalePrice~LandSquareFeet, data=multi)
abline(sale_land, col="blue")

sale_land_gross <- lm(SalePrice~GrossSquareFeet+LandSquareFeet, data=multi)
summary(sale_land_gross)

# decision tree
dec <- data.frame(nyc$SALE.PRICE, nyc$GROSS.SQUARE.FEET)
colnames(dec) <- c("SalePrice", "GrossSquareFeet")
dec <- na.omit(dec)

# split up data into training and testing
sampl <- sample(150, 100)
train <- dec[sampl, ]
test <- dec[-sampl, ]

# dimensions of train and test data
dim(train)
dim(test)

dtree <- rpart(SalePrice~., train, method="class")
dtree
rpart.plot(dtree)
prp(dtree, faclen = 2)
draw.tree(dtree,cex=1)


### QUESTION 2

# models
d <- data.frame(nyc$SALE.PRICE, nyc$GROSS.SQUARE.FEET, nyc$YEAR.BUILT)
colnames(d) <- c("SalePrice", "GrossSquareFeet", "YearBuilt")
d <- na.omit(d)

reg <- lm(SalePrice~GrossSquareFeet+YearBuilt, data=d)
summary(reg)
plot(reg)
coef(reg)

# just year built
reg2 <- lm(SalePrice~YearBuilt, data=d)
summary(reg2)
plot(SalePrice~YearBuilt, data=d)
abline(reg2, col="red")
