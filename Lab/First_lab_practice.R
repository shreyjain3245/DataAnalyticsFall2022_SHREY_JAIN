# Creating a datagrame
# Example: RPI Weather dataframe.

days <- c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun') # days
temp <- c(28, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')
help(data.frame)
RPI_Weather_Week <- data.frame(days, temp, snowed)
RPI_Weather_Week 
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5, c("days", "temp")]
RPI_Weather_Week$temp

subset(RPI_Weather_Week, subset.snowed=TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow

empty.DataFrame = data.frame()
v1 <- 1:10
v1

letters

v2 <- letters[1:10]
df <- data.frame(col.names.1 = v1, col.name.2 = v2)
df


# R Basics with Examples.
## Data Types
# numerics,character and logical
temperature <- 4.5 # degrees in Fahrenheit 
class(temperature)
RPI <- "Rensselaer Polytechnic Institue"
class(RPI)
Rpi <- 3.14159265359
class(Rpi)
isSnowing <- FALSE
class(isSnowing)
R <- FALSE
class(R)



## Vectors 
num_vec <- c(1,3,5,99) 
class(num_vec)

cha_vec <- c("R","P","I")
class(cha_vec)
boolean_vec <- c(T,FALSE,F) # T = TRUE, you can simply use T for TURE
class(boolean_vec)
# mixed variable types
vec_mixed <- c("RPI", 1824, 3.14)
vec_mixed
class(vec_mixed) # Note: the 1824 and 3.14 are converted to characters
vec_mixed_boolean <- c(TRUE,"RPI", 1824, 3.14)
vec_mixed_boolean
class(vec_mixed_boolean)
vec_numeric_boolean <- c(TRUE,1824,3.14)
vec_numeric_boolean
class(vec_numeric_boolean)

temperature <- c(80,81.3,83,84.2,82.5)
names(temperature) <- c("Mon","Tue","Web","Thur","Fri")
temperature
# You can do the same thing by: 
Week_Days <-c("Mon","Tue","Web","Thur","Fri")
names(temperature) <- Week_Days
temperature

# Indexing Vectors 
# Note: indexing in R starts with 1, in python programming language indexing start with 0.
vec1 <- c('R','P','I')
vec2 <- c(1,8,2,4)
vec1[1]
vec2[2]

# Matrix 
m <- c(1:10)
m
matrix(m,nrow = 2)
matrix(1:12,byrow = FALSE,nrow = 4)
matrix(1:12,byrow = TRUE,nrow = 4)

# stock prices
goog <- c(560,564,563,562,561)
msft <- c(480,482,484,483,482)
stocks <- c(goog,msft)
stocks
print(stocks)
stock.matrix <- matrix(stocks,byrow = T,nrow = 2)
stock.matrix
days <- c("Mon","Tue","Wed","Thur","Fri")
st.names <- c("goog","msft")
colnames(stock.matrix) <- days
rownames(stock.matrix) <- st.names
print(stock.matrix)
mat <- matrix(1:25,byrow = T, nrow = 5)
mat
mat*2
mat/2
mat^2
1/mat
mat > 15 
mat[mat > 15]
mat + mat 
mat / mat 
colSums(stock.matrix)
rowSums(stock.matrix)
rowMeans(stock.matrix)
# Bind the Columns
FB <- c(223,224,225,223.5,222)
tech.stocks <- rbind(stock.matrix,FB) # Row bind
tech.stocks
avg <- rowMeans(tech.stocks)
avg
tech.stocks <- cbind(tech.stocks,avg) # Column bind
tech.stocks
mat <- matrix(1:50,byrow = T, nrow = 5)
mat
mat[1,] # first row with all the columns
mat[,1] # first column and all the rows
mat[1:3,]
mat[1:2,1:3]
mat[,9:10]
mat[2:3,5:6]
# Factor and Catergorical variables
animals <- c('dog','cat','dog','cat','cat')
id <- c(1,2,3,4,5)
temps <- c('cold','med','hot','hot','hot','cold','med')
temps
fact.temps <- factor(temps, ordered = TRUE, levels = c('cold','med','hot'))
fact.temps
summary(fact.temps)
summary(temps)

undergrads <-c('Freshman','Junior', 'Sophomore','Junior','Senior','Sophomore','Junior','Freshman','Senior','Junior')
undergrads
factor.undergrads <-factor(undergrads,ordered = TRUE,levels = c('Freshman','Sophomore','Junior','Senior'))
factor.undergrads

summary(factor.undergrads)

# Exercise
A <- c(1,2,3)
B <- c(4,5,6)
A <- rbind(A,B)
A
mat <- matrix(1:9, nrow = 3)
mat
is.matrix(mat)
mat2 <- matrix(1:25, byrow = T, nrow = 5)
mat2
mat2[2:3,2:3]
mat2[4:5,4:5]
sum(mat2)
help("runif")
u <- runif(20)
u
runif(matrix(20))
matrix(runif(20),nrow = 4)


write.csv(df, file = "saved_df1.csv")
df2 <- read.csv("saved_df1.csv")
df2
