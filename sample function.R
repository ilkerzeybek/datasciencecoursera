#Function that simulates 100 random variables and takes their mean.
myfunction <- function(){
  x <- rnorm(100)
  mean(x)
}
x <- 5:10
class(x)
x <- as.numeric(x)
class(x)
y <- list(foo = c(1L,2L,3L,4L), bar = 0.6, baz = "hello")
a <- y[["foo"]]
class(a)
data <- read.csv("hw1_data.csv")
head(data)
data[1:2,]
nrow(data)
data[152:153,]
data[47,]
ozone <- data[,1]
is.na(ozone)
na_values <- is.na(ozone)
na_values <- na_values[na_values == TRUE]
na_values
length(na_values)
mean(ozone)
bad <- is.na(ozone)
ozone_good <- ozone[!bad]
mean(ozone_good)
filtered_data <- data[data$Ozone > 31,]
filtered_data2 <- filtered_data[filtered_data$Temp > 90,]
good <- complete.cases(filtered_data2)
last_data <- filtered_data2[good,]
mean(last_data$Solar.R)
data_6month <- data[data$Month == 6,]
mean(data_6month$Temp)
data_5month <- data[data$Month == 5,]
ozone_5month <- data_5month$Ozone
good_o <- complete.cases(ozone_5month)
ozone_5month <- ozone_5month[good_o]
max(ozone_5month)
x <- 1:4
y <- 2:3
x+y
class(x+y)
