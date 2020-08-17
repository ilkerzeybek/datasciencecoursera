#Function that simulates 100 random variables and takes their mean.
myfunction <- function(){
  x <- rnorm(100)
  mean(x)
}
x <- 5:10
class(x)
x <- as.numeric(x)
class(x)
