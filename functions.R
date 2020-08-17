add2 <- function(x, y){
  x+y
}
add2(5,10)

above10 <- function(x){
  use <- x > 10
  x[use]
}

x <- 1:25
above10(x)

above <- function(x, n){
  use <- x > n
  x[use]
}

above(x, 13)

above_default <- function(x, n = 10){
  use <- x > n
  x[use]
}

above_default(x)
above_default(x, 12)


column_mean <- function(x, removeNA = TRUE){
  nc <- ncol(x)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(x[,i], na.rm = removeNA)
  }
  means
}

column_mean(airquality)
