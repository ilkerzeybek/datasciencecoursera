library(datasets)
data(iris)
head(iris)
s <- split(iris, iris$Species)
s
mean(s$virginica$Sepal.Length)

apply(iris[, 1:4], 2, mean)

data(mtcars)
head(mtcars)

with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)

mean_list <- lapply(split(mtcars$hp, mtcars$cyl), mean)
mean_list
abs(mean_list$`4` - mean_list$`8`)
