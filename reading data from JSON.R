library(jsonlite)
json_data <- fromJSON("https://api.github.com/users/jtleek/repos")
names <- names(json_data)
names
names(json_data$owner)
json_data$owner$login ## user is the same for all repos since we are looking
                      ## into API for jtleek user.
iris_json <- toJSON(iris, pretty = T)
cat(iris_json)
iris2 <- fromJSON(iris_json)
head(iris2)
