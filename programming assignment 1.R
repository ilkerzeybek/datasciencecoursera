pollutantmean <- function(directory, pollutant, id = 1:332)
{
  files_full <- list.files(directory, full.names = TRUE)
  df <- data.frame()
  for (i in id){
    df <- rbind(df, read.csv(files_full[i]))
  }
  mean_data <- mean(df[, pollutant], na.rm = TRUE)
  mean_data
}
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

complete <- function(directory, id = 1:332){
  df <- data.frame()
  files_full <- list.files(directory, full.names = TRUE)
  for(i in id){
    temp <- read.csv(files_full[i])
    nobs <- sum(complete.cases(temp))
    df <- rbind(df, data.frame(i, nobs))
  }
  colnames(df) <- c("id", "nobs")
  df
}
complete("specdata", c(2, 4, 8, 10, 12))

corr <- function(directory, threshold = 0){
  full_files <- list.files(directory, full.names = TRUE)
  vector <- vector(mode = "numeric", length = 0)
  for(i in 1:length(full_files)){
    temp <- read.csv(full_files[i])
    sum <- sum((!is.na(temp$sulfate)) & (!is.na(temp$nitrate)))
    if(sum > threshold){
      sulfate <- temp[which(!is.na(temp$sulfate)),]
      nitrate <- sulfate[which(!is.na(sulfate$nitrate)),]
      vector <- c(vector, cor(nitrate$sulfate, nitrate$nitrate))
    }
  }
  vector
}
cr <- corr("specdata", 150)
head(cr)
