best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  subset <- data[,c(2, 7, 11, 17, 23)]
  colnames(subset) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  if(!state %in% subset[,"State"]){
    stop("invalid state")
  }
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  else{
    subset_state <- subset[which(subset[,"State"] == state),]
    subset_state_min <- subset_state[which(as.numeric(subset_state[,outcome]) ==
                                          min(as.numeric(subset_state[,outcome]),
                                              na.rm = TRUE)),"Hospital"]
    return(min(subset_state_min))
  }
}

rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  subset <- data[,c(2, 7, 11, 17, 23)]
  colnames(subset) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  if(!state %in% subset[,"State"]){
    stop("invalid state")
  }
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  else{
    subset_state <- subset[which(subset[,"State"] == state),]
    subset_state[,outcome] <- as.numeric(subset_state[,outcome])
    sorted_subset_state <- subset_state[order(subset_state[,outcome], subset_state[,"Hospital"]),]
    sorted_subset_state <- sorted_subset_state[!is.na(sorted_subset_state[,outcome]),]
    if(num == "best"){
      return(sorted_subset_state[1,"Hospital"])
    }
    else if(num == "worst"){
      return(sorted_subset_state[length(sorted_subset_state[,"Hospital"]), "Hospital"])
    }
    else if(num > 0 && num <= length(sorted_subset_state[,"Hospital"])){
      return(sorted_subset_state[num, "Hospital"])
    }
    else {
      return(NA)
    }
  }
}

rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  colIndex <- integer(0)
  if('heart attack' == outcome)
    colIndex <- 11
  else if('heart failure' == outcome)
    colIndex <-  17
  else if('pneumonia' == outcome)
    colIndex <- 23
  else {
    stop("invalid outcome")
  }
  data[ ,colIndex] <- as.numeric(data[ ,colIndex])
  data <- data[complete.cases(data), ]
  bestHospitals <- data[order(data$State, data[,colIndex], data$Hospital.Name), ]
  statelevels <- factor(bestHospitals[ , 7])
  ranks <- list()
  if(num == "best") {
    ranks <- tapply(bestHospitals[['Hospital.Name']], statelevels, function(name) { return(name[1]) })
  }
  else if(num == "worst") {
    ranks  <- tapply(bestHospitals[['Hospital.Name']], statelevels, function(name) { return(name[length(name)]) })
  }
  else {
    ranks <- tapply(bestHospitals[['Hospital.Name']], statelevels, function(name) { return(name[num]) })
  }
  data.frame(hospital = ranks, state = names(ranks))
}
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
