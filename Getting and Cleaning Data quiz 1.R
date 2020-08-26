download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", destfile = "housing-idaho.csv")
data <- fread("housing-idaho.csv")
data_no_na <- data[!is.na(VAL)]
sum(data_no_na$VAL >= 24)
fes <- as.matrix(data$FES)

library(rJava)
library(xlsx)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx", destfile = "ngap.xlsx")
data <- read.xlsx("ngap.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)
sum(data$Zip*data$Ext,na.rm=T)

library(XML)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml", destfile = "baltimore.xml")
doc <- xmlTreeParse("baltimore.xml", useInternalNodes = TRUE)
root_node <- xmlRoot(doc)
zipcode <- xpathSApply(root_node, "//zipcode", xmlValue)
sum(zipcode == 21231 )

library(data.table)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", destfile = "2ndidaho.csv")
DT <- fread("2ndidaho.csv")
system.time(DT[,mean(pwgtp15), by = SEX])
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
