data <- read.csv("veri.csv", encoding = "UTF-8")
data[,2] <- as.Date(data[,2], "%m/%d/%Y")

daysToWeeks <- function(x){
  weeks <- floor(x/7)
  remaining_days <- x - (weeks*7)
  if(remaining_days == 0){
    return(paste(weeks, "hafta"))
  }
  else {
    return(paste(weeks, "hafta", remaining_days, "gün", sep = " "))
  }
}
col_na <- which(is.na(data$TABURCUGÜNÜ) == TRUE)
col <- data$TABURCUGÜNÜ[!is.na(data$TABURCUGÜNÜ)]
a <- lapply(col, daysToWeeks)



## VA 24 SAAT datası intergrowth uygulamasında çevrilmek üzere

ID <- data$X.U.FEFF.DOSYANO
Sex <- data$CİNSİYET
for(i in 1:152){
  if(Sex[i] == 1){
    Sex[i] = "Male"
  }
  else{
    Sex[i] = "Female"
  }
}
GA <- data$GESTASYONHAFTA * 7 + data$GESTASYONGÜN
Weight <- data$VA24SAAT / 1000
Length <- data$BOY24SAAT
HeadCircumference <- data$BÇ24SAAT
VA24SAAT <- data.frame(ID, Sex, GA, Weight, Length, HeadCircumference)
write.csv(VA24SAAT, "VA24SAAT.csv")

## 75. ve 114.'ü satırların ID'leri aynı.
which(ID == 1750176)

GA96 <- GA + 4
Weight96 <- data$VA96SAAT / 1000
Length96 <- data$BOY96SAAT
HeadCircumference96 <- data$BÇ96SAAT
VA96SAAT <- data.frame(ID, Sex, GA96, Weight96, Length96, HeadCircumference96)
write.csv(VA96SAAT, "VA96SAAT.csv")

GA7gun <- GA + 7
Weight7gun <- data$VA7GÜN / 1000
Boy7gun <- data$BOY7GÜN
BasCevre7gun <- data$BÇ7GÜN
VA7GUN <- data.frame(ID, Sex, GA7gun, Weight7gun, Boy7gun, BasCevre7gun)
write.csv(VA7GUN, "VA7GUN.csv")

GA14gun <- GA + 14
Weight14gun <- data$VA14GÜN / 1000
Boy14gun <- data$BOY14GÜN
BasCevre14gun <- data$BÇ14GÜN
VA14GUN <- data.frame(ID, Sex, GA14gun, Weight14gun, Boy14gun, BasCevre14gun)
write.csv(VA14GUN, "VA14GUN.csv")

GA21gun <- GA + 21
Weight21gun <- data$VA21GÜN / 1000
Boy21gun <- data$BOY21GÜN
BasCevre21gun <- data$BÇ21GÜN
VA21GUN <- data.frame(ID, Sex, GA21gun, Weight21gun, Boy21gun, BasCevre21gun)
write.csv(VA21GUN, "VA21GUN.csv")

GA28gun <- GA + 28
Weight28gun <- data$VA28GUN / 1000
Boy28gun <- data$BOY28GÜN
BasCevre28gun <- data$BÇ28GÜN
VA28GUN <- data.frame(ID, Sex, GA28gun, Weight28gun, Boy28gun, BasCevre28gun)
write.csv(VA28GUN, "VA28GUN.csv")


VA28GUNoutput <- read.csv("VA28GUNoutput.csv")
VA28GUNoutput$LengthCentile <- cut(VA28GUNoutput$LengthCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA28GUNoutput$WeightCentile <- cut(VA28GUNoutput$WeightCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA28GUNoutput$HeadCircumferenceCentile <- cut(VA28GUNoutput$HeadCircumferenceCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
write.csv(VA28GUNoutput, "VA28GUNson.csv")

VA21GUNoutput <- read.csv("VA21GUNoutput.csv")
VA21GUNoutput$LengthCentile <- cut(VA21GUNoutput$LengthCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA21GUNoutput$WeightCentile <- cut(VA21GUNoutput$WeightCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA21GUNoutput$HeadCircumferenceCentile <- cut(VA21GUNoutput$HeadCircumferenceCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                              labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
write.csv(VA21GUNoutput, "VA21GUNson.csv")

VA14GUNoutput <- read.csv("VA14GUNoutput.csv")
VA14GUNoutput$LengthCentile <- cut(VA14GUNoutput$LengthCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA14GUNoutput$WeightCentile <- cut(VA14GUNoutput$WeightCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA14GUNoutput$HeadCircumferenceCentile <- cut(VA14GUNoutput$HeadCircumferenceCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                              labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
write.csv(VA14GUNoutput, "VA14GUNson.csv")

VA7GUNoutput <- read.csv("VA7GUNoutput.csv")
VA7GUNoutput$LengthCentile <- cut(VA7GUNoutput$LengthCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA7GUNoutput$WeightCentile <- cut(VA7GUNoutput$WeightCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                   labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA7GUNoutput$HeadCircumferenceCentile <- cut(VA7GUNoutput$HeadCircumferenceCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                              labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
write.csv(VA7GUNoutput, "VA7GUNson.csv")

VA96SAAToutput <- read.csv("VA96SAAToutput.csv")
VA96SAAToutput$WeightCentile <- cut(VA96SAAToutput$WeightCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                  labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA96SAAToutput <- subset(VA96SAAToutput, select = -c(Length, LengthZScore,
                                                     LengthCentile, HeadCircumference,
                                                     HeadCircumferenceCentile, HeadCircumferenceZScore))
write.csv(VA96SAAToutput, "VA96SAATson.csv")

VA24SAAToutput <- read.csv("VA24SAAToutput.csv")
VA24SAAToutput$LengthCentile <- cut(VA24SAAToutput$LengthCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                  labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA24SAAToutput$WeightCentile <- cut(VA24SAAToutput$WeightCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                  labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
VA24SAAToutput$HeadCircumferenceCentile <- cut(VA24SAAToutput$HeadCircumferenceCentile, breaks=c(0, 3, 10, 50, 90, 97, Inf),
                                             labels = c("< 3", "3 - 10", "10 - 50", "50 - 90", "90 - 97", "> 97"))
write.csv(VA24SAAToutput, "VA24SAATson.csv")


GA252gun <- rep(252,152)
Weight40hafta <- data$VADÜZ40HAFTA
Length40hafta <- data$BOYDÜZ40HAFTA
HeadCircumference40hafta <- data$BÇDÜZ40HAFTA
GUNDATA <- data.frame(ID, Sex, GA252gun, Weight40hafta, Length40hafta, HeadCircumference40hafta)
write.csv(GUNDATA, "DÜZ40HAFTA.csv")

GAtaburculuk <- data$GESTASYONHAFTA * 7 + data$GESTASYONGÜN + data$TABURCUGÜNÜ
WeightTaburculuk <- data$VATABURCULUK
LengthTaburculuk <- data$BOYTABURCULUK
HeadCircumferenceTaburculuk <- data$BÇTABURCULUK
TABDATA <- data.frame(ID, Sex, GAtaburculuk, WeightTaburculuk, LengthTaburculuk, HeadCircumferenceTaburculuk)
write.csv(TABDATA, "TABURCULUK.csv")

concatanate <- function(x, y){
  z <- c()
  for(i in 1:152){
    if(y[i] > 0 && !is.na(y[i])){
      z[i] <- paste(as.character(x[i]), " ", as.character(y[i]), "/7", sep = "")
    }
    else{
      z[i] <- as.character(x[i])
    }
  }
  z
}

GAhafta <- data$GESTASYONHAFTA
GAgün <- data$GESTASYONGÜN
z <- concatanate(GAhafta, GAgün)
GA <- as.matrix(z)
Sex <- data$CİNSİYET
Weight <- data$VA28GUN
Head <- data$BÇ28GÜN
Length <- data$BOY28GÜN
df <- data.frame(GA, Sex, Weight, Head, Length)
df <- df[df$Sex == 1,]
df <- df[,c(1,3,4,5)]
write.csv(df, "FENTON28GÜNerkek.csv")

update <- function(x, y, sabit = 4){
  z <- c()
  for(i in 1:152){
    if(y[i] + sabit > 7){
      z[i] <- paste(as.character(x[i] + 1), " ", as.character(y[i] + sabit - 7), "/7", sep = "")
    }
    else if(y[i] + sabit == 7){
      z[i] <- as.character(x[i] + 1)
    }
    else {
      z[i] <- paste(as.character(x[i]), " ", as.character(y[i] + sabit), "/7", sep = "")
    }
  }
  z
}
GA96saat <- update(GAhafta, GAgün, 4)
GA96saat <- as.matrix(GA96saat)
df <- data.frame(GA96saat, Sex, Weight)
df <- df[df$Sex == 2,]

GA <- rep(36, 152)
Sex <- data$CİNSİYET
Weight <- data$VATABURCULUK
Head <- data$BÇTABURCULUK
Length <- data$BOYTABURCULUK
df <- data.frame(GA, Sex, Weight, Head, Length)
df <- df[df$Sex == 2,]
df <- df[,c(1,3,4,5)]
write.csv(df, "FENTONTABURCULUKdişi.csv")


GAweek <- data$GESTASYONHAFTA
GAday <- data$GESTASYONGÜN
tamHafta <- floor(data$TABURCUGÜNÜ/7)
GAweek <- GAweek + tamHafta
kalanGun <- data$TABURCUGÜNÜ - tamHafta*7
GAday <- GAday + kalanGun
tamHafta2 <- floor(GAday / 7)
GAweek <- GAweek + tamHafta2
GAday <- GAday - tamHafta2*7
which(GAday == 7)
is.na(GAday)
GA <- concatanate(GAweek, GAday)
