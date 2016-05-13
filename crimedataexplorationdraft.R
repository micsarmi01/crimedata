dat = read.csv("/Users/wiseR3B3L/Documents/CS/R Projects/crimedata/crimetableset.csv")

library(rpart)
library(rpart.plot)
summary(dat)

#Data Cleaning Split hours and Days
Hours <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
HourOnly <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%H")
MonthOnly <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%m")

#Add to DF dat
dat["HourOnly"] = NA
dat$HourOnly=as.numeric(HourOnly)

dat["MonthOnly"] = NA
dat$MonthOnly=as.numeric(MonthOnly)

dat["IsViolent"] = NA
dat$IsViolent = ifelse(dat$Category=="ASSAULT"
                       |dat$Category=="SEX OFFENSES FORCIBLE"
                       |dat$Category=="ROBBERY"
                       |dat$Category=="KIDNAPPING", 1, 0)

myData = data.frame(table(dat$PdDistrict,dat$DayOfWeek,dat$HourOnly))

names(myData) <- c("PdDistrict", "DayOfWeek", "HourOnly", "Freq")

myData["Index"] = NA

hist(myData)

quantile(myData$Freq,probs=seq(0,1,length.out=10))

myData$Index[myData$Freq > 81] = 0.9
myData$Index[myData$Freq <= 81] = 0.8
myData$Index[myData$Freq <= 63] = 0.7
myData$Index[myData$Freq <= 52] = 0.6
myData$Index[myData$Freq <= 44] = 0.5
myData$Index[myData$Freq <= 37] = 0.4
myData$Index[myData$Freq <= 30] = 0.3
myData$Index[myData$Freq <= 22] = 0.2
myData$Index[myData$Freq <= 14] = 0.1


dat["Index"] = NA

dat[dat$PdDistrict == "RICHMOND" & dat$DayOfWeek == "Sunday" & dat$HourOnly == 4,]


