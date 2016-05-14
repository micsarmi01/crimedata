dat = read.csv("/Users/wiseR3B3L/Documents/CS/R Projects/crimedata/crimetableset.csv")


library(rpart)
library(rpart.plot)
library(e1071)
summary(dat)

#Data Cleaning Split hours and Days
Hours <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
HourOnly <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%H")
MonthOnly <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%m")

unique(dat$Category)

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

dat$IsViolent = factor(dat$IsViolent, labels=c("nonViolent", "violent"))

df = data.frame(table(dat$PdDistrict, dat$DayOfWeek, dat$HourOnly))
quantile(df$Freq,probs=seq(0,1,length.out=20))

ViolentDF = data.frame(table(dat$PdDistrict[dat$IsViolent=="violent"], dat$DayOfWeek[dat$IsViolent=="violent"], dat$HourOnly[dat$IsViolent=="violent"]))
tempCrimeData = data.frame(table(dat$PdDistrict[dat$IsViolent=="nonViolent"], dat$DayOfWeek[dat$IsViolent=="nonViolent"], dat$HourOnly[dat$IsViolent=="nonViolent"]))

ViolentDF["WeightedFreq"]  = NA
tempCrimeData["WeightedFreq"]  = NA
tempCrimeData["ViolentWeighted"] =NA
tempCrimeData["Combined"] =NA
tempCrimeData["CrimeIndex"] =NA
df["WeightedFreq"]  = NA


ViolentDF["WeightedFreq"] = ViolentDF$Freq*.7
tempCrimeData["WeightedFreq"] = tempCrimeData$Freq*.3
tempCrimeData$ViolentWeighted = ViolentDF$WeightedFreq
tempCrimeData$ViolentWeighted <- as.numeric(tempCrimeData$ViolentWeighted )
tempCrimeData$WeightedFreq <- as.numeric(tempCrimeData$WeightedFreq)
tempCrimeData$Combined = tempCrimeData$ViolentWeighted + tempCrimeData$WeightedFreq
tempCrimeData$CrimeIndex = tempCrimeData$Combined / 62.3

head(tempCrimeData, 10)
head(ViolentDF, 10)
head(tempCrimeData, 100)
#In the view sort by Combined Column
View(tempCrimeData)

crimeIndexData = data.frame(tempCrimeData$Var1, tempCrimeData$Var2, tempCrimeData$Var3, tempCrimeData$CrimeIndex)

names(crimeIndexData) <- c("Pd District", "Day Of Week", "Hour Only", "Crime Index")



