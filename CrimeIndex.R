dat = read.csv("/Users/michaelsarmiento/Desktop/crimetableset.csv")


library(rpart)
library(rpart.plot)
library(e1071)
summary(dat)

#Data Cleaning Split hours and Days
Hours <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
HourOnly <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%H")
MonthOnly <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%m")

unique(dat$Category)

violent <- ifelse(dat$Category=="ASSUALT"
                  ||dat$Category=="SEX OFFENSES FORCIBLE"
                  ||dat$Category=="ROBBERY"
                  ||dat$Category=="KIDNAPPING", 1, 0)



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
NonDF = data.frame(table(dat$PdDistrict[dat$IsViolent=="nonViolent"], dat$DayOfWeek[dat$IsViolent=="nonViolent"], dat$HourOnly[dat$IsViolent=="nonViolent"]))

ViolentDF["WeightedFreq"]  = NA
NonDF["WeightedFreq"]  = NA
NonDF["ViolentWeighted"] =NA
NonDF["Combined"] =NA
NonDF["CrimeIndex"] =NA
df["WeightedFreq"]  = NA


ViolentDF["WeightedFreq"] = ViolentDF$Freq*.7
NonDF["WeightedFreq"] = NonDF$Freq*.3
NonDF$ViolentWeighted = ViolentDF$WeightedFreq
NonDF$ViolentWeighted <- as.numeric(NonDF$ViolentWeighted )
NonDF$WeightedFreq <- as.numeric(NonDF$WeightedFreq)
NonDF$Combined = NonDF$ViolentWeighted + NonDF$WeightedFreq
NonDF$CrimeIndex = NonDF$Combined / 62.3

head(NonDF, 10)
head(ViolentDF, 10)
head(NonDF, 100)
#In the view sort by Combined Column
View(NonDF)

newdata <- NonDF[order(NonDF$Combined),]

View(newdata)



