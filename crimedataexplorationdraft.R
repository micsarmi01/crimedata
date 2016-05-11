dat = read.csv("/Users/michaelsarmiento/Desktop/crimetableset.csv")

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

#regular crime by day
barplot(table(dat$DayOfWeek)/100000, ylim = c(0, .14), col="red", main="Crime By Day in by 10k")

#regular crime by day
table(dat$DayOfWeek[dat$Category=="ASSAULT"])

#Assaults by day
barplot(table(dat$DayOfWeek[dat$Category=="ASSAULT"]),col = "red",ylim = c(0, 1200), main="Assaults by the day")

#Assaults by hour

str(dat$DayOfWeek)

head(dat,4)

#Assaults by hour
##plot(density(dat$Category))

par(mfrow=c(1,2)) 
barplot(table(dat$HourOnly[dat$Category=="ASSAULT"]),col = "red",ylim=c(0, 400), main="Assaults by the hour", xlab="Hour of the day",las=2)

#All Cime by the hour
barplot(table(dat$HourOnly),col = "blue", main="All Crimes by the hour", xlab="Hour of the day",las=2)

#Assaults by month
table(dat$MonthOnly[dat$Category=="ASSAULT"])
barplot(table(dat$MonthOnly[dat$Category=="ASSAULT"]),col = "red",ylim=c(0,700), main="Assaults by the month",las=2)

#Robbery by month
barplot(table(dat$MonthOnly[dat$Category=="ROBBERY"]),col = "red",ylim=c(0,230), main="Robberies by the month",las=2)

#Assaults by district
summary(dat$PdDistrict)
table(dat$PdDistrict[dat$Category=="ASSAULT"])
plot(dat$PdDistrict[dat$Category=="ASSAULT"],ylim=c(0,1200), pch=19, main="Assaults by district",las=2)


#probability of an Assault
NROW(dat$PdDistrict[dat$Category=="ASSAULT"&dat$PdDistrict=="MISSION"])/NROW(dat$PdDistrict[dat$PdDistrict=="MISSION"])
NROW(dat$PdDistrict[dat$Category=="ASSAULT"&dat$PdDistrict=="MISSION"])/NROW(dat$PdDistrict[dat$PdDistrict=="MISSION"])

#Given the Day of week, Month, Hour, and District, Predict the type of crime catagory

#Between 4-

barplot(table(dat$PdDistrict[dat$Category=="ASSAULT"])/table(dat$PdDistrict), las=2,ylim=c(0,.13), main="Assault probability by District")

barplot(table(dat$PdDistrict[dat$Category=="ASSAULT"&dat$HourOnly==0])/table(dat$PdDistrict[dat$HourOnly==0]), las=2,ylim=c(0,.15), main="Assault probability by District")

#Categorizing Violent 
#ASSAULT / SEX OFFENSES FORCIBLE / ROBBERY / KIDNAPPING
