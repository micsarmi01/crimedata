
#dat = read.csv("/Users/wiseR3B3L/Documents/CS/R Projects/crimedata-master/crimetableset.csv")
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

#dat$IsViolent = factor(dat$IsViolent, labels=c("nonViolent", "violent"))


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

barplot(table(dat$HourOnly[dat$Category=="ASSAULT"]),col = "red",ylim=c(0, 400), main="Assaults by the hour", xlab="Hour of the day",las=2)

#Assaults by month
table(dat$MonthOnly[dat$Category=="ASSAULT"])
barplot(table(dat$MonthOnly[dat$Category=="ASSAULT"]),col = "red",ylim=c(0,700), main="Assaults by the month",las=2)

#Robbery by month
barplot(table(dat$MonthOnly[dat$Category=="ROBBERY"]),col = "red",ylim=c(0,230), main="Robberies by the month",las=2)

#Assaults by district
summary(dat$PdDistrict)
table(dat$PdDistrict[dat$Category=="ASSAULT"])
plot(dat$PdDistrict[dat$Category=="ASSAULT"],ylim=c(0,1200), pch=19, main="Assaults by district",las=2)

#probability of an Assault by district 
par(las=2)
par(mar=c(6,4,2,2))
barplot(table(dat$PdDistrict[dat$Category=="ASSAULT"])/table(dat$PdDistrict), ylim = range(0, .15))

#probability of violent crimes by district
par(las=2)
par(mar=c(6,4,2,2))
barplot(table(dat$PdDistrict[dat$IsViolent==1])/table(dat$PdDistrict), ylim = range(0, .2))

#probability of violent crimes by hour
par(las=2)
par(mar=c(6,4,2,2))
barplot(table(dat$HourOnly[dat$IsViolent==1])/table(dat$HourOnly), ylim = range(0, .25), main = "Violent Crimes by Hour")


#Given the Day of week, Month, Hour, and District, Predict the type of crime catagory

#Between 4-

#Split data into test and training data
split_data = function(dat, frac=c(0.75, 0.25)) {
  # at least one set must be specified
  k = length(frac)
  stopifnot(k > 0)
  
  n = nrow(dat)
  frac = frac/(sum(frac))
  starts = c(1, round(cumsum(frac) * n)[-k])
  ends = c(starts[-1]-1,n)
  samp = sample(1:n)
  data_sets = list()
  for (i in 1:k) {
    data_sets[[i]] = dat[samp[starts[i]:ends[i]],]
  }
  return(data_sets)
} 

dat$IsViolent = factor(dat$IsViolent, labels=c("nonViolent", "violent"))
dat$HourOnly = factor(dat$HourOnly)
set.seed(132)
split = split_data(dat)
tr_dat = split[[1]]
te_dat = split[[2]]

fit = naiveBayes(IsViolent ~ ., data=tr_dat)
predicts = predict(fit, newdata=te_dat)
conf_mtx = table(predicts, te_dat$IsViolent)

#####The accuracy of the first model presented with the confusion matrix

mean(predicts == te_dat$IsViolent)
conf_mtx


#Second Model
fit = naiveBayes(IsViolent ~ HourOnly+DayOfWeek+PdDistrict, data=tr_dat)
predicts = predict(fit, newdata=te_dat)
conf_mtx = table(predicts, te_dat$IsViolent)

#####The accuracy of the first model presented with the confusion matrix

mean(predicts == te_dat$IsViolent)
conf_mtx

#Third Model
fit = naiveBayes(IsViolent ~ DayOfWeek+PdDistrict, data=tr_dat)
predicts = predict(fit, newdata=te_dat)
conf_mtx = table(predicts, te_dat$IsViolent)

#####The accuracy of the first model presented with the confusion matrix

mean(predicts == te_dat$IsViolent)
conf_mtx

df = data.frame(table(dat$Dates))


#Cluster Analysis

#hc = hclust(dist(dat$DayOfWeek), method="complete")
# plot the “dendrogram”
#plot(hc)

sort(table(dat$PdDistrict, dat$DayOfWeek, dat$HourOnly))

dat["Index"] = NA

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



