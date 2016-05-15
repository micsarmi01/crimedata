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

names(crimeIndexData) <- c("PdDistrict", "DayOfWeek", "HourOnly", "CrimeIndex")

#dessicion tree model
set.seed(132)
split = split_data(crimeIndexData)
tr_dat = split[[1]]
te_dat = split[[2]]

fit = rpart(CrimeIndex ~ ., data=tr_dat)

prp(fit, extra=1, varlen=-10, box.col="lightgreen")

predicted = predict(fit, te_dat)
errors = te_dat$CrimeIndex - predicted
rmse = sqrt(mean(errors^2))


#linear model
fit = lm(CrimeIndex ~ .,data=crimeIndexData)

summary(fit)

predicted = predict(fit)
actual = crimeIndexData$CrimeIndex

rng = range(c(predicted, actual))
plot(predicted, actual, pch=20, xlim=rng, ylim=rng, xlab="predicted", ylab="actual")
lines(c(rng[1], rng[2]), c(rng[1], rng[2]), lty=2, col="blue", lwd=1.5)


#glm model
fit = glm(CrimeIndex ~ .,data=crimeIndexData, family = binomial)

summary(fit)

predicted = predict(fit, type = "response")
actual = crimeIndexData$CrimeIndex

rng = range(c(predicted, actual))
plot(predicted, actual, pch=20, xlim=rng, ylim=rng, xlab="predicted", ylab="actual")
lines(c(rng[1], rng[2]), c(rng[1], rng[2]), lty=2, col="blue", lwd=1.5)











## Load Polygon Shapefile of Police District Boundaries, along with supporting packages
## Define center of polygons for labeling location
```{r}
library(rgdal)         # for readOGR(...)
library(ggplot2)
library(RColorBrewer)
library(rgeos)
dat = rawData
SFMap = readOGR("/Users/wiseR3B3L/Downloads/Historic Police Districts/","SFPD")
map.df <- fortify(SFMap)
centers <- SpatialPointsDataFrame(gCentroid(SFMap, byid=TRUE), SFMap@data, match.ID=FALSE)
centDF = as.data.frame(centers)
centroids <- gCentroid(SFMap, byid=TRUE)
```

```{r}
Hours <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
HourOnly <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%H")
MonthOnly <- format(as.POSIXct(strptime(dat$Dates,"%m/%d/%Y %H:%M",tz="")) ,format = "%m")
dat["HourOnly"] = NA
dat$HourOnly=as.numeric(HourOnly)
dat["MonthOnly"] = NA
dat$MonthOnly=as.numeric(MonthOnly)
dat["IsViolent"] = NA
dat$IsViolent = ifelse(dat$Category=="ASSAULT"
                       |dat$Category=="SEX OFFENSES FORCIBLE"
                       |dat$Category=="ROBBERY"
                       |dat$Category=="KIDNAPPING", 1, 0)
```

## HeatMap of Total crime in San Francisco, May 2014 -> May 2015
```{r}
#all crime heatmap
ggplot(dat, aes(x=X, y=Y))  + geom_point(colour="red", size = 0.1)  + stat_density2d(aes(fill = ..level..), alpha=1, geom="polygon")+ scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) + coord_fixed() + geom_path(data=map.df,aes(x=long, y=lat,group=group), colour="black") + geom_text(data=centDF, mapping=aes(x=x, y=y, label=district), size=3, vjust=0.5, hjust=0.5) + ggtitle("San Francisco ALL Crime Occurences Heatmap\n May 2014 - May 2015") + theme(plot.title = element_text(lineheight=2, face="bold"))
```

## HeatMap of all VIOLENT crime in San Francisco, May 2014 -> May 2015
```{r}
#violent crimes heatmap
ggplot(dat[dat$IsViolent == 1,], aes(x=X, y=Y))  + geom_point(colour="red", size = 0.1)  + stat_density2d(aes(fill = ..level..), alpha=1, geom="polygon")+ scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) + coord_fixed() + geom_path(data=map.df,aes(x=long, y=lat,group=group), colour="black") + geom_text(data=centDF, mapping=aes(x=x, y=y, label=district), size=3, vjust=0.5, hjust=0.5) + ggtitle("San Francisco Violent Crime Occurences Heatmap\n May 2014 - May 2015") + theme(plot.title = element_text(lineheight=2, face="bold"))
```

## HeatMap of Violent crimes in San Francisco, from 5am to 11am  May 2014 -> May 2015
```{r}
#5am - 11am Heatmap Violent Crimes
ggplot(dat[(dat$IsViolent == 1 & dat$HourOnly > 5 & dat$HourOnly < 11),], aes(x=X, y=Y)) + geom_point(colour="red", size = 0.1)  + scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) + stat_density2d(aes(fill = ..level..), alpha=1, geom="polygon") + coord_fixed() + geom_path(data=map.df,aes(x=long, y=lat,group=group), colour="black") + geom_text(data=centDF, mapping=aes(x=x, y=y, label=district), size=3, vjust=0.5, hjust=0.5) + ggtitle("San Francisco Violent Crime Occurences Heatmap\n5am -11am, May 2014 - May 2015") + theme(plot.title = element_text(lineheight=2, face="bold"))
```

## HeatMap of Violent crimes in San Francisco, from 12pm to 6pm  May 2014 -> May 2015
```{r}
#1200-1800 Heatmap Violent Crimes
ggplot(dat[(dat$IsViolent == 1 & dat$HourOnly > 12 & dat$HourOnly < 18),], aes(x=X, y=Y))  + geom_point(colour="red", size = 0.1) + stat_density2d(aes(fill = ..level..), alpha=1, geom="polygon") + scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) + coord_fixed() + geom_path(data=map.df,aes(x=long, y=lat,group=group), colour="black") + geom_text(data=centDF, mapping=aes(x=x, y=y, label=district), size=3, vjust=0.5, hjust=0.5) + ggtitle("San Francisco Violent Crime Occurences Heatmap\n 12pm - 6pm, May 2014 - May 2015") + theme(plot.title = element_text(lineheight=2, face="bold"))
```

## HeatMap of Violent crimes in San Francisco, from 7pm to 5am  May 2014 -> May 2015
```{r}
ggplot(dat[(dat$IsViolent == 1 & (dat$HourOnly > 19 | dat$HourOnly < 5)),], aes(x=X, y=Y))  + geom_point(colour="red", size = 0.1) + stat_density2d(aes(fill = ..level..), alpha=1, geom="polygon") + scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) + coord_fixed() + geom_path(data=map.df,aes(x=long, y=lat,group=group), colour="black") + geom_text(data=centDF, mapping=aes(x=x, y=y, label=district), size=3, vjust=0.5, hjust=0.5) + ggtitle("San Francisco Violent Crime Occurences Heatmap\n7pm - 5am, May 2014 - May 2015") + theme(plot.title = element_text(lineheight=2, face="bold"))
```
