dat = read.csv("/Users/michaelsarmiento/Desktop/crimetableset.csv")

library(ggmap)
library(rpart)
library(rpart.plot)
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

#Map Stuff
map = get_map(c(-122.431297,	37.773972),
              maptype="road", zoom=12)
# plot it
p = ggmap(map)
p

library = data.frame(long=dat$X[dat$IsViolent==1],lat=dat$Y[dat$IsViolent==1])

p = p + geom_point(data=library, aes(x=long,y=lat),
                   color="red", size=4)+
geom_tile(data = library, aes(x = long, y = lat, alpha = Frequency),
          fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank())
p