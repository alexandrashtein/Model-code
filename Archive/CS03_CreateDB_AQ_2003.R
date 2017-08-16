# #install libraries 
rm(list=ls())
# install.packages("lme4")
# install.packages("reshape")
# install.packages("foreign") 
# install.packages("ggplot2")
# install.packages("plyr")
# install.packages("data.table")
# install.packages("Hmisc")
# install.packages("mgcv")
# install.packages("gdata")
# install.packages("car")
# install.packages("dplyr")
# install.packages("ggmap")
# install.packages("broom")
# install.packages("splines")
# install.packages("DataCombine")
# install.packages("readr")
# install.packages("bit64")
# install.packages("devtools")
# install_github("allanjust/aodlur")


#load libraries 
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
library(readr)
library(bit64)
library(devtools)
install_github("allanjust/aodlur")
library("aodlur")


#load LU grid 
lu<-fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1_km_maiac_grid.csv")

#load aod data
# aqua aod
aqua<-readRDS("N:/Projects/P019.IL.Israel.MAIAC.PM/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_TR_0014.RDS")
aqua <- aqua[aqua$aodid %in% lu$aodid, ] 
aqua<- aqua[yr == "2013"]

# terra aod
terra<-readRDS("N:/Projects/P019.IL.Israel.MAIAC.PM/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_TR_0014.RDS")
terra <- terra[terra$aodid %in% lu$aodid, ] 
terra <- terra[yr == "2013"]

# cutting the data according to the project area
pol=readOGR(dsn="N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
# pol=readOGR(dsn="N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Tel_aviv")
# Convert to data.frame
terra = as.data.frame(terra)

# Spatial subset
coordinates(terra) = ~ long_aod + lat_aod
proj4string(terra) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
terra = terra[pol, ]
# Convert back to data.table
terra = as.data.table(terra)
aqua=terra
rm(terra)

# #fix a bug, convert first from dplyr format to data.frame
# aqua.2013<-as.data.frame(aqua.2013)
# #convert to data.table
# aqua.2013<-as.data.table(aqua.2013)


#create single aod point per aodid per day- for aqua
aqua=terra
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),yr=mean(yr) )
sum(is.na(aqua1$aod))

#create single aod point per aodid per day- for terra
# terra <-terra %>%
#   group_by(aodid,day) %>%
#   summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),yr=mean(yr) )

#create full LU-aod TS
days<-seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
days2013 <- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
days2013$aodid<-as.character(days2013$aodid)

#merge aqua data
setkey(aqua,aodid,day)
setkey(days2013 ,aodid,day)
db2013 <- merge(days2013,aqua, all.x = T)

#merge terra data
# setkey(terra,aodid,day)
# setkey(days2013 ,aodid,day)
# db2013 <- merge(days2013,aqua, all.x = T)

#add land use data
setkey(db2013,aodid)
setkey(lu,aodid)
db2013 <- merge(db2013, lu, all.x = T)
head(db2013)
gc()
#get rid of duplicate names to cut down on DB size
db2013<-dplyr::select(db2013,-lat_aod.y, -long_aod.y)
gc()

#add season
db2013$month <- as.numeric(format(db2013$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
db2013$season<-recode(db2013$month,"1=1;2=1;3=2;4=2;5=2;6=z3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
db2013$seasonSW<-recode(db2013$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

# precentage of NA in the data
length(db2013$aod)
sum(is.na(db2013$aod))*100/length(db2013$aod)

################ add TEMPORAL Variables

#add ndvi
##join first ndviid to db2013 then join by aodid and month

########### import datasets
## Useful function to delete columns in data.table

deleteColsFromDataTable <- function (train, toDeleteColNames) 
{
  for (myNm in toDeleteColNames)
    train <- train [,(myNm):=NULL,with=F]
  return (train)
}


##### Add NDVI

#import NDVI
#ndvi<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
ndvi<-readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
ndvi=filter(ndvi,c==2013)

setnames(db2013,"month","m")
db2013$m=as.character(db2013$m)

# add ndviid to db2013
#join actual NDVI to aod
setkey(ndvi, ndviid, m)
setkey(db2013,ndviid, m)
db2013<- merge(db2013, ndvi,all.x = T)


###### Add Pbl
pbl<-fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2013_2015_11_12am.csv")
pbl$pblid=paste(pbl$lon,pbl$lat, sep="_")
setnames(pbl,"date","day")
setnames(pbl,"PBLid","pblid")
pbl$day=as.Date(pbl$day)

#create single pbl point per day
pbld <-pbl %>%
  group_by(pblid,day) %>%
  summarise(lon_pbl=mean(lon),lat_pbl=mean(lat),pbl=mean(hpbl) )

#join pbl to aod
setnames(db2013,"PBLid","pblid")

setkey(pbl, pblid, day )
setkey(db2013,  pblid, day)
db2013 <- merge(db2013, pbl, all.x = T)

toDeleteColNames=c("V1.y","lon","lat")
deleteColsFromDataTable(db2013, toDeleteColNames)

###### Add Temperature

Temp <- fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/Temp_H.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2013]


jointo.pt <- makepointsmatrix(datatable = db2013, 
                             xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Temp, 
                               xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                       jointo = db2013, joinfrom = Temp, 
                       jointovarname = "aodid", joinfromvarname = "stn", 
                       joinprefix = "nearest", valuefield = "Temp", 
                       knearest = 15, maxdistance = 60000, 
                       nearestmean = FALSE, verbose = T)

setkey(db2013,aodid,day)
setkey(joinout,aodid,day)
db2013 <- merge(db2013, joinout[,list(day,Temp,aodid)], all.x = T)
head(db2013)
summary(db2013$Temp)

###### Add WD
WD <- fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/WD_H.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2013]

jointo.pt <- makepointsmatrix(datatable = db2013, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = WD, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2013, joinfrom = WD, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "WD", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2013,aodid,day)
setkey(joinout,aodid,day)
db2013 <- merge(db2013, joinout[,list(day,WD,aodid)], all.x = T)
head(db2013)
summary(db2013$WD)

###### Add WS
WS <- fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/WS_H.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2013]

jointo.pt <- makepointsmatrix(datatable = db2013, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = WS, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2013, joinfrom = WS, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "WS", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2013,aodid,day)
setkey(joinout,aodid,day)
db2013 <- merge(db2013, joinout[,list(day,WS,aodid)], all.x = T)
head(db2013)
summary(db2013$WS)


###### Add RH
RH <- fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/RH_H.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2013]

jointo.pt <- makepointsmatrix(datatable = db2013, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = RH, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2013, joinfrom = RH, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "RH", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2013,aodid,day)
setkey(joinout,aodid,day)
db2013 <- merge(db2013, joinout[,list(day,RH,aodid)], all.x = T)
head(db2013)
summary(db2013$RH)

###### Add Rain
Rain <- fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/Rain_H.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2013]

jointo.pt <- makepointsmatrix(datatable = db2013, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Rain, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2013, joinfrom = Rain, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Rain", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2013,aodid,day)
setkey(joinout,aodid,day)
db2013 <- merge(db2013, joinout[,list(day,Rain,aodid)], all.x = T)
head(db2013)
summary(db2013$Rain)

###### Add NO2
NO2 <- fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/NO2_H.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2013]

jointo.pt <- makepointsmatrix(datatable = db2013, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = NO2, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2013, joinfrom = NO2, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "NO2", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2013,aodid,day)
setkey(joinout,aodid,day)
db2013 <- merge(db2013, joinout[,list(day,NO2,aodid)], all.x = T)
head(db2013)
summary(db2013$NO2)

#MEAN PM25- Daily mean PM2.5

PM25 <- fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Pollution_stn_May16/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2013<- PM25[c==2013]


# Join the closest PM2.5 value for each day

jointo.pt <- makepointsmatrix(datatable = db2013, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pmall2013, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2013, joinfrom = pmall2013, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM25", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2013,aodid,day)
setkey(joinout,aodid,day)
db2013 <- merge(db2013, joinout[,list(day,PM25,aodid)], all.x = T)
head(db2013)
summary(db2013$PM25)


setnames(db2013,"PM25.y","closest_PM25")

saveRDS(db2013,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/db2013/db2013.RDS")


##########

# calculate IDW for PM2.5

for(i in unique(db2013$day)) {

x<-pmall2013[pmall2013$day==i, ]
y= db2013[db2013$day==i, ]

library(gstat)
#defaults to idw (gstat)
library(sp)
coordinates(x) = ~ x_stn_ITM + y_stn_ITM
coordinates(y) = ~ x_aod_ITM + y_aod_ITM
#location statment uneeded since we defined coordinates
inter = gstat(formula = PM25 ~ 1,  data =x)
z<-predict(object = inter, newdata = y)
# head(z)
db2013$pred[db2013$day==i] = z$var1.pred
# spplot(z, "var1.pred", at = 0:100)
}

setnames(db2013,"pred","PM25_IDW")

# adding the mean PM25

pm.m <- makepointsmatrix(pmall2013, "x_stn_ITM", "y_stn_ITM", "stn")

aod.m <- makepointsmatrix(db2013[db2013[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            db2013, pmall2013 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = NA, nearestmean = T)


#create single aod point per aodid per day 
x <-closestaodse %>%
    group_by(aodid,day) %>%
    summarise(meanpm25=mean(closestmean) )


#join to DB
setkey(x,aodid,day)
setkey(db2013,aodid,day)
db2013 <- merge(db2013,x,all.x = T)

summary(lm(db2013$PM25_IDW~db2013$meanpm25.x))

summary(lm(aod~meanpm25.x,data=db2013))

#### ADD PM10 and PM25

#MEAN PM10

PM10 <- fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm_10all2013<- PM10[c==2013]

# Join the closest PM10 value for each day

jointo.pt <- makepointsmatrix(datatable = db2013, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pm_10all2013, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2013, joinfrom = pm_10all2013, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM10", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2013,aodid,day)
setkey(joinout,aodid,day)
db2013 <- merge(db2013, joinout[,list(day,PM10,aodid)], all.x = T)
head(db2013)
summary(db2013$PM10)


setnames(db2013,"PM10","closest_PM10")


#take out uneeded


#save mod3 
gc()
saveRDS(db2013,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/AQ.PM25.2013.mod3.rds")
x1db2013<- readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/AQ.PM25.2013.mod3.rds")

#calculate weights
x1db2013[, m := as.numeric(format(day, "%m")) ]
x1db2013<-x1db2013[,obs:=1]
x1db2013[is.na(aod), obs:= 0]
ws.2013<-select(x1db2013,obs,Elev,hpbl,m,Temp,aodid,day)

#to save memory
gc()

w1 <- glm(obs ~ Elev+Temp+hpbl+as.factor(m),family=binomial,data=ws.2013)
ws.2013$prob <- predict(w1 ,type = c("response"))  
ws.2013$wt <- 1/ws.2013$prob
#ws.2013$normwt <- ws.2013$wt/mean(ws.2013$wt)
#tray scaled and compare
ws.2013$normwt <- scale(ws.2013$wt)
ws.2013[, c("prob", "wt","obs","Elev", "hpbl" , "m","Temp"  ) := NULL]
gc()

setkey(x1db2013,aodid,day)
setkey(ws.2013,aodid,day)
x1db2013 <- merge(x1db2013,ws.2013,all.x = T)

#ADD HOUR SPECIFIC VARIABLES 

#SPLIT the DATA
#create mod 2 file
db2013.m2 <- db2013[!is.na(aod.t)]
#rm db2013
rm(x1db2013)
gc()
#save mod2
saveRDS(db2013.m2,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/AQ.PM25.2013.mod2.rds")
gc()



#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
#x1db2013days <- sort(unique(db2013.m2$day))
# ADD PM2.5
jointo.pt <- makepointsmatrix(datatable = pmall2013, 
                             xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2013, 
                               xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                       jointo = pmall2013 , joinfrom = db2013, 
                       jointovarname = "stn", joinfromvarname = "aodid", 
                       joinprefix = "nearest", valuefield = "aod", 
                       knearest = 9, maxdistance = 1100, 
                       nearestmean = TRUE, verbose = T)


setkey(pmall2013,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(pmall2013, joinout, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

PM25.m1<-filter(PM25.m1, aod <1.4)

PM25.m1<-filter(PM25.m1,UN >0  & UN  <0.04)

summary(lm(PM25.m1$PM25~PM25.m1$nearestmean)) 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                       jointo = pmall2013 , joinfrom = db2013, 
                       jointovarname = "stn", joinfromvarname = "aodid", 
                       joinprefix = "aodmean", valuefield = "aod", 
                       knearest = 9, maxdistance = 3000, 
                       nearestmean = TRUE, verbose = T)

setkey(PM25.m1,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(PM25.m1, joinout[,list(stn,day,aodmeanmean)], all.x = T)


#save mod 1
saveRDS(PM25.m1,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/AQ.PM25.2013.mod1.rds")


mod1=readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/AQ.PM25.2013.mod1.rds")
mod2=readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/AQ.PM25.2013.mod2.rds")
mod3=readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/AQ.PM25.2013.mod3.rds")
