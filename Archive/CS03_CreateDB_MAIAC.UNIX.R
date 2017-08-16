### This code explores the basic correlation between the new MAIAC version (08.2016) and 
### the PM in Israel

rm(list=ls())

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
library("sp")
library("rgdal")

#load aod data
# new MAIAC data (08.2016)
maiac=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/MAIACTAOT_Israel_2011.csv")

# cutting the data according to the project area 
# pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
# # pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Tel_aviv")
# # Convert to data.frame
# maiac = as.data.frame(maiac)
# # Spatial subset
# coordinates(maiac) = ~ lon + lat
# proj4string(maiac) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# maiac = maiac[pol, ]
# # Convert back to data.table
# maiac = as.data.table(maiac)

# creat id field
maiac$aodid=paste(formatC(round(maiac$lon,3),format='f',3),formatC(round(maiac$lat,3),format='f',3),sep="-")

# set names abbriviations
setnames(maiac,"AOT_Uncertainty","UN")
setnames(maiac,"AOT_QA","QA")
setnames(maiac,"Optical_Depth_047","aod_047")
setnames(maiac,"Optical_Depth_055","aod_055")
setnames(maiac,"date","day")
setnames(maiac,"lon","long_aod")
setnames(maiac,"lat","lat_aod")

#create single aod point per aodid per day
maiac1 <-maiac %>%
  group_by(aodid,day) %>%
  summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod_047=mean(aod_047,na.rm=TRUE),aod_055=mean(aod_055,na.rm=TRUE),UN=mean(UN,na.rm=TRUE),QA=mean(QA,na.rm=TRUE),RelAZ=mean(RelAZ,na.rm=TRUE))

# saving as shapefile only unique id
# maiac=as.data.table(maiac)
# setkey(maiac,aodid)
# maiac_grid=maiac[!duplicated(aodid)]
# coordinates(maiac_grid) = ~ long_aod + lat_aod
# proj4string(maiac_grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# writeOGR(maiac_grid,dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/IL_maiac.grid.unique.north/MAIAC_grid_082016", layer="1km_maiac_grid_latlon", driver="ESRI Shapefile")

# converting to ITM crs and adding X and Y columns
# maiac = as.data.frame(maiac)
# coordinates(maiac) = ~ long_aod + lat_aod
# proj4string(maiac) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# maiac$long_aod2 <-  coordinates(maiac)[,1]
# maiac$lat_aod2 <-  coordinates(maiac)[,2]
# maiac2 = spTransform(maiac, "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs")
# maiac2$x_aod_ITM <-  coordinates(maiac2)[,1]
# maiac2$y_aod_ITM <-  coordinates(maiac2)[,2]
# maiac2=as.data.table(maiac2)
# maiac2[,c("long_aod","lat_aod"):=NULL]
# setnames(maiac2,"long_aod2","long_aod")
# setnames(maiac2,"lat_aod2","lat_aod")
# maiac=maiac2
# rm(maiac2)
maiac$day=as.Date(maiac$day)
maiac=as.data.table(maiac)

## Add General variables

# creating a filter field of the forward scattering (0) and the backward scaterring (1)
maiac$FS_BS=0
# First option for data devision be Zenit angle:
maiac <- maiac[RelAZ< 90, FS_BS := 1]
maiac <- maiac[RelAZ>= 90 & RelAZ< 100, FS_BS := 2]
maiac <- maiac[RelAZ>= 100 & RelAZ< 110, FS_BS := 3]
maiac <- maiac[RelAZ>= 110 & RelAZ< 120, FS_BS := 4]
maiac <- maiac[RelAZ>= 120 & RelAZ< 130, FS_BS := 5]
maiac <- maiac[RelAZ>= 130 & RelAZ< 140, FS_BS := 6]
maiac <- maiac[RelAZ>= 140 & RelAZ< 150, FS_BS := 7]
maiac <- maiac[RelAZ>= 150 & RelAZ< 160, FS_BS := 8]
maiac <- maiac[RelAZ>= 160 & RelAZ< 170, FS_BS := 9]
maiac <- maiac[RelAZ>= 170 & RelAZ< 180, FS_BS := 10]

# Second option for data devision be Zenit angle:
maiac$FS_BS_2=0
maiac <- maiac[RelAZ< 90, FS_BS_2 := 1]
maiac <- maiac[RelAZ>= 90 & RelAZ< 120, FS_BS_2:= 2]
maiac <- maiac[RelAZ>= 120 & RelAZ< 150, FS_BS_2 := 3]
maiac <- maiac[RelAZ>= 150 & RelAZ< 180, FS_BS_2 := 4]
maiac <- maiac[is.na(RelAZ), FS_BS_2 := 5]

#add season
maiac$month <- as.numeric(format(maiac$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
maiac$season<-recode(maiac$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
maiac$seasonSW<-recode(maiac$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

################ add Spatial Variables
#load clipped/LU grid 
lu<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1_km_maiac_grid.csv")
all(lu$aodid %in% maiac$aodid)
mean(lu$aodid %in% maiac$aodid)
maiac <- maiac[maiac$aodid %in% lu$aodid, ]

#create full LU-aod TS
days<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
days2011 <- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
days2011$aodid<-as.character(days2011$aodid)

#merge maiac data
setkey(maiac,aodid,day)
setkey(days2011 ,aodid,day)
db2011 <- merge(days2011,maiac, all.x = T)

# precentage of NA in the data
sum(is.na(db2011$aod_055))*100/length(db2011$aod_055)

#add land use data
setkey(db2011,aodid)
setkey(lu,aodid)
db2011 <- merge(db2011, lu, all.x = T)
head(db2011)
gc()
#get rid of duplicate names to cut down on DB size
db2011[,c("lat","lon"):=NULL]
gc()
# setnames(db2011,"long_aod.x","long_aod")
# setnames(db2011,"lat_aod.x","lat_aod")
# setnames(db2011,"x_aod_ITM.y","x_aod_ITM")
# setnames(db2011,"y_aod_ITM.y","y_aod_ITM")


#Extracting information from QA
# extract the snow variable and adjacency in the second example 
# for that you need to know the position of each one ie 4:5 etc. 
system.time(db2011[, CloudMask := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}))])
# system.time(db2011[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
# system.time(db2011[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
# system.time(db2011[, CloudDetection := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[9:12]), collapse = "")}))])
# system.time(db2011[, AerosolModel := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[14:15]), collapse = "")}))])
# 
# summary(db2011$CloudMask)
# summary(db2011$MaskLandWaterSnow)
# summary(db2011$AerosolModel)

#save maiac aod data for 2011
# saveRDS(db2011,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/AQ.AOD.2011.data.rds")
saveRDS(db2011,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/TR.AOD.2011.data.rds")
db2011=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/TR.AOD.2011.data.rds")
# db2011=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/AQ.AOD.2011.data.rds")

################ add TEMPORAL Variables

#add ndvi
##join first ndviid to db2011 then join by aodid and month

########### import datasets
##### Add NDVI

#import NDVI
ndvi<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
ndvi=filter(ndvi,c==2011)

setnames(db2011,"month","m")
db2011$m=as.character(db2011$m)

# add ndviid to db2011
#join actual NDVI to aod
setkey(ndvi, ndviid, m)
setkey(db2011,ndviid, m)
db2011<- merge(db2011, ndvi,all.x = T)
#delete unnecessery columns
db2011[,c("lat_ndvi","long_ndvi"):=NULL]

###### Add Pbl
pbl<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_11_12am.csv")
pbl$pblid=paste(pbl$lon,pbl$lat, sep="_")
setnames(pbl,"date","day")
setnames(pbl,"PBLid","pblid")
pbl$day=as.Date(pbl$day)

#create single pbl point per day
pbld <-pbl %>%
  group_by(pblid,day) %>%
  summarise(lon_pbl=mean(lon),lat_pbl=mean(lat),pbl=mean(hpbl) )

#join pbl to aod
setnames(db2011,"PBLid","pblid")

setkey(pbl, pblid, day )
setkey(db2011,  pblid, day)
db2011 <- merge(db2011, pbl, all.x = T)

db2011[,c("V1.y","lon","lat","c"):=NULL]

###### Add Temperature

## Hourly Temperature (for AQUA- average of 11:30 to 14:30)
Temp <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/Temp_H.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2011]

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Temp, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = Temp, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Temp", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,Temp,aodid)], all.x = T)
head(db2011)
summary(db2011$Temp)
setnames(db2011,"Temp.x","Temp_H") # Hourly temperature

## Daily Temperature 
Temp <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/IMS_stn_July16/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2011]

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Temp, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = Temp, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Temp", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,Temp,aodid)], all.x = T)
head(db2011)
summary(db2011$Temp)
setnames(db2011,"Temp.y","Temp_D") # Daily temperature

###### Add Hourly average WD
WD <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/WD_H.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2011]

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = WD, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = WD, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "WD", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,WD,aodid)], all.x = T)
head(db2011)
summary(db2011$WD)
setnames(db2011,"WD","WD_H")

###### Add WS
WS <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/WS_H.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2011]

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = WS, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = WS, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "WS", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,WS,aodid)], all.x = T)
head(db2011)
summary(db2011$WS)
setnames(db2011,"WS","WS_H")

###### Add RH
RH <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/RH_H.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2011]

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = RH, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = RH, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "RH", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,RH,aodid)], all.x = T)
head(db2011)
summary(db2011$RH)
setnames(db2011,"RH","RH_H")

###### Add Rain
## Hourly Rain
Rain <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/Rain_H.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2011]

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Rain, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = Rain, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Rain", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,Rain,aodid)], all.x = T)
head(db2011)
summary(db2011$Rain)
setnames(db2011,"Rain","Rain_H")

## Daily Rain
Rain <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/IMS_stn_July16/Rain_D_Sum.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2011]

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Rain, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = Rain, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Rain", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,Rain,aodid)], all.x = T)
head(db2011)
summary(db2011$Rain)
setnames(db2011,"Rain","Rain_D")

###### Add NO2
NO2 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/NO2_H.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2011]

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = NO2, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = NO2, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "NO2", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,NO2,aodid)], all.x = T)
head(db2011)
summary(db2011$NO2)
setnames(db2011,"NO2","NO2_H")

## Add Daily PM25

# Add Daily closest PM2.5

PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Pollution_stn_May16/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2011<- PM25[c==2011]


# Join the closest PM2.5 value for each day

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pmall2011, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = pmall2011, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM25", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,PM25,aodid)], all.x = T)
head(db2011)
summary(db2011$PM25)

setnames(db2011,"PM25","PM25_D_closest")

# Add daily mean PM2.5 

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pmall2011, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = pmall2011, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM25", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,PM25,aodid)], all.x = T)
head(db2011)
summary(db2011$PM25)

setnames(db2011,"PM25","PM25_D_mean")

## Add IDW PM2.5

# calculate IDW for PM2.5

for(i in unique(db2011$day)) {
  
  x<-pmall2011[pmall2011$day==i, ]
  y= db2011[db2011$day==i, ]
  
  library(gstat)
  #defaults to idw (gstat)
  library(sp)
  coordinates(x) = ~ x_stn_ITM + y_stn_ITM
  coordinates(y) = ~ x_aod_ITM + y_aod_ITM
  #location statment uneeded since we defined coordinates
  inter = gstat(formula = PM25 ~ 1,  data =x)
  z<-predict(object = inter, newdata = y)
  # head(z)
  db2011$pred[db2011$day==i] = z$var1.pred
  # spplot(z, "var1.pred", at = 0:100)
}

setnames(db2011,"pred","PM25_IDW")

saveRDS(db2011,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/AQ.AOD.2011.Tmp_Sp.rds")

# Add mean PM25 without distance limitation

pm.m <- makepointsmatrix(pmall2011, "x_stn_ITM", "y_stn_ITM", "stn")

aod.m <- makepointsmatrix(db2011[db2011[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            db2011, pmall2011 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = NA, nearestmean = T)

#create single aod point per aodid per day 
x <-closestaodse %>%
  group_by(aodid,day) %>%
  summarise(meanpm25=mean(closestmean) )

#join to DB
setkey(x,aodid,day)
setkey(db2011,aodid,day)
db2011 <- merge(db2011,x,all.x = T)

summary(lm(db2011$PM25_IDW~db2011$meanpm25.x))

summary(lm(aod~meanpm25.x,data=db2011))

#### ADD PM10 

#MEAN PM10

PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Pollution_stn_May16/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm_10all2011<- PM10[c==2011]

# Join the closest PM10 value for each day

jointo.pt <- makepointsmatrix(datatable = db2011, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pm_10all2011, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2011, joinfrom = pm_10all2011, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM10", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2011,aodid,day)
setkey(joinout,aodid,day)
db2011 <- merge(db2011, joinout[,list(day,PM10,aodid)], all.x = T)
head(db2011)
summary(db2011$PM10)

setnames(db2011,"PM10","PM10_D_closest")

#take out uneeded

#save mod3 
gc()
saveRDS(db2011,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQ.PM25.2011.mod3.rds")
x1db2011<- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQ.PM25.2011.mod3.rds")

#calculate weights
x1db2011[, m := as.numeric(format(day, "%m")) ]
x1db2011<-x1db2011[,obs:=1]
x1db2011[is.na(aod_055), obs:= 0]
ws.2011<-select(x1db2011,obs,Elev,hpbl,m,Temp_D,aodid,day)

#to save memory
gc()

w1 <- glm(obs ~ Elev+Temp_D+hpbl+as.factor(m),family=binomial,data=ws.2011)
ws.2011$prob <- predict(w1 ,type = c("response"))  
ws.2011$wt <- 1/ws.2011$prob
#ws.2011$normwt <- ws.2011$wt/mean(ws.2011$wt)
#tray scaled and compare
ws.2011$normwt <- scale(ws.2011$wt)
ws.2011[, c("prob", "wt","obs","Elev", "hpbl" , "m","Temp_D"  ) := NULL]
gc()

setkey(x1db2011,aodid,day)
setkey(ws.2011,aodid,day)
x1db2011 <- merge(x1db2011,ws.2011,all.x = T)

#ADD HOUR SPECIFIC VARIABLES 

#SPLIT the DATA
#create mod 2 file
db2011.m2 <- x1db2011[!is.na(aod_055)]
#rm db2011
rm(x1db2011)
gc()
#save mod2
saveRDS(db2011.m2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQ.PM25.2011.mod2.rds")
gc()


# building model 1 -MOD1
#MEAN PM25- Daily mean PM2.5

PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Pollution_stn_May16/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]

# Add field classification (General or transportation)
PM_Type <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM_monitors_classification/PM_monitors.csv")
setnames(PM_Type,"Code","stn")
PM_Type$stn=substr(PM_Type$stn, 2, 4)
PM25=left_join(PM25,PM_Type,by="stn")
PM25=as.data.table(PM25)
PM25[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM25$stn_type<-0
PM25[Type=="'Gener'",stn_type:=1]
PM25[Type=="'Trans'",stn_type:=0]
PM25[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2011<- PM25[c==2011]
setnames(pmall2011,"X.x","x_stn_ITM")
setnames(pmall2011,"Y.x","y_stn_ITM")

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
#x1db2005days <- sort(unique(db2005.m2$day))
# ADD PM2.5
jointo.pt <- makepointsmatrix(datatable = pmall2011, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2011, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2011 , joinfrom = db2011, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)


setkey(pmall2011,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(pmall2011, joinout, all.x = T)

PM25.m1<-PM25.m1[!is.na(aod_055)]
setnames(PM25.m1,"nearestmean", "aod_055_mean")

PM25.m1$nearestknn=NULL
PM25.m1$nearestnobs=NULL

#save mod 1
# saveRDS(PM25.m1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2011.PM25.rds")
saveRDS(PM25.m1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2011.PM25.rds")
# PM25.m1=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2011.PM25.rds")
PM25.m1=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2011.PM25.rds")

# joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
#                         jointo = pmall2005 , joinfrom = db2005, 
#                         jointovarname = "stn", joinfromvarname = "aodid", 
#                         joinprefix = "aodmean", valuefield = "aod", 
#                         knearest = 9, maxdistance = 3000, 
#                         nearestmean = TRUE, verbose = T)
# 
# setkey(PM25.m1,stn,day)
# setkey(joinout,stn,day)
# PM25.m1 <- merge(PM25.m1, joinout[,list(stn,day,aodmeanmean)], all.x = T)

summary(PM25.m1$aod_055_mean)
PM25.m1<-filter(PM25.m1,aod_055_mean < 1.5)
# basic correlation
summary(lm(PM25.m1$PM25~PM25.m1$aod_047)) 
summary(lm(PM25.m1$PM25~PM25.m1$aod_055_mean)) 
plot(PM25.m1$PM25~PM25.m1$aod_055_mean,type= "p",ylim=c(0,300),ylab="PM2.5",xlab="AOD",main="PM2.5 ~ AOD_055")


# correlation after filtering
PM25.m1<-filter(PM25.m1,UN >0  & UN  <0.04)
PM25.m1=filter(PM25.m1,aod_055_mean < 1.5)
PM25.m1_fs=filter(PM25.m1,FS_BS !=1)
summary(lm(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean))
plot(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean,type= "p",ylim=c(0,300),ylab="PM2.5",xlab="AOD",main="PM2.5 ~ AOD_055_FS")
plot(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean,type= "p",ylim=c(0,300),ylab="PM2.5",xlab="AOD",main="PM2.5 ~ AOD_055_BS")

# Correlation between PM~ AOD in each azimuth bin (steps of 10)
R2=c()
len=c()
for (i in 1:10)
{
  PM25.m1_fs=filter(PM25.m1,FS_BS == i)
  if (nrow(PM25.m1_fs)!=0)
  {
    a=round(summary(lm(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean))$r.squared,2)  
    R2=c(R2,a)
    b=nrow(PM25.m1_fs)
    len=c(len,b)
  }
  else
  {
   a=NA
   b=0
   R2=c(R2,a)
   len=c(len,b)
  }
}

print(R2)
print(len)
res=data.frame(R2,len)
plot(R2)
write.csv(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm_aod_sensetivity_to_BS/2011_sen.csv")

# Correlation between PM~ AOD in each azimuth bin (steps of 30)
R2=c()
len=c()
for (i in 1:4)
{
  PM25.m1_fs=filter(PM25.m1,FS_BS_2 == i)
  if (nrow(PM25.m1_fs)!=0)
  {
    a=round(summary(lm(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean))$r.squared,2)  
    R2=c(R2,a)
    b=nrow(PM25.m1_fs)
    len=c(len,b)
  }
  else
  {
    a=NA
    b=0
    R2=c(R2,a)
    len=c(len,b)
  }
}

print(R2)
print(len)
res=data.frame(R2,len)
plot(R2)
write.csv(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm_aod_sensetivity_to_BS/2011_sen_2.csv")

sen=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm_aod_sensetivity_to_BS/sens_all_years.csv",header = TRUE)
sen=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm_aod_sensetivity_to_BS/sens_all_years_2.csv",header = TRUE)
plot(sen$`2010`, col="RED",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2")
points(sen$`2011`, col="BLUE",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",add=TRUE)
points(sen$`2012`, col="GRAY",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",add=TRUE)
points(sen$`2013`, col="BLACK",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",add=TRUE)
points(sen$`2011`, col="ORANGE",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",add=TRUE)

# add variable that excludes aod observations higher than 2.5
PM25.m1$filt_2.5=0
PM25.m1<-PM25.m1[aod_055>2.5, filt_1.5:= 1]
# add variable that excludes aod observations higher than 2.5
PM25.m1$filt_1.5=0
PM25.m1<-PM25.m1[aod_055>1.5, filt_1.5:= 1]

################### cleaning mod 1
mod1=PM25.m1
mod1=as.data.table(mod1)
setnames(mod1,"PM25", "pm25")
setnames(mod1,"aod_055_mean", "aod")

#delete water flags
mod1<-filter(mod1, MaskLandWaterSnow=="00")
#mod1<-filter(mod1, AerosolModel=="10")
# delete clouded flags
mod1<-filter(mod1, CloudMask=="001")
mod1<-filter(mod1, MaskAdjacency=="000")
mod1<-filter(mod1, CloudDetection=="0000")
mod1=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2011.PM25.rds")

#filter nasa
mod1<-filter(mod1,UN >0  & UN  <0.04)

#massimos thresholds
x<-select(mod1,aod,stn)
x$c<-1
x <- x %>%
  group_by (stn) %>%
  summarise(saod=sum(c))
#merge back count
setkey(x,stn)
setkey(mod1,stn)
mod1 <- merge(mod1,x, all.x = T)

mod1$exobs<-0
mod1<-mod1[aod < quantile(aod, c(.50)) & pm25 >  quantile(pm25, c(.90)), exobs := 2]
mod1<-mod1[aod > quantile(aod, c(.90)) & pm25 <  quantile(pm25, c(.50)), exobs := 3]
mod1<-mod1[aod > 1.8 , exobs := 4]
mod1<-mod1[saod < 30 , exobs := 5]

#take out bad exobs
mod1<-filter(mod1,exobs==0)
saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_clean/mod1.AQ.2011.PM25.clean.rds")
saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_clean/mod1.TR.2011.PM25.clean.rds")


### simple linear modeling
summary(lm(mod1$pm25~mod1$aod))
plot(mod1$pm25~mod1$aod,type= "p",ylim=c(0,300),ylab="PM2.5",xlab="AOD",main="PM2.5 ~ AOD_055_clean")

### Mixed effect modeling
m1.formula <- as.formula(pm25~aod+(1+aod|day))
m1_sc <- lmer(m1.formula,data=mod1)
mod1$pred.m1 <- predict(m1_sc)
#check fits of model
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
