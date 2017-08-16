### This code builds the 3 input databases (mod1, mod2, mod3) for the model from the new MAIAC version (08.2016) 

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
install_github("allanjust/aodlur", dependencies = TRUE)
library("aodlur")
library(sp)
library(rgdal)
library(stringi)

#load aod data from new MAIAC data (08.2016)
maiac=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/MAIACTAOT_Israel_2006.csv")
# maiac=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/MAIACAAOT_Israel_2006.csv")

# cutting the data according to the project area 
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
# pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Tel_aviv")
# Convert to data.frame
maiac = as.data.frame(maiac)
# Spatial subset
coordinates(maiac) = ~ lon + lat
proj4string(maiac) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
maiac = maiac[pol, ]
# Convert back to data.table
maiac = as.data.table(maiac)

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

# saveRDS(maiac,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/raw_data/AOD.AQ.2006.RDS")
saveRDS(maiac,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/raw_data/AOD.TR.2006.RDS")
# maiac=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/raw_data/AOD.TR.2006.RDS")

# Use the QA data to remove problematic observations
# The QA have to be used bofore the next stage of creating a single aod point per aodid per day

system.time(maiac[, CloudMask := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}))])
system.time(maiac[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(maiac[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
system.time(maiac[, CloudDetection := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[9:12]), collapse = "")}))])
system.time(maiac[, GlintMask := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[13]), collapse = "")}))])
system.time(maiac[, AerosolModel := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[14:15]), collapse = "")}))])

summary(maiac$CloudMask)
summary(maiac$MaskLandWaterSnow)
summary(maiac$CloudDetection)
summary(maiac$AerosolModel)
summary(maiac$GlintMask)
summary(maiac$MaskAdjacency)

# remove cloudy QA
maiac=filter(maiac,CloudMask!="011")
maiac=filter(maiac,CloudMask!="010")
# remove observatiobs surrounded  by more than 8 cloudy pixels QA
maiac=filter(maiac,MaskAdjacency!="010")
# remove water QA
maiac=filter(maiac,MaskLandWaterSnow!="01")
# remove Adjacent to snow QA
maiac=filter(maiac,MaskAdjacency!="100")

# create single aod point per aodid per day
maiac <-maiac %>%
  group_by(aodid,day) %>%
  summarise(long_aod=mean(long_aod,na.rm=TRUE),lat_aod=mean(lat_aod,na.rm=TRUE),aod_047=mean(aod_047,na.rm=TRUE),aod_055=mean(aod_055,na.rm=TRUE),UN=mean(UN,na.rm=TRUE),RelAZ=mean(RelAZ,na.rm=TRUE))

# saving as shapefile only unique id
# maiac=as.data.table(maiac)
# setkey(maiac,aodid)
# maiac_grid=maiac[!duplicated(aodid)]
# coordinates(maiac_grid) = ~ long_aod + lat_aod
# proj4string(maiac_grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# # writeOGR(maiac_grid,dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/IL_maiac.grid.unique.north/MAIAC_grid_082016", layer="1km_maiac_grid_latlon", driver="ESRI Shapefile")

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

# creating a filter field of the forward scattering (FS=1) and the backward scaterring (BS=0 or else)
# maiac$FS_BS=1
# # First option for data devision be Azimuth angle:
# maiac <- maiac[RelAZ> 90, FS_BS := 0]

# maiac$FS_BS_1=0
# # First option for data devision be Azimuth angle:
# maiac <- maiac[RelAZ< 90, FS_BS_1 := 1]
# maiac <- maiac[RelAZ>= 90 & RelAZ< 100, FS_BS_1 := 2]
# maiac <- maiac[RelAZ>= 100 & RelAZ< 110, FS_BS_1 := 3]
# maiac <- maiac[RelAZ>= 110 & RelAZ< 120, FS_BS_1 := 4]
# maiac <- maiac[RelAZ>= 120 & RelAZ< 130, FS_BS_1 := 5]
# maiac <- maiac[RelAZ>= 130 & RelAZ< 140, FS_BS_1 := 6]
# maiac <- maiac[RelAZ>= 140 & RelAZ< 150, FS_BS_1 := 7]
# maiac <- maiac[RelAZ>= 150 & RelAZ< 160, FS_BS_1 := 8]
# maiac <- maiac[RelAZ>= 160 & RelAZ< 170, FS_BS_1 := 9]
# maiac <- maiac[RelAZ>= 170 & RelAZ< 180, FS_BS_1 := 10]
# 
# # Second option for data devision be Zenit angle:
# maiac$FS_BS_2=0
# maiac <- maiac[RelAZ< 90, FS_BS_2 := 1]
# maiac <- maiac[RelAZ>= 90 & RelAZ< 120, FS_BS_2:= 2]
# maiac <- maiac[RelAZ>= 120 & RelAZ< 150, FS_BS_2 := 3]
# maiac <- maiac[RelAZ>= 150 & RelAZ< 180, FS_BS_2 := 4]
# maiac <- maiac[is.na(RelAZ), FS_BS_2 := 5]

#add season
maiac$month <- as.numeric(format(maiac$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
maiac$season<-car::recode(maiac$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
maiac$seasonSW<-car::recode(maiac$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")


################ add Spatial Variables
#load clipped/LU grid 
lu<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid_fixed.csv")

lu$V1=NULL
maiac$V1=NULL
all(lu$aodid %in% maiac$aodid)
mean(lu$aodid %in% maiac$aodid)
maiac <- maiac[maiac$aodid %in% lu$aodid, ]

#create full LU-aod TS
days<-seq.Date(from = as.Date("2006-01-01"), to = as.Date("2006-12-31"), 1)
#create date range
days2006 <- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
days2006$aodid<-as.character(days2006$aodid)

#merge maiac data
setkey(maiac,aodid,day)
setkey(days2006 ,aodid,day)
db2006 <- merge(days2006,maiac, all.x = T)

# precentage of NA in the data
sum(is.na(db2006$aod_055))*100/length(db2006$aod_055)

#add land use data
setkey(db2006,aodid)
setkey(lu,aodid)
db2006 <- merge(db2006, lu, all.x = T)
head(db2006)
gc()
#get rid of duplicate names to cut down on DB size
# db2006[,c("lat","lon"):=NULL]
# gc()
# setnames(db2006,"long_aod.x","long_aod")
# setnames(db2006,"lat_aod.x","lat_aod")
# setnames(db2006,"x_aod_ITM.y","x_aod_ITM")
# setnames(db2006,"y_aod_ITM.y","y_aod_ITM")

db2006[,c("V1","lon","lat"):=NULL]

#save maiac aod data for 2006
# saveRDS(db2006,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/AQ.AOD.2006.data.rds")
saveRDS(db2006,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/TR.AOD.2006.data.rds")
# db2006=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/TR.AOD.2006.data.rds")
# db2006=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/maiac_aod/AQ.AOD.2006.data.rds")

################ add TEMPORAL Variables

#### add ndvi

#import NDVI
ndvi<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
ndvi=as.data.table(ndvi)
ndvi=filter(ndvi,c=="2006")
ndvi$m<-stri_pad_left(str=ndvi$m, 2, pad="0")

db2006$m=substr(db2006$day,6,7)

# add ndviid to db2006
#join actual NDVI to aod
setkey(ndvi, ndviid, m)
setkey(db2006,ndviid, m)
db2006<- merge(db2006, ndvi,all.x = T)

#delete unnecessery columns
db2006[,c("lat_ndvi","long_ndvi","c.y"):=NULL]

###### Add Pbl

# add daily average PBL
pblid=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/1km_grid_PBLid/1km_grid_PBLid.csv")
mean(unique(pblid$aodid) %in% unique(db2006$aodid))

hpbl=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_dailyavg.csv")
hpbl$year=substr(hpbl$date,1,4)
hpbl=filter(hpbl,hpbl$year=="2006")
hpbl=as.data.table(hpbl)
setnames(hpbl,"PBLid","pblid")
setnames(hpbl,"date","day")
hpbl$day=as.Date(hpbl$day)
hpbl[,c("V1","year"):=NULL]

setkey(db2006,pblid,day)
setkey(hpbl,pblid,day)
db2006 <- merge(db2006,hpbl,all.x = T)
#
# ## ADD ventilation coefficient
db2006$vc_D=c(db2006$WS_D/(db2006$daily_hpbl*1000))
# mod1$vc_H=c(mod1$WS_H/(mod1$pbl_11*1000))

#add overpass PBL 
# pbl<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_11_12am.csv")
pbl<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_8_9am.csv")
pbl$pblid=paste(formatC(round(pbl$lon,3),format='f',3),formatC(round(pbl$lat,3),format='f',3),sep="-")
setnames(pbl,"date","day")
pbl$day=as.Date(pbl$day)

#create single pbl point per day
pbl <-pbl %>%
  group_by(pblid,day) %>%
  summarise(lon_pbl=mean(lon),lat_pbl=mean(lat),pbl=mean(hpbl) )

pbl=as.data.table(pbl)
#join pbl to aod
setkey(pbl, pblid, day )
setkey(db2006,  pblid, day)
db2006 <- merge(db2006, pbl, all.x = T)

db2006[,c("lon_pbl","lat_pbl"):=NULL]
db2006=as.data.table(db2006)
summary(db2006$pbl)

setnames(db2006,"pbl","pbl_08")
# setnames(db2006,"pbl","pbl_11")

#add night PBL         
pbl<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_2_3am.csv")
pbl$pblid=paste(formatC(round(pbl$lon,3),format='f',3),formatC(round(pbl$lat,3),format='f',3),sep="-")
setnames(pbl,"date","day")
pbl$day=as.Date(pbl$day)

#create single pbl point per day
pbl <-pbl %>%
  group_by(pblid,day) %>%
  summarise(lon_pbl=mean(lon),lat_pbl=mean(lat),pbl=mean(hpbl))

pbl=as.data.table(pbl)

#join pbl to aod
setkey(pbl, pblid, day )
setkey(db2006,  pblid, day)
db2006 <- merge(db2006, pbl, all.x = T)

db2006=as.data.table(db2006)
db2006[,c("lon_pbl","lat_pbl"):=NULL]

setnames(db2006,"pbl","pbl_02")
summary(db2006$pbl_02)

#### Add daily average PBL
hpbl=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_dailyavg.csv")
hpbl$year=substr(hpbl$date,1,4)
hpbl=filter(hpbl,hpbl$year=="2014")
hpbl=as.data.table(hpbl)
setnames(hpbl,"PBLid","pblid")
setnames(hpbl,"date","day")
hpbl$day=as.Date(hpbl$day)
hpbl[,c("V1","year"):=NULL]

setkey(db2006,pblid,day)
setkey(hpbl,pblid,day)
mod1 <- merge(db2006,hpbl,all.x = T)


###### Add Temperature

## Hourly Temperature (for AQUA- average of 11:30 to 14:30)
# Temp <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Hourly_Temp_aqua_IMS_Pollution_stn.csv")
## Hourly Temperature (for TERRA- average of 09:00 to 11:30)
Temp <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Hourly_Temp_terra_IMS_Pollution_stn.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Temp, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = Temp, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Temp", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,Temp,aodid)], all.x = T)
head(db2006)
summary(db2006$Temp)
setnames(db2006,"Temp","Temp_H") # Hourly temperature

## Daily Temperature 
Temp <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Daily_Temp_data_IMS_Pollution.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Temp, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = Temp, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Temp", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,Temp,aodid)], all.x = T)
head(db2006)
summary(db2006$Temp)
setnames(db2006,"Temp","Temp_D") # Daily temperature

###### Add Hourly average WD
# WD <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/WD_H.csv")
# WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
# WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
# WD[, c := as.numeric(format(day, "%Y")) ]
# WD[,c("Year","Month","Day","date"):=NULL]
# WD <- WD[X != 'NaN']
# WD <- WD[WD != 'NaN']
# WD <- WD[c == 2006]
# 
# jointo.pt <- makepointsmatrix(datatable = db2006, 
#                               xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 
# 
# joinfrom.pt <- makepointsmatrix(datatable = WD, 
#                                 xvar = "X", yvar = "Y", idvar = "stn") 
# 
# joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
#                         jointo = db2006, joinfrom = WD, 
#                         jointovarname = "aodid", joinfromvarname = "stn", 
#                         joinprefix = "nearest", valuefield = "WD", 
#                         knearest = 15, maxdistance = 60000, 
#                         nearestmean = FALSE, verbose = T)
# 
# setkey(db2006,aodid,day)
# setkey(joinout,aodid,day)
# db2006 <- merge(db2006, joinout[,list(day,WD,aodid)], all.x = T)
# head(db2006)
# summary(db2006$WD)
# setnames(db2006,"WD","WD_H")

###### Add Hourly WS
# for AQUA
# WS <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/WS_H.csv")
# for TERRA
WS <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Terra_Hourly_data_July16/WS_H.csv")

WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = WS, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = WS, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "WS", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,WS,aodid)], all.x = T)
head(db2006)
summary(db2006$WS)
setnames(db2006,"WS","WS_H")

###### Add daily WS
WS <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/IMS_stn_July16/WS_D.csv")

WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = WS, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = WS, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "WS", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,WS,aodid)], all.x = T)
head(db2006)
summary(db2006$WS)
setnames(db2006,"WS","WS_D")

###### Add Hourly RH
# for AQUA
# RH <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/RH_H.csv")
# for TERRA
RH <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Terra_Hourly_data_July16/RH_H.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = RH, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = RH, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "RH", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,RH,aodid)], all.x = T)
head(db2006)
summary(db2006$RH)
setnames(db2006,"RH","RH_H")

###### Add Daily RH
RH <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/IMS_stn_July16/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = RH, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = RH, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "RH", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,RH,aodid)], all.x = T)
head(db2006)
summary(db2006$RH)
setnames(db2006,"RH","RH_D")

###### Add hourly Rain
## for AQUA
# Rain <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Hourly_Rain_AQUA_IMS_Pollution_stn.csv")
## for TERRA
Rain <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Hourly_Rain_terra_IMS_Pollution_stn.csv")

Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Rain, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = Rain, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Rain", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,Rain,aodid)], all.x = T)
head(db2006)
summary(db2006$Rain)
setnames(db2006,"Rain","Rain_H")

## Daily Rain
Rain <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Daily_Rain_Sum_IMS_Pollutants.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Rain, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = Rain, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "Rain", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,Rain,aodid)], all.x = T)
head(db2006)
summary(db2006$Rain)
setnames(db2006,"Rain","Rain_D")

###### Add Hourly NO2
# for AQUA
# NO2 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/NO2_H.csv")
# for TERRA
NO2 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/NO2_H.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = NO2, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = NO2, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "NO2", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,NO2,aodid)], all.x = T)
head(db2006)
summary(db2006$NO2)
setnames(db2006,"NO2","NO2_H")

###### Add Daily NO2

NO2 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Pollution_stn_May16/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = NO2, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = NO2, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "NO2", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,NO2,aodid)], all.x = T)
head(db2006)
summary(db2006$NO2)
setnames(db2006,"NO2","NO2_D")

###### Add Hourly SO2
# for AQUA 
# SO2 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/SO2_H.csv")
# for TERRA
SO2 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/SO2_H.csv")
SO2$date<-paste(SO2$Day,SO2$Month,SO2$Year,sep="/")
SO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
SO2[, c := as.numeric(format(day, "%Y")) ]
SO2[,c("Year","Month","Day","date"):=NULL]
SO2 <- SO2[X != 'NaN']
SO2<- SO2[SO2 != 'NaN']
SO2<- SO2[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = SO2, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = SO2, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "SO2", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,SO2,aodid)], all.x = T)
head(db2006)
summary(db2006$SO2)
setnames(db2006,"SO2","SO2_H")

###### Add Daily SO2

SO2 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Pollution_stn_May16/SO2_D.csv")
SO2$date<-paste(SO2$Day,SO2$Month,SO2$Year,sep="/")
SO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
SO2[, c := as.numeric(format(day, "%Y")) ]
SO2[,c("Year","Month","Day","date"):=NULL]
SO2 <- SO2[X != 'NaN']
SO2<- SO2[SO2 != 'NaN']
SO2<- SO2[c == 2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = SO2, 
                                xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = SO2, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "SO2", 
                        knearest = 15, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,SO2,aodid)], all.x = T)
head(db2006)
summary(db2006$SO2)
setnames(db2006,"SO2","SO2_D")

#### Add Hourly mean PM2.5 
# for aqua
# PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/PM25_H.csv")

# for terra
PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/PM25_H.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2006<- PM25[c==2006]

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pmall2006, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = pmall2006, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM25", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,PM25,aodid)], all.x = T)
head(db2006)
summary(db2006$PM25)

setnames(db2006,"PM25","PM25_H_mean")

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
pmall2006<- PM25[c==2006]


# Join the closest PM2.5 value for each day

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pmall2006, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = pmall2006, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM25", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = FALSE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,PM25,aodid)], all.x = T)
head(db2006)
summary(db2006$PM25)

setnames(db2006,"PM25","PM25_D_closest")

# Add daily mean PM2.5 

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pmall2006, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = pmall2006, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM25", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,PM25,aodid)], all.x = T)
head(db2006)
summary(db2006$PM25)

setnames(db2006,"PM25","PM25_D_mean")

## Add IDW PM2.5

# calculate IDW for PM2.5

for(i in unique(db2006$day)) {
  
  x<-pmall2006[pmall2006$day==i, ]
  y= db2006[db2006$day==i, ]
  
  library(gstat)
  #defaults to idw (gstat)
  library(sp)
  coordinates(x) = ~ x_stn_ITM + y_stn_ITM
  coordinates(y) = ~ x_aod_ITM + y_aod_ITM
  #location statment uneeded since we defined coordinates
  inter = gstat(formula = PM25 ~ 1,  data =x)
  z<-predict(object = inter, newdata = y)
  # head(z)
  db2006$pred[db2006$day==i] = z$var1.pred
  # spplot(z, "var1.pred", at = 0:100)
}

setnames(db2006,"pred","PM25_IDW")

#### ADD Hourly PM10 
# PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/PM10_H.csv")
PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/PM10_H.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm_10all2006<- PM10[c==2006]

# Join the closest PM10 value for each day

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pm_10all2006, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = pm_10all2006, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM10", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,PM10,aodid)], all.x = T)
head(db2006)
summary(db2006$PM10)

setnames(db2006,"PM10","PM10_H_mean")

#### ADD Daily PM10 

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
pm_10all2006<- PM10[c==2006]

# Join the closest PM10 value for each day

jointo.pt <- makepointsmatrix(datatable = db2006, 
                              xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = pm_10all2006, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = db2006, joinfrom = pm_10all2006, 
                        jointovarname = "aodid", joinfromvarname = "stn", 
                        joinprefix = "nearest", valuefield = "PM10", 
                        knearest = 9, maxdistance = 60000, 
                        nearestmean = TRUE, verbose = T)

setkey(db2006,aodid,day)
setkey(joinout,aodid,day)
db2006 <- merge(db2006, joinout[,list(day,PM10,aodid)], all.x = T)
head(db2006)
summary(db2006$PM10)

setnames(db2006,"PM10","PM10_D_closest")
setnames(db2006,"PM10","PM10_D_mean")

## Add daily IDW PM10

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
pmall2006<- PM10[c==2006]

# calculate IDW for PM10

for(i in unique(db2006$day)) {
  
  x<-pmall2006[pmall2006$day==i, ]
  y= db2006[db2006$day==i, ]
  
  library(gstat)
  #defaults to idw (gstat)
  library(sp)
  coordinates(x) = ~ x_stn_ITM + y_stn_ITM
  coordinates(y) = ~ x_aod_ITM + y_aod_ITM
  #location statment uneeded since we defined coordinates
  inter = gstat(formula = PM10 ~ 1,  data =x)
  z<-predict(object = inter, newdata = y)
  # head(z)
  db2006$pred[db2006$day==i] = z$var1.pred
  # spplot(z, "var1.pred", at = 0:100)
}

setnames(db2006,"pred","PM10_IDW")

## Add ventilation coefficient variable (VC)
# db2006$vc_H= c(db2006$WS_H/(db2006$pbl_11*1000))
# db2006$vc_D= c(db2006$WS_D/(db2006$pbl_11*1000))
db2006$vc_H= c(db2006$WS_H/(db2006$pbl_08*1000))
db2006$vc_D= c(db2006$WS_D/(db2006$pbl_08*1000))

#take out uneeded

#save mod3 
gc()
saveRDS(db2006,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/TR.MAIAC.2006.mod3.rds")

# x1db2006<- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/TR.PM25.2006.mod3.rds")
# saveRDS(db2006,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQ.PM25.2006.mod3.rds")
# x1db2006<- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQ.PM25.2006.mod3.rds")

x1db2006<- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/TR.MAIAC.2006.mod3.rds")

#calculate weights
x1db2006[, m := as.numeric(format(day, "%m")) ]
x1db2006<-x1db2006[,obs:=1]
x1db2006[is.na(aod_055), obs:= 0]
ws.2006<-dplyr::select(x1db2006,obs,Elev,pbl_08,m,Temp_D,aodid,day)

#to save memory
gc()

w1 <- glm(obs ~ Elev+Temp_D+pbl_08+as.factor(m),family=binomial,data=ws.2006)
ws.2006$prob <- predict(w1 ,type = c("response"))  
ws.2006$wt <- 1/ws.2006$prob
#ws.2006$normwt <- ws.2006$wt/mean(ws.2006$wt)
#tray scaled and compare
ws.2006$normwt <- scale(ws.2006$wt)
ws.2006[, c("prob", "wt","obs","Elev", "pbl_08" , "m","Temp_D"  ) := NULL]
gc()

setkey(x1db2006,aodid,day)
setkey(ws.2006,aodid,day)
x1db2006 <- merge(x1db2006,ws.2006,all.x = T)
x1db2006[,c("m","obs"):=NULL]
saveRDS(x1db2006,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/TR.MAIAC.2006.mod3.rds")
# saveRDS(x1db2006,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQ.MAIAC.2006.mod3.rds")

# Create MOD2 

#SPLIT the DATA
#create mod 2 file
db2006.m2 <- db2006[!is.na(aod_055)]
#rm db2006
rm(x1db2006)
gc()
#save mod2
saveRDS(db2006.m2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/TR.MAIAC.2006.mod2.rds")
# saveRDS(db2006.m2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQ.MAIAC.2006.mod2.rds")
gc()


####### building model 1 -MOD1

### Create daily PM2.5 mod1
# daily database
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
PM25=dplyr::left_join(PM25,PM_Type,by="stn")
PM25=as.data.table(PM25)
PM25[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM25$stn_type<-0
PM25[Type=="'Gener'",stn_type:=1]
PM25[Type=="'Trans'",stn_type:=0]
PM25[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2006<- PM25[c==2006]
setnames(pmall2006,"X.x","x_stn_ITM")
setnames(pmall2006,"Y.x","y_stn_ITM")

# ADD AOD 055 to PM25 mod 1
jointo.pt <- makepointsmatrix(datatable = pmall2006, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2006, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2006 , joinfrom = db2006, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)

setkey(pmall2006,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(pmall2006, joinout, all.x = T)

PM25.m1<-PM25.m1[!is.na(aod_055)]
setnames(PM25.m1,"nearestmean", "aod_055_mean")

PM25.m1[,nearestknn:=NULL]
PM25.m1[,nearestnobs:=NULL]
PM25.m1[,c.y:=NULL]
setnames(PM25.m1,"c.x", "year")

# ADD AOD 047
db2006_s=db2006[, c("aodid","x_aod_ITM","y_aod_ITM","aod_047","day"), with = FALSE]

jointo.pt <- makepointsmatrix(datatable = PM25.m1, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2006_s, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = PM25.m1 , joinfrom = db2006_s, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)

setkey(PM25.m1,stn,day)
setkey(joinout,stn,day)
PM25.m1<- merge(PM25.m1, joinout, all.x = T)

setnames(PM25.m1,"nearestmean", "aod_047_mean")
setnames(PM25.m1,"aod_047.x", "aod_047")
setnames(PM25.m1,"x_aod_ITM.x", "x_aod_ITM")
setnames(PM25.m1,"y_aod_ITM.x", "y_aod_ITM")
PM25.m1[,c("nearest.x","nearestknn","nearestnobs","x_aod_ITM.y", "y_aod_ITM.y", "aod_047.y","nearest.y"):=NULL]


# add variable that excludes aod observations higher than 2.5
# PM25.m1$filt_2.5=0
# PM25.m1<-PM25.m1[aod_055_mean>2.5, filt_2.5:= 1]
# # add variable that excludes aod observations higher than 2.5
# PM25.m1$filt_1.5=0
# PM25.m1<-PM25.m1[aod_055>1.5, filt_1.5:= 1]

# Join 200 m spatial variables
# add 200 m key field to the database
key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

setkey(PM25.m1,stn)
setkey(key_field,stn)
PM25.m1= merge(PM25.m1,key_field,all.x = T)

lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")

setkey(lu_200m,Key200_id)
setkey(PM25.m1,Key200_id)
PM25.m1 <- merge(PM25.m1, lu_200m, all.x = T)
PM25.m1[,c("X_ITM_200m","Y_ITM_200m","V1_200m"):=NULL]
setnames(PM25.m1,"aod_047.x" ,"aod_047")

PM25.m1_D=PM25.m1
# # delete hourly meteorological variables
PM25.m1_D[,c("Temp_H","WS_H" ,"RH_H","Rain_H","NO2_H" ,"SO2_H","PM25_D_closest","PM25_D_mean","PM25_IDW","PM10_D_closest","PM10_IDW","PM10_H_mean","vc_H","PM25_H_mean"):=NULL]

# Save RDS files
saveRDS(PM25.m1_D,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM25_Daily.rds")


##### Create Hourly PM2.5 mod1

# hourly terra database
# PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/PM25_H.csv")
# hourly aqua database
PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/PM25_H.csv")

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
PM25=dplyr::left_join(PM25,PM_Type,by="stn")
PM25=as.data.table(PM25)
PM25[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM25$stn_type<-0
PM25[Type=="'Gener'",stn_type:=1]
PM25[Type=="'Trans'",stn_type:=0]
PM25[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2006<- PM25[c==2006]
setnames(pmall2006,"X.x","x_stn_ITM")
setnames(pmall2006,"Y.x","y_stn_ITM")

# ADD AOD 055 to PM25 mod 1
jointo.pt <- makepointsmatrix(datatable = pmall2006, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2006, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2006 , joinfrom = db2006, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)

setkey(pmall2006,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(pmall2006, joinout, all.x = T)

PM25.m1<-PM25.m1[!is.na(aod_055)]
setnames(PM25.m1,"nearestmean", "aod_055_mean")

PM25.m1[,nearestknn:=NULL]
PM25.m1[,nearestnobs:=NULL]
PM25.m1[,c.y:=NULL]
setnames(PM25.m1,"c.x", "year")

# ADD AOD 047
db2006_s=db2006[, c("aodid","x_aod_ITM","y_aod_ITM","aod_047","day"), with = FALSE]

jointo.pt <- makepointsmatrix(datatable = PM25.m1, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2006_s, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = PM25.m1 , joinfrom = db2006_s, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)

setkey(PM25.m1,stn,day)
setkey(joinout,stn,day)
PM25.m1<- merge(PM25.m1, joinout, all.x = T)

setnames(PM25.m1,"nearestmean", "aod_047_mean")
setnames(PM25.m1,"aod_047.x", "aod_047")
setnames(PM25.m1,"x_aod_ITM.x", "x_aod_ITM")
setnames(PM25.m1,"y_aod_ITM.x", "y_aod_ITM")
PM25.m1[,c("nearest.x","nearestknn","nearestnobs","x_aod_ITM.y", "y_aod_ITM.y", "aod_047.y","nearest.y"):=NULL]

# Join 200 m spatial variables
# add 200 m key field to the database
key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

setkey(PM25.m1,stn)
setkey(key_field,stn)
PM25.m1= merge(PM25.m1,key_field,all.x = T)

lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")

setkey(lu_200m,Key200_id)
setkey(PM25.m1,Key200_id)
PM25.m1 <- merge(PM25.m1, lu_200m, all.x = T)
PM25.m1[,c("X_ITM_200m","Y_ITM_200m","V1_200m"):=NULL]
setnames(PM25.m1,"aod_047.x" ,"aod_047")

PM25.m1_H=PM25.m1
# delete daily meteorological variables
PM25.m1_H[,c("c.y","Temp_D","WS_D" ,"RH_D","Rain_D","NO2_D","SO2_D","PM25_D_closest","PM25_D_mean","PM25_IDW","PM10_D_closest","PM10_IDW","PM10_H_mean","vc_D"):=NULL ] 

# Save RDS files
saveRDS(PM25.m1_H,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM25_Hourly.rds")
# saveRDS(PM25.m1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM25_Daily.rds")

# Create mod 1 for PM10

### PM10 mod1 Daily
# daily database
# PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Yuval/Pollution_stn_May16/PM10_D.csv")

PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]

# Add field classification (General or transportation)
PM_Type <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM_monitors_classification/PM_monitors.csv")
setnames(PM_Type,"Code","stn")
PM_Type$stn=substr(PM_Type$stn, 2, 4)
PM10=left_join(PM10,PM_Type,by="stn")
PM10=as.data.table(PM10)
PM10[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM10$stn_type<-0
PM10[Type=="'Gener'",stn_type:=1]
PM10[Type=="'Trans'",stn_type:=0]
PM10[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2006<- PM10[c==2006]
setnames(pmall2006,"X.x","x_stn_ITM")
setnames(pmall2006,"Y.x","y_stn_ITM")

# ADD AOD 055 to MOD1
jointo.pt <- makepointsmatrix(datatable = pmall2006, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2006, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2006 , joinfrom = db2006, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)

setkey(pmall2006,stn,day)
setkey(joinout,stn,day)
PM10.m1 <- merge(pmall2006, joinout, all.x = T)

PM10.m1<-PM10.m1[!is.na(aod_055)]
setnames(PM10.m1,"nearestmean", "aod_055_mean")

PM10.m1[,nearestknn:=NULL]
PM10.m1[,nearestnobs:=NULL]
PM10.m1[,c.y:=NULL]
setnames(PM10.m1,"c.x", "year")

# ADD AOD 047
db2006_s=db2006[, c("aodid","x_aod_ITM","y_aod_ITM","aod_047","day"), with = FALSE]

jointo.pt <- makepointsmatrix(datatable = PM10.m1, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2006_s, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = PM10.m1 , joinfrom = db2006_s, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)

setkey(PM10.m1,stn,day)
setkey(joinout,stn,day)
PM10.m1<- merge(PM10.m1, joinout, all.x = T)

setnames(PM10.m1,"nearestmean", "aod_047_mean")
setnames(PM10.m1,"aod_047.x", "aod_047")
PM10.m1[,c("nearest.x","nearestknn","nearestnobs","x_aod_ITM.y", "y_aod_ITM.y", "aod_047.y","nearest.y"):=NULL]

# Join 200 m spatial variables
# add 200 m key field to the database
key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

setkey(PM10.m1,stn)
setkey(key_field,stn)
PM10.m1= merge(PM10.m1,key_field,all.x = T)

lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")

setkey(lu_200m,Key200_id)
setkey(PM10.m1,Key200_id)
PM10.m1 <- merge(PM10.m1, lu_200m, all.x = T)
PM10.m1[,c("X_ITM_200m","Y_ITM_200m"):=NULL]
setnames(PM10.m1,"aod_047.x" ,"aod_047")

PM10.m1_D=PM10.m1
# delete unneeded variables from daily database
PM10.m1_D[,c("aod_047.x","aod_055","nearest.x","Temp_H","WS_H" ,"RH_H","Rain_H","NO2_H" ,"SO2_H","PM25_D_closest","PM25_D_mean","PM25_IDW","PM25_H_mean","PM10_H_mean","PM10_D_closest","PM10_IDW","vc_H","V1_200m","c.y","lon_200m","lat_200m","m"):=NULL] 
setnames(PM10.m1_D,"c.x","c")

# Save RDS files
saveRDS(PM10.m1_D,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM10_Daily.rds")
# saveRDS(PM10.m1_D,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM10_Daily.rds")

### PM10 mod1 Hourly

# hourly terra database
# PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/PM10_H.csv")
# hourly aqua database
PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/PM10_H.csv")

PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]

# Add field classification (General or transportation)
PM_Type <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM_monitors_classification/PM_monitors.csv")
setnames(PM_Type,"Code","stn")
PM_Type$stn=substr(PM_Type$stn, 2, 4)
PM10=left_join(PM10,PM_Type,by="stn")
PM10=as.data.table(PM10)
PM10[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM10$stn_type<-0
PM10[Type=="'Gener'",stn_type:=1]
PM10[Type=="'Trans'",stn_type:=0]
PM10[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2006<- PM10[c==2006]
setnames(pmall2006,"X.x","x_stn_ITM")
setnames(pmall2006,"Y.x","y_stn_ITM")


#--------->mod1
#PM10
# ADD AOD 055
jointo.pt <- makepointsmatrix(datatable = pmall2006, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2006, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2006 , joinfrom = db2006, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)

setkey(pmall2006,stn,day)
setkey(joinout,stn,day)
PM10.m1 <- merge(pmall2006, joinout, all.x = T)

PM10.m1<-PM10.m1[!is.na(aod_055)]
setnames(PM10.m1,"nearestmean", "aod_055_mean")

PM10.m1[,nearestknn:=NULL]
PM10.m1[,nearestnobs:=NULL]
PM10.m1[,c.y:=NULL]
setnames(PM10.m1,"c.x", "year")

# ADD AOD 047
db2006_s=db2006[, c("aodid","x_aod_ITM","y_aod_ITM","aod_047","day"), with = FALSE]

jointo.pt <- makepointsmatrix(datatable = PM10.m1, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2006_s, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = PM10.m1 , joinfrom = db2006_s, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1100, 
                        nearestmean = TRUE, verbose = T)

setkey(PM10.m1,stn,day)
setkey(joinout,stn,day)
PM10.m1<- merge(PM10.m1, joinout, all.x = T)

setnames(PM10.m1,"nearestmean", "aod_047_mean")
setnames(PM10.m1,"aod_047.x", "aod_047")
PM10.m1[,c("nearest.x","nearestknn","nearestnobs","x_aod_ITM.y", "y_aod_ITM.y", "aod_047.y","nearest.y"):=NULL]

# Join 200 m spatial variables
# add 200 m key field to the database
key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

setkey(PM10.m1,stn)
setkey(key_field,stn)
PM10.m1= merge(PM10.m1,key_field,all.x = T)

lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")

setkey(lu_200m,Key200_id)
setkey(PM10.m1,Key200_id)
PM10.m1 <- merge(PM10.m1, lu_200m, all.x = T)
PM10.m1[,c("X_ITM_200m","Y_ITM_200m"):=NULL]
setnames(PM10.m1,"aod_047.x" ,"aod_047")

PM10.m1_H=PM10.m1
# delete unneeded variables from hourly database
PM10.m1_H[,c("aod_047.x","aod_055","nearest.x","Temp_D","WS_D" ,"RH_D","Rain_D","NO2_D","SO2_D","PM25_D_closest","PM25_D_mean","PM25_IDW","PM25_H_mean","PM10_H_mean","PM10_D_closest","PM10_IDW","vc_D")] =NULL 

saveRDS(PM10.m1_H,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM10_Hourly.rds")
# saveRDS(PM10.m1_H,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM10_Hourly.rds")

# PM10.m1=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM10_Daily.rds")
# PM10.m1=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2006.PM10_Hourly.rds")

# ## Exploring mod 1 
# 
# summary(PM25.m1$aod_055_mean)
# PM25.m1<-filter(PM25.m1,aod_055_mean < 1.5)
# # basic correlation
# summary(lm(PM25.m1$PM25~PM25.m1$aod_047_mean)) 
# summary(lm(PM25.m1$PM25~PM25.m1$aod_055_mean)) 
# plot(PM25.m1$PM25~PM25.m1$aod_055_mean,type= "p",ylim=c(0,300),ylab="PM2.5",xlab="AOD",main="PM2.5 ~ AOD_055")
# 
# # correlation after filtering
# PM25.m1<-filter(PM25.m1,UN >0  & UN  <0.04)
# PM25.m1=filter(PM25.m1,aod_055_mean < 1.5)
# PM25.m1_fs=filter(PM25.m1,FS_BS !=0)
# summary(lm(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean))
# plot(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean,type= "p",ylim=c(0,300),ylab="PM2.5",xlab="AOD",main="PM2.5 ~ AOD_055_FS")
# plot(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean,type= "p",ylim=c(0,300),ylab="PM2.5",xlab="AOD",main="PM2.5 ~ AOD_055_BS")
# 
# # Correlation between PM~ AOD in each azimuth bin (steps of 10)
# R2=c()
# len=c()
# for (i in 1:10)
# {
#   PM25.m1_fs=filter(PM25.m1,FS_BS == i)
#   if (nrow(PM25.m1_fs)!=0)
#   {
#     a=round(summary(lm(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean))$r.squared,2)  
#     R2=c(R2,a)
#     b=nrow(PM25.m1_fs)
#     len=c(len,b)
#   }
#   else
#   {
#     a=NA
#     b=0
#     R2=c(R2,a)
#     len=c(len,b)
#   }
# }
# 
# print(R2)
# print(len)
# res=data.frame(R2,len)
# plot(R2)
# write.csv(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm_aod_sensetivity_to_BS/2006_sen.csv")
# 
# # Correlation between PM~ AOD in each azimuth bin (steps of 30)
# R2=c()
# len=c()
# for (i in 1:4)
# {
#   PM25.m1_fs=filter(PM25.m1,FS_BS_2 == i)
#   if (nrow(PM25.m1_fs)!=0)
#   {
#     a=round(summary(lm(PM25.m1_fs$PM25~PM25.m1_fs$aod_055_mean))$r.squared,2)  
#     R2=c(R2,a)
#     b=nrow(PM25.m1_fs)
#     len=c(len,b)
#   }
#   else
#   {
#     a=NA
#     b=0
#     R2=c(R2,a)
#     len=c(len,b)
#   }
# }
# 
# print(R2)
# print(len)
# res=data.frame(R2,len)
# plot(R2)
# write.csv(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm_aod_sensetivity_to_BS/2006_sen_2.csv")
# 
# sen=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm_aod_sensetivity_to_BS/sens_all_years.csv",header = TRUE)
# sen=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm_aod_sensetivity_to_BS/sens_all_years_2.csv",header = TRUE)
# plot(sen$`2010`, col="RED",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",pch=16)
# points(sen$`2006`, col="BLUE",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",pch=16)
# points(sen$`2012`, col="GRAY",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",pch=16)
# points(sen$`2013`, col="BLACK",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",pch=16)
# points(sen$`2006`, col="ORANGE",ylim=c(0,0.6),xlab="azimuth_bin",ylab="R2",pch=16)
# legend(3.5,0.6,c("2010","2006","2012","2013","2006"),pch=16,col=c("RED","BLUE","GRAY","BLACK","ORANGE"))
# 
# ################### cleaning mod 1
# mod1=PM25.m1
# mod1=as.data.table(mod1)
# setnames(mod1,"PM25", "pm25")
# setnames(mod1,"aod_055_mean", "aod")
# 
# #filter nasa
# mod1<-filter(mod1,UN >0  & UN  <0.04)
# 
# #massimos thresholds
# x<-select(mod1,aod,stn)
# x$c<-1
# x <- x %>%
#   group_by (stn) %>%
#   summarise(saod=sum(c))
# #merge back count
# setkey(x,stn)
# setkey(mod1,stn)
# mod1 <- merge(mod1,x, all.x = T)
# 
# mod1$exobs<-0
# mod1<-mod1[aod < quantile(aod, c(.50)) & pm25 >  quantile(pm25, c(.90)), exobs := 2]
# mod1<-mod1[aod > quantile(aod, c(.90)) & pm25 <  quantile(pm25, c(.50)), exobs := 3]
# mod1<-mod1[aod > 1.8 , exobs := 4]
# mod1<-mod1[saod < 30 , exobs := 5]
# 
# #take out bad exobs
# mod1<-filter(mod1,exobs==0)
# # saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_clean/mod1.AQ.2006.PM25.clean.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_clean/mod1.TR.2006.PM25.clean.rds")
# 
# 
# ### simple linear modeling
# summary(lm(mod1$pm25~mod1$aod))
# plot(mod1$pm25~mod1$aod,type= "p",ylim=c(0,300),ylab="PM2.5",xlab="AOD",main="PM2.5 ~ AOD_055_clean")
# 
# ### Mixed effect modeling
# m1.formula <- as.formula(pm25~aod+(1+aod|day))
# m1_sc <- lmer(m1.formula,data=mod1)
# mod1$pred.m1 <- predict(m1_sc)
# #check fits of model
# print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
