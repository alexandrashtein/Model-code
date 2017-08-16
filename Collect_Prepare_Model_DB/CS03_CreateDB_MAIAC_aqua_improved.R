### This code builds the 3 input databases (mod1, mod2, mod3) for the model from the new MAIAC version (08.2016) 
### This code was not used yet - need to be evaluated and compared to the previous version first

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
# library(devtools)
# install_github("allanjust/aodlur", dependencies = TRUE)
library("aodlur")
library(sp)
library(rgdal)
library(stringi)
library(data.table)

######################### Reading all the data needed ##################################

#############################  Load aod data (Loop on years) ###########################

# Create a list of filenames that have 'MAIACAAOT_Israel_' in it from the WORKDIR folder. 
# Make sure the files in this folder with AOD.AQ are all included in this analysis

setwd("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/AQUA")
filenames <- list.files(pattern="MAIACAAOT_Israel_.*\\.csv$", full.names=TRUE)

############################# Load 1 km Spatial data ########################################

lu<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid.csv")
lu$V1=NULL
# Project border layer
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
# Grid for correcting aodid
replace=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/correcting_grid/aod_to_replace.csv")
# load clipped/LU grid 
## replacing aodids not indetical to the lu aodids
all(replace$aodid %in% lu$aodid)
setkey(lu,aodid)
setkey(replace,aodid)
lu=merge(lu,replace,all.x = T)
lu$correct[is.na(lu$correct)] =lu$aodid[is.na(lu$correct)]
lu=as.data.table(lu)
lu[,aodid:=NULL]
setnames(lu,"correct","aodid")

############################# Load 200 m Spatial data ########################################

#### Load the 200 meter key field join table

key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

#### Load the 200 meter spatial variables 
lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")
lu_200m[,c("X_ITM_200m","Y_ITM_200m","V1_200m"):=NULL]

############################# Load Temporal data #######################################
# NDVI data
ndvi<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
ndvi=as.data.table(ndvi)
ndvi$m<-stri_pad_left(str=ndvi$m, 2, pad="0")

# Daily average  pbl data
hpbl=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_dailyavg.csv")
hpbl$year=substr(hpbl$date,1,4)
hpbl=as.data.table(hpbl)
setnames(hpbl,"PBLid","pblid")
setnames(hpbl,"date","day")
hpbl$day=as.Date(hpbl$day)
hpbl[,c("V1","year"):=NULL]

# midday PBL data
pbl_d<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_11_12am.csv")
pbl_d$pblid=paste(formatC(round(pbl_d$lon,3),format='f',3),formatC(round(pbl_d$lat,3),format='f',3),sep="-")
setnames(pbl_d,"date","day")
pbl_d$day=as.Date(pbl_d$day)
pbl_d=as.data.table(pbl_d)

# morning PBL data
pbl_m<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_8_9am.csv")
pbl_m$pblid=paste(formatC(round(pbl_m$lon,3),format='f',3),formatC(round(pbl_m$lat,3),format='f',3),sep="-")
setnames(pbl_m,"date","day")
pbl_m$day=as.Date(pbl_m$day)
pbl_m=as.data.table(pbl_m)

# Night PBL data
pbl_n<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_2_3am.csv")
pbl_n$pblid=paste(formatC(round(pbl_n$lon,3),format='f',3),formatC(round(pbl_n$lat,3),format='f',3),sep="-")
setnames(pbl_n,"date","day")
pbl_n$day=as.Date(pbl_n$day)
pbl_n=as.data.table(pbl_n)

##################################### Load all meteorological variables and pollutants and join all the one met_all files ######################

# Read Hourly Meteorological AQUA data
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/"
files=list.files(path,pattern = ".csv")
names=c("Rain_H","RH_H","Temp_H","WS_H")
name=c("Rain","RH","Temp","WS")

# Read Hourly Meteorological TERRA data
# path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Terra_Hourly_data_July16/"
# files=list.files(path,pattern = ".csv")
# names=c("Rain_H","RH_H","Temp_H","WS_H")
# name=c("Rain","RH","Temp","WS")

met_all=data.table()

for (i in 1:4)
{
  met=fread(paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Terra_Hourly_data_July16/", files[i]))
  met[,c("Hour","Holiday"):=NULL]
  met$var=paste(names[i])
  setnames(met,name[i],"value")
  met_all=rbind(met_all,met)
}

# Read Daily Meteorological data
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/IMS_stn_July16/"
files=list.files(path,pattern = ".csv")
files
names=c("Rain_D","RH_D","Temp_D","WS_D")
name=c("Rain","RH","Temp","WS")

for (i in 1:4)
{
  met=fread(paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/IMS_stn_July16/", files[i]))
  met[,c("DOW","Holiday"):=NULL]
  met$var=paste(names[i])
  setnames(met,name[i],"value")
  met_all=rbind(met_all,met)
}

## Read Daily Meteorological data for 2003-2004
met_all=data.table()
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/"
files=list.files(path,pattern = ".csv")
files
names=c("Rain_D","RH_D","Temp_D","WS_D")
name=c("Rain","RH","Temp","WS")

for (i in 1:4)
{
  met=fread(paste0(path, names[i], ".csv"))
  met[,c("DOW","Holiday","date","V1"):=NULL]
  met$var=paste(names[i])
  setnames(met,name[i],"value")
  met_all=rbind(met_all,met)
}


# Read Hourly Meteorological AQUA data for 2003-2004
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/"
files=list.files(path,pattern = ".csv")
names=c("Rain_H","RH_H","Temp_H","WS_H")
name=c("Rain","RH","Temp","WS")

# Read Hourly Meteorological TERRA data for 2003-2004
# path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/Terra_Hourly_data_May16/"
# files=list.files(path,pattern = ".csv")
# names=c("Rain_H","RH_H","Temp_H","WS_H")
# name=c("Rain","RH","Temp","WS")

for (i in 1:4)
{
  met=fread(paste0(path, names[i], ".csv"))
  met[,c("DOW","Holiday","Hour","date","V1"):=NULL]
  met$var=paste(names[i])
  setnames(met,name[i],"value")
  met_all=rbind(met_all,met)
}

###### Add Hourly NO2
# for AQUA
NO2_Hourly <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/NO2_H.csv")
# for TERRA
# NO2_Hourly <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/NO2_H.csv")
###### Add Daily NO2
NO2_Daily  <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/NO2_D.csv")

###### Add Hourly SO2
# for AQUA 
SO2_Hourly <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/SO2_H.csv")
# for TERRA
# SO2_Hourly <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/SO2_H.csv")

###### Add Daily SO2
SO2_Daily <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/SO2_D.csv")
SO2_Daily=SO2_Daily[0<SO2 & SO2< 1000,]

###### Add Daily O3
O3_Daily  <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/O3_D.csv")

### Combine all the dataset to one file

files=list(NO2_Hourly,NO2_Daily,SO2_Hourly,SO2_Daily,O3_Daily)
names=c("NO2_H","NO2_D","SO2_H","SO2_D","O3_D")
name=c("NO2","NO2","SO2","SO2","O3")

for (i in 1:5)
{
  met=files[[i]]
  met[,c("DOW","Holiday","Hour","date","V1"):=NULL]
  met$var=paste(names[i])
  setnames(met,name[i],"value")
  met_all=rbind(met_all,met)
}

# Clean some weird values
met_all$value[met_all$value==-9999.0] = NA

######################################### Add PM data

## Add PM2.5 data
#### Add Hourly mean PM2.5 
# for aqua
PM25_Hourly <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/PM25_H.csv")
# for terra
# PM25_Hourly <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/PM25_H.csv")
# Add Daily PM2.5
PM25_Daily  <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM25_D.csv")
PM25_Daily<-PM25_Daily[PM25 > 0.000000000001 & PM25 < 1000 ]

#### ADD Hourly PM10 
# for AQUA
PM10_Hourly <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/PM10_H.csv")

# for TERRA
# PM10_Hourly <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/PM10_H.csv")

##  Add daily PM10
PM10_Daily  <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM10_D.csv")
PM10_Daily <-PM10_Daily [PM10 > 0.000000000001 & PM10 <  2000 ]

# Add PM station type classification (General or transportation)
PM_Type <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM_monitors_classification/PM_monitors.csv")
PM_Type[,c("Name","Region","X","Y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
setnames(PM_Type,"Code","stn")
PM_Type$stn=substr(PM_Type$stn, 2, 4)
PM_Type$stn_type<-0
PM_Type[Type=="'Gener'",stn_type:=1]
PM_Type[Type=="'Trans'",stn_type:=0]
PM_Type[Type=="'NaN'",stn_type:=2]

################################################################# Start creating the database

y=2007
# for (y in 2003:2004) 
#   {
  # load aod data from new MAIAC data (08.2016)
  maiac=fread(sprintf("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/AQUA/MAIACAAOT_Israel_%s.csv", y))
  
  # # cutting the data according to the project area
  # pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
  # Convert to data.frame
  maiac = as.data.frame(maiac)
  # Spatial subset
  coordinates(maiac) = ~ lon + lat
  proj4string(maiac) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  maiac = maiac[pol, ]
  # Convert back to data.table
  maiac = as.data.table(maiac)
  
  # # creat id field
  maiac$aodid=paste(formatC(round(maiac$lon,3),format='f',3),formatC(round(maiac$lat,3),format='f',3),sep="-")
  
  ## set names abbriviations
  setnames(maiac,"AOT_Uncertainty","UN")
  setnames(maiac,"AOT_QA","QA")
  setnames(maiac,"Optical_Depth_047","aod_047")
  setnames(maiac,"Optical_Depth_055","aod_055")
  setnames(maiac,"date","day")
  setnames(maiac,"lon","long_aod")
  setnames(maiac,"lat","lat_aod")
  maiac$date=maiac$day
  
  maiac=as.data.table(maiac)
  # saveRDS(maiac,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/raw_data/AOD.AQ.",y,".RDS"))
  
  # # Use the QA data to remove problematic observations
  # # The QA have to be used bofore the next stage of creating a single aod point per aodid per day
  
  system.time(maiac[, CloudMask := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
  system.time(maiac[, MaskLandWaterSnow := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
  system.time(maiac[, MaskAdjacency := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
  system.time(maiac[, CloudDetection := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[9:12]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
  system.time(maiac[, GlintMask := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[13]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
  system.time(maiac[, AerosolModel := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[14:15]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
  
  # Make sure that the values that were created are reasonable
  # summary(maiac$CloudMask)
  # summary(maiac$MaskLandWaterSnow)
  # summary(maiac$CloudDetection)
  # summary(maiac$AerosolModel)
  # summary(maiac$GlintMask)
  # summary(maiac$MaskAdjacency)
  
  # remove cloudy QA
  maiac=filter(maiac,CloudMask!="011") # cloudy
  maiac=filter(maiac,CloudMask!="010") # possibly cloudy
  maiac=filter(maiac,CloudMask!="101") # cloud shadow
  # maiac=filter(maiac,CloudMask!="111") # water sediments (?)
  # remove observatiobs surrounded  by more than 8 cloudy pixels QA
  # remove observatiobs Adjacent to cloud & snow
  maiac=filter(maiac,MaskAdjacency=="000")
  maiac=filter(maiac,MaskAdjacency!="010") # water
  ## remove Adjacent to snow QA
  maiac=filter(maiac,MaskAdjacency!="100") #snow
  # # remove water QA
  maiac=filter(maiac,MaskLandWaterSnow!="01") # water
  maiac=filter(maiac,MaskLandWaterSnow!="10") # snow
  maiac=filter(maiac,MaskLandWaterSnow=="00") # Keep only Land AOD (00)
  # remove Glint observations
  maiac=filter(maiac,GlintMask!="1")
  # remove observatiobs Adjacent to cloud
  maiac=filter(maiac,CloudDetection=="0000")
  
  ## create single aod point per aodid per day
  maiac <-maiac %>%
    dplyr::group_by(aodid,day) %>%
    dplyr::summarise(long_aod=mean(long_aod,na.rm=TRUE),lat_aod=mean(lat_aod,na.rm=TRUE),aod_047=mean(aod_047,na.rm=TRUE),aod_055=mean(aod_055,na.rm=TRUE),UN=mean(UN,na.rm=TRUE),RelAZ=mean(RelAZ,na.rm=TRUE))

  maiac=as.data.table(maiac)
  
  # Clean maiac data according to UN values
  maiac<-dplyr::filter(maiac,UN < 0.04 & UN > 0)
  maiac <- as.data.table(maiac)
  
  # saveRDS(maiac,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/raw_data/AOD_after_QA.AQ.",y,".RDS"))
  
  # maiac=readRDS(paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/raw_data/AOD_after_QA.AQ.",y,".RDS"))
  # maiac$day=as.Date(maiac$day)

  ################ add Spatial Variables
  
  # Make sure
  all(lu$aodid %in% maiac$aodid)
  mean(lu$aodid %in% maiac$aodid)
  maiac <- maiac[maiac$aodid %in% lu$aodid, ]
  
  #create full LU-aod TS
  days<-seq.Date(from = as.Date(paste0(y,"-01-01")), to = as.Date(paste0(y,"-12-31")), 1)
  #create date range
  # days <- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
  days <- data.table(expand.grid(aodid = lu$aodid, day = days))
  days$aodid<-as.character(days$aodid)
  
  # merge maiac data
  setkey(maiac,aodid,day)
  setkey(days,aodid,day)
  db <- merge(days,maiac, all.x = T)
  
  # precentage of NA in the data
  sum(is.na(db$aod_055))*100/length(db$aod_055)
  
  #add land use data
  setkey(db,aodid)
  setkey(lu,aodid)
  db<- merge(db, lu, all.x = T)
  # summary(db)
  gc()
  
  # db[,.(aodid,long_aod,lon,lat_aod,lat)]
  db[,c("long_aod","lat_aod"):=NULL]
  setnames(db,"lon","lon_aod")
  setnames(db,"lat","lat_aod")
  
  # summary(db)
  
  ################ add TEMPORAL Variables
  
  #add season
  db$month <- as.numeric(substr(db$day,6,7))
  #1-winter, 2-spring,3-summer,4-autum
  db$season<-car::recode(db$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
  #1-winter, 2-summer
  db$seasonSW<-car::recode(db$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
  
  #### add ndvi
  ndvi_y=filter(ndvi,c==y)
  setnames(db,"month","m")
  
  #join actual NDVI to aod
  ndvi_y=as.data.table(ndvi_y)
  ndvi_y$m <- as.numeric(ndvi_y$m)
  setkey(ndvi_y, ndviid, m)
  setkey(db,ndviid,m)
  db<- merge(db, ndvi_y,all.x = T)
  
  #delete unnecessery columns
  db[,c("lat_ndvi","long_ndvi"):=NULL]
  
  ###### Add daily average Pbl
  
  setkey(db,pblid,day)
  setkey(hpbl,pblid,day)
  db <- merge(db,hpbl,all.x = T)
  
  ###### Add midday\noon PBL 
  setkey(pbl_d, pblid, day )
  setkey(db,pblid, day)
  db <- merge(db, pbl_d, all.x = T)
  
  db[,c("V1.y","lon","lat","time"):=NULL]
  db=as.data.table(db)
  summary(db$hpbl)
  
  setnames(db,"hpbl","pbl_11")
  
  ###### Add morning PBL 
  setkey(pbl_m, pblid, day )
  setkey(db,  pblid, day)
  db <- merge(db, pbl_m, all.x = T)
  
  db[,c("V1","lon","lat","time"):=NULL]
  db=as.data.table(db)
  summary(db$hpbl)
  
  setnames(db,"hpbl","pbl_08")

  #add night PBL         
  
  #join pbl to aod
  setkey(pbl_n, pblid, day )
  setkey(db,  pblid, day)
  db <- merge(db, pbl_n, all.x = T)
  
  db=as.data.table(db)
  db[,c("V1","lon","lat","time","PBLid"):=NULL]
  
  setnames(db,"hpbl","pbl_02")
  summary(db$pbl_02)
  
  db[,c("V1.x"):=NULL]
  
  # Adding metreg (5 regions)
  
  met=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Climatic_zones/5_climatic_zones/unique_grid_met5.csv")
  met=as.data.table(met)
  setkey(met,"aodid")
  setnames(met,"metreg","metreg_5")
  setkey(db,"aodid")
  db=merge(db,met,all.x = T)
  
  ## Create function that joins for each day and aodid the mean\closest meteorological\pollution data 
  
  join_meteo = function(db, meteo, field, year,nearestmean,field_new,num,distance) {
    
    meteo$date<-paste(meteo$Day,meteo$Month,meteo$Year,sep="/")
    meteo[, day:=as.Date(date, "%d/%m/%Y")]
    meteo[, c := as.numeric(format(day, "%Y")) ]
    meteo[,c("Year","Month","Day","date"):=NULL]
    meteo <- meteo[X != 'NaN']
    meteo<-meteo[!is.na(as.data.frame(meteo)[, field]), ]
    #clear non continous stations
    setnames(meteo,"X","x_stn_ITM")
    setnames(meteo,"Y","y_stn_ITM")
    meteoall<- meteo[c==year]
    
    jointo.pt <- makepointsmatrix(datatable = db, 
                                  xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 
    
    joinfrom.pt <- makepointsmatrix(datatable = meteoall, 
                                    xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 
    
    joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                            jointo = db, joinfrom = meteoall, 
                            jointovarname = "aodid", joinfromvarname = "stn", 
                            joinprefix = "nearest", valuefield = field, 
                            knearest = num, maxdistance = distance, 
                            nearestmean = nearestmean, verbose = T)
    
    setkey(db,aodid,day)
    setkey(joinout,aodid,day)
    db <- merge(db, joinout[,list(day,nearestmean,aodid)], all.x = T)
    setnames(db,"nearestmean",field_new)
    return(db)
  }
  
  # remove unreasonable values from met file
  met_all=met_all[value> -20.0 & value <1000,]
  
  # add all temporal meteorological and pollution variables
  
  for (i in unique(met_all$var))
  {
    db = join_meteo(db = db, meteo = met_all[var==i], field = "value", year = y,nearestmean=TRUE,field_new=i,num = 9, distance = 100000)
  }
  
## Add daily PM2.5 -Using IDW- PARALLER
  PM25=PM25_Daily
  PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
  PM25[, day:=as.Date(date, "%d/%m/%Y")]
  PM25[, c := as.numeric(format(day, "%Y")) ]
  PM25[,c("Year","Month","Day","date"):=NULL]
  PM25 <- PM25[X != 'NaN']
  PM25<-as.data.table(as.data.frame(PM25)[!is.na(PM25$PM25), ]) 
  #clear non continous stations
  setnames(PM25,"X","x_stn_ITM")
  setnames(PM25,"Y","y_stn_ITM")
  pmall<- PM25[c==y]
  
library(foreach)
library(doMC)
library(gstat)
registerDoMC(cores=15)

final = foreach(i = unique(db$day)) %dopar% {
  x<-pmall[pmall$day==i, ]
  prd= db[db$day==i, ]
  #defaults to idw (gstat)
  # library(sp)
  coordinates(x) = ~ x_stn_ITM + y_stn_ITM
  coordinates(prd) = ~ x_aod_ITM + y_aod_ITM
  #location statment uneeded since we defined coordinates
  inter = gstat(formula = PM25 ~ 1,  data =x)
  prd$pred<-predict(object = inter, newdata = prd)$var1.pred
  prd=as.data.frame(prd)
  #prd@data
}
# final = lapply(final, as.data.table)
db = do.call(rbind, final)
db = as.data.table(db)
setnames(db,"pred","PM25_IDW")

## Add daily IDW PM10

PM10=PM10_Daily
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(date, "%d/%m/%Y")]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-as.data.table(as.data.frame(PM10)[!is.na(PM10$PM10), ])
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall<- PM10[c==y]

library(foreach)
library(doMC)
library(gstat)
registerDoMC(cores=20)

final = foreach(i = unique(db$day)) %dopar% {
  x<-pmall[pmall$day==i, ]
  prd= db[db$day==i, ]
  #defaults to idw (gstat)
  # library(sp)
  coordinates(x) = ~ x_stn_ITM + y_stn_ITM
  coordinates(prd) = ~ x_aod_ITM + y_aod_ITM
  #location statment uneeded since we defined coordinates
  inter = gstat(formula = PM10 ~ 1,  data =x)
  prd$pred<-predict(object = inter, newdata = prd)$var1.pred
  prd=as.data.frame(prd)
  #prd@data
}

# final = lapply(final, as.data.table)
db = do.call(rbind, final) 
db = as.data.table(db)
setnames(db,"pred","PM10_IDW")

############################################### Save MOD3 ##########################################
gc()
# saveRDS(db,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/TR.MAIAC.",y,".mod3.rds")
# x1db<- readRDS(paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/TR.PM25.",y.".mod3.rds")

saveRDS(db,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/AQ.MAIAC.",y,".mod3.rds"))
x1db<- readRDS(paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/AQ.MAIAC.",y,".mod3.rds"))
# 
# #calculate weights
x1db[, m := as.numeric(format(day, "%m")) ]
x1db<-x1db[,obs:=1]
x1db[is.na(aod_055), obs:= 0]
ws<-dplyr::select(x1db,obs,Elev,daily_hpbl,m,Temp_D,aodid,day)
# 
# #to save memory
gc()

w1 <- glm(obs ~ Elev+Temp_D+daily_hpbl+as.factor(m),family=binomial,data=ws)
ws$prob <- predict(w1 ,type = c("response"))
ws$wt <- 1/ws$prob
ws$normwt <- ws$wt/mean(ws$wt)
ws[, c("prob", "wt","obs","Elev", "daily_hpbl" , "m","Temp_D"  ) := NULL]
gc()

setkey(x1db,aodid,day)
setkey(ws,aodid,day)
x1db <- merge(x1db,ws,all.x = T)
x1db[,c("m","obs"):=NULL]
# saveRDS(x1db,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/TERRA/TR.MAIAC.",y,".mod3.rds"))
  saveRDS(x1db,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/AQ.MAIAC.",y,".mod3.rds"))

############################################### Create MOD2 ##########################################

#SPLIT the DATA
#create mod 2 file
db.m2 <- db[!is.na(aod_047)]
#rm db
rm(x1db)
gc()
#save mod2
# saveRDS(db.m2,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/TR.MAIAC.",y,".mod2.rds")
 saveRDS(db.m2,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/AQ.MAIAC.",y,".mod2.rds"))
gc()

############################################# building model 1 -MOD1##################################

############################################# Create daily PM2.5 mod1

#join nearset mean aod 

## Create function that joins for each day and stn the mean\closest aod data 

### To make sure that this part works well!!
join_aod = function(pm,db, field,type, year,field_new,num,distance) {
  
  pm$date<-paste(pm$Day,pm$Month,pm$Year,sep="/")
  pm[, day:=as.Date(date, "%d/%m/%Y")]
  pm[, c := as.numeric(format(day, "%Y")) ]
  pm[,c("Year","Month","Day","date"):=NULL]
  pm <- pm[X != 'NaN']
  pm<-pm[!is.na(as.data.frame(pm)[, type]), ]
  pm=pm[c==year]
  setnames(pm,"X","x_stn_ITM")
  setnames(pm,"Y","y_stn_ITM")  
  jointo.pt <- makepointsmatrix(datatable = pm, 
                                xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 
  
  joinfrom.pt <- makepointsmatrix(datatable = db,
                                  xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 
  
  joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                          jointo = pm, joinfrom = db, 
                          jointovarname = "stn", joinfromvarname = "aodid", 
                          joinprefix = "nearest", valuefield = field, 
                          knearest = num, maxdistance = distance, 
                          nearestmean = TRUE, verbose = T)
  
  setkey(pm,stn,day)
  setkey(joinout,stn,day)
  pm.m1 <- merge(pm, joinout, all.x = T)
  setnames(pm.m1,"nearestmean",field_new)
  pm.m1<-pm.m1[!is.na(as.data.frame(pm.m1)[, field_new]), ]
  return(pm.m1)
}

# PM25.D.m1 = join_aod(pm = PM25_Daily, db=db, year=y, field = "aod_055", type="PM25",field_new="aod_055_mean",num = 9, distance = 1100)
PM25.D.m1 = join_aod(pm = PM25_Daily, db=db, year=y, field = "aod_047", type="PM25",field_new="aod_047_mean",num = 9, distance = 1100)

#join stations classification 

setkey(PM25.D.m1,stn)
setkey(PM_Type,stn)
PM25.D.m1  <- merge(PM25.D.m1,PM_Type, all.x = T)

# Join key field for 200 m spatial variables
setkey(PM25.D.m1,stn)
setkey(key_field,stn)
PM25.D.m1= merge(PM25.D.m1,key_field,all.x = T)

# Join 200 m spatial variables
setkey(lu_200m,Key200_id)
setkey(PM25.D.m1,Key200_id)
PM25.D.m1<- merge(PM25.D.m1, lu_200m, all.x = T)

## delete hourly meteorological variables
PM25.D.m1[,c("Temp_H","WS_H" ,"RH_H","Rain_H","NO2_H" ,"SO2_H","PM25_D_closest","PM25_D_mean","PM25_IDW","PM10_D_closest","PM10_IDW","PM10_H_mean","vc_H","PM25_H_mean"):=NULL]

# Save RDS files
 saveRDS(PM25.D.m1,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1//mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.",y,".PM25_Daily.rds"))

############################################# Create Hourly PM2.5 mod1

PM25.H.m1= join_aod(pm = PM25_Hourly, db=db, year=y, field = "aod_055", type="PM25",field_new="aod_055_mean",num = 9, distance = 1100)
PM25.H.m1= join_aod(pm = PM25_Hourly, db=db, year=y, field = "aod_047", type="PM25",field_new="aod_047_mean",num = 9, distance = 1100)

# Add field PM station classification (General or transportation)
setkey(PM25.H.m1,stn)
setkey(PM_Type,stn)
PM25.H.m1<- merge(PM25.H.m1,PM_Type, all.x = T)

# Join 200 m spatial variables
# add 200 m key field to the database

setkey(PM25.H.m1,stn)
setkey(key_field,stn)
PM25.H.m1= merge(PM25.H.m1,key_field,all.x = T)

# Join 200 m spatial variables
setkey(lu_200m,Key200_id)
setkey(PM25.H.m1,Key200_id)
PM25.H.m1<- merge(PM25.H.m1, lu_200m, all.x = T)

# delete daily meteorological variables
PM25.H.m1[,c("c.y","Temp_D","WS_D" ,"RH_D","Rain_D","NO2_D","SO2_D","PM25_D_closest","PM25_D_mean","PM25_IDW","PM10_D_closest","PM10_IDW","PM10_H_mean","vc_D"):=NULL ] 

# Save RDS files
 saveRDS(PM25.H.m1,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.",y,".PM25_Hourly.rds"))
# saveRDS(PM25.H.m1,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.",y,".PM25_Hourly.rds")

############################################# Create daily PM10 mod1

# PM10.D.m1 = join_aod(pm = PM10_Daily, db=db, year=y, field = "aod_055", type="PM10",field_new="aod_055_mean",num = 9, distance = 1100)
PM10.D.m1 = join_aod(pm = PM10_Daily, db=db, year=y, field = "aod_047", type="PM10",field_new="aod_047_mean",num = 9, distance = 1100)

# Add field PM station classification (General or transportation)
setkey(PM10.D.m1,stn)
setkey(PM_Type,stn)
PM10.D.m1 <- merge(PM10.D.m1,PM_Type, all.x = T)

# add 200 m key field to the database
setkey(PM10.D.m1,stn)
setkey(key_field,stn)
PM10.D.m1= merge(PM10.D.m1,key_field,all.x = T)

# Join 200 m spatial variables
setkey(lu_200m,Key200_id)
setkey(PM10.D.m1,Key200_id)
PM10.D.m1 <- merge(PM10.D.m1 , lu_200m, all.x = T)

# delete unneeded variables from daily database
PM10.D.m1 [,c("aod_047.x","aod_055","nearest.x","Temp_H","WS_H" ,"RH_H","Rain_H","NO2_H" ,"SO2_H","PM25_D_closest","PM25_D_mean","PM25_IDW","PM25_H_mean","PM10_H_mean","PM10_D_closest","PM10_IDW","vc_H","V1_200m","c.y","lon_200m","lat_200m","m"):=NULL] 

# Save RDS files
saveRDS(PM10.D.m1 ,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily/mod1.AQ.",y,".PM10_Daily.rds"))
# saveRDS(PM10.D.m1,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1//mod1.AQ.2003_2015.PM25_Daily/mod1.TR.",y,".PM10_Daily.rds")

############################################# Create Hourly PM10 mod1

# PM10.H.m1= join_aod(pm = PM10_Hourly, db=db, year=y, field = "aod_055", type="PM10",field_new="aod_055_mean",num = 9, distance = 1100)
PM10.H.m1= join_aod(pm = PM10_Hourly, db=db, year=y, field = "aod_047", type="PM10",field_new="aod_047_mean",num = 9, distance = 1100)

# Add field PM station classification (General or transportation)
setkey(PM10.H.m1,stn)
setkey(PM_Type,stn)
PM10.H.m1 <- merge(PM10.H.m1,PM_Type, all.x = T)

# add 200 m key field to the database
setkey(PM10.H.m1,stn)
setkey(key_field,stn)
PM10.H.m1= merge(PM10.H.m1,key_field,all.x = T)

# Join 200 m spatial variables
setkey(lu_200m,Key200_id)
setkey(PM10.H.m1,Key200_id)
PM10.H.m1 <- merge(PM10.H.m1 , lu_200m, all.x = T)

# delete unneeded variables from hourly database
PM10.H.m1[,c("aod_047.x","aod_055","nearest.x","Temp_D","WS_D" ,"RH_D","Rain_D","NO2_D","SO2_D","PM25_D_closest","PM25_D_mean","PM25_IDW","PM25_H_mean","PM10_H_mean","PM10_D_closest","PM10_IDW","vc_D")] =NULL 

saveRDS(PM10.H.m1,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Hourly/mod1.AQ.",y,".PM10_Hourly.rds"))
# saveRDS(PM10.m1_H,paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.",y,".PM10_Hourly.rds")
  
# }


########################################################### Merge all years to one database ###############################################################

########################################################### mod 1 PM25/PM10 daily

# for PM2.5 mod1
files=list.files("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily",pattern = "rds")
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/"

# for PM10 mod1
files=list.files("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily",pattern = "Daily.rds")
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily/"

all_years=data.table()
len=c()
num_days=c()

for (i in (1:13))
{
  new_year=readRDS(paste0(path,files[i]))
  # new_year[,c("nearest","aod_055_mean", "V1.y","PM10_D_mean","month","season","seasonSW","FS_BS"):=NULL] # for pm2.5
  new_year[,c("V1.y","obs","nearest","aod_055_mean","SO2_D","SO2_D.s","PM10_D_mean","month","season","seasonSW","FS_BS","ndviid","vc_D", "c","year"):=NULL]
  all_years=rbind(all_years,new_year)
  days=length(unique(new_year$day))
  num=nrow(new_year)
  len=c(len,num)
  num_days=c(num_days,days)
}

range(all_years$day)
length(unique(all_years$day))
13*365

# check duplicates
temp=dplyr::select(all_years,stn,day)
any(duplicated(temp)==TRUE)

# check 
a=names(new_year) %in% names(all_years)
b=names(new_year)
b[which(a==FALSE)]

# Create land use variables updated for both periods (2004 & 2014) for 1 km grid
names(all_years)
all_years$date_cf=0
all_years$date_cf[all_years$day < "2009-01-01"]=1
unique(all_years$date_cf)

Reduce_var=function(all_years){
all_years$P_Ur.s=0
all_years$P_Ur.s[all_years$date_cf ==1]=all_years$P_Ur_2004.s[all_years$date_cf ==1]
all_years$P_Ur.s[all_years$date_cf ==0]=all_years$P_Ur_2014.s[all_years$date_cf ==0]
sum(all_years$P_Ur.s.s[all_years$date_cf ==0]-all_years$P_Ur_2014.s[all_years$date_cf ==0]) # check of there is logic in the result. should be ZERO
sum(all_years$P_Ur.s[all_years$date_cf ==0]-all_years$P_Ur_2004.s[all_years$date_cf ==0]) # check of there is logic in the result. should be POSIYIVE

all_years$P_Ag.s=0
all_years$P_Ag.s[all_years$date_cf ==1]=all_years$P_Ag_2004.s[all_years$date_cf ==1]
all_years$P_Ag.s[all_years$date_cf ==0]=all_years$P_Ag_2014.s[all_years$date_cf ==0]
sum(all_years$P_Ag.s[all_years$date_cf ==0]-all_years$P_Ag_2014.s[all_years$date_cf ==0])
sum(all_years$P_Ag.s[all_years$date_cf ==0]-all_years$P_Ag_2004.s[all_years$date_cf ==0])

all_years$P_In.s=0
all_years$P_In.s[all_years$date_cf ==1]=all_years$P_In_2004.s[all_years$date_cf ==1]
all_years$P_In.s[all_years$date_cf ==0]=all_years$P_In_Min_2014.s[all_years$date_cf ==0]

all_years$P_OS.s=0
all_years$P_OS.s[all_years$date_cf ==1]=all_years$P_OS_2004.s[all_years$date_cf ==1]
all_years$P_OS.s[all_years$date_cf ==0]=all_years$P_OS_2014.s[all_years$date_cf ==0]

# Create land use variables updated for both periods (2004 & 2014) for 200 m grid

all_years$P_Ur_200m=0
all_years$P_Ur_200m[all_years$date_cf ==1]=all_years$P_Ur_2004_200m[all_years$date_cf ==1]
all_years$P_Ur_200m[all_years$date_cf ==0]=all_years$P_Ur_2014_200m[all_years$date_cf ==0]

all_years$P_In_200m=0
all_years$P_In_200m[all_years$date_cf ==1]=all_years$P_In_2004_200m[all_years$date_cf ==1]
all_years$P_In_200m[all_years$date_cf ==0]=all_years$P_In_Min_2014_200m[all_years$date_cf ==0]

# Create ndvi (from landsad) updated for both periods (2004 & 2014)

all_years$su_ndvi_L=0
all_years$su_ndvi_L[all_years$date_cf ==1]=all_years$su_ndvi_L0002[all_years$date_cf ==1]
all_years$su_ndvi_L[all_years$date_cf ==0]=all_years$su_ndvi_L14[all_years$date_cf ==0]

all_years$sp_ndvi_L=0
all_years$sp_ndvi_L[all_years$date_cf ==1]=all_years$sp_ndvi_L0002[all_years$date_cf ==1]
all_years$sp_ndvi_L[all_years$date_cf ==0]=all_years$sp_ndvi_L14[all_years$date_cf ==0]

all_years$su_ndvi_L_200m=0
all_years$su_ndvi_L_200m[all_years$date_cf ==1]=all_years$Su_NDVI0002_200m[all_years$date_cf ==1]
all_years$su_ndvi_L_200m[all_years$date_cf ==0]=all_years$Su_NDVI14_200m[all_years$date_cf ==0]

all_years$sp_ndvi_L_200m=0
all_years$sp_ndvi_L_200m[all_years$date_cf ==1]=all_years$Sp_NDVI0002_200m[all_years$date_cf ==1]
all_years$sp_ndvi_L_200m[all_years$date_cf ==0]=all_years$Sp_NDVI14_200m[all_years$date_cf ==0]

all_years[,c("Sp_NDVI0002_200m","Sp_NDVI14_200m","Su_NDVI0002_200m","Su_NDVI14_200m","date_cf","su_ndvi_L0002","su_ndvi_L14","sp_ndvi_L0002","sp_ndvi_L14","P_Ur_2004","P_In_2004","P_OS_2004","P_Ag_2004","P_OS_Ag_2004","P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2014","P_In_2004_200m","P_Ur_2004_200m","P_Ag_2014_200m",
             "P_In_Min_2014_200m","P_OS_Ag_2014_200m","P_OS_2014_200m" ,"P_Ur_2014_200m","aridity_in","pblid","Key200_id","ndviid_200m"):= NULL]

return(all_years)
}

all_years=Reduce_var(all_years)
names(all_years)
summary(all_years)

saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_re.rds")
saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily/mod1.AQ.2003_2015.PM10_Daily_re.rds")

########################################################### mod 2 & 3 PM25/PM10 daily

# MOD3
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/"
files=list.files("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA")
# MOD2
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/"
files=list.files("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA",pattern = 'mod2.rds')

all_years=data.table()
len=c()
num_days=c()
 
for (i in (1:13))
{
  new_year=readRDS(paste0(path,files[i]))
  # for mod 2 choose the following columns:
  new_year=new_year[,c("aodid", "day","pblid","ndviid","ndvi.s","daily_hpbl.s", "aod_047","aod_055","UN","RelAZ","lon_aod","lat_aod","x_aod_ITM","y_aod_ITM","Elev.s","sp_ndvi_L0002",
                       "su_ndvi_L0002","sp_ndvi_L14","su_ndvi_L14","P_In_2004.s","P_OS_2004.s","P_Ag_2004.s","P_Ur_2004.s",
                       "dis_inventory.s","Dis_Mroads.s","road_den.s","Pop_dens.s","Dis_Rd1_2012.s","Dis_Rd2_2012.s","Dist_Railw.s","metreg", "metreg_IA", "Dist_WB.s",
                       "P_Ag_2014.s","P_In_Min_2014.s","P_OS_2014.s","P_Ur_2014.s","Temp_D.s","WS_D.s","RH_D.s","Rain_D.s","NO2_D.s","PM25_D_closest","PM25_D_mean","PM25_IDW","PM10_IDW"), with=FALSE]
  # for mod 3 choose the following columns:
  # new_year=new_year[,c("day","aodid","PM10_IDW","PM25_IDW","lon_aod","lat_aod","RelAZ","c"), with=FALSE]
  all_years=rbind(all_years,new_year)
  days=length(unique(new_year$day))
  num=nrow(new_year)
  len=c(len,num)
  num_days=c(num_days,days)
}

saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily.rds")


Reduce_var=function(all_years){
  all_years$P_Ur.s=0
  all_years$P_Ur.s[all_years$date_cf ==1]=all_years$P_Ur_2004.s[all_years$date_cf ==1]
  all_years$P_Ur.s[all_years$date_cf ==0]=all_years$P_Ur_2014.s[all_years$date_cf ==0]
  sum(all_years$P_Ur.s.s[all_years$date_cf ==0]-all_years$P_Ur_2014.s[all_years$date_cf ==0]) # check of there is logic in the result. should be ZERO
  sum(all_years$P_Ur.s[all_years$date_cf ==0]-all_years$P_Ur_2004.s[all_years$date_cf ==0]) # check of there is logic in the result. should be POSIYIVE
  
  all_years$P_Ag.s=0
  all_years$P_Ag.s[all_years$date_cf ==1]=all_years$P_Ag_2004.s[all_years$date_cf ==1]
  all_years$P_Ag.s[all_years$date_cf ==0]=all_years$P_Ag_2014.s[all_years$date_cf ==0]
  sum(all_years$P_Ag.s[all_years$date_cf ==0]-all_years$P_Ag_2014.s[all_years$date_cf ==0])
  sum(all_years$P_Ag.s[all_years$date_cf ==0]-all_years$P_Ag_2004.s[all_years$date_cf ==0])
  
  all_years$P_In.s=0
  all_years$P_In.s[all_years$date_cf ==1]=all_years$P_In_2004.s[all_years$date_cf ==1]
  all_years$P_In.s[all_years$date_cf ==0]=all_years$P_In_Min_2014.s[all_years$date_cf ==0]
  
  all_years$P_OS.s=0
  all_years$P_OS.s[all_years$date_cf ==1]=all_years$P_OS_2004.s[all_years$date_cf ==1]
  all_years$P_OS.s[all_years$date_cf ==0]=all_years$P_OS_2014.s[all_years$date_cf ==0]
  
  # Create ndvi (from landsad) updated for both periods (2004 & 2014)
  
  all_years$su_ndvi_L=0
  all_years$su_ndvi_L[all_years$date_cf ==1]=all_years$su_ndvi_L0002[all_years$date_cf ==1]
  all_years$su_ndvi_L[all_years$date_cf ==0]=all_years$su_ndvi_L14[all_years$date_cf ==0]
  
  all_years$sp_ndvi_L=0
  all_years$sp_ndvi_L[all_years$date_cf ==1]=all_years$sp_ndvi_L0002[all_years$date_cf ==1]
  all_years$sp_ndvi_L[all_years$date_cf ==0]=all_years$sp_ndvi_L14[all_years$date_cf ==0]

  all_years[,c("Sp_NDVI0002_200m","Sp_NDVI14_200m","Su_NDVI0002_200m","Su_NDVI14_200m","date_cf","su_ndvi_L0002","su_ndvi_L14","sp_ndvi_L0002","sp_ndvi_L14","P_Ur_2004","P_In_2004","P_OS_2004","P_Ag_2004","P_OS_Ag_2004","P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2014","P_In_2004_200m","P_Ur_2004_200m","P_Ag_2014_200m",
               "P_In_Min_2014_200m","P_OS_Ag_2014_200m","P_OS_2014_200m" ,"P_Ur_2014_200m","aridity_in","pblid","Key200_id","ndviid_200m","daily_hpbl","WS_D","RH_D","Rain_D","NO2_D","SO2_D","pbl_02","pbl_11"):= NULL]
  
  return(all_years)
}


all_years$date_cf=0
all_years$date_cf[all_years$day < "2009-01-01"]=1
unique(all_years$date_cf)
all_years=Reduce_var(all_years)

saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")
saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM10_Daily_Re.rds")



