# this code checks which precentage of the data is null.
# Created: 09.2016, by: Alex Shtein

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


############## for old version of MAIAC data 

#load clipped/LU grid 
lu<-fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1_km_maiac_grid_fixed.csv")
coordinates(lu)=~lon+lat
proj4string(lu) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
lu2<-lu
lu<-fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1_km_maiac_grid_fixed.csv")


# terra aod
terra_all<-readRDS("N:/Projects/P019.IL.Israel.MAIAC.PM/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_TR_0014.RDS")
terra_all <- terra_all[terra_all$aodid %in% lu$aodid, ] 

percent=c()

for (i in 2002:2013)
{
terra <- terra_all[yr == as.character(i)]

#create single aod point per aodid per day- for terra
terra <-terra %>%
  group_by(aodid,day) %>%
  summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),yr=mean(yr) )
terra=as.data.table(terra)

#create full LU-aod TS
sd=paste(i,"01","01",sep="-")
ed=paste(i,"12","31",sep="-")
days<-seq.Date(from = as.Date(sd), to = as.Date(ed), 1)
#create date range
days<- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
days$aodid<-as.character(days$aodid)

#merge terra data
setkey(terra,aodid,day)
setkey(days ,aodid,day)
db <- merge(days,terra, all.x = T)

# precentage of NA in the data
length(db$aod)
a=sum(is.na(db$aod))*100/length(db$aod)
percent=c(percent,a)
}
perecent

#aqua aod
# data converted from hdf to csv in matlab
aqua_all<-readRDS("N:/Projects/P019.IL.Israel.MAIAC.PM/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua_all<-aqua_all[yr==2011,]
aqua_all$aodid=paste(formatC(round(aqua_all$long_aod,3),format='f',3),formatC(round(aqua_all$lat_aod,3),format='f',3),sep="-")
dat=aqua_all[!duplicated(aodid)]
coordinates(dat) = ~ long_aod + lat_aod
proj4string(dat) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
plot(dat,col="orange")
plot(lu2,col="BLACK",add=TRUE)

# data converted from hdf to csv in r
aqua_all<-read.csv("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/MAIAC_data_082016/old_MAIAC_data/2011/MAIACAAOT_Israel_2011.csv")
aqua_all$aodid=paste(formatC(round(aqua_all$lon,3),format='f',3),formatC(round(aqua_all$lat,3),format='f',3),sep="-")
aqua_all=as.data.table(aqua_all)
dat=aqua_all[date=="2011-01-01",]
all(is.na(dat$Optical_Depth))
# #ploting the data
# dat=aqua_all[!duplicated(aodid)]
# coordinates(dat) = ~ lon + lat
# proj4string(dat) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# plot(dat,col="red")
# plot(lu,col="BLACK",add=TRUE)

all(lu$aodid %in% aqua_all$aodid)
mean(lu$aodid %in% aqua_all$aodid)

aqua_all <-aqua_all[aqua_all$aodid %in% lu$aodid, ] 
aqua=aqua_all
setnames(aqua,"AOT_Uncertainty","UN")
setnames(aqua,"Optical_Depth","aod")
setnames(aqua,"date","day")
setnames(aqua,"lon","long_aod")
setnames(aqua,"lat","lat_aod")
aqua$day= as.Date(aqua$day)

percent=c()

for (i in 2002:2013)
{
  aqua <- aqua_all[yr == as.character(i)]
  
  #create single aod point per aodid per day- for aqua
 aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod,na.rm=TRUE),UN=mean(UN))
 aqua=as.data.table(aqua)
  
 dat=aqua[day=="2011-01-01",]
 all(is.na(dat$aod))
 
  #create full LU-aod TS
  sd=paste(i,"01","01",sep="-")
  ed=paste(i,"12","31",sep="-")
  days<-seq.Date(from = as.Date(sd), to = as.Date(ed), 1)
  #create date range
  days<- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
  days$aodid<-as.character(days$aodid)
  
  #mergeaqua data
  setkey(aqua,aodid,day)
  setkey(days,aodid,day)
  db <- merge(days,aqua, all.x = T)
  db=db[day=="2011-01-01",]
  # precentage of NA in the data
  length(db$aod)
  a=sum(is.na(db$aod))*100/length(db$aod)
  percent=c(percent,a)
}
perecent

############### for new version of MAIAC data (082016)

# terra/aqua aod

percent=c()
mean_lu=c()
# dir="N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/MAIACTAOT_Israel" # for terra
dir="N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/MAIACAAOT_Israel" # for aqua
file="csv"
for (year in 2002:2014)
{
  name=paste(dir,year,sep="_")
  maiac=fread(paste(name,file,sep='.'))

  # creat id field
  maiac$aodid=paste(formatC(round(maiac$lon,3),format='f',3),formatC(round(maiac$lat,3),format='f',3),sep="-")
  maiac
  
  # set names abbriviations
  setnames(maiac,"AOT_Uncertainty","UN")
  setnames(maiac,"AOT_QA","QA")
  setnames(maiac,"Optical_Depth_047","aod_047")
  setnames(maiac,"date","day")
  setnames(maiac,"lon","long_aod")
  setnames(maiac,"lat","lat_aod")
  maiac$day=as.Date(maiac$day)
  #create single aod point per aodid per day
  maiac <-maiac %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod_047=mean(aod_047,na.rm=TRUE),UN=mean(UN),QA=mean(QA))

  #load clipped/LU grid 
  lu<-fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1_km_maiac_grid.csv")
  b=mean(lu$aodid %in% maiac$aodid)
  mean_lu=c(mean_lu,b)
  maiac <- maiac[maiac$aodid %in% lu$aodid, ]
  
  #create full LU-aod TS
  sd=paste(year,"01","01",sep="-")
  ed=paste(year,"12","31",sep="-")
  days<-seq.Date(from = as.Date(sd), to = as.Date(ed), 1)
  #create date range
  days <- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
  days$aodid<-as.character(days$aodid)
  
  #merge maiac data
  setkey(maiac,aodid,day)
  setkey(days ,aodid,day)
  db <- merge(days,maiac, all.x = T)
  db=as.data.table(db)
  
  # precentage of NA in the data
  a=sum(is.na(db$aod_047))*100/length(db$aod_047)
  percent=c(percent,a)
}
percent
years=c(2002:2014)
percent_all=data.frame(years,percent,mean_lu)
write.csv(percent_all,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/aqua_per_NA.csv")


###########################################
##checking the converstion from hdf to tiff
###########################################

library(sp)
library(rgdal)

hdf=raster("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/MAIAC_data_082016/old_MAIAC_data/2011/MAIACAAOT.h06v07.20110021105_1.tif")

ref_grid = "N:/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/MAIAC_data_082016/old_MAIAC_data/MAIACLatlon.h06v07.hdf"
sds = get_subdatasets(ref_grid) # Read current file

hdf_to_tif = function(path, n) {
  
  sds = get_subdatasets(path)
  x = strsplit(sds[n], ":")[[1]]
  x = paste0(x[1], ":", x[2], ':"', x[3], '":', x[4], ":", x[5], ":", x[6])
  system(
    paste0(
      "gdal_translate -of GTiff ",
      "\"", 
      x,
      "\" ",
      gsub(".hdf", paste0("_", n, ".tif"), path, fixed = TRUE)
    )
  )
  raster(gsub(".hdf", paste0("_", n, ".tif"), path, fixed = TRUE)) # Convert to raster
  
}
lon = hdf_to_tif(ref_grid, 2)
lat = hdf_to_tif(ref_grid, 1)

# Creare 'row' and 'col' rasters
row = lon
row[] = rowFromCell(lon, 1:ncell(lon))
col = lon
col[] = colFromCell(lon, 1:ncell(lon))

# Combine to multi-band raster
grid = stack(hdf, lon, lat)
names(grid) = c("hdf", "lon", "lat")

# Convert to data.frame
grid = as.data.frame(grid)

# Spatial subset
coordinates(grid) = ~ lon + lat
proj4string(grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grid = grid[pol, ]

#plot the hdf data for this day
# Create a color vector for the meuse points
color <- rep("xx", nrow(grid@data))
color[(grid@data$hdf > 0)&(grid@data$hdf <= 0.2)] <- "PINK"
color[(grid@data$hdf > 0.2)&(grid@data$hdf <= 0.4)] <- "ORANGE"
color[(grid@data$hdf > 0.4)&(grid@data$hdf <= 0.6)] <- "RED"
color[(is.na(grid@data$hdf))] <- "white"
plot(grid,col=color, pch=16)
plot(pol,add=TRUE)

old=readRDS("N:/Projects/P019.IL.Israel.MAIAC.PM/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
old<-old[day == "2011-01-02",]
pol=readOGR(dsn="N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
coordinates(old)=~long_aod+lat_aod 
proj4string(old) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
old=old[pol,]
color[(old@data$aod> 0)&(old@data$aod <= 0.2)] <- "PINK"
color[(old@data$aod > 0.2)&(old@data$aod <= 0.4)] <- "ORANGE"
color[(old@data$aod > 0.4)&(old@data$aod <= 0.6)] <- "RED"
color[(is.na(old@data$aod))] <- "white"
plot(pol)
plot(old,col=color,add=TRUE)

new=read.csv("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/MAIAC_data_082016/old_MAIAC_data/2011/MAIACAAOT_Israel_2011.csv")
new=as.data.table(new)
new<-new[date == "2011-01-01",]
new<-new[!is.na(Optical_Depth),]
coordinates(new)=~lon+lat
proj4string(new) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
color[(new@data$Optical_Depth> 0)&(new@data$Optical_Depth <= 0.2)] <- "PINK"
color[(new@data$Optical_Depth > 0.2)&(new@data$Optical_Depth<= 0.4)] <- "ORANGE"
color[(new@data$Optical_Depth > 0.4)&(new@data$Optical_Depth<= 0.6)] <- "RED"
color[(is.na(new@data$Optical_Depth))] <- "white"
plot(pol)
plot(new,col=color,add=TRUE)
