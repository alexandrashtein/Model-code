
rm(list=ls())
library("base")
library("data.table")
library("dplyr")

ldf<-fread("N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\ecmwf_hpbl_israel_2003_2016_new.csv")
ldf.1=as.data.table(ldf)
old=NULL

# Extracting only night time pbl 
for (I in 2003:2015){
  ldf.1<-filter(ldf,substr(ldf$time,1,4)==I)
  ldf.1<-filter(ldf.1,substr(ldf.1$time,12,13)=="00")
  ldf.1$time = as.POSIXct(ldf.1$time, tz="UTC")
  attributes(ldf.1$time)$tzone = "Asia/Jerusalem"
  ldf.1$date<-as.Date(ldf.1$time,format="%Y-%m-%d")
  ldf.1$hpbl<-ldf.1$hpbl/1000
  # ldf.1$time<-NULL

  ldf.n<-rbind(old,ldf.1)
  old<-ldf.n
}

# Extracting TERRA overpass time pbl 
ldf.1=as.data.table(ldf)
old=NULL

for (I in 2003:2015){
  ldf.1<-filter(ldf,substr(ldf$time,1,4)==I)
  ldf.1<-filter(ldf.1,substr(ldf.1$time,12,13)=="06") # +2/3 hours it is 8/9 am 
  ldf.1$time = as.POSIXct(ldf.1$time, tz="UTC")
  attributes(ldf.1$time)$tzone = "Asia/Jerusalem"
  ldf.1$date<-as.Date(ldf.1$time,format="%Y-%m-%d")
  ldf.1$hpbl<-ldf.1$hpbl/1000
  # ldf.1$time<-NULL
  
  ldf.n<-rbind(old,ldf.1)
  old<-ldf.n
}

ldf.n$time2=substr(ldf.n$time,12,16)
ldf.n$PBLid = paste(formatC(round(ldf.n$lon,3),format='f',3),formatC(round(ldf.n$lat,3),format='f',3),sep="-")
ldf.n=as.data.table(ldf.n)

write.csv(ldf.n,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_2_3pm.csv")


# Extracting AQUA overpass time pbl
ldf.1=as.data.table(ldf)
old=NULL

for (I in 2003:2015){
  ldf.1<-filter(ldf,substr(ldf$time,1,4)==I)
  ldf.1<-filter(ldf.1,substr(ldf.1$time,12,13)=="09")
  ldf.1$time = as.POSIXct(ldf.1$time, tz="UTC")
  attributes(ldf.1$time)$tzone = "Asia/Jerusalem"
  ldf.1$date<-as.Date(ldf.1$time,format="%Y-%m-%d")
  ldf.1$hpbl<-ldf.1$hpbl/1000
  # ldf.1$time<-NULL
  
  ldf.n<-rbind(old,ldf.1)
  old<-ldf.n
}

ldf.n$time2=substr(ldf.n$time,12,16)
ldf.n$PBLid = paste(formatC(round(ldf.n$lon,3),format='f',3),formatC(round(ldf.n$lat,3),format='f',3),sep="-")
ldf.n=as.data.table(ldf.n)

write.csv(ldf.n,"N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\newmodel.2003_2015_11_12am.csv")

# Extracting AQUA overpass time pbl- for 14-15
ldf.1=as.data.table(ldf)
old=NULL

for (I in 2003:2015){
  ldf.1<-filter(ldf,substr(ldf$time,1,4)==I)
  ldf.1<-filter(ldf.1,substr(ldf.1$time,12,13)=="12")
  ldf.1$time = as.POSIXct(ldf.1$time, tz="UTC")
  attributes(ldf.1$time)$tzone = "Asia/Jerusalem"
  ldf.1$date<-as.Date(ldf.1$time,format="%Y-%m-%d")
  ldf.1$hpbl<-ldf.1$hpbl/1000
  # ldf.1$time<-NULL
  
  ldf.n<-rbind(old,ldf.1)
  old<-ldf.n
}

ldf.n$time2=substr(ldf.n$time,12,16)
ldf.n$PBLid = paste(formatC(round(ldf.n$lon,3),format='f',3),formatC(round(ldf.n$lat,3),format='f',3),sep="-")
ldf.n=as.data.table(ldf.n)

write.csv(ldf.n,"N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\newmodel.2003_2015_14_15pm.csv")

# Extracting Daily average time pbl 

for (I in 2003:2015){
  ldf.1<-filter(ldf,substr(ldf$time,1,4)==I)
  ldf.1$time = as.POSIXct(ldf.1$time, tz="UTC")
  attributes(ldf.1$time)$tzone = "Asia/Jerusalem"
  ldf.1$date<-as.Date(ldf.1$time,format="%Y-%m-%d")
  ldf.1$hpbl<-ldf.1$hpbl/1000
  ldf.n<-rbind(old,ldf.1)
  old<-ldf.n
}

ldf.n$time2=substr(ldf.n$time,12,16)
ldf.n$PBLid = paste(formatC(round(ldf.n$lon,3),format='f',3),formatC(round(ldf.n$lat,3),format='f',3),sep="-")
ldf.n=as.data.table(ldf.n)

daily_hpbl<-ldf.n %>%
  group_by(date,PBLid) %>%
  dplyr::summarise(daily_hpbl = mean(hpbl, na.rm=TRUE)) 
daily_hpbl=as.data.table(daily_hpbl)

write.csv(daily_hpbl,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_dailyavg.csv")

write.csv(ldf.n,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_2_3am.csv")


write.csv(ldf.n,"N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\newmodel.2004_2015_14_15am.csv")

ldf.n=read.csv("N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\newmodel.2004_2015_8_9am.csv")

ldf.n=read.csv("N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\newmodel.2004_2015_11_12am.csv")

ldf.n=read.csv("N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\newmodel.2004_2015_14_15am.csv")

# Creating a unique PBLid grid

ldf.2=ldf.1[,ldf.1$time == '2004-12-31 11:00:00']

write.csv(ldf.2,"N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\pbl_grid.csv")

setwd("N:\\Projects\\P028.IL.Israel.MAIAC.PM.V2\\raw\\HPBL_Israel\\PBL_grid")
MAIC_PBL_grid=read.csv("MAIAC_1Km_grid_PBLID.csv")
MAIC_PBL_grid$PBLid=paste(MAIC_PBL_grid$lon,MAIC_PBL_grid$lat, sep="_")
head(MAIC_PBL_grid)

write.csv(MAIC_PBL_grid,"MAIAC_1Km_grid_PBLID.csv")

# calculating daily average PBL height

