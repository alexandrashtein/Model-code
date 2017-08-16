library(data.table)
library(stringi)

# explore how many missing days in each year of data
files=list.files("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/AQUA")
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/AQUA/"
len_v=c()
for (i in (1:14))
{
  maiac_data=fread(paste0(path,files[i]))
  len=length(unique(maiac_data$date))
  len_v=c(len_v,len)
}

data=data.frame(years=c(2002:2015),len_v)

files=list.files("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily")
# files=list.files("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA")
# files=list.files("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA")

files
path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/"
# path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/"
# path="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/"

# correct names of 3 first years

for (i in (1:3))
{
  new_year=readRDS(paste0(path,files[i]))
  setnames(new_year,"long_aod","lon_aod")
  saveRDS(new_year,paste0(path,files[i]))
}


all_years=data.table()
len=c()
num_days=c()

# for mod 1
for (i in (1:13))
{
  new_year=readRDS(paste0(path,files[i]))
  new_year[,c("nearest","aod_055_mean", "V1.y","PM10_D_mean","month","season","seasonSW","FS_BS"):=NULL]
  all_years=rbind(all_years,new_year)
  days=length(unique(new_year$day))
  num=nrow(new_year)
  len=c(len,num)
  num_days=c(num_days,days)
}

# for mo2 & mod 3
for (i in (1:13))
{
new_year=readRDS(paste0(path,files[i]))
new_year=new_year[,c("aodid", "day","pblid","ndviid","ndvi","daily_hpbl", "aod_047","aod_055","UN","RelAZ","lon_aod","lat_aod","x_aod_ITM","y_aod_ITM","Elev","sp_ndvi_L0002",
"su_ndvi_L0002","sp_ndvi_L14","su_ndvi_L14","P_In_2004","P_OS_2004","P_Ag_2004","P_Ur_2004",
"dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw","metreg",
"P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2014","Temp_D","WS_D","RH_D","Rain_D","NO2_D","SO2_D","PM25_D_closest","PM25_D_mean","PM25_IDW",
"Sp_NDVI0002_200m","Sp_NDVI14_200m","Su_NDVI0002_200m","Su_NDVI14_200m","su_ndvi_L0002","su_ndvi_L14","sp_ndvi_L0002","sp_ndvi_L14","P_In_2004_200m","P_Ur_2004_200m","P_Ag_2014_200m",
"P_In_Min_2014_200m","P_OS_Ag_2014_200m","P_OS_2014_200m" ,"P_Ur_2014_200m"), with=FALSE]
all_years=rbind(all_years,new_year)
days=length(unique(new_year$day))
num=nrow(new_year)
len=c(len,num)
num_days=c(num_days,days)
}

a=names(new_year) %in% names(all_years)
b=names(new_year)
b[which(a==FALSE)]

a=names(all_years) %in% names(new_year)
b=names(all_years)
b[which(a==FALSE)]

#complete missing variables

#### add ndvi

#import NDVI
ndvi<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
ndvi=as.data.table(ndvi)
ndvi$m<-stri_pad_left(str=ndvi$m, 2, pad="0")
all_years$m=substr(all_years$day,6,7)
all_years$c=substr(all_years$day,1,4)

# add ndviid to all_years
#join actual NDVI to aod
setkey(ndvi, ndviid, m, c)
setkey(all_years,ndviid, m , c)
all_years= merge(all_years,ndvi,all.x = T)

#delete unnecessery columns
all_years[,c("lat_ndvi","long_ndvi"):=NULL]
summary(all_years)

# add daily average PBL
# Add PBLid
PBLid=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/lu_pbl.csv")
PBLid$pblid=paste(formatC(round(PBLid$POINT_X,3),format='f',3),formatC(round(PBLid$POINT_Y,3),format='f',3),sep="-")
PBLid[,c("POINT_X","POINT_Y"):=NULL]

# replacing aodids not indetical to the all_years aodids
replace=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/correcting_grid/aod_to_replace.csv")
all(replace$aodid %in% PBLid$aodid)
setkey(PBLid,aodid)
setkey(replace,aodid)
PBLid=merge(PBLid,replace,all.x = T)
PBLid$correct[is.na(PBLid$correct)] =PBLid$aodid[is.na(PBLid$correct)]
PBLid=as.data.table(PBLid)
PBLid[,aodid:=NULL]
setnames(PBLid,"correct","aodid")

# replacing aodids not indetical to the all_years aodids
# replace=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/correcting_grid/aod_to_replace.csv")
# all(replace$aodid %in% all_years$aodid)
# setkey(all_years,aodid)
# setkey(replace,aodid)
# all_years=merge(all_years,replace,all.x = T)
# all_years$correct[is.na(all_years$correct)] =all_years$aodid[is.na(all_years$correct)]
# all_years=as.data.table(all_years)
# setnames(all_years,"correct","aodid_2")
# 
# mean(PBLid$aodid %in% all_years$aodid_2)
# mean(all_years$aodid_2 %in% PBLid$aodid)
# x=all_years$aodid %in% PBLid$aodid
# unique(all_years$aodid[which(x==FALSE)])
# setnames(PBLid,"aodid","aodid_2")

all_years[,c("pblid"):=NULL]
setkey(all_years,aodid)
setkey(PBLid,aodid)
all_years=merge(all_years,PBLid,all.x = T)
unique(all_years$pblid)

hpbl=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_dailyavg.csv")
hpbl=as.data.table(hpbl)
setnames(hpbl,"PBLid","pblid")
setnames(hpbl,"date","day")
hpbl$day=as.Date(hpbl$day)
hpbl[,c("V1","year"):=NULL]

setkey(all_years,pblid,day)
setkey(hpbl,pblid,day)
all_years <- merge(all_years,hpbl,all.x = T)
summary(all_years)

#add overpass PBL 
pbl<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_11_12am.csv")
# pbl<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2014_8_9am.csv")
pbl$pblid=paste(formatC(round(pbl$lon,3),format='f',3),formatC(round(pbl$lat,3),format='f',3),sep="-")
setnames(pbl,"date","day")
pbl$day=as.Date(pbl$day)

pbl=as.data.table(pbl)
#join pbl to aod
setkey(pbl, pblid, day )
setkey(all_years,  pblid, day)
all_years<- merge(all_years, pbl, all.x = T)

all_years[,c("lon_pbl","lat_pbl"):=NULL]
all_years[,c("lon","lat","time"):=NULL]
all_years=as.data.table(all_years)
summary(all_years$hpbl)

# setnames(all_years,"pbl","pbl_08")
setnames(all_years,"hpbl","pbl_11")

#add night PBL         
pbl<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2003_2015_2_3pm.csv")
pbl$pblid=paste(formatC(round(pbl$lon,3),format='f',3),formatC(round(pbl$lat,3),format='f',3),sep="-")
setnames(pbl,"date","day")
pbl$day=as.Date(pbl$day)

pbl=as.data.table(pbl)

#join pbl to aod
setkey(pbl, pblid, day )
setkey(all_years,  pblid, day)
all_years <- merge(all_years, pbl, all.x = T)

all_years=as.data.table(all_years)
all_years[,c("lon_pbl","lat_pbl"):=NULL]
all_years[,c("lon","lat","time"):=NULL]
all_years[,c("V1.y","V1.x","PBLid"):=NULL]

setnames(all_years,"hpbl","pbl_02")
summary(all_years$pbl_02)

# Add 2004 land use for 1km grid
P_Ur=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse_2004/Landuse_Tables_by_squares/P_Ur_2004.csv")
P_Ur=data.table(aodid=P_Ur$AODID,P_Ur_2004=P_Ur$P_Ur_2004)

# replacing aodids not indetical to the lu aodids
replace=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/correcting_grid/aod_to_replace.csv")
all(replace$aodid %in% P_Ur$aodid)
setkey(P_Ur,aodid)
setkey(replace,aodid)
P_Ur=merge(P_Ur,replace,all.x = T)
P_Ur$correct[is.na(P_Ur$correct)] =P_Ur$aodid[is.na(P_Ur$correct)]
P_Ur=as.data.table(P_Ur)
P_Ur[,aodid:=NULL]
setnames(P_Ur,"correct","aodid")

all_years$aodid=paste(formatC(round(all_years$lon_aod,3),format='f',3),formatC(round(all_years$lat_aod,3),format='f',3),sep="-")

mean(P_Ur$aodid %in% all_years$aodid)
P_Ur=as.data.table(P_Ur)
P_Ur$aodid=as.character(P_Ur$aodid)
setkey(P_Ur,aodid)
setkey(all_years,aodid)
all_years=merge(all_years,P_Ur,all.x=T)
summary(all_years)

saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily.rds")
# saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily.rds")
# saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily.rds")

all_years=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily.rds")

# Create land use variables updated for both periods (2004 & 2014) for 1 km grid
names(all_years)
all_years$date_cf=0
all_years$date_cf[all_years$day < "2009-01-01"]=1
unique(all_years$date_cf)

all_years$P_Ur=0
all_years$P_Ur[all_years$date_cf ==1]=all_years$P_Ur_2004[all_years$date_cf ==1]
all_years$P_Ur[all_years$date_cf ==0]=all_years$P_Ur_2014[all_years$date_cf ==0]
sum(all_years$P_Ur[all_years$date_cf ==0]-all_years$P_Ur_2014[all_years$date_cf ==0]) # check of there is logic in the result. should be ZERO
sum(all_years$P_Ur[all_years$date_cf ==0]-all_years$P_Ur_2004[all_years$date_cf ==0]) # check of there is logic in the result. should be POSIYIVE

all_years$P_Ag=0
all_years$P_Ag[all_years$date_cf ==1]=all_years$P_Ag_2004[all_years$date_cf ==1]
all_years$P_Ag[all_years$date_cf ==0]=all_years$P_Ag_2014[all_years$date_cf ==0]
sum(all_years$P_Ag[all_years$date_cf ==0]-all_years$P_Ag_2014[all_years$date_cf ==0])
sum(all_years$P_Ag[all_years$date_cf ==0]-all_years$P_Ag_2004[all_years$date_cf ==0])

all_years$P_In=0
all_years$P_In[all_years$date_cf ==1]=all_years$P_In_2004[all_years$date_cf ==1]
all_years$P_In[all_years$date_cf ==0]=all_years$P_In_Min_2014[all_years$date_cf ==0]

all_years$P_OS=0
all_years$P_OS[all_years$date_cf ==1]=all_years$P_OS_2004[all_years$date_cf ==1]
all_years$P_OS[all_years$date_cf ==0]=all_years$P_OS_2014[all_years$date_cf ==0]

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


# add dust classification data

DD=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/dust_days_classification/DDAqTer.csv")
DD$date=paste(DD$Day,DD$Month,DD$Year,sep="/")
DD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
DD[, c := as.numeric(format(day, "%Y")) ]
DD[,c("Year","Month","Day","date"):=NULL]
DD$c=as.character(DD$c)
setnames(DD,"StationID","stn")

setkey(DD,stn,day,c)
setkey(all_years,stn,day,c)
all_years=merge(all_years,DD, all.x = T)
all_years[,c("Max","X","Y","DustPassTer"):=NULL]
all_years[,c("X","Y","Max"):=NULL]

all_years[,c("Sp_NDVI0002_200m","Sp_NDVI14_200m","Su_NDVI0002_200m","Su_NDVI14_200m","date_cf","su_ndvi_L0002","su_ndvi_L14","sp_ndvi_L0002","sp_ndvi_L14","P_Ur_2004","P_In_2004","P_OS_2004","P_Ag_2004","P_OS_Ag_2004","P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2014","P_In_2004_200m","P_Ur_2004_200m","P_Ag_2014_200m",
             "P_In_Min_2014_200m","P_OS_Ag_2014_200m","P_OS_2014_200m" ,"P_Ur_2014_200m","aridity_in","pblid","Key200_id","ndviid_200m"):= NULL]

names(all_years)
summary(all_years)

saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_re.rds")

saveRDS(all_years,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")

all_years=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_re.rds")

#correcting aodid in all mods

mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_re.rds")
summary(mod1)
mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")
summary(mod2)

# replacing aodids not indetical to the lu aodids
replace=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/correcting_grid/aod_to_replace.csv")
mean(replace$aodid %in% mod1$aodid)
mean(replace$correct %in% mod1$aodid)

setnames(replace,"correct","aodid2")
setnames(replace,"aodid","correct2")
setnames(replace,"correct2","correct")
setnames(replace,"aodid2","aodid")

setkey(mod3,aodid)
setkey(replace,aodid)
mod3=merge(mod3,replace,all.x = T)
mod3$correct[is.na(mod3$correct)] =mod3$aodid[is.na(mod3$correct)]
mod3=as.data.table(mod3)

setkey(mod3,correct)
setnames(lu,"aodid","correct")
setkey(lu,correct)
mod3=merge(mod3,lu,all.x=T)

mod3[,c("long_aod","lat_aod","aodid"):=NULL]
setnames(mod3,"lat","lat_aod")
setnames(mod3,"lon","long_aod")
setnames(mod3,"correct","aodid")
