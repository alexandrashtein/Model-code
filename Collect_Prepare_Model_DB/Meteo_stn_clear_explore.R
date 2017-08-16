## This code deals with the new meteorological data (source: Adar Rosenfeld)
## Goals: (1) Use only the data from stations that are different from those from Yuval's dataset (2) Explore and clean the data

# Install and load libraries
# install.packages("base")
# install.packages("dplyr")
# install.packages("sp")
library("base")
library("dplyr")
library("sp")
library("plyr")
library("date")
library("dplyr")
library(data.table)

## This code chooses only the stations the additional stations in Adar's dataset- the ones that are not included in Yuval's data

## The same stations in Yuval's and Adar's dataset were found by spatial location in ArcGIS

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Adar")

stn_adar=read.csv("AG_stations_Adar_Unique_aoi.csv") # The stations that Yuval's data do not contain

head(stn_adar)

# Arranging Daily data from Adar in the same structure

daily_data=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Adar/Meteorological_Daily_Data.csv")

daily_data_sub=subset(daily_data, stn %in% stn_adar$stn) # Chooses only the unique stations from Adar's list

daily_data_sub[,c("tempcmax","tempcmin","lat","long","elev","serial number","stn","V1"):= NULL]

write.csv(daily_data_sub, "AG_Meteorological_Daily_Data_unique.csv")

# Creatong a continous date vector (from 2000-2015) and match the existing observations to it.

daily_data_sub$date=as.Date(daily_data_sub$date, format="%Y-%m-%d")

date=seq(as.Date("2004-01-01"), as.Date("2015-12-31"), by="days")

new_daily_data=data.frame(date)

new_daily_data=join(new_daily_data,daily_data_sub,by="date")

# keeping the same structure as Yuval's data
new_daily_data$Year=substr(new_daily_data$date,1,4)
new_daily_data$Month=substr(new_daily_data$date,6,7)  
new_daily_data$Day=substr(new_daily_data$date,9,10) 
new_daily_data=new_daily_data[,2:ncol(new_daily_data)]

new_daily_data[,"V1":= NULL]
setnames(new_daily_data,"tempcmean","Temp")
setnames(new_daily_data,"itm_e","X")
setnames(new_daily_data,"itm_n","Y")
setnames(new_daily_data,"name","stn")

write.csv(new_daily_data, "AG_Meteorological_Daily_Data_unique2.csv")

dailyd=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Adar/AG_Meteorological_Daily_Data_unique2.csv")
# dailyd=as.data.table(dailyd)
# dailyd$date=as.Date(dailyd$date)

dailyd$date<-paste(dailyd$Day,dailyd$Month,dailyd$Year,sep="/")
dailyd[, date:=as.Date(strptime(date, "%d/%m/%Y"))]

stn_name=unique(dailyd$stn)
stn_name=stn_name[-1]
stn_num=length(unique(dailyd$stn))
stn_num=stn_num-1


#calculate basic statistics
name=vector()
max=vector()
min=vector()
mean=vector()
median=vector()
which.max=vector()
class(which.max) <- "Date"
which.min=vector()
class(which.min) <- "Date"
time_range_min=vector()
class(time_range_min) <- "Date"
time_range_max=vector()
class(time_range_max) <- "Date"

# For temperature- units: °C, 

for (i in 1:stn_num)
{
  temp=filter(dailyd,stn == paste(stn_name[i])) 
  name[i]= paste(stn_name[i])
  max[i]=max(temp$Temp, na.rm = TRUE)
  min[i]=min(temp$Temp, na.rm = TRUE)
  mean[i]=mean(temp$Temp, na.rm = TRUE)
  median[i]=median(temp$Temp, na.rm = TRUE)
  which.max[i]=temp$date[which.max(temp$Temp)]
  which.min[i]=temp$date[which.min(temp$Temp)]
  time_range_min[i]=range(temp$date)[1]
  time_range_max[i]=range(temp$date)[2]
}

stats=data.frame(name,time_range_min,time_range_max,mean,median,max,which.max,min,which.min)
summary(stats)

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_July16/stats_summary")
write.csv(stats,"stats_temp_Hourly_data.csv")
write.csv(stats,"stats_temp_daily_data.csv")

# For WD- between 0-360 (units:degrees)
for (i in 1:stn_num)
{
  temp=filter(dailyd,stn == paste(stn_name[i])) 
  name[i]= paste(stn_name[i])
  max[i]=max(temp$WD, na.rm = TRUE)
  min[i]=min(temp$WD, na.rm = TRUE)
  mean[i]=mean(temp$WD, na.rm = TRUE)
  median[i]=median(temp$WD, na.rm = TRUE)
  which.max[i]=temp$date[which.max(temp$WD)]
  which.min[i]=temp$date[which.min(temp$WD)]
  time_range_min[i]=range(temp$date)[1]
  time_range_max[i]=range(temp$date)[2]
}

stats=data.frame(name,time_range_min,time_range_max,mean,median,max,which.max,min,which.min)
write.csv(stats,"stats_WD_daily_data.csv")

# For WS- units m/s 
for (i in 1:stn_num)
{
  temp=filter(dailyd,stn == paste(stn_name[i])) 
  name[i]= paste(stn_name[i])
  max[i]=max(temp$WS, na.rm = TRUE)
  min[i]=min(temp$WS, na.rm = TRUE)
  mean[i]=mean(temp$WD, na.rm = TRUE)
  median[i]=median(temp$WS, na.rm = TRUE)
  which.max[i]=temp$date[which.max(temp$WS)]
  which.min[i]=temp$date[which.min(temp$WS)]
  time_range_min[i]=range(temp$date)[1]
  time_range_max[i]=range(temp$date)[2]
}

stats=data.frame(name,time_range_min,time_range_max,mean,median,max,which.max,min,which.min)
write.csv(stats,"stats_WS_daily_data.csv")

# For RH- precntage- between 0-100

for (i in 1:stn_num)
{
  temp=filter(dailyd,stn == paste(stn_name[i])) 
  name[i]= paste(stn_name[i])
  max[i]=max(temp$WS, na.rm = TRUE)
  min[i]=min(temp$WS, na.rm = TRUE)
  mean[i]=mean(temp$WD, na.rm = TRUE)
  median[i]=median(temp$WS, na.rm = TRUE)
  which.max[i]=temp$date[which.max(temp$WS)]
  which.min[i]=temp$date[which.min(temp$WS)]
  time_range_min[i]=range(temp$date)[1]
  time_range_max[i]=range(temp$date)[2]
}

stats=data.frame(name,time_range_min,time_range_max,mean,median,max,which.max,min,which.min)
write.csv(stats,"stats_RH_daily_data.csv")


# plot the data and save it
dailyd=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Daily_Data_Adar/Meteorological_Daily_Data_unique2.csv")
dailyd$date=as.Date(dailyd$date)
dailyd=as.data.table(dailyd)

time_series=seq(as.Date("2005-01-01"), as.Date("2015-12-31"), by="days")
time_series=data.frame(time_series)
setnames(time_series,"time_series","date")

for (i in 1:stn_num)
{
  temp=filter(dailyd,stn == paste(stn_name[i]))
  # Create a complete time series
  temp=join(time_series,temp,by="date")
  # plot the time series
  mypath <- file.path("C:","Data",paste("Temp_", stn_name[i], ".png", sep = ""))
  png(file=mypath)
  plot(temp$date,temp$Temp,type= "l",ylim=c(-5,50),main=paste(stn_name[i]))
  dev.off()
}

# Join the data into Yuval's data set


# Hourly data from Adar- arrange the data in a same structure as Yuval's data
setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/Meteorological_Data/Adar_Stations/Hourly_data/IMS/All_data")
files = list.files(pattern = "\\.csv$") 
files 

dat=data.frame()
for(filename in files) {
  h = read.csv(filename)
  dat=merge.data.frame(dat,h,all = TRUE)
}

write.csv(dat,"dat.csv")
dat=read.csv("dat.csv")
names(dat)=c("num","stn","lon", "lat","date","hour","ws", "tempc","rh","wd","elevation","x","elevation.m")
dat=dat[,-10]
head(dat)

# The unique stations from Adar's 

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/Meteorological_Data/Adar_Stations/Stations/Layers")
stn=read.csv("stations_Adar_Unique_aoi.csv")

dat_stn=join(stn,dat,by="stn")

dat$source=rep("AG",nrow(dat))

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/Meteorological_Data/Adar_Stations/Hourly_data/IMS/All_data")
write.csv(dat_stn,"dat_stn_AG.csv")
  
setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/Meteorological_Data/Adar_Stations/Hourly_data/All_Unique_data")

ag=read.csv("dat_stn_AG.csv")
ims=read.csv("dat_IMS.csv")

dat=merge.data.frame(ag,ims,all = TRUE)

# keeping the same structure as Yuval's data
dat$Year=substr(dat$date,7,10)
dat$Month=substr(dat$date,4,5)  
dat$Day=substr(dat$date,1,2) 

dat$date=as.Date(dat$date, format="%d/%m/%Y")

# Create a vector of hours- for each day- every three hours.

date=seq(as.Date("2000-01-01"), as.Date("2015-12-31"), by="days")

new_daily_data=data.frame(date)

new_daily_data=join(new_daily_data,dat,by="date")

write.csv(dat,"dat.csv")