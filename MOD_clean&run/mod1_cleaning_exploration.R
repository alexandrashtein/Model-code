## This code explores how many stations there are in each year in our database
## Before and after cleaning of the data

library(dplyr)
library(data.table)

## Original PM2.5 data
PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y",tz="GMT"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")

# PM10

PM10<- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y",tz="GMT"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10<- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")

stn_yearly_mean_PM10 <-PM10 %>%
  dplyr::group_by(c) %>%
  dplyr::summarise(len=length(unique(stn)))
stn_yearly_mean_2 =as.data.frame(stn_yearly_mean_2)
setnames(stn_yearly_mean_PM10,"len","PM10")
num=cbind(stn_yearly_mean_PM10 ,stn_yearly_mean_PM25)

num[,3]=NULL
setnames(num,"c","Year")
setnames(num,"PM25","PM25_org")
setnames(num,"PM10","PM10_org")

## MOD1 data
PM25= readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_re.rds")

# stn <-PM25 %>%
#   dplyr::group_by(day) %>%
#   dplyr::summarise(len=length(unique(stn)))
# stn = as.data.table(stn)
# stn
# plot(stn$len~stn$day)
# 
# stn$year=year(stn$day)

# stn_yearly_mean <-stn %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(len=round(mean(len)))

stn_yearly_mean_PM25_2 <-PM25 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(len=length(unique(stn)))
stn_yearly_mean_2 =as.data.frame(stn_yearly_mean_2)
setnames(stn_yearly_mean_PM25,"len","PM25")

## Take a look at one year only
## MOD1 not cleaned
PM25=filter(PM25,year=="2015")

stn_daily_PM25 <-PM25 %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(len=length(unique(stn)))
stn_daily_PM25  =as.data.frame(stn_daily_PM25 )
setnames(stn_daily_PM25,"len","PM25")

## MOD1 cleaned
PM25= readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")
PM25=filter(PM25,year=="2015")

stn_daily_PM25_c <-PM25 %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(len=length(unique(stn)))
stn_daily_PM25_c  =as.data.frame(stn_daily_PM25_c)
setnames(stn_daily_PM25_c,"len","PM25")

par(mfrow=c(1,2))
hist(stn_daily_PM25$PM25)
hist(stn_daily_PM25_c$PM25)

# MOD1 cleaned
PM25= readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")

PM25_mod1_c <-PM25 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(len=length(unique(stn)))
PM25_mod1_c=as.data.table(PM25_mod1_c)
setnames(PM25_mod1_c,"len","PM25_clean")

mod1_stn=cbind(mod1_stn,PM25_mod1_c[,2])


## MOD1 data

# MOD1 not cleaned
PM10= readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily/mod1.AQ.2003_2015.PM10_Daily_re.rds")

PM10_mod1 <-PM10 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(len=length(unique(stn)))
PM10_mod1=as.data.table(PM10_mod1)
setnames(PM10_mod1,"len","PM10")

mod1_stn=merge(PM10_mod1,PM25_mod1)

# MOD1 cleaned
PM10= readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily/mod1.AQ.2003_2015.PM10_Daily_Re_Clean.rds")

PM10_mod1_c <-PM10 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(len=length(unique(stn)))
PM10_mod1_c=as.data.table(PM10_mod1_c)
setnames(PM10_mod1_c,"len","PM10_clean")

mod1_stn=cbind(mod1_stn,PM10_mod1_c[,2])
