
###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2003:2003){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2003]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2003]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2003]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2003]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2003]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2003]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2003"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
aod2003 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2003,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2003, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2003<- Temp[c==2003]
# tst<-met2003 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2003,stn)
met2003 <- merge(met2003, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2003 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2003<- WS[c==2003]
# tst<-met2003 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2003,stn)
met2003 <- merge(met2003, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2003 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2003<- RH[c==2003]
# tst<-met2003 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2003,stn)
met2003 <- merge(met2003, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2003 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2003<- WD[c==2003]
# tst<-met2003 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2003,stn)
met2003 <- merge(met2003, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2003 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2003<- Rain[c==2003]
# tst<-met2003 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2003,stn)
met2003 <- merge(met2003, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2003 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2003<- NO2[c==2003]
# tst<-met2003 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2003,stn)
met2003 <- merge(met2003, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2003 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2003]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2003<- PM25[c==2003]

pm.m <- makepointsmatrix(pmall2003, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2003 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2003 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2003<- PM10[c==2003]

pm.m <- makepointsmatrix(pmall2003, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2003 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2003 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2003.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2003.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2003.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2003.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2004:2004){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2004]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2004]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2004]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2004]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2004]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2004]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2004"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2004-01-01"), to = as.Date("2004-12-31"), 1)
#create date range
aod2004 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2004,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2004, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2004<- Temp[c==2004]
# tst<-met2004 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2004,stn)
met2004 <- merge(met2004, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2004 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2004<- WS[c==2004]
# tst<-met2004 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2004,stn)
met2004 <- merge(met2004, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2004 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2004<- RH[c==2004]
# tst<-met2004 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2004,stn)
met2004 <- merge(met2004, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2004 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2004<- WD[c==2004]
# tst<-met2004 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2004,stn)
met2004 <- merge(met2004, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2004 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2004<- Rain[c==2004]
# tst<-met2004 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2004,stn)
met2004 <- merge(met2004, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2004 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2004<- NO2[c==2004]
# tst<-met2004 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2004,stn)
met2004 <- merge(met2004, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2004 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2004]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2004<- PM25[c==2004]

pm.m <- makepointsmatrix(pmall2004, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2004 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2004 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2004<- PM10[c==2004]

pm.m <- makepointsmatrix(pmall2004, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2004 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2004 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2004.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2004.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2004.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2005:2005){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2005]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2005]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2005]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2005]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2005]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2005]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2005"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2005-01-01"), to = as.Date("2005-12-31"), 1)
#create date range
aod2005 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2005,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2005, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2005<- Temp[c==2005]
# tst<-met2005 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2005,stn)
met2005 <- merge(met2005, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2005 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2005<- WS[c==2005]
# tst<-met2005 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2005,stn)
met2005 <- merge(met2005, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2005 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2005<- RH[c==2005]
# tst<-met2005 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2005,stn)
met2005 <- merge(met2005, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2005 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2005<- WD[c==2005]
# tst<-met2005 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2005,stn)
met2005 <- merge(met2005, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2005 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2005<- Rain[c==2005]
# tst<-met2005 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2005,stn)
met2005 <- merge(met2005, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2005 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2005<- NO2[c==2005]
# tst<-met2005 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2005,stn)
met2005 <- merge(met2005, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2005 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2005]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2005<- PM25[c==2005]

pm.m <- makepointsmatrix(pmall2005, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2005 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2005 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2005<- PM10[c==2005]

pm.m <- makepointsmatrix(pmall2005, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2005 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2005 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2005.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2005.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2005.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2005.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2006:2006){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2006]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2006]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2006]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2006]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2006]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2006]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2006"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2006-01-01"), to = as.Date("2006-12-31"), 1)
#create date range
aod2006 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2006,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2006, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2006<- Temp[c==2006]
# tst<-met2006 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2006,stn)
met2006 <- merge(met2006, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2006 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2006<- WS[c==2006]
# tst<-met2006 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2006,stn)
met2006 <- merge(met2006, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2006 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2006<- RH[c==2006]
# tst<-met2006 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2006,stn)
met2006 <- merge(met2006, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2006 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2006<- WD[c==2006]
# tst<-met2006 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2006,stn)
met2006 <- merge(met2006, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2006 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2006<- Rain[c==2006]
# tst<-met2006 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2006,stn)
met2006 <- merge(met2006, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2006 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2006<- NO2[c==2006]
# tst<-met2006 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2006,stn)
met2006 <- merge(met2006, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2006 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2006]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
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

pm.m <- makepointsmatrix(pmall2006, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2006 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2006 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
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

pm.m <- makepointsmatrix(pmall2006, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2006 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2006 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2006.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2006.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2006.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2006.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2007:2007){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2007]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2007]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2007]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2007]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2007]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2007]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2007"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2007-01-01"), to = as.Date("2007-12-31"), 1)
#create date range
aod2007 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2007,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2007, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2007<- Temp[c==2007]
# tst<-met2007 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2007,stn)
met2007 <- merge(met2007, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2007 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2007<- WS[c==2007]
# tst<-met2007 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2007,stn)
met2007 <- merge(met2007, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2007 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2007<- RH[c==2007]
# tst<-met2007 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2007,stn)
met2007 <- merge(met2007, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2007 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2007<- WD[c==2007]
# tst<-met2007 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2007,stn)
met2007 <- merge(met2007, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2007 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2007<- Rain[c==2007]
# tst<-met2007 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2007,stn)
met2007 <- merge(met2007, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2007 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2007<- NO2[c==2007]
# tst<-met2007 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2007,stn)
met2007 <- merge(met2007, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2007 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2007]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2007<- PM25[c==2007]

pm.m <- makepointsmatrix(pmall2007, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2007 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2007 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2007<- PM10[c==2007]

pm.m <- makepointsmatrix(pmall2007, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2007 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2007 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2007.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2007.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2007.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2007.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2008:2008){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2008]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2008]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2008]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2008]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2008]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2008]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2008"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2008-01-01"), to = as.Date("2008-12-31"), 1)
#create date range
aod2008 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2008,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2008, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2008<- Temp[c==2008]
# tst<-met2008 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2008,stn)
met2008 <- merge(met2008, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2008 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2008<- WS[c==2008]
# tst<-met2008 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2008,stn)
met2008 <- merge(met2008, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2008 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2008<- RH[c==2008]
# tst<-met2008 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2008,stn)
met2008 <- merge(met2008, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2008 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2008<- WD[c==2008]
# tst<-met2008 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2008,stn)
met2008 <- merge(met2008, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2008 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2008<- Rain[c==2008]
# tst<-met2008 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2008,stn)
met2008 <- merge(met2008, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2008 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2008<- NO2[c==2008]
# tst<-met2008 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2008,stn)
met2008 <- merge(met2008, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2008 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-as.data.table(read.csv("/media/NAS/Uni/Data/Israel/Dust/dust.csv"))
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2008]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2008<- PM25[c==2008]

pm.m <- makepointsmatrix(pmall2008, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2008 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2008 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2008<- PM10[c==2008]

pm.m <- makepointsmatrix(pmall2008, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2008 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2008 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2008.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2008.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2008.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2008.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2009:2009){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2009]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2009]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2009]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2009]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2009]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2009]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2009"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2009-01-01"), to = as.Date("2009-12-31"), 1)
#create date range
aod2009 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2009,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2009, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2009<- Temp[c==2009]
# tst<-met2009 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2009,stn)
met2009 <- merge(met2009, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2009 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2009<- WS[c==2009]
# tst<-met2009 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2009,stn)
met2009 <- merge(met2009, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2009 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2009<- RH[c==2009]
# tst<-met2009 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2009,stn)
met2009 <- merge(met2009, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2009 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2009<- WD[c==2009]
# tst<-met2009 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2009,stn)
met2009 <- merge(met2009, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2009 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2009<- Rain[c==2009]
# tst<-met2009 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2009,stn)
met2009 <- merge(met2009, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2009 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2009<- NO2[c==2009]
# tst<-met2009 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2009,stn)
met2009 <- merge(met2009, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2009 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2009]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2009<- PM25[c==2009]

pm.m <- makepointsmatrix(pmall2009, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2009 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2009 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2009<- PM10[c==2009]

pm.m <- makepointsmatrix(pmall2009, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2009 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2009 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2009.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2009.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2009.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2009.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2010:2010){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2010]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2010]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2010]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2010]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2010]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2010]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2010"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-12-31"), 1)
#create date range
aod2010 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2010,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2010, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2010<- Temp[c==2010]
# tst<-met2010 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2010,stn)
met2010 <- merge(met2010, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2010 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2010<- WS[c==2010]
# tst<-met2010 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2010,stn)
met2010 <- merge(met2010, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2010 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2010<- RH[c==2010]
# tst<-met2010 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2010,stn)
met2010 <- merge(met2010, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2010 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2010<- WD[c==2010]
# tst<-met2010 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2010,stn)
met2010 <- merge(met2010, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2010 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2010<- Rain[c==2010]
# tst<-met2010 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2010,stn)
met2010 <- merge(met2010, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2010 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2010<- NO2[c==2010]
# tst<-met2010 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2010,stn)
met2010 <- merge(met2010, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2010 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2010]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2010<- PM25[c==2010]

pm.m <- makepointsmatrix(pmall2010, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2010 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2010 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2010<- PM10[c==2010]

pm.m <- makepointsmatrix(pmall2010, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2010 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2010 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2010.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2010.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2010.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2010.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2011:2011){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2011]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2011]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2011]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2011]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2011]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2011]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2011"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
aod2011 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2011,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2011, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2011<- Temp[c==2011]
# tst<-met2011 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2011,stn)
met2011 <- merge(met2011, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2011 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2011<- WS[c==2011]
# tst<-met2011 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2011,stn)
met2011 <- merge(met2011, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2011 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2011<- RH[c==2011]
# tst<-met2011 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2011,stn)
met2011 <- merge(met2011, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2011 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2011<- WD[c==2011]
# tst<-met2011 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2011,stn)
met2011 <- merge(met2011, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2011 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2011<- Rain[c==2011]
# tst<-met2011 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2011,stn)
met2011 <- merge(met2011, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2011 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2011<- NO2[c==2011]
# tst<-met2011 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2011,stn)
met2011 <- merge(met2011, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2011 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2011]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
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

pm.m <- makepointsmatrix(pmall2011, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2011 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2011 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2011<- PM10[c==2011]

pm.m <- makepointsmatrix(pmall2011, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2011 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2011 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2011.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2011.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2011.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2011.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2012:2012){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2012]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2012]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2012]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2012]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2012]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2012]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2012"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
aod2012 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2012,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2012, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2012<- Temp[c==2012]
# tst<-met2012 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2012,stn)
met2012 <- merge(met2012, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2012 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2012<- WS[c==2012]
# tst<-met2012 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2012,stn)
met2012 <- merge(met2012, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2012 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2012<- RH[c==2012]
# tst<-met2012 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2012,stn)
met2012 <- merge(met2012, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2012 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2012<- WD[c==2012]
# tst<-met2012 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2012,stn)
met2012 <- merge(met2012, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2012 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2012<- Rain[c==2012]
# tst<-met2012 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2012,stn)
met2012 <- merge(met2012, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2012 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2012<- NO2[c==2012]
# tst<-met2012 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2012,stn)
met2012 <- merge(met2012, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2012 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2012]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2012<- PM25[c==2012]

pm.m <- makepointsmatrix(pmall2012, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2012 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2012 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2012<- PM10[c==2012]

pm.m <- makepointsmatrix(pmall2012, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2012 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2012 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2012.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2012.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2012.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2012.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2013:2013){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2013]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2013]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2013]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2013]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2013]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2013]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2013"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
aod2013 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2013,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2013, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2013<- Temp[c==2013]
# tst<-met2013 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2013,stn)
met2013 <- merge(met2013, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2013 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2013<- WS[c==2013]
# tst<-met2013 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2013,stn)
met2013 <- merge(met2013, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2013 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2013<- RH[c==2013]
# tst<-met2013 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2013,stn)
met2013 <- merge(met2013, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2013 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2013<- WD[c==2013]
# tst<-met2013 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2013,stn)
met2013 <- merge(met2013, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2013 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2013<- Rain[c==2013]
# tst<-met2013 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2013,stn)
met2013 <- merge(met2013, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2013 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2013<- NO2[c==2013]
# tst<-met2013 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2013,stn)
met2013 <- merge(met2013, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2013 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2013]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
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

pm.m <- makepointsmatrix(pmall2013, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2013 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2013 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2013<- PM10[c==2013]

pm.m <- makepointsmatrix(pmall2013, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2013 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2013 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2013.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2013.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2013.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2013.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2014:2014){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2014]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2014]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2014]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2014]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2014]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2014]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2014"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), 1)
#create date range
aod2014 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2014,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2014, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2014<- Temp[c==2014]
# tst<-met2014 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2014,stn)
met2014 <- merge(met2014, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2014 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2014<- WS[c==2014]
# tst<-met2014 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2014,stn)
met2014 <- merge(met2014, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2014 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2014<- RH[c==2014]
# tst<-met2014 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2014,stn)
met2014 <- merge(met2014, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2014 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2014<- WD[c==2014]
# tst<-met2014 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2014,stn)
met2014 <- merge(met2014, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2014 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2014<- Rain[c==2014]
# tst<-met2014 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2014,stn)
met2014 <- merge(met2014, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2014 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2014<- NO2[c==2014]
# tst<-met2014 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2014,stn)
met2014 <- merge(met2014, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2014 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2014]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2014<- PM25[c==2014]

pm.m <- makepointsmatrix(pmall2014, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2014 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2014 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2014<- PM10[c==2014]

pm.m <- makepointsmatrix(pmall2014, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2014 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2014 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2014.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2014.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2014.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2014.rds")


###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")



########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2015:2015){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2015]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2015]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2015]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2015]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2015]

#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
NO2<- NO2[NO2 != 'NaN']
NO2<- NO2[c == 2015]


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2015"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:29),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM ,MaskAdjacency)


#create full LU TS
days<-seq.Date(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), 1)
#create date range
aod2015 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2015,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2015, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk
m2<-m2[, c(3,4,8,9,22,23) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(pblid)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
names(m2)

#---------> Temporal additions

#Temp
#xtract year met
met2015<- Temp[c==2015]
# tst<-met2015 %>%
#     group_by(stn) %>%
#     summarise(data = n())
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
setkey(metreg,stn)
setkey(met2015,stn)
met2015 <- merge(met2015, metreg[,list(stn,metreg)], all.x = T)
temp.a <-met2015 %>%
    group_by(metreg,day) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)



#WS
#xtract year met
met2015<- WS[c==2015]
# tst<-met2015 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2015,stn)
met2015 <- merge(met2015, metreg[,list(stn,metreg)], all.x = T)
WS.a <-met2015 %>%
    group_by(metreg,day) %>%
    summarise(WSa = mean(WS)) 
setkey(m3,metreg,day)
setkey(WS.a,metreg,day)
m3 <- merge(m3, WS.a, all.x = T)
summary(m3)

#RH
#xtract year met
met2015<- RH[c==2015]
# tst<-met2015 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2015,stn)
met2015 <- merge(met2015, metreg[,list(stn,metreg)], all.x = T)
RH.a <-met2015 %>%
    group_by(metreg,day) %>%
    summarise(RHa = mean(RH)) 
setkey(m3,metreg,day)
setkey(RH.a,metreg,day)
m3 <- merge(m3, RH.a, all.x = T)
summary(m3)

#WD
#xtract year met
met2015<- WD[c==2015]
# tst<-met2015 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2015,stn)
met2015 <- merge(met2015, metreg[,list(stn,metreg)], all.x = T)
WD.a <-met2015 %>%
    group_by(metreg,day) %>%
    summarise(WDa = mean(WD)) 
setkey(m3,metreg,day)
setkey(WD.a,metreg,day)
m3 <- merge(m3, WD.a, all.x = T)
summary(m3)


#Rain
#xtract year met
met2015<- Rain[c==2015]
# tst<-met2015 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2015,stn)
met2015 <- merge(met2015, metreg[,list(stn,metreg)], all.x = T)
Rain.a <-met2015 %>%
    group_by(metreg,day) %>%
    summarise(Raina = mean(Rain)) 
setkey(m3,metreg,day)
setkey(Rain.a,metreg,day)
m3 <- merge(m3, Rain.a, all.x = T)
summary(m3)



#NO2
#xtract year met
met2015<- NO2[c==2015]
# tst<-met2015 %>%
#     group_by(stn) %>%
#     summarise(data = n())
setkey(met2015,stn)
met2015 <- merge(met2015, metreg[,list(stn,metreg)], all.x = T)
NO2.a <-met2015 %>%
    group_by(metreg,day) %>%
    summarise(NO2a = mean(NO2)) 
setkey(m3,metreg,day)
setkey(NO2.a,metreg,day)
m3 <- merge(m3, NO2.a, all.x = T)
summary(m3)


#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m3, day, pblid)
m3<-left_join(m3, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2015]

setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]


#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+tempa+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2015<- PM25[c==2015]

pm.m <- makepointsmatrix(pmall2015, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2015 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2015 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2015<- PM10[c==2015]

pm.m <- makepointsmatrix(pmall2015, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2015 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2015 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2015.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2015.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2015.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2015.rds")

