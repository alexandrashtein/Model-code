##### install packages
# install.packages("foreach")
# install.packages(doSNOW)
# install.packages("e1071")
# install.packages("MASS")
# install.packages("MuMIn")
# install.packages("gamm4")

rm(list = ls())
#add all packages
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
library(splines)
library(data.table)

#sourcing
source("/media/qnap/Data/code/R_functions/CV_splits.r") 
source("/media/qnap/Data/code/R_functions/rmspe.r")
source("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/code/forward_lmer.R")

#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("PM25")

#load data

# AQUA data
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/2007/mod1.AQ.2007.PM25_Daily.rds")
# mod1<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/2007/mod1.AQ.2007.PM25_Hourly.rds")

# TERRA data
# mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/2007/mod1.TR.2007.PM25_Daily.rds")
# mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/2007/mod1.TR.2007.PM25_Hourly.rds")

names(mod1)

## Add missing variables:
# add daily average PBL
hpbl=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_dailyavg.csv")
hpbl$year=substr(hpbl$date,1,4)
hpbl=filter(hpbl,hpbl$year=="2007")
hpbl=as.data.table(hpbl)
setnames(hpbl,"PBLid","pblid")
setnames(hpbl,"date","day")
hpbl$day=as.Date(hpbl$day)
hpbl[,c("V1","year"):=NULL]

setkey(mod1,pblid,day)
setkey(hpbl,pblid,day)
mod1 <- merge(mod1,hpbl,all.x = T)
#
# ## ADD ventilation coefficient
mod1$vc_D=c(mod1$WS_D/(mod1$daily_hpbl*1000))
# mod1$vc_H=c(mod1$WS_H/(mod1$pbl_11*1000))

# # # Add MODIS NDVI id data
# mod1$aodid=paste(formatC(round(mod1$long_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")
# NDVIid=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/NDVI_join/1_km_MAIAC_grid_ndviid.csv")
# mod1=left_join(mod1,NDVIid,by="aodid")
# 
# # #import NDVI
# ndvi<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
# ndvi=as.data.table(ndvi)
# ndvi=filter(ndvi,c=="2007")
# library(stringi)
# ndvi$m<-stri_pad_left(str=ndvi$m, 2, pad="0")
# mod1$m=substr(mod1$day,6,7)
# 
# # add ndviid to mod1
# mod1=as.data.table(mod1)
# setnames(mod1,"ndviid.y","ndviid")
# #join actual NDVI to aod
# ndvi=as.data.table(ndvi)
# setkey(ndvi, ndviid, m)
# setkey(mod1,ndviid, m)
# mod1<- merge(mod1, ndvi,all.x = T)
# 
## delete unnecessery columns and change columns names
# mod1=as.data.table(mod1)
# mod1[,c("lat_ndvi","long_ndvi","c"):=NULL]

# If P_Ur_2004 is missing
mod1$aodid=paste(formatC(round(mod1$long_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")
setkey(mod1,aodid)
p_ur=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse_2004/Landuse_Tables_by_squares/P_Ur_2004.csv")
setnames(p_ur,"AODID","aodid")
setkey(p_ur,aodid)
p_ur[,c("COUNT","SUM"):=NULL]
mod1=merge(mod1,p_ur,all.x = T)

# Add urban\rural area category
# first option
mod1$Ur_Ru_1 <- NA
# mod1$Ur_Ru_1<- mod1$P_Ur_2014
mod1$Ur_Ru_1<- mod1$P_Ur_2004
mod1$Ur_Ru_1[mod1$Ur_Ru_1<=20] <-1 
mod1$Ur_Ru_1[20<= mod1$Ur_Ru_1 & mod1$Ur_Ru_1<=40] <-2
mod1$Ur_Ru_1[40<= mod1$Ur_Ru_1 & mod1$Ur_Ru_1<=60] <-3
mod1$Ur_Ru_1[60<= mod1$Ur_Ru_1 & mod1$Ur_Ru_1<=80] <-4
mod1$Ur_Ru_1[mod1$Ur_Ru_1>=80] <-5

# Second option
mod1$Ur_Ru <- NA
# mod1$Ur_Ru<- mod1$P_Ur_2014
mod1$Ur_Ru<- mod1$P_Ur_2004
mod1$Ur_Ru[mod1$Ur_Ru<=20] <-1 
mod1$Ur_Ru[20<= mod1$Ur_Ru & mod1$Ur_Ru<=80] <-2
mod1$Ur_Ru[mod1$Ur_Ru>=80] <-3

# Third option
mod1$Ur_Ru_2 <- NA
# mod1$Ur_Ru_2<- mod1$P_Ur_2014
mod1$Ur_Ru_2<- mod1$P_Ur_2004
mod1$Ur_Ru_2[mod1$Ur_Ru_2<=20] <-1 
mod1$Ur_Ru_2[20<= mod1$Ur_Ru_2 & mod1$Ur_Ru_2<=50] <-2
mod1$Ur_Ru_2[50<= mod1$Ur_Ru_2 & mod1$Ur_Ru_2<=80] <-3
mod1$Ur_Ru_2[mod1$Ur_Ru_2>=80] <-4


# delete unnecessery columns for 2008-2014
# mod1[,c("m","lon_200m","lat_200m","ndviid","FS_BS","V1_200m","P_In_2004","P_OS_2004","P_Ag_2004","P_OS_Ag_2004", "sp_ndvi_L0002","su_ndvi_L0002" ):=NULL]

# delete unnecessery columns for 2002-2006
mod1[,c("sp_ndvi_L14","su_ndvi_L14","m","lon_200m","lat_200m","ndviid","FS_BS","Su_NDVI14_200m","Sp_NDVI14_200m","P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ag_2014_200m","P_In_Min_2014_200m","P_OS_Ag_2014_200m","P_OS_2014_200m"):=NULL]
# setnames(mod1,"x_aod_ITM.x","x_aod_ITM")
# setnames(mod1,"y_aod_ITM.x","y_aod_ITM")
names(mod1)

#scale variables

# names for AQUA daily mod1- for year > 2014

# names=c("Elev","sp_ndvi_L14","su_ndvi_L14","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
#         "Dist_WB","Temp_D","P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2014",
#         "WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11","vc_D","aod_055_mean","aod_047_mean","Elev_200m","Pop_dens_200m",
#         "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"Sp_NDVI14_200m","Su_NDVI14_200m","P_Ag_2014_200m","P_In_Min_2014_200m","P_OS_Ag_2014_200m","P_OS_2014_200m","P_Ur_2014_200m",
#         "dist_inven_200m","ndvi","daily_hpbl")

# names for hourly mod1 AQUA- for years > 2014

# names=c("Elev","sp_ndvi_L14","su_ndvi_L14","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2007","Dis_Rd2_2007","Dist_Railw",
# "Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2004","Temp_H",
# "WS_H","RH_H","Rain_H","NO2_H" ,"SO2_H","pbl_02","pbl_11","vc_H","aod_055_mean","aod_047_mean","Elev_200m","Pop_dens_200m",
# "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"Sp_NDVI14_200m","Su_NDVI14_200m","P_Ag_2014_200m","P_In_Min_2014_200m","P_OS_Ag_2014_200m","P_OS_2014_200m","P_Ur_2004_200m","dist_inven_200m","ndvi")


# names for Daily mod1 AQUA- for years < 2014

names=c("Elev","sp_ndvi_L0002","su_ndvi_L0002","P_In_2004","P_OS_2004","P_Ag_2004","P_Ur_2004",
"dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
"Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","Temp_D","WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11","vc_D","Elev_200m","Pop_dens_200m",
"Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"P_In_2004_200m","P_Ur_2004_200m","Sp_NDVI0002_200m","Su_NDVI0002_200m",
"dist_inven_200m","ndvi","daily_hpbl")

# 
# # names for hourly mod1 AQUA- for years < 2014
# 
# names=c("Elev","sp_ndvi_L0002","su_ndvi_L0002","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2007","Dis_Rd2_2007","Dist_Railw",
# "Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","P_Ag_2004","P_In_2004" ,"P_OS_2004" ,"P_OS_Ag_2004","Temp_H",
# "WS_H","RH_H","Rain_H","NO2_H" ,"SO2_H","pbl_02","pbl_11","vc_H","Elev_200m","Pop_dens_200m",
# "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"Sp_NDVI0002_200m","Su_NDVI0002_200m","P_In_2004_200m","P_Ur_2004_200m","dist_inven_200m","ndvi")
# 

########### names for TERRA

# names for TERRA daily mod1- for year > 2014

# names=c("Elev","sp_ndvi_L14","su_ndvi_L14","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2007","Dis_Rd2_2007","Dist_Railw",
# "Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","Temp_D", "P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2004",
# "WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_08","vc_D","aod_055_mean","aod_047_mean","Elev_200m","Pop_dens_200m",
# "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"P_Ag_2014_200m","P_In_Min_2014_200m","P_OS_Ag_2014_200m","P_OS_2014_200m","P_Ur_2004_200m","Sp_NDVI0002_200m",
# "Sp_NDVI14_200m","Su_NDVI0002_200m","Su_NDVI14_200m","dist_inven_200m","ndvi","daily_hpbl")

# names for hourly mod1 TERRA- for years > 2014

# names= c("Elev","sp_ndvi_L14","su_ndvi_L14","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2007",
# "Dis_Rd2_2007","Dist_Railw","Dist_WB","metreg","ndvi",
# "P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2004",
# "WS_H","RH_H","Rain_H","NO2_H" ,"SO2_H","pbl_02","pbl_08","vc_H","Temp_H",
# "Elev_200m","Pop_dens_200m", "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m", "Sp_NDVI14_200m",
# "P_Ag_2014_200m","P_In_Min_2014_200m","P_OS_2014_200m","P_Ur_2004_200m","dist_inven_200m")

# names for Daily mod1 TERRA- for years > 2014

# names=c("Elev","sp_ndvi_L14","su_ndvi_L14","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2007",
# "Dis_Rd2_2007","Dist_Railw","Dist_WB","metreg","ndvi",
# "P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2004",
# "Temp_D","WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_08","vc_D","Elev_200m","Pop_dens_200m", "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m",
# "P_Ag_2014_200m","P_In_Min_2014_200m","P_OS_2014_200m","P_Ur_2004_200m","dist_inven_200m","daily_hpbl")

# names for Daily mod1 TERRA- for years < 2014

# names=c("Elev","sp_ndvi_L0002","su_ndvi_L0002","P_In_2004","P_OS_2004","P_Ag_2004",
# "P_OS_Ag_2004","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2007","Dis_Rd2_2007","Dist_Railw",
# "Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","Temp_D","WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_08","vc_D","aod_055_mean","aod_047_mean","Elev_200m","Pop_dens_200m",
# "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"P_In_2004_200m","P_Ur_2004_200m","Sp_NDVI0002_200m","Su_NDVI0002_200m",
# "dist_inven_200m","ndvi","daily_hpbl")


# names for hourly mod1 TERRA- for years < 2014
# 
# names=c("Elev","sp_ndvi_L0002","su_ndvi_L0002","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2007","Dis_Rd2_2007","Dist_Railw",
# "Dist_WB","metreg","P_Ag_2004","P_In_2004" ,"P_OS_2004" ,"Temp_H",
# "WS_H","RH_H","Rain_H","NO2_H" ,"SO2_H","pbl_02","pbl_08","vc_H","aod_055_mean","aod_047_mean","Elev_200m","Pop_dens_200m",
# "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"Sp_NDVI0002_200m",
# "Su_NDVI0002_200m","P_In_2004_200m","P_Ur_2004_200m","dist_inven_200m","ndvi")

mean(names %in% names(mod1))
a=names %in% names(mod1)
b=names
b[which(a==FALSE)]

mod1 = mod1 %>% as.data.frame 
scaled = mod1[,names] %>% dplyr::mutate_each(funs(scale))
colnames(scaled) = paste0(colnames(scaled), ".scaled")
mod1 = cbind(mod1, scaled)
names(mod1)

#Raw full correlation
# m1.formula <- as.formula(PM25  ~ aod_055_mean)
# m1.formula <- as.formula(PM25  ~ aod_055_mean.scaled)
m1.formula <- as.formula(PM25  ~ aod_047_mean)
mod1fit = lm(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res[res$type=="PM25", 'm1.raw'] <- print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

# Raw cleaned correlation
mod1=as.data.table(mod1)
mod1<-filter(mod1,RelAZ < 90)
mod1<-filter(mod1,UN < 0.04 & UN > 0)
mod1<-filter(mod1,aod_047_mean < 1.5)

# check massimo clean

#massimos thresholds
x<-dplyr::select(mod1,aod_047_mean,stn)
x$c<-1
x <- x %>%
  group_by (stn) %>%
  summarise(saod=sum(c))

x=as.data.table(x)
mod1=as.data.table(mod1)

#merge back count
setkey(x,stn)
setkey(mod1,stn)
mod1 <- merge(mod1,x, all.x = T)

mod1$exobs<-0
mod1<-mod1[aod_047_mean < quantile(aod_047_mean, c(.30)) & PM25 >  quantile(PM25, c(.90)), exobs := 2]
mod1<-mod1[aod_047_mean > quantile(aod_047_mean, c(.90)) & PM25 <  quantile(PM25, c(.30)), exobs := 3]
# mod1<-mod1[aod_055 > 2 , exobs := 4] # already filtered
mod1<-mod1[saod < 20 , exobs := 5]

#take out bad exobs
mod1<-filter(mod1,exobs==0)

mod1=as.data.table(mod1)
mod1fit = lm(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res[res$type=="PM25", 'm1.raw'] <- print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

mod1=as.data.table(mod1)
#based mixed model
m1.formula <- as.formula(PM25 ~ aod_047_mean
#temporal
+(1+aod_047_mean|day))  

#stage 1
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res[res$type=="PM25", 'm1.R2']=print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

#RMSPE
res[res$type=="PM25", 'm1.rmspe'] <- print(rmse(residuals(mod1fit)))

#spatial
spatialall<-mod1 %>%
  group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))

#temporal
#temporal (take out daily PM from yearly mean)
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

## for Daily dataset- 2014 year

mod1=mod1[!is.na(mod1$NO2_D.scaled),]

##v1 mixed model
# m1.formula <- as.formula(PM25 ~ aod_047_mean
#                          # #spatial
#                          +Elev.scaled+dis_inventory.scaled + Dis_Rd1_2012.scaled
#                          + Pop_dens.scaled+#Dist_Railw.scaled+
#                          + P_In_Min_2014.scaled+P_Ur_2014.scaled+P_Ag_2014.scaled+#P_OS_2014.scaled
#                          #temporal
#                          + daily_hpbl.scaled+vc_D.scaled
#                          #met
#                          + Temp_D.scaled+Rain_D.scaled+RH_D.scaled+WS_D.scaled+SO2_D.scaled+NO2_D.scaled
#                          +(1+aod_047_mean|day/Ur_Ru_1))

##v1 mixed model
m1.formula <- as.formula(PM25 ~ aod_047_mean
                         # #spatial
                         +Elev.scaled+dis_inventory.scaled + #Dis_Rd1_2012.scaled
                         + Pop_dens.scaled+#Dist_Railw.scaled+
                           + P_In_2004.scaled+P_Ur_2004.scaled+P_Ag_2004.scaled+P_OS_2004.scaled
                           #temporal
                           + #daily_hpbl.scaled+vc_D.scaled
                         #met
                         + Temp_D.scaled+Rain_D.scaled+RH_D.scaled+WS_D.scaled+SO2_D.scaled+NO2_D.scaled
                         +(1+aod_047_mean|day/metreg))

#stage 1
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)
res[res$type=="PM25", 'm1.R2']=print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

#RMSPE
res[res$type=="PM25", 'm1.rmspe'] <- print(rmse(residuals(mod1fit)))

#spatial
spatialall<-mod1 %>%
  group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))

#temporal
#temporal (take out daily PM from yearly mean)
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

# #check linearity for daily mod1
# 
# x<-gam(PM25~ s(Elev.scaled),data=mod1)
# plot(x)
# summary(x)
# 
# x<-gam(PM25~ s(Dis_Rd1_2007.scaled),data=mod1)
# plot(x)
# summary(x)
# 
# x<-gam(PM25~ s(dis_inventory.scaled),data=mod1)
# plot(x)
# summary(x)
# 
# x<-gam(PM25~ s(WS_D.scaled),data=mod1)
# plot(x)
# summary(x)
# 
# x<-gam(PM25~ s(P_In_Min_2014.scaled),data=mod1)
# plot(x)
# summary(x)
# 
# x<-gam(PM25~ s(P_Ur_2014.scaled),data=mod1)
# plot(x)
# summary(x)
# 
# x<-gam(PM25~ s(P_Ag_2014.scaled),data=mod1)
# plot(x)
# summary(x)
# 
# x<-gam(PM25~ s(daily_hpbl.scaled),data=mod1)
# plot(x)
# summary(x)
# 
# x<-gam(PM25~ s(Elev.scaled)+s(Dis_Rd1_2007.scaled)+s(dis_inventory.scaled)+s(P_In_Min_2014.scaled)+s(P_Ur_2014.scaled)+s(P_Ag_2014.scaled)+s(daily_hpbl.scaled)+s(WS_D.scaled),data=mod1)
# plot(x)
# summary(x)


#save MOD1 
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2007.PM25.predm1.rds")
 saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2007.PM25_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_hourly.AQ.2007.PM25_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_hourly.AQ.2007.PM25.predm1.rds")

# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.TR.2007.PM25.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.TR.2007.PM25_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_hourly.TR.2007.PM25_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_hourly.TR.2007.PM25.predm1.rds")

# Save AQUA results
    # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Daily.rds")
    # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Daily_clean.rds")
    # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Hourly_clean.rds")
    # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Hourly.rds")  
# Save TERRA results
    # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Daily.rds")
    # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Daily_clean.rds")
    # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Hourly_clean.rds")
    # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Hourly.rds")
    
## Building cross-validation (cv) dataset
        
    library(doSNOW)
    library(foreach)
    library(lme4)
    cl<-makeCluster(4) #change the 2 to your number of CPU cores
    registerDoSNOW(cl)
    
    test_final =  foreach(i=1:10 ,.packages=c("lme4","stats","splines"))  %dopar% {
      splits_s <- splitdf(mod1)
      test_s <- splits_s$testset
      train_s <- splits_s$trainset
      out_train_s <- lmer(m1.formula,data =  train_s )
      test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL )
      test_s$iter <- i
      test_s
    } 
    stopCluster(cl)
    
    # The doMC library is not available in Windows
    # library(foreach)
    # library(doMC)
    # registerDoMC(cores=4)
    # 
    # test_final = foreach(i = 1:10) %dopar% {
    #   
    #     splits_s <- splitdf(mod1)
    #     test_s <- splits_s$testset
    #     train_s <- splits_s$trainset
    #     out_train_s <- lmer(m1.formula,data =  train_s )
    #     test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL )
    #     test_s$iter <- i
    #     test_s
    #     
    # }
    
#BIND 1 dataset
mod1.cv = do.call(rbind, test_final)

      #save
# AQUA
      # saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.AQ.2007.PM25_Daily_CV.rds")     
       saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.AQ.2007.PM25_Daily_clean_CV.rds")
      # saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.AQ.2007.PM25_Hourly_CV.rds")
      # saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.AQ.2007.PM25_Hourly_clean_CV.rds")

# TERRA
      # saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.TR.2007.PM25_Hourly_clean_CV.rds")
      # saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.TR.2007.PM25_Hourly_CV.rds")
      # saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.TR.2007.PM25_Daily_CV.rds")     
      # saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.TR.2007.PM25_Daily_clean_CV.rds")
      
#table updates
      m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=mod1.cv)
      res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$r.squared)
      res[res$type=="PM25", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[1,1])
      res[res$type=="PM25", 'm1cv.Ise'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[1,2])
      res[res$type=="PM25", 'm1cv.slope'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[2,1])
      res[res$type=="PM25", 'm1cv.slopese'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[2,2])
      #RMSPE
      res[res$type=="PM25", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))
      
      library(dplyr)
      
      #spatial
      spatialall.cv<-mod1.cv %>%
        dplyr::group_by(stn) %>%
        dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
      m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
      res[res$type=="PM25", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
      res[res$type=="PM25", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))
      #temporal
      tempoall.cv<-left_join(mod1.cv,spatialall.cv)
      tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
      tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
      mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
      res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

# Save AQUA results
      # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Daily.rds")
       saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Daily_clean.rds")
      # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Hourly_clean.rds")
      # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Hourly.rds")  
 
# Save TERRA results
      # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Daily.rds")
      # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Daily_clean.rds")
      # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Hourly_clean.rds")
      # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Hourly.rds")
      
#local stage
head(mod1)

# Explore svm
library(e1071)
mod1.cv$res.m1<- mod1.cv$PM25-mod1.cv$pred.m1.cv
mod1.cv<-filter(mod1.cv,!is.na(mod1.cv$Pop_dens_200m.scaled))

# first explore variables in linear model
# reg=lm(res.m1~ Elev_200m.scaled + Road_den_200m.scaled + Road_dis_200m.scaled + Pop_dens_200m.scaled+
#        Sp_NDVI14_200m.scaled+ P_Ur_2014_200m.scaled +  P_Ag_2014_200m.scaled +P_In_Min_2014_200m.scaled +
#       P_OS_2014_200m.scaled  ,data=mod1.cv)
# 
# reg=lm(res.m1~ Elev_200m.scaled + Road_den_200m.scaled + Road_dis_200m.scaled + Pop_dens_200m.scaled+    
#          Sp_NDVI14_200m.scaled +  P_Ag_2014_200m.scaled +P_In_Min_2014_200m.scaled + 
#           P_Ur_2014_200m.scaled ,data=mod1.cv)
# 
# reg=lm(res.m1~ Elev_200m.scaled + Pop_dens_200m.scaled+Road_dis_200m.scaled +     
#          Dis_Railways_200m.scaled +  Su_NDVI0002_200m.scaled + P_Ur_2014_200m.scaled+dist_inven_200m.scaled
#        +P_In_2014_200m.scaled ,data=mod1.cv)
# 
# summary(reg)

# then svm:
#for early years
tryx <-svm(res.m1~ Elev_200m.scaled + Road_den_200m.scaled + Road_dis_200m.scaled + Pop_dens_200m.scaled+
             Sp_NDVI0002_200m.scaled +  P_In_2004_200m.scaled +
             P_Ur_2004_200m.scaled ,type="nu-regression",cross=10,data=mod1.cv)
mod1.cv$predsvmloc <- predict(object=tryx)

#for late years 
# tryx <-svm(res.m1~ Elev_200m.scaled + Pop_dens_200m.scaled+Road_dis_200m.scaled +Sp_NDVI14_200m.scaled+
#              Dis_Railways_200m.scaled + P_Ag_2014_200m.scaled +P_In_Min_2014_200m.scaled
#             +P_Ur_2014_200m.scaled ,type="nu-regression",cross=10,data=mod1.cv)
# mod1.cv$predsvmloc <- predict(object=tryx)

## reg
mod1.cv$pred.m1.bothsvm <- mod1.cv$pred.m1 + mod1.cv$predsvmloc
res[res$type=="PM25", 'm1cvloc.R2'] <- print(summary(lm(PM25~pred.m1.bothsvm,data=mod1.cv))$r.squared)
res[res$type=="PM25", 'm1cvloc.Ise'] <-print(summary(lm(PM25~pred.m1.bothsvm,data=mod1.cv))$coef[1,2])
res[res$type=="PM25", 'm1cvloc.slope'] <-print(summary(lm(PM25~pred.m1.bothsvm,data=mod1.cv))$coef[2,1])
res[res$type=="PM25", 'm1cvloc.slopese'] <-print(summary(lm(PM25~pred.m1.bothsvm,data=mod1.cv))$coef[2,2])

#RMSPE
m1.fit.all.loc<-lm(PM25~pred.m1.bothsvm,data=mod1.cv)
res[res$type=="PM25", 'm1cvloc.rmspe'] <- print(rmse(residuals(m1.fit.all.loc)))

#spatial
spatialall.cv.loc<-mod1.cv %>%
    dplyr::group_by(stn) %>%
    dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1.bothsvm, na.rm=TRUE)) 
m1.fit.all.cv.loc.s <- lm(barpm ~ barpred, data=spatialall.cv.loc)
res[res$type=="PM25", 'm1cvloc.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv.loc))$r.squared)
res[res$type=="PM25", 'm1cvloc.R2.time'] <- print(rmse(residuals(m1.fit.all.cv.loc.s)))
       
#temporal
tempoall.loc.cv<-left_join(mod1.cv,spatialall.cv.loc)
tempoall.loc.cv$delpm <-tempoall.loc.cv$PM25-tempoall.loc.cv$barpm
tempoall.loc.cv$delpred <-tempoall.loc.cv$pred.m1.both-tempoall.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempoall.loc.cv)
res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.loc.cv))$r.squared)

# Save AQUA results 
 # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Daily.rds")
 saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Daily_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Hourly_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Hourly.rds")  

# Save TERRA results
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Daily.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Daily_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Hourly_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Hourly.rds")


# Save RDS and csv files
# 
# # (1) read AQUA daily clean model results
# A_res_clean_D=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Daily_clean.rds")
# A_res_clean_D= subset(A_res_clean_D, select = -c(m1cvloc.I))
# A_res_clean_D=A_res_clean_D[,c(1:35)]
# 
# # (2) read AQUA daily all model results
# A_res_all_D=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Daily.rds")
# A_res_all_D= subset(A_res_all_D, select = -c(m1cvloc.I))
# A_res_all_D=A_res_all_D[,c(1:35)]
# 
# # (3) read AQUA Hourly clean model results
# A_res_clean_H=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Hourly_clean.rds")
# A_res_clean_H= subset(A_res_clean_H, select = -c(m1cvloc.I))
# A_res_clean_H=A_res_clean_H[,c(1:35)]
# 
# # (4) read AQUA Hourly all obs model results
# A_res_all_H=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.AQ.2007.PM25_Hourly.rds")
# A_res_all_H=A_res_all_H[,c(1:35)]
# 
# # (5) read TERRA Daily clean model results
# T_res_clean_D=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Daily_clean.rds")
# T_res_clean_D=T_res_clean_D[,c(1:35)]
# 
# # (6) read TERRA Daily all obs n model results
# T_res_D_all=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Daily.rds")
# T_res_D_all=T_res_D_all[,c(1:35)]
# 
# # (7) read TERRA Hourly clean model results
# T_res_clean_H=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Hourly_clean.rds")
# T_res_clean_H=T_res_clean_H[,c(1:35)]
# 
# # (8) read TERRA Hourly all obs n model results
# T_res_all_H=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.TR.2007.PM25_Hourly.rds")
# T_res_all_H=T_res_all_H[,c(1:35)]
# 
# res_all=rbind(A_res_clean_D,A_res_all_D,A_res_clean_H,A_res_all_H,T_res_clean_D,T_res_D_all,T_res_clean_H,T_res_all_H)
# res_all$names=c("AQUA_Daily_clean_PM25","AQUA_Daily_all_PM25","AQUA_hourly_clean_PM25","AQUA_hourly_all_PM25","TERRA_Daily_clean_PM25","TERRA_Daily_all_PM25","TERRA_hourly_clean_PM25","TERRA_hourly_all_PM25")
# write.csv(res_all,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.mod1.2007.PM25.csv")

### mod 2 (around 2-4 h)

mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQ.MAIAC.2007.mod2.rds")
mod2=as.data.table(mod2)

# Add missing variables
# Add daily hPBL 
# hpbl=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_dailyavg.csv")
# hpbl$year=substr(hpbl$date,1,4)
# hpbl=dplyr::filter(hpbl,hpbl$year=="2007")
# hpbl=as.data.table(hpbl)
# setnames(hpbl,"PBLid","pblid")
# setnames(hpbl,"date","day")
# hpbl$day=as.Date(hpbl$day)
# hpbl[,c("V1","year"):=NULL]
# 
# setkey(mod2,pblid,day)
# setkey(hpbl,pblid,day)
# mod2 <- merge(mod2,hpbl,all.x = T)
# mod2=as.data.table(mod2)

# ## ADD ventilation coefficient
# mod2$vc_D=c(mod2$WS_D/(mod2$daily_hpbl*1000))

# # Add MODIS NDVI id data
# NDVIid=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/NDVI_Join/1_km_MAIAC_grid_ndviid.csv")
# mod2=as.data.table(mod2)
# NDVIid=as.data.table(NDVIid)
# setkey(mod2,aodid)
# setkey(NDVIid,aodid)
# mod2=merge(mod2,NDVIid,all.x = T)
# 
# #import NDVI
# ndvi<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
# ndvi=as.data.table(ndvi)
# ndvi=dplyr::filter(ndvi,c=="2007")
# library(stringi)
# ndvi$m<-stri_pad_left(str=ndvi$m, 2, pad="0")
# mod2$m=substr(mod2$day,6,7)
# 
# # add ndviid to mod2
# mod2=as.data.table(mod2)
# setnames(mod2,"ndviid.y","ndviid")
# #join actual NDVI to aod
# ndvi=as.data.table(ndvi)
# setkey(ndvi, ndviid, m)
# setkey(mod2,ndviid, m)
# mod2<- merge(mod2, ndvi,all.x = T)
# 
# # delete unnecessery columns and change columns names
# mod2=as.data.table(mod2)
# mod2[,c("lat_ndvi","long_ndvi","c"):=NULL]

# Add Urban \ Rural area classifiaction

# If P_Ur_2004 is missing
setkey(mod2,aodid)
p_ur=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse_2004/Landuse_Tables_by_squares/P_Ur_2004.csv")
setnames(p_ur,"AODID","aodid")
setkey(p_ur,aodid)
p_ur[,c("COUNT","SUM"):=NULL]
mod2=merge(mod2,p_ur,all.x = T)

# for earlier years
mod2$Ur_Ru_1 <- NA
mod2$Ur_Ru_1<- mod2$P_Ur_2004
mod2$Ur_Ru_1[mod2$Ur_Ru_1<=20] <-1 
mod2$Ur_Ru_1[20<= mod2$Ur_Ru_1 & mod2$Ur_Ru_1<=40] <-2
mod2$Ur_Ru_1[40<= mod2$Ur_Ru_1 & mod2$Ur_Ru_1<=60] <-3
mod2$Ur_Ru_1[60<= mod2$Ur_Ru_1 & mod2$Ur_Ru_1<=80] <-4
mod2$Ur_Ru_1[mod2$Ur_Ru_1>=80] <-5

# # first option
# mod2$Ur_Ru_1 <- NA
# mod2$Ur_Ru_1<- mod2$P_Ur_2014
# mod2$Ur_Ru_1[mod2$Ur_Ru_1<=20] <-1 
# mod2$Ur_Ru_1[20<= mod2$Ur_Ru_1 & mod2$Ur_Ru_1<=40] <-2
# mod2$Ur_Ru_1[40<= mod2$Ur_Ru_1 & mod2$Ur_Ru_1<=60] <-3
# mod2$Ur_Ru_1[60<= mod2$Ur_Ru_1 & mod2$Ur_Ru_1<=80] <-4
# mod2$Ur_Ru_1[mod2$Ur_Ru_1>=80] <-5
# 
# 
# # Third option
# mod2$Ur_Ru_2 <- NA
# mod2$Ur_Ru_2<- mod2$P_Ur_2014
# mod2$Ur_Ru_2[mod2$Ur_Ru_2<=20] <-1 
# mod2$Ur_Ru_2[20<= mod2$Ur_Ru_2 & mod2$Ur_Ru_2<=50] <-2
# mod2$Ur_Ru_2[50<= mod2$Ur_Ru_2 & mod2$Ur_Ru_2<=80] <-3
# mod2$Ur_Ru_2[mod2$Ur_Ru_2>=80] <-4

#scale variables
mod2 = mod2 %>% as.data.frame 
names(mod2)
# names=c("Elev","sp_ndvi_L14", "su_ndvi_L14","dis_inventory","Dis_Mroads","road_den","Pop_dens",
#         "Dis_Rd1_2007","Dis_Rd2_2007","Dist_Railw", "Dist_WB","ndvi",
#         "P_Ag_2014","P_In_Min_2014","P_OS_2014","P_OS_Ag_2014","P_Ur_2014",
#         "Temp_D","WS_D", "RH_D","Rain_D","NO2_D","SO2_D", "PM25_D_closest", "PM25_D_mean","PM25_IDW",
#         "pbl_02","pbl_11", "vc_D", "daily_hpbl")

names=c("Elev","sp_ndvi_L0002","su_ndvi_L0002","P_In_2004","P_OS_2004","P_Ag_2004","P_Ur_2004"
        ,"dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","Temp_D","WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11","vc_D","ndvi","daily_hpbl")

names=c("Elev","sp_ndvi_L0002","su_ndvi_L0002","P_In_2004","P_OS_2004","P_Ag_2004","P_Ur_2004"
        ,"dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","Temp_D","WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11","ndvi")

all(names %in% names(mod2))
a=names %in% names(mod2)
b=names[which(a==FALSE)]
b

scaled = mod2[, names] %>% dplyr::mutate_each(funs(scale))
colnames(scaled) = paste0(colnames(scaled), ".scaled")
mod2 = cbind(mod2, scaled)
setnames(mod2,"aod_047","aod_047_mean")
mod2=as.data.table(mod2)
names(mod2)

# Data filtering
mod2<-filter(mod2,RelAZ < 90)
mod2<-filter(mod2,UN < 0.04 & UN > 0)
mod2<-filter(mod2,aod_047_mean < 2)

mod2$pred.m2 = predict(object=mod1fit,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)
head(mod2)
summary(mod2$pred.m2)
mod2 <- select(mod2,day,aodid,PM25_D_mean,long_aod,lat_aod,pred.m2,aod_047_mean,PM25_D_closest,PM25_IDW)
gc()

# mod2<-filter(mod2,!is.na(PM25_D_mean))
mod2<-filter(mod2,!is.na(pred.m2))
mod2$m = as.numeric(format(mod2$day, "%m")) 
mod2$bimon = (mod2$m + 1) %/% 2
gc()
names(mod2)
# saveRDS(mod2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily.2007.PM.predm2.rds")
 saveRDS(mod2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily_clean.2007.PM.predm2.rds")
# res[res$type=="PM25", 'm2.R2'] <- print(summary(lm(PM25_IDW~pred.m2,data=mod2))$r.squared)
#keep(mod2,res,rmse,splitdf, sure=TRUE) 


# Save AQUA results 
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.AQ.2007.PM25_mean_Daily.rds")
 # saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.AQ.2007.PM25_Daily.rds")
 saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.AQ.2007.PM25_Daily_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.AQ.2007.PM25_Hourly_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.AQ.2007.PM25_Hourly.rds")  

# Save TERRA results
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.TR.2007.PM25_Daily.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.TR.2007.PM25_Daily_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.TR.2007.PM25_Hourly_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/results.mod1.TR.2007.PM25_Hourly.rds")

#check spatial patterns by plotting a map in mod2
out <-mod2 %>%
  group_by(aodid) %>%
  summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod_047_mean))
out<-na.omit(out)
write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/tst_2007.csv")

#run lme regression, this *should* include the thin plate spline yet will not run (computational limitations) thus we break it down into 2 components  
 mod2.fit.s2 = lme(pred.m2 ~ PM25_IDW,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"), data= mod2 )
# mod2=filter(mod2,!is.na(mod2$PM25_D_mean))
# mod2.fit.s2 = lme(pred.m2 ~ PM25_D_mean,random = list(aodid= ~1 + PM25_D_mean),control=lmeControl(opt = "optim"), data= mod2)
#correlate to see everything from mod2 and the mpm works
mod2$pred.m2.s2 = predict(mod2.fit.s2)
mod2$resid = residuals(mod2.fit.s2)
#check R2 
print(summary(lm(pred.m2~pred.m2.s2,data=mod2))$r.squared)

# Manual process
mod2=as.data.table(mod2)
setkey(mod2,day, aodid)

#split the files to the separate bi monthly data sets
Tall_bimon1 <- subset(mod2 ,mod2$bimon == "1")
Tall_bimon2 <- subset(mod2 ,mod2$bimon == "2")
Tall_bimon3 <- subset(mod2 ,mod2$bimon == "3")
Tall_bimon4 <- subset(mod2 ,mod2$bimon == "4")
Tall_bimon5 <- subset(mod2 ,mod2$bimon == "5")
Tall_bimon6 <- subset(mod2 ,mod2$bimon == "6")

# install.packages("mgcv")
library(mgcv)
#run the separate splines (smooth) for x and y for each bimon
fit2_1 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.m2.s2 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.m2.s2 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.m2.s2 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.m2.s2 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.m2.s2 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.m2.s2 - fit2_6$fitted)

#remerge to 1 file
mod2$fitted <- c(fit2_1$fitted,fit2_2$fitted,fit2_3$fitted,fit2_4$fitted, fit2_5$fitted, fit2_6$fitted)
mod2$pred.m2.s3 <- c(Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

library("data.table")

#this is important so that its sorted as in the first gamm
mod2=as.data.table(mod2)
setkey(mod2,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
 mod2.fit.s4 <- lme(pred.m2.s3~ PM25_IDW ,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"),data= mod2)
# mod2.fit.s4 <- lme(pred.m2.s3~ PM25_D_mean ,random = list(aodid= ~1 + PM25_D_mean),control=lmeControl(opt = "optim"),data= mod2)
mod2$pred.m2.s4 = predict(mod2.fit.s4)
#check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 

# Parallel
# library(doSNOW)
# library(foreach)
# library(lme4)
# cl<-makeCluster(4) #change the 2 to your number of CPU cores
# registerDoSNOW(cl)

# The doMC library is not available in Windows
# library(foreach)
# library(doMC)
# registerDoMC(cores=20)

# mod2_split = split(mod2, mod2$bimon)
# 
# final = foreach(i = 1:6,.packages=c("mgcv","base")) %dopar% {
#   
#   fit = gam(resid ~ s(long_aod,lat_aod),  data= mod2_split[[i]] )
#   cbind(mod2_split[[i]], pred.m2.s3 = (mod2_split[[i]]$pred.m2.s2 - fit$fitted))
#   
# }
# stopCluster(cl)
# 
# final = do.call(rbind, final)
# # sort 
# final= arrange(final,day, aodid)
# 
# #rerun the lme on the predictions including the spatial spline (smooth)
# mod2.fit.s4 <- lme(pred.m2.s3 ~PM25_D_mean ,random = list(aodid= ~1 + PM25_D_mean ),control=lmeControl(opt = "optim"),data= final)
# mod2$pred.m2.s4 = predict(mod2.fit.s4)
# #check correlations
# print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 


## real mod3 starts here :)
#mod 3 (5-8 h)
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQ.MAIAC.2007.mod3.rds")
mod3[, m := as.numeric(format(day, "%m")) ]
mod3 <- dplyr::select(mod3,day,aodid,m,PM25_D_mean,PM25_IDW,long_aod,lat_aod,RelAZ)
mod3$bimon = (mod3$m + 1) %/% 2
setkey(mod3,day, aodid)
# mod3<-mod3[!is.na(PM25_D_mean)]

#generate m.3 mix model  predictions 
mod3$pred.m3.mix <-  predict(mod2.fit.s4,mod3)

#create unique grid
ugrid <-mod3 %>%
    dplyr::group_by(aodid) %>%
    dplyr::summarise(long_aod = mean(long_aod, na.rm=TRUE),  lat_aod = mean(lat_aod, na.rm=TRUE)) 

ugrid<-as.data.table(ugrid)

#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
mod3_bimon1 <- mod3[bimon == 1, ]
mod3_bimon2 <- mod3[bimon == 2, ]
mod3_bimon3 <- mod3[bimon == 3, ]
mod3_bimon4 <- mod3[bimon == 4, ]
mod3_bimon5 <- mod3[bimon == 5, ]
mod3_bimon6 <- mod3[bimon == 6, ]

#addin unique grid to each bimon           
uniq_gid_bimon1 <- ugrid
uniq_gid_bimon2 <- ugrid
uniq_gid_bimon3 <- ugrid
uniq_gid_bimon4 <- ugrid
uniq_gid_bimon5 <- ugrid
uniq_gid_bimon6 <- ugrid

#get predictions for Bimon residuals
uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)

#merge things back togheter
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
setkey(uniq_gid_bimon1,aodid)
setkey(mod3_bimon1,aodid)
mod3_bimon1 <- merge(mod3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(mod3_bimon2,aodid)
mod3_bimon2 <- merge(mod3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(mod3_bimon3,aodid)
mod3_bimon3 <- merge(mod3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(mod3_bimon4,aodid)
mod3_bimon4 <- merge(mod3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(mod3_bimon5,aodid)
mod3_bimon5 <- merge(mod3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(mod3_bimon6,aodid)
mod3_bimon6 <- merge(mod3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(mod3_bimon1,mod3_bimon2,mod3_bimon3,mod3_bimon4,mod3_bimon5,mod3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
summary(mod3$pred.m3)
   # saveRDS(mod3,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.pred_mean_PM.AQ_Daily.2007.rds")
  # saveRDS(mod3,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.pred.AQ_Daily.2007.rds")
  saveRDS(mod3,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.pred.AQ_Daily_clean.2007.rds")

# keep(data.m3,mod3,res,rmse, sure=TRUE) 
gc()

#calculate stage 3 R2- CV ten folds approach will take 6 weeks...we don't currently do CV for stage 3.
# mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2007.PM25.predm1.rds")
 mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2007.PM25_clean.predm1.rds")

##fix
mod1$aodid=paste(formatC(round(mod1$long_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")
mod1<-mod1[,c("aodid","day","PM25","pred.m1","stn"),with=FALSE]
#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
mod1 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=mod1))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=mod1))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=mod1))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=mod1))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=mod1))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=mod1))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))

#spatial
###to check
spatialall<-mod1 %>%
    dplyr::group_by(stn) %>%
    dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))

#temporal
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)
# saveRDS(res, "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily.2007.PM25_mean.rds")
 # saveRDS(res, "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily.2007.PM25.rds")
  saveRDS(res, "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily_clean.2007.PM25.rds")

#create final prediction data set for use in health outcome studies

#import mod2
# mod2<- readRDS( "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily.2007.PM.predm2.rds")
 mod2<- readRDS( "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily_clean.2007.PM.predm2.rds")
mod2=as.data.table(mod2)
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, long_aod, lat_aod, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1[,list(aodid,day,pred.m1,PM25)], all.x = T,allow.cartesian = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
summary(mod3best$bestpred)
mod3best[bestpred < 0 , bestpred  := 0.5]
mod3best<-select(mod3best,day,aodid,long_aod,lat_aod,bestpred)
#save
 # saveRDS(mod3best,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred.AQ_Daily.2007.PM25_mean.rds")
 # saveRDS(mod3best,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred.AQ_Daily.2007.PM25.rds")
  saveRDS(mod3best,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred.AQ_Daily_clean.2007.PM25.rds")
mod3best<-filter(mod3best,!is.na(bestpred))

#save for plotting in QGIS
out <- mod3best %>% group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
out<-na.omit(out)
# write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/map.bestpred.AQ_Daily.2007.PM25.csv")
 write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/map.bestpred.AQ_Daily_clean.2007.PM25.csv")

 #save res
 # saveRDS(res, "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily.2007.PM25_mean.rds")
 # saveRDS(res, "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily.2007.PM25.rds")
 saveRDS(res, "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily_clean.2007.PM25.rds")

# keep(rmse,splitdf, sure=TRUE) 
gc()

### combine results summary 

# # Filtered data using IDW
# res_clean_IDW=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily_clean.2007.PM25.rds")
# res_clean_IDW=res_clean_IDW[,1:33]
# # Filtered data using mean PM
# res_clean_mean_PM=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily.2007.PM25_mean.rds")
# res_clean_mean_PM=res_clean_mean_PM[,1:33]
# # All data using IDW
# # res_all_IDW=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily.2007.PM25.rds")
# # res_all_IDW=res_all_IDW[,1:33]
# # All data using mean_PM
# res_all_mean_PM=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/resALL.AQ_Daily.2007.PM25.rds")
# res_all_mean_PM=res_all_mean_PM[,1:33]
# 
# res_all=base::rbind(res_clean_IDW,res_clean_mean_PM,res_all_IDW)
# res_all=base::rbind(res_clean_IDW,res_clean_mean_PM,res_all_mean_PM)
# 
# res_all$names=c("AQUA_Daily_clean_PM25_IDW","AQUA_Daily_clean_PM25_mean_PM","AQUA_Daily_all_PM25_IDW")
# res_all$names=c("AQUA_Daily_clean_PM25_IDW","AQUA_Daily_clean_PM25_mean_PM","AQUA_Daily_all_PM25_mean_PM")
# write.csv(res_all,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2007/results.all_mods.2007.PM25.csv")

