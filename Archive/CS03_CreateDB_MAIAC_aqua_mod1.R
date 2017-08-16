
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

# remove.packages("allanjust/aodlur")
# devtools::install_github("allanjust/aodlur",force = TRUE)
library("aodlur")
library(sp)
library(rgdal)
library(stringi)

db= readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/AQ.MAIAC.2008.mod2.rds")

# saving as shapefile only unique id
# db=as.data.table(db)
# mod2_grid=db[!duplicated(aodid)]
# coordinates(mod2_grid) = ~ lon_aod + lat_aod
# proj4string(mod2_grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# writeOGR(mod2_grid,dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/", layer="mod2_grid", driver="ESRI Shapefile")

### Create daily PM2.5 mod1
# daily database
PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM25_D.csv")

PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y",tz="GMT"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25<-PM25[PM25 > 0.000000000001 & PM25 < 1000 ]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]

#clear non continous stations
pmall<- PM25[c==2008]
setnames(pmall,"X","x_stn_ITM")
setnames(pmall,"Y","y_stn_ITM")

db$aodid_b=db$aodid

# ADD AOD 047

jointo.pt <- makepointsmatrix(datatable = pmall, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall , joinfrom = db, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = FALSE, verbose = T)

setkey(pmall,stn,day)
setkey(joinout,stn,day)
PM25.m1<- merge(pmall, joinout, all.x = T)

PM25.m1_D=PM25.m1

# # delete hourly meteorological variables
PM25.m1_D[,c("Temp_H","WS_H" ,"RH_H","Rain_H","NO2_H" ,"SO2_H","PM25_D_closest","PM25_D_mean","PM25_IDW","PM10_D_closest","PM10_IDW","PM10_H_mean","vc_H","PM25_H_mean"):=NULL]

# Define modes
mod1=PM25.m1_D
# mod1$aodid=paste(formatC(round(mod1$lon_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")

# saving as shapefile only unique id
# db=as.data.table(db)
# mod2_grid=db[!duplicated(aodid)]
# coordinates(mod2_grid) = ~ lon_aod + lat_aod
# proj4string(mod2_grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# writeOGR(mod2_grid,dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/", layer="mod2_grid", driver="ESRI Shapefile")

## Scale variables

names=c("daily_hpbl","ndvi","Elev","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","Temp_D","P_In_Min_2014","P_OS_2014","P_Ur_2014","P_Ag_2014",
        "WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11")

mod1 = mod1 %>% as.data.frame 
scaled = mod1[,names] %>% dplyr::mutate_each(funs(scale))
colnames(scaled) = paste0(colnames(scaled), ".s")
mod1 = cbind(mod1, scaled)
names(mod1)

mod1=as.data.table(mod1)
mod1=mod1[!is.na(aod_047)]

# Mixed effects model

m1.formula <- as.formula(PM25 ~ aod_047
                         # +aod_047_mean*c +aod_047_mean*FS_BS+FS_BS+stn_type+aod_047_mean*stn_type
                         #spatial
                         +Elev.s +ndvi.s+Pop_dens.s+Dis_Rd1_2012.s
                         # +dis_inventory.s 
                         # +road_den.s
                         # +Dist_Railw.s
                         # +Dist_WB.s
                         +P_In_Min_2014.s
                         +P_Ur_2014.s
                         +P_Ag_2014.s+P_OS_2014.s
                         #temporal
                         +daily_hpbl.s+Temp_D.s+Rain_D.s +RH_D.s+NO2_D.s
                         # +pbl_02.s+vc_D.s+WS_D.s+O3_D+SO2_D.s
                         +(1+aod_047|day/metreg))

m1.formula <- as.formula(PM25 ~ aod_047
                         # +aod_047_mean*c +aod_047_mean*FS_BS+FS_BS+stn_type+aod_047_mean*stn_type
                         #spatial
                         +Elev+ndvi+Pop_dens+Dis_Rd1_2012
                         # +dis_inventory.s 
                         # +road_den.s
                         # +Dist_Railw.s
                         # +Dist_WB.s
                         +P_In_Min_2014
                         +P_Ur_2014
                         +P_Ag_2014+P_OS_2014
                         #temporal
                         +daily_hpbl+Temp_D+Rain_D +RH_D+NO2_D
                         # +pbl_02.s+vc_D.s+WS_D.s+O3_D+SO2_D.s
                         +(1+aod_047|day/metreg))


#stage 1
# mod1fit <- lmer(m1.formula,data=mod1,weights=normwt)
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

#spatial
spatialall<-mod1 %>%
  group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)

# Predict mod2
mod2=db[!is.na(aod_047)]

# Scale mod2

names=c("daily_hpbl","ndvi","Elev","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","Temp_D","P_In_Min_2014","P_OS_2014","P_Ur_2014","P_Ag_2014",
        "WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11")

mod2 = mod2 %>% as.data.frame 
scaled = mod2[,names] %>% dplyr::mutate_each(funs(scale))
colnames(scaled) = paste0(colnames(scaled), ".s")
mod2 = cbind(mod2, scaled)
names(mod2)
mod2=as.data.table(mod2)

mod2$pred.m2 = predict(object=mod1fit,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)
mod2 =as.data.table(mod2)

# Join mod2 to mod1
mod1=setkey(mod1,"aodid_b","day")
mod2=setkey(mod2,"aodid_b","day")
mod1=merge(mod1,mod2,all.x=T)

mod1=mod1[!is.na(mod1$pred.m2)]

mod1=mod1[!is.na(mod1$PM10_IDW)]

#spatial
spatialall<-mod1 %>%
  group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m2, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)


