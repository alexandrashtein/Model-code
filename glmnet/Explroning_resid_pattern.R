##### install packages

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

#load data

# AQUA data
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")
mod1=as.data.table(mod1)
mod1$PM25.sq=sqrt(mod1$PM25)

mod1=filter(mod1,aod_047<1.5)  

# Mixed effects model

m1.formula <- as.formula(PM25~ aod_047
                         # +aod_047_mean*c +aod_047_mean*FS_BS+FS_BS+stn_type+aod_047_mean*stn_type
                         #spatial
                         +Elev.s +ndvi.s+Dis_Rd1_2012.s
                         # +dis_inventory.s +Pop_dens.s
                         # +road_den.s
                         # +Dist_Railw.s
                         # +Dist_WB.s
                         +P_In.s 
                         +P_Ur.s
                         +P_Ag.s+P_OS.s
                         #temporal
                         +daily_hpbl.s+SO2_D.s+NO2_D.s
                         # +pbl_02.s+vc_D.s+WS_D.s+O3_D+RH_D.sTemp_D.s+Rain_D.s
                         +(1+aod_047|day/metreg))

#stage 1
# mod1fit <- lmer(m1.formula,data=mod1,weights=normwt)
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)

# For non-transformed data
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

# Residuals plot for non-transformed data
mod1$resid=mod1$PM25-mod1$pred.m1
plot(mod1$resid~mod1$pred.m1)

## How many stations are in each day?

stn <-mod1 %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(num=length(unique(stn))) 

stn <-as.data.table(stn)

# Spatial Residuals plot for non-transformed data
mod1_1=filter(mod1, day=="2015-01-01")
  
ggplot() +
  geom_point(data = mod1_1, aes(x = x_stn_ITM, y = y_stn_ITM, color=resid))+
  scale_color_gradientn("Residuals mod1",colours = terrain.colors(10))+
  ggtitle("Residuals mod1 for 01.01.2015")

# For transformed data
# mod1$pred.m1.sq <- predict(mod1fit)
# print(summary(lm(PM25.sq~pred.m1.sq,data=mod1))$r.squared)
# 
# # Residuals plot for transformed data
# mod1$pred.m1.sq2=mod1$pred.m1.sq*mod1$pred.m1.sq
# print(summary(lm(PM25~pred.m1.sq2,data=mod1))$r.squared)
# mod1$resid=mod1$PM25-mod1$pred.m1.sq2
# plot(mod1$resid~mod1$pred.m1.sq2)

## MOD2

mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")
mod2=as.data.table(mod2)
mod2$year=year(mod2$day)

# Data filtering
# mod2<-filter(mod2,RelAZ < 90)
mod2<-filter(mod2,UN < 0.04 & UN > 0)
mod2<-filter(mod2,aod_047 < 1.5)
mod2$c=substr(mod2$day,1,4)
mod2=filter(mod2, year=="2015")

# Predict PM for mod2 from formula craeted in the calibration stage
mod2$pred.m2 = predict(object=mod1fit,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)
summary(mod2$pred.m2)
mod2 <- mod2[,c("day","lon_aod","lat_aod","pred.m2","aod_047","PM25_IDW","aodid")]

gc()

# Delete implossible values
mod2<-filter(mod2,!is.na(pred.m2))
mod2<-filter(mod2,pred.m2 > 0)
summary(mod2$pred.m2)

# Check the correlations between prediction and results of interpolation
sim = lm(PM25_IDW ~pred.m2 , data= mod2)
summary(sim)
mod2$pred.sim <- predict(sim)
print(summary(lm(PM25_IDW~pred.sim,data=mod2))$r.squared)

mod2.fit.s2 = lme(pred.m2 ~ PM25_IDW,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"), data= mod2 )

#correlate to see everything from mod2 and the mpm works

mod2$pred.m2.s2 = predict(mod2.fit.s2)
mod2$resid = residuals(mod2.fit.s2)
#check R2 
print(summary(lm(pred.m2~pred.m2.s2,data=mod2))$r.squared)

## Plot the residuals for one bimon
resid_m2=filter(mod2, bimon=="1.")

ggplot() +
  geom_point(data = resid_m2, aes(x = lon_aod, y = lat_aod, color=resid))+
  scale_color_gradientn("Residuals mod2",colours = terrain.colors(10))+
  ggtitle("Residuals mod2 Jan-Feb 2015")

# Add bimons
mod2$m = as.numeric(format(mod2$day, "%m")) 
mod2$bimon = (mod2$m + 1) %/% 2
mod2$bimon=paste(mod2$bimon,mod2$c,sep=".")

mod2= arrange(mod2,day, aodid)
mod2=as.data.table(mod2)

library(foreach)
library(doMC)
registerDoMC(cores=3)
library(mgcv)

mod2_split = split(mod2, mod2$bimon) #split the data to bi-monthly periods, in each year (overall 78 periods)

# for each period produce a fit of a gam model (interpolation of residualds) 

fits = foreach(i = 1:length(mod2_split)) %dopar% {
  
  mgcv::gam(resid ~ s(lon_aod,lat_aod),  data= mod2_split[[i]] )
  
}

# use the fits (from the previous stage) for removing the residuals from mod2 prediction

final = foreach(i = 1:length(mod2_split)) %dopar% {
  
  cbind(mod2_split[[i]], pred.m2.s3 = (mod2_split[[i]]$pred.m2.s2 - fits[[i]]$fitted))
  
}

final = do.call(rbind, final)
## sort 
final= arrange(final,day, aodid)

gc()

## rerun the lme on the predictions including the spatial spline (smooth)
mod2.fit.s4 <- lme(pred.m2.s3 ~PM25_IDW ,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"),data= final)
mod2$pred.m2.s4 = predict(mod2.fit.s4)
## check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 

## real mod3 starts here :)
#mod 3 (5-8 h)

mod3 <- readRDS("/media/qnap/Data/Echo/yearly.aod.16022017/MOD1&2&3/MAIACAAOT_roi_2011_v2017-03-25_MOD3.rds")
library(lubridate)
mod3$year=year(mod3$day)
mod3=filter(mod3,year=="2015")
mod3 <- dplyr::select(mod3,day,aodid,PM25_IDW,lon,lat)
mod3=as.data.table(mod3)
mod3[, m := as.numeric(format(day, "%m")) ]
mod3$bimon = (mod3$m + 1) %/% 2
summary(mod3$bimon)

#generate m.3 mix model predictions 
mod3$pred.m3.mix <-  predict(mod2.fit.s4,mod3)
summary(mod3$pred.m3.mix)

#create unique grid
ugrid <-mod3 %>%
  dplyr::group_by(aodid) %>%
  dplyr::summarise(lon = mean(lon, na.rm=TRUE),  lat = mean(lat, na.rm=TRUE)) 

ugrid<-as.data.table(ugrid)

# Adding gam prediction 

mod3= arrange(mod3,day, aodid)
mod3_split = split(mod3, mod3$bimon) #split back into bimons to include the gam prediction in final prediction        
uniq_gid_bimon <- ugrid

## Adding gam prediction using a loop

final_mod3 = list()

for (i in 1:length(mod3_split)) 
{
  uniq_gid_bimon$gpred <- predict.gam(fits[[i]],uniq_gid_bimon)
  setkey(uniq_gid_bimon,aodid)
  mod3_split[[i]]=as.data.table(mod3_split[[i]])
  setkey(mod3_split[[i]],aodid)
  result=merge(mod3_split[[i]], uniq_gid_bimon[,list(aodid,gpred)], all.x = T)
  final_mod3[[i]]=result
}

final_mod3 = do.call(rbind, final_mod3)

## sort 
final_mod3= arrange(final_mod3,day, aodid)

mod3=final_mod3
rm(final_mod3)
gc()

mod3=as.data.table(mod3)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
summary(mod3$pred.m3)

# filter unreasonable values
mod3=filter(mod3, pred.m3 >0)
mod3=as.data.table(mod3)

