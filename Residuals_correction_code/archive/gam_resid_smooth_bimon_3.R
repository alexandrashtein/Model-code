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

# Load mod1
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")
mod1=as.data.table(mod1)

# Mixed effects model

m1.formula <- as.formula(PM25 ~ aod_047
                         # +aod_047_mean*c +aod_047_mean*FS_BS+FS_BS+stn_type+aod_047_mean*stn_type
                         #spatial
                         +Elev.s +ndvi.s+Pop_dens.s+Dis_Rd1_2012.s
                         # +dis_inventory.s 
                         # +road_den.s
                         # +Dist_Railw.s
                         # +Dist_WB.s
                         +P_In.s 
                         +P_Ur.s
                         +P_Ag.s+P_OS.s
                         #temporal
                         +daily_hpbl.s+Temp_D.s+Rain_D.s+RH_D.s+NO2_D.s
                         # +pbl_02.s+vc_D.s+WS_D.s+O3_D+SO2_D.s
                         +(1+aod_047|day/metreg))
#stage 1
mod1fit <- lmer(m1.formula,data=mod1)


# Load mod2 prediction

mod2=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily_clean.2003_2015.PM25.predm2.rds")
mod2=as.data.table(mod2)

# Prediction for MOD3
mod2.fit.s2 = lme(pred.m2 ~ PM25_IDW,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"), data= mod2 )
#correlate to see everything from mod2 and the mpm works
mod2$pred.m2.s2 = predict(mod2.fit.s2)
mod2$resid = residuals(mod2.fit.s2)

#Choose one bimon
mod2=mod2[bimon=="1.2015"]

## How many obs per day?

mod2_obs_per_day <-mod2 %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(len=length(unique(aodid)))

mod2_obs_per_day =as.data.table(mod2_obs_per_day)

bad_days=mod2_obs_per_day[len<500]

# Remove all days with less than 50 observations

mod2=mod2[!day %in% bad_days$day]

#run lme regression, this *should* include the thin plate spline yet will not run (computational limitations) thus we break it down into 2 components  

mod2= arrange(mod2,day, aodid)
mod2=as.data.table(mod2)

library(foreach)
library(doMC)
registerDoMC(cores=30)

mod2_split = split(mod2, mod2$day) #split the data by days

# for each period produce a fit of a gam model (interpolation of residualds) 

fits = foreach(i = 1:length(mod2_split)) %dopar% {
  
  gam(resid ~ s(lon_aod,lat_aod),  data= mod2_split[[i]] )
  
}

# use the fits (from the previous stage) for removing the residuals from mod2 prediction

final = foreach(i = 1:length(mod2_split)) %dopar% {
  
  cbind(mod2_split[[i]], pred.m2.s3 = (mod2_split[[i]]$pred.m2.s2 - fits[[i]]$fitted))
  
}

final = do.call(rbind, final)
## sort 
final= arrange(final,day, aodid)

## rerun the lme on the predictions including the spatial spline (smooth)
mod2.fit.s4 <- lme(pred.m2.s3 ~PM25_IDW ,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"),data= final)
mod2$pred.m2.s4 = predict(mod2.fit.s4)
## check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 
print(summary(lm(pred.m2~pred.m2.s2,data=mod2))$r.squared)

# Spatial spline for whole period

mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")
mod2=as.data.table(mod2)

# Data filtering
mod2<-filter(mod2,UN < 0.04 & UN > 0)
mod2<-filter(mod2,aod_047 < 3.5)
mod2$c=substr(mod2$day,1,4)

# Predict PM for mod2 from formula craeted in the calibration stage
mod2$pred.m2 = predict(object=mod1fit,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)
head(mod2)
summary(mod2$pred.m2)
mod2 <- mod2[,c("day","aodid","PM25_D_mean","lon_aod","lat_aod","pred.m2","aod_047","PM25_D_closest","PM25_IDW","c")]

gc()

# Delete implossible values
mod2<-filter(mod2,!is.na(pred.m2))
mod2<-filter(mod2,pred.m2 > 0)
summary(mod2$pred.m2)

# Add bimons
mod2$m = as.numeric(format(mod2$day, "%m")) 
mod2$bimon = (mod2$m + 1) %/% 2
mod2$bimon=paste(mod2$bimon,mod2$c,sep=".")
gc()

mod2.fit.s2 = lme(pred.m2 ~ PM25_IDW,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"), data= mod2 )
#correlate to see everything from mod2 and the mpm works
mod2$pred.m2.s2 = predict(mod2.fit.s2)
mod2$resid = residuals(mod2.fit.s2)
#check R2 
print(summary(lm(pred.m2~pred.m2.s2,data=mod2))$r.squared)

#run lme regression, this *should* include the thin plate spline yet will not run (computational limitations) thus we break it down into 2 components  

mod2= arrange(mod2,day, aodid)
mod2=as.data.table(mod2)

library(foreach)
library(doMC)
registerDoMC(cores=30)

mod2_split = split(mod2, mod2$bimon) #split the data to bi-monthly periods, in each year (overall 78 periods)

# for each period produce a fit of a gam model (interpolation of residualds) 

fits = foreach(i = 1:length(mod2_split)) %dopar% {
  
  gam(resid ~ s(lon_aod,lat_aod),  data= mod2_split[[i]] )
  
}

# use the fits (from the previous stage) for removing the residuals from mod2 prediction

final = foreach(i = 1:length(mod2_split)) %dopar% {
  
  cbind(mod2_split[[i]], pred.m2.s3 = (mod2_split[[i]]$pred.m2.s2 - fits[[i]]$fitted))
  
}

final = do.call(rbind, final)
## sort 
final= arrange(final,day, aodid)

## rerun the lme on the predictions including the spatial spline (smooth)
mod2.fit.s4 <- lme(pred.m2.s3 ~PM25_IDW ,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"),data= final)
mod2$pred.m2.s4 = predict(mod2.fit.s4)
## check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 

# For one bimon only
## rerun the lme on the predictions including the spatial spline (smooth)
final2=filter(final,bimon=="1.2015"|bimon=="2.2015")
mod2.fit.s4 <- lme(pred.m2.s3 ~PM25_IDW ,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"),data= final2)

mod2_bi=filter(mod2,bimon=="1.2015"|bimon=="2.2015")
mod2_bi$pred.m2.s4 = predict(mod2.fit.s4)

## check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2_bi))$r.squared) # 0.82
print(summary(lm(pred.m2 ~ pred.m2.s2,data=mod2_bi))$r.squared) # 0.84

## Add the daily Resid prediction

#run lme regression, this *should* include the thin plate spline yet will not run (computational limitations) thus we break it down into 2 components  

mod2= arrange(mod2,day, aodid)
mod2=as.data.table(mod2)

bad_days=mod2_obs_per_day[len<1000]

# Remove all days with less than 50 observations

mod2=mod2[!day %in% bad_days$day]

library(foreach)
library(doMC)
registerDoMC(cores=30)

mod2_split = split(mod2, mod2$day) #split the data to bi-monthly periods, in each year (overall 78 periods)

# for each period produce a fit of a gam model (interpolation of residualds) 

fits = foreach(i = 1:length(mod2_split)) %dopar% {
  
  gam(resid ~ s(lon_aod,lat_aod),  data= mod2_split[[i]] )
  
}

# use the fits (from the previous stage) for removing the residuals from mod2 prediction

final = foreach(i = 1:length(mod2_split)) %dopar% {
  
  cbind(mod2_split[[i]], pred.m2.s3_by_day = (mod2_split[[i]]$pred.m2.s2 - fits[[i]]$fitted))
  
}

final = do.call(rbind, final)
## sort 
final= arrange(final,day, aodid)

mod2.fit.s4_db <- lme(pred.m2.s3_by_day ~PM25_IDW ,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"),data= final)
mod2$pred.m2.s4_db = predict(mod2.fit.s4_db)
## check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4_db,data=mod2))$r.squared) 
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 
print(summary(lm(pred.m2 ~ pred.m2.s2,data=mod2))$r.squared) 


# mod 3 (5-8 h)
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily.rds")
mod3=as.data.table(mod3)
mod3$c=substr(mod3$day,1,4)
mod3 <- dplyr::select(mod3,day,aodid,PM25_D_mean,PM25_IDW,lon_aod,lat_aod,RelAZ,c)
mod3[, m := as.numeric(format(day, "%m")) ]
mod3$bimon = (mod3$m + 1) %/% 2
mod3$bimon = paste(mod3$bimon,mod3$c,sep=".")

mod3=mod3[bimon=="01.2015"]

setkey(mod3, day, aodid)
setkey(mod2, day, aodid)
mod3<- merge(mod3, mod2[,list(aodid, day, pred.m2,resid)], all.x = T)
summary(mod3)

#generate m.3 mix model predictions 

mod3= arrange(mod3,day, aodid)
mod3$pred.m3.mix <-  predict(mod2.fit.s4,mod3)
summary(mod3$pred.m3.mix)

#create unique grid
ugrid <-mod3 %>%
  dplyr::group_by(aodid) %>%
  dplyr::summarise(lon_aod = mean(lon_aod, na.rm=TRUE),  lat_aod = mean(lat_aod, na.rm=TRUE)) 

ugrid<-as.data.table(ugrid)

## fit gam prediction for each day

gpred=predict.gam(fit_l,ugrid,se.fit=TRUE)

## fit SE of GAM prediction for each day
ugrid_l$gpred <-gpred_l$fit
ugrid_l$gpred_se <-gpred_l$se.fit

# Create a data frame of number of mod2 prediction for each day and the max SE for each day

