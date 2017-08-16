##### Load libraries
rm(list=ls())
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
library(sp)
library(spdep) ## this library allow to calculate Spatial neighbors 
library("rgdal")
library("rgeos")
library(ncf)
library("spatstat")
library(gstat)
library(foreach)
library(doMC)

########## Load all needed data ################
#load Mod1
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")

#load Mod2
mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")
mod2=as.data.table(mod2)
mod2 <- mod2[!is.na(NO2_D.s)]

mod2$PM25_IDW.x <- NULL
# Data filtering
mod2<-filter(mod2,UN < 0.04 & UN > 0)
mod2<-filter(mod2,aod_047 < 3.5)
mod2=as.data.table(mod2)
# mod2$c=substr(mod2$day,1,4)

#load Mod 3 (5-8 h)
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily.rds")
# mod3$c <- substr(mod3$day,1,4)
mod3 <- dplyr::select(mod3,day,aodid,lon_aod,lat_aod)

## Add X,Y to MOD3
mod3$X=mod3$lon_aod
mod3$Y=mod3$lat_aod
newProj=CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs")
coordinates(mod3) = ~ X + Y
proj4string(mod3) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mod3 = spTransform(mod3, newProj)
mod3=as.data.table(mod3)

## Load stations data

PM25 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/PM25_2003_2015.rds")
PM25 <- as.data.table(PM25)

# Mixed effects model
m1.formula <- as.formula(PM25 ~ aod_047 +Elev.s +ndvi.s+Dis_Rd1_2012.s+P_In.s +P_Ur.s +P_Ag.s+P_OS.s
                         +daily_hpbl.s+Temp_D.s+NO2_D.s
                         +(1+aod_047|day/metreg))

mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

## Building cross-validation (cv) dataset

mons <- unique(mod1$stn)

# Remove two stations that do not have observations in MOD1
mons <- mons[!mons %in% "HEF"]
mons <- mons[!mons %in% "REM"]

xout <- 1 # number of monitors to hold out
n.iter <- length(mons)

PM25_cv <- list() # a list to cotain the final cv results

for(i in 1:n.iter)
  {
  mons.test <- combn(mons, xout)[,i] 
  test <- mod1[stn %in% mons.test, ] # Test data set includs the data from one station  
  train<- mod1[!stn %in% mons.test, ] # Train data set includes all data except one station
  
  # Calibration stage- fit the model
  trainmod <-  lmer(m1.formula, data =  train)
  
  ####################### Interpolation of PM stations ##############################
  
  PM25_for_IDW=PM25[!stn %in% mons.test, ] # Remove the test station from interpolation
  days <- unique(mod3$day)
  
  registerDoMC(cores=10)
  
  test_final = foreach(m = 1:length(days)) %dopar% {
    
    mod3_s=mod3[day==days[m]]
    PM25_s=PM25_for_IDW[day==days[m]]
    
    ## Use IDW interpolation for residuals interpolation from monitors
    coordinates(PM25_s) = ~ X + Y
    proj4string(PM25_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
    coordinates(mod3_s) = ~ X + Y
    proj4string(mod3_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
    inter = gstat(formula = PM25 ~ 1,  data =PM25_s)
    mod3_s$PM25_IDW <-predict(object = inter, newdata = mod3_s)$var1.pred
    mod3_s=as.data.table(mod3_s)
    mod3_s
  }
  
  # Convert back the list into a data.table
  mod3_s <-  do.call(rbind, test_final)
  mod3_s <- mod3_s[,list(aodid,day,PM25_IDW)]
  
  # Join the result to mod3
  setkey(mod3,aodid,day)
  setkey(mod3_s,aodid,day)
  mod3 <- merge(mod3,mod3_s,all.x=T)
  
  # join the interpolation results to mod2
  setkey(mod2,aodid,day)
  setkey(mod3_s,aodid,day)
  mod2 <- merge(mod2,mod3_s,all.x=T)
  
  ############################## Predict for MOD3 ########################
  
  # MOD2 - Predict PM for mod2 from formula created in the calibration stage
  mod2$pred.m2 = predict(object=trainmod,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)
  mod2 <- mod2[!is.na(pred.m2)]
  mod2.fit.s2 = lme(pred.m2 ~ PM25_IDW,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"), data= mod2 )
  
  # Generate m.3 mix model predictions 
  mod3$pred.m3.mix <-  predict(mod2.fit.s2,mod3)
  
  ## Join mod3 prediction to stations data
  setkey(PM25,aodid,day)
  mod3_t=mod3[,list(day,aodid,pred.m3.mix)]
  setkey(mod3_t,aodid,day)
  PM25=merge(PM25,mod3_t)
  rm(mod3_t)
  
  ## Compute the difference between observed and predicted
  PM25$diff=PM25$PM25-PM25$pred.m3.mix
  
  ########################## Test Auto- correlation in residuals ########################
  
  ## The following loop calculates Moran I for the residuals of each day
  
  days <- unique(PM25$day)
  ## Create a data.frame that will store the results from the Moran I loop
  df=data.frame(day=numeric(0),sign=numeric(0), moran_stat=numeric(0))
  class(df$day) <- "Date"
  
  ## Paraller Moran I
  registerDoMC(cores=10)
  df.res = foreach(j = 1:length(days),.packages=c('spdep','sp')) %dopar% {
    df=data.frame(day=numeric(0),sign=numeric(0), moran_stat=numeric(0))
    class(df$day) <- "Date"
    PM25_s <- PM25[day==days[j]] 
    
    ## Convert monitors to spatial dataframe
    coordinates(PM25_s) <-  ~ X + Y
    proj4string(PM25_s) <-  "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
    
    w <- 1/as.matrix(dist(coordinates(PM25_s))) ## Define weights
    diag(w) <- 0
    
    res=moran.mc(PM25_s$diff,mat2listw(w),10000) ## A simulation- based approach - moran.mc
    
    df[1,"day"] <- days[j]
    df[1,"sign"] <- as.numeric(res$p.value < 0.05)
    df[1,"moran_stat"] <- round(as.numeric(paste(res$statistic[1])),2) # For simulation based approach
    df
  }
  
  df <- do.call(rbind,df.res)
  rm(df.res)
  
  # Save Moran I resutls
  saveRDS(df,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Moran_I_per_stn/moran_1st.rds")
  
  # Find the days where there is positive spatial auto-correlation (pss) in the residuals
  # psac=df[df$sign==1 & df$moran_stat > 0,]
  psac=df[df$sign==1 & df$moran_stat > 0.2,]
  psac_days=unique(psac$day) ## Only in these days residuals correction will be carried out. 
  
  ####################### Interpolation of Residuals ##############################
  
  ## The following loop adds the residuals interpolation for these days.
  ## first we will take one of the test station out
  
  PM25_for_IDW=PM25[!stn %in% mons.test, ] # Remove the test station from interpolation
  
  registerDoMC(cores=10)
  
  test_final = foreach(m = 1:length(psac_days)) %dopar% {
    
    mod3_s=mod3[day==psac_days[m]]
    PM25_s=PM25_for_IDW[day==psac_days[m]]
    
    ## Use IDW interpolation for residuals interpolation from monitors
    coordinates(PM25_s) = ~ X + Y
    proj4string(PM25_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
    coordinates(mod3_s) = ~ X + Y
    proj4string(mod3_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
    inter = gstat(formula = diff ~ 1,  data =PM25_s)
    mod3_s$resid_IDW <-predict(object = inter, newdata = mod3_s)$var1.pred
    mod3_s=as.data.table(mod3_s)
    mod3_s
  }
  
  # Convert back the list into a data.table
  mod3_s <-  do.call(rbind, test_final)
  mod3_s <- mod3_s[,list(aodid,day,resid_IDW)]
  
  # Join the result to mod3
  setkey(mod3,aodid,day)
  setkey(mod3_s,aodid,day)
  mod3 <- merge(mod3,mod3_s,all.x=T)
  
  # Create an improved prediction
  mod3$resid_IDW[is.na(mod3$resid_IDW)] <- 0
  mod3$pred.m3.mix.I <-  mod3$pred.m3.mix + mod3$resid_IDW 
  
  # Add to PM monitors data prediction from MOD3 (improved and non-Improved model)
  setkey(PM25, aodid, day)
  mod3_s <- mod3[,list(aodid,day,resid_IDW,pred.m3.mix.I)]
  setkey(mod3_s, aodid, day)
  PM25 <- merge(PM25,mod3_s,all.x=TRUE)
  
  # Save PM25 to a new object
  PM25_pred <- PM25[stn==mons.test]
  PM25_cv[[i]] <- PM25_pred
  
  # Keep only needed columns for the next loop
  PM25 <- PM25[,list(aodid,day,X,Y,stn,PM25)]
  mod3 <- mod3[,list(day,aodid,X,Y,lon_aod,lat_aod)]
  mod2$PM25_IDW <- NULL
}

saveRDS(PM25_cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/resid_check/PM25_cv.rds")

final <- do.call(rbind,PM25_cv)
final <-as.data.frame(PM25_cv[[1]])

# Calculate R2 and RMSE for mod3 prediction without resid correction
source("/media/qnap/Data/code/R_functions/rmspe.r")

fit.improved<- summary(lm(PM25~pred.m3.mix.I, data=final))
print(summary(lm(PM25~pred.m3.mix.I, data=final))$r.squared)  
print(rmse(resid(fit.improved)))

# Calculate R2 and RMSE for mod3 prediction with resid correction
fit.mix <- summary(lm(PM25~pred.m3.mix, data=final))
print(summary(lm(PM25~pred.m3.mix, data=final))$r.squared)  
print(rmse(residuals(fit.mix)))
final$resid.mix <- resid(fit.mix)

# Calculate difference

final$diff_Pred.ms= final$PM25-final$pred.m3.mix
final$diff_Pred.ms.resid= final$PM25-final$pred.m3.mix.I

# RMSE
round(sqrt(mean(abs(final$diff_Pred.ms)^2)),2)
round(sqrt(mean(abs(final$diff_Pred.ms.resid)^2)),2)

rmse(final$diff_Pred.ms)
rmse(final$diff_Pred.ms.resid)

plot(AFU$PM25~AFU$day,type="l", main="AFU station time series",ylab="PM25", xlab="day")
lines(AFU$pred.m3.mix~AFU$day,col="red")

