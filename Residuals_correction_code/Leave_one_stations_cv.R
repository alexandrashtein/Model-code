##### Load libraries

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

########## Load all needed data ################
#load Mod1
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")

#load Mod2
mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")
mod2=as.data.table(mod2)
mod2 <- mod2[!is.na(NO2_D.s)]
# Data filtering
mod2<-filter(mod2,UN < 0.04 & UN > 0)
mod2<-filter(mod2,aod_047 < 3.5)
mod2$c=substr(mod2$day,1,4)

#load Mod 3 (5-8 h)
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily.rds")
mod3$c <- substr(mod3$day,1,4)
mod3 <- dplyr::select(mod3,day,aodid,PM25_D_mean,PM25_IDW,lon_aod,lat_aod,RelAZ,c)
# mod3 <- filter(mod3,c=='2015')

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
n.iter <- length(month)

## Parallel processing

library(foreach)
library(doMC)
registerDoMC(cores=10)

mod3_pred <- foreach(i=1:n.iter, .packages = c("data.table", "lme4") ) %dopar% {
  
  mons.test <- combn(mons, xout)[,i] 
  test <- mod1[stn %in% mons.test, ] # Test data set includes the data from one station  
  train<- mod1[!stn %in% mons.test, ] # Train data set includes all data except one station
  
  # Calibration stage- fit the model
  trainmod <-  lmer(m1.formula, data =  train)
  
  # MOD2 - Predict PM for mod2 from formula craeted in the calibration stage
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
  
  ## Compute the difference between observed and predicted
  PM25$diff=PM25$PM25-PM25$pred.m3.mix
  
  list(mod3, PM25) ## Save the outputs in a list object (each list component will have the mod3)
  
  ## The following loop calculates Moran I for the residuals of each day
  days <- unique(PM25$day)
  for (i in 1:length(days))
  {
    
    PM25_s <- PM25[day==days[i]] 
    
    ## Convert monitors to spatial dataframe
    coordinates(PM25_s) <-  ~ X + Y
    proj4string(PM25_s) <-  "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
    
    ######## First method to Define weights
    w <- 1/as.matrix(dist(coordinates(PM25_s)))
    diag(w) <- 0
    # res=moran.test(PM25_s$diff,mat2listw(w))
    
    ## A simulation- based approach - moran.mc
    
    res=moran.mc(PM25_s$diff,mat2listw(w),10000) 
    
    df[i,"day"] <- days[i]
    df[i,"sign"] <- as.numeric(res$p.value < 0.05)
    df[i,"moran_stat"] <- round(as.numeric(paste(res$statistic[1])),2) # For simulation based approach
  }
  # Find the days where there is positive spatial auto-correlation (pss) in the residuals
  psac=df[df$sign==1 & df$moran_stat > 0,]
  psac_days=unique(pss$day) ## Only in these days residuals correction will be carried out. 
  
  # join to mod3 the list of days where residuals correction should be applied
  mod3$pss_days <- pss_days
  mod3[, pss_days:=day==pss_days] 
  
}

# Extract only PM2.5 database 
PM25_list = lapply(mod3_pred, "[", 2)

final[, sqrt(mean((PM25 - predcv)^2))]
print(summary(lm(PM25~predcv,data=final))$r.squared)


