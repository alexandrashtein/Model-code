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

########## Load data ################
#load Mod1
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")

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

# Remove two problematic stations
mons <- mons[!mons %in% "HEF"]
mons <- mons[!mons %in% "REM"]

xout <- 1 # number of monitors to hold out
n.iter <- 46

library(foreach)
library(doMC)
registerDoMC(cores=10)

final <- foreach(i=1:2, .packages = c("data.table", "lme4") ) %dopar% {

    mons.test <- combn(mons, xout)[,i] 
    test <- mod1[stn %in% mons.test, ] 
    train<- mod1[!stn %in% mons.test, ]
    
    m1.formula <- as.formula(PM25 ~ aod_047 +Elev.s +ndvi.s+Dis_Rd1_2012.s+P_In.s +P_Ur.s +P_Ag.s+P_OS.s
                             +daily_hpbl.s+Temp_D.s+NO2_D.s
                             +(1+aod_047|day/metreg))
    
    # Calibration stage- fit the model
    trainmod <-  lmer(m1.formula, data =  train)
    
    # Predict for mod1
    test$predcv <- predict(object=trainmod, newdata=test, allow.new.levels=TRUE, re.form=NULL )
    test$itercv <- i
    # export these results
    test[, list(day, stn, PM25, predcv, itercv)]
}

## Bind the list into one table 
mod1.cv = do.call(rbind, final)




