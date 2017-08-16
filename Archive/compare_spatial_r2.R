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


## My data 

mod1.keep=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/all_years/mod1.AQ.2003_2015.PM25_Daily_filt04_clean.rds")
mod1.keep=as.data.table(mod1.keep)
mod1.keep$day=as.Date(mod1.keep$day)

# Remove outliers in 2006
mod1.06=filter(mod1.keep,year == 2006)
mod1.06=filter(mod1.06,aod_047_mean <0.9)
mod1.06=filter(mod1.06,PM25 <200)

# Remove outliers in 2007
mod1.07=filter(mod1.keep,year == 2007)
mod1.07=filter(mod1.07,aod_047_mean <1.2)

# Remove outliers in 2008
mod1.08=filter(mod1.keep,year == 2008)
mod1.08=filter(mod1.08,aod_047_mean <1)
mod1.08=filter(mod1.08,PM25 <200)

# Remove outliers in 2011
mod1.11=filter(mod1.keep,year == 2011)
mod1.11=filter(mod1.11,aod_047_mean <1.2)
mod1.11=filter(mod1.11,PM25 <250)

# Remove outliers in 2012
mod1.12=filter(mod1.keep,year == 2012)
mod1.12=filter(mod1.12,PM25 <250)

# Bind all the data together
mod1.keep=filter(mod1.keep,year != 2006)
mod1.keep=filter(mod1.keep,year != 2007)
mod1.keep=filter(mod1.keep,year != 2008)
mod1.keep=filter(mod1.keep,year != 2011)
mod1.keep=filter(mod1.keep,year != 2012)

mod1.keep=rbind(mod1.keep,mod1.06,mod1.07,mod1.08,mod1.11,mod1.12)
mod1.keep=as.data.table(mod1.keep)
mod1.keep$day=as.Date(mod1.keep$day)

# Mixed effects model

m1.formula <- as.formula(PM25 ~ aod_047_mean
                         +(1+aod_047_mean|day))

m1.formula <- as.formula(PM25 ~ aod_047_mean
                         # +aod_047_mean*c +aod_047_mean*FS_BS+FS_BS+stn_type+aod_047_mean*stn_type
                         #spatial
                         +Elev.s +ndvi.s+Pop_dens.s+Dis_Rd1_2012.s
                         # +dis_inventory.s 
                         # +road_den.s
                         +Dist_WB.s
                         +P_In.s
                         +P_Ur.s
                         +P_Ag.s
                         +P_OS.s
                         #temporal
                         +daily_hpbl.s+Temp_D.s +Rain_D.s +RH_D.s+NO2_D.s+SO2_D.s
                         # +pbl_02.s +vc_D.s+WS_D.s
                         +(1+aod_047_mean|day/metreg))

years=c(2003:2015)
spatial_r2=c()
r2=c()

for (i in 1:length(years))
{
mod1=filter(mod1.keep,year==years[i])  
mod1=as.data.table(mod1)
#stage 1
mod1fit <- lmer(m1.formula,data=mod1)
mod1$pred.m1 <- predict(mod1fit)
m1.R2=print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)
r2=c(r2,m1.R2)

#spatial
spatialall<-mod1 %>%
  group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
m1.R2.space <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
spatial_r2=c(spatial_r2,m1.R2.space)
}

mean(r2)
mean(spatial_r2)

## Meytar's data 

mod1.keep.m=readRDS("/media/qnap/Projects/P019.IL.Israel.MAIAC.PM/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1C.AQ.PM25.rds")

# Mixed effects model

m1.formula <- as.formula(PM25 ~ aod
                         +(1+aod|day))
years=c(2003:2013)
spatial_r2_m=c()
r2_m=c()

for (i in 1:length(years))
{
  mod1=filter(mod1.keep.m,c==years[i])  
  mod1=as.data.table(mod1)
  #stage 1
  mod1fit <- lmer(m1.formula,data=mod1)
  mod1$pred.m1 <- predict(mod1fit)
  m1.R2=print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)
  r2_m=c(r2_m,m1.R2)
  
  #spatial
  spatialall<-mod1 %>%
    group_by(stn) %>%
    dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
  m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
  m1.R2.space <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
  spatial_r2_m=c(spatial_r2_m,m1.R2.space)
}

mean(spatial_r2_m)
r2_m=c(r2_m,0,0)
spatial_r2_m=c(spatial_r2_m,0,0)
compare=data.frame(m=round(spatial_r2_m,2),a=round(spatial_r2,2),years=c(2003:2015))
compare_r2=data.frame(m=round(r2_m,2),a=round(r2,2),years=c(2003:2015))

mod1.03=filter(mod1.keep,year=="2003")
mod1.04=filter(mod1.keep,year=="2004")
mod1.05=filter(mod1.keep,year=="2005")
mod1.06=filter(mod1.keep,year=="2006")
mod1.07=filter(mod1.keep,year=="2007")
mod1.08=filter(mod1.keep,year=="2008")
mod1.09=filter(mod1.keep,year=="2009")
mod1.10=filter(mod1.keep,year=="2010")
mod1.11=filter(mod1.keep,year=="2011")
mod1.12=filter(mod1.keep,year=="2012")
mod1.13=filter(mod1.keep,year=="2013")
mod1.14=filter(mod1.keep,year=="2014")
mod1.15=filter(mod1.keep,year=="2015")

par(mfrow=c(2,3))
plot(mod1.03$PM25~mod1.03$aod_047_mean,xlab="aod",ylab="PM2.5",main="2003")
plot(mod1.04$PM25~mod1.04$aod_047_mean,xlab="aod",ylab="PM2.5",main="2004")
plot(mod1.05$PM25~mod1.05$aod_047_mean,xlab="aod",ylab="PM2.5",main="2005")
plot(mod1.06$PM25~mod1.06$aod_047_mean,xlab="aod",ylab="PM2.5",main="2006")
plot(mod1.07$PM25~mod1.07$aod_047_mean,xlab="aod",ylab="PM2.5",main="2007")
plot(mod1.08$PM25~mod1.08$aod_047_mean,xlab="aod",ylab="PM2.5",main="2008")


par(mfrow=c(2,3))
plot(mod1.09$PM25~mod1.09$aod_047_mean,xlab="aod",ylab="PM2.5",main="2009")
plot(mod1.10$PM25~mod1.10$aod_047_mean,xlab="aod",ylab="PM2.5",main="2010")
plot(mod1.11$PM25~mod1.11$aod_047_mean,xlab="aod",ylab="PM2.5",main="2011")
plot(mod1.12$PM25~mod1.12$aod_047_mean,xlab="aod",ylab="PM2.5",main="2012")
plot(mod1.13$PM25~mod1.13$aod_047_mean,xlab="aod",ylab="PM2.5",main="2013")
plot(mod1.14$PM25~mod1.14$aod_047_mean,xlab="aod",ylab="PM2.5",main="2014")
plot(mod1.15$PM25~mod1.15$aod_047_mean,xlab="aod",ylab="PM2.5",main="2015")


## Load new data

mod1=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2008.PM25_Daily_re.rds")
# mod1=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2009.PM25_Daily.rds")
mod1=as.data.table(mod1)

## Scale
names=c("Elev","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","Temp_D","P_In","P_OS","P_Ur","P_Ag","WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11","ndvi","daily_hpbl")

# names=c("Elev","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw", "Dist_WB","Temp_D","P_In_2004","P_OS_2004","P_Ur_2004","P_Ag_2004", "WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11")
mod1 = mod1 %>% as.data.frame 
f = function(x) scale(x)[,1]
scaled = mod1[,names] %>% dplyr::mutate_each(funs(f))
colnames(scaled) = paste0(colnames(scaled), ".s")
mod1 = cbind(mod1, scaled)
names(mod1)

mod1 = mod1 %>% as.data.table
