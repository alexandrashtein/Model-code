##### install packages

# rm(list = ls())
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

#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=32)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time" #loc m1
#,"m2.R2" #mod2
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese",
"type")#Extra

res$type <- c("PM25")

#load data

# AQUA data
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")

mod1=as.data.table(mod1)

# Leave one station out

mod1=mod1[stn!="BSV"]

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
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)
res[res$type=="PM25", 'm1.R2']=print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

### Run mod 2 

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

#check spatial patterns by plotting a map in mod2
out <-mod2 %>%
  dplyr::group_by(aodid) %>%
  dplyr::summarise(x=mean(lon_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod_047))
out<-na.omit(out)
out=as.data.table(out)

mod2=as.data.table(mod2)

# Plot the PM2.5 Prediction map
out=out[out$predm2<40,]
ggplot() +  geom_point(data=out, aes(x=x, y=y,color=predm2),alpha=1, size=1)+
  scale_color_gradient("PM25 prediction",low="lightblue2",high="red")+ggtitle("PM25 prediction for 2003-2015")

mod2.fit.s2 = lme(pred.m2 ~ PM25_IDW,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"), data= mod2 )
#correlate to see everything from mod2 and the mpm works
mod2$pred.m2.s2 = predict(mod2.fit.s2)
mod2$resid = residuals(mod2.fit.s2)
#check R2 
print(summary(lm(pred.m2~pred.m2.s2,data=mod2))$r.squared)

## real mod3 starts here :)
#mod 3 (5-8 h)
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily.rds")
mod3$c=substr(mod3$day,1,4)
mod3 <- dplyr::select(mod3,day,aodid,PM25_IDW,lon_aod,lat_aod,c)

mod3= arrange(mod3,day, aodid)
mod3$pred.m3.mix <-  predict(mod2.fit.s2,mod3)
summary(mod3$pred.m3.mix)
hist(mod3$pred.m3)
summary(mod3$pred.m3)

# filter unreasonable values
mod3=filter(mod3, pred.m3.mix >0)
mod3=as.data.table(mod3)

# saveRDS(mod3,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.2015_without_ASS_station.rds")
# saveRDS(mod3,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.2015_without_AFU_station.rds")
saveRDS(mod3,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.2015_without_BSV_station.rds")
  