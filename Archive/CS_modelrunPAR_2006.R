##### install packages
# install.packages("foreach")
# install.packages(doSNOW)
# install.packages("e1071")
# install.packages("MASS")
# install.packages("MuMIn")

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
source("N:/Data/code/R_functions/CV_splits.r") 
source("N:/Data/code/R_functions/rmspe.r")
source("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/code/forward_lmer.R")

###year 2006
#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.raw.space","m1.raw.time","m1.time","m1.time.space","m1.time.time","m1.space","m1.space.space","m1.space.time","m1.noaod","m1.noaod.space","m1.noaod.time"
,"m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.I","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.t31","m3.t33" #mod3 tests
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("PM25")

res=subset(res, , -c(m1.raw.space , m1.raw.time , m1.time , m1.time.space , m1.time.time , m1.space , 
                 m1.space.space , m1.space.time,m1.noaod , m1.noaod.space , m1.noaod.time))

#load data

mod1 <-readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/2006/mod1.AQ.2006.PM25_Daily.rds")
summary(mod1)

## Add missing variables:
# add daily average PBL
hpbl=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_dailyavg.csv")
hpbl$year=substr(hpbl$date,1,4)
hpbl=filter(hpbl,hpbl$year=="2006")
hpbl=as.data.table(hpbl)
setnames(hpbl,"PBLid","pblid")
setnames(hpbl,"date","day")
hpbl$day=as.Date(hpbl$day)
hpbl[,c("V1","year"):=NULL]
  
setkey(mod1,pblid,day)
setkey(hpbl,pblid,day)
mod1 <- merge(mod1,hpbl,all.x = T)

## ADD ventilation coefficient
mod1$vc_D=c(mod1$WS_D/(mod1$daily_hpbl*1000))

# Add MODIS NDVI id data
mod1$aodid=paste(formatC(round(mod1$long_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")
NDVIid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/NDVI_join/1_km_MAIAC_grid_ndviid.csv")
mod1=left_join(mod1,NDVIid,by="aodid")

#import NDVI
ndvi<-readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
ndvi=as.data.table(ndvi)
ndvi=filter(ndvi,c=="2014")
library(stringi)
ndvi$m<-stri_pad_left(str=ndvi$m, 2, pad="0")

mod1$m=substr(mod1$day,6,7)

# add ndviid to mod1
mod1=as.data.table(mod1)
setnames(mod1,"ndviid.y","ndviid")
#join actual NDVI to aod
setkey(ndvi, ndviid, m)
setkey(mod1,ndviid, m)
mod1<- merge(mod1, ndvi,all.x = T)

# Delete unnecessery columns

mod1=as.data.table(mod1)
mod1[,c("lat_ndvi","long_ndvi","c"):=NULL]
mod1[,c( "sp_ndvi_L14","su_ndvi_L14","m","lon_200m","lat_200m","ndviid","FS_BS","P_Ag_2014", "P_In_Min_2014", "P_OS_2014","P_OS_Ag_2014","P_Ur_2014"):=NULL]
setnames(mod1,"x_aod_ITM.x","x_aod_ITM")
setnames(mod1,"y_aod_ITM.x","y_aod_ITM")
names(mod1)

#scale variables
# names for daily mod1
names=c("Elev","sp_ndvi_L0002","su_ndvi_L0002","P_In_2004","P_OS_2004","P_Ag_2004",
        "P_OS_Ag_2004","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","Temp_D","WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11","vc_D","aod_055_mean","aod_047_mean","Elev_200m","Pop_dens_200m",
        "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"P_In_2004_200m","P_Ur_2004_200m","Sp_NDVI0002_200m","Su_NDVI0002_200m",
        "dist_inven_200m","ndvi","daily_hpbl")

# names for hourly mod1
names=c("Elev","sp_ndvi_L0002","su_ndvi_L0002","sp_ndvi_L14","su_ndvi_L14","P_In_2004","P_OS_2004","P_Ag_2004",
        "P_OS_Ag_2004","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","metreg_IA" ,"metreg" ,"aridity_in","P_Ag_2014","P_In_Min_2014" ,"P_OS_2014" ,"P_OS_Ag_2014","P_Ur_2014","Temp_H",            
        "WS_H","RH_H","Rain_H","NO2_H" ,"SO2_H","pbl_02","pbl_11","vc_H","aod_055_mean","aod_047_mean","Elev_200m","Pop_dens_200m",
        "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m" ,"P_In_2004_200m","P_Ur_2004_200m","Sp_NDVI0002_200m",
        "Sp_NDVI14_200m","Su_NDVI0002_200m","Su_NDVI14_200m","P_Ag_2014_200m","P_In_Min_2014_200m",
        "P_OS_Ag_2014_200m","P_OS_2014_200m","P_Ur_2014_200m","dist_inven_200m","ndvi")

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

#Raw cleaned correlation
mod1=as.data.table(mod1)
mod1<-filter(mod1,RelAZ < 90)
mod1<-filter(mod1,UN < 0.04 & UN > 0)
mod1<-filter(mod1,aod_047_mean < 2)

# check massimo clean 

#massimos thresholds
x<-select(mod1,aod_047_mean,stn)
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
mod1<-mod1[aod_047_mean < quantile(aod_047_mean, c(.50)) & PM25 >  quantile(PM25, c(.90)), exobs := 2]
mod1<-mod1[aod_047_mean > quantile(aod_047_mean, c(.90)) & PM25 <  quantile(PM25, c(.50)), exobs := 3]
# mod1<-mod1[aod_055 > 2 , exobs := 4] # already filtered
mod1<-mod1[saod < 30 , exobs := 5]

#take out bad exobs
mod1<-filter(mod1,exobs==0)

mod1fit = lm(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res[res$type=="PM25", 'm1.raw'] <- print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

#based mixed model
m1.formula <- as.formula(PM25 ~ aod_047_mean.scaled
#temporal
+(1+aod_047_mean.scaled|day))  

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

#v1 mixed model
m1.formula <- as.formula(PM25 ~ aod_047_mean.scaled
#spatial
+Elev.scaled+ sp_ndvi_L0002.scaled + su_ndvi_L0002.scaled + dis_inventory.scaled + Dis_Mroads.scaled +   
+ road_den.scaled + Pop_dens.scaled + Dis_Rd1_2012.scaled + Dis_Rd2_2012.scaled + Dist_Railw.scaled + Dist_WB.scaled+ 
P_In_2004.scaled + P_OS_2004.scaled + P_Ag_2004.scaled +
#temporal
ndvi.scaled+ daily_hpbl.scaled+vc_D.scaled
#met
+Temp_D.scaled+Rain_D.scaled+RH_D.scaled+WS_D.scaled+SO2_D.scaled+NO2_D.scaled
+(1+aod_047_mean.scaled|day/metreg))  

##v2 mixed model
m1.formula <- as.formula(PM25 ~ aod_047_mean.scaled
                         #spatial
                         +Elev.scaled+ sp_ndvi_L0002.scaled + dis_inventory.scaled    
                           + ns(road_den.scaled,3) + Pop_dens.scaled + Dis_Rd1_2012.scaled + Dist_WB.scaled+ 
                           P_In_2004.scaled + P_OS_2004.scaled + P_Ag_2004.scaled
                           #temporal
                           + daily_hpbl.scaled+vc_D.scaled
                         #met
                         +Temp_D.scaled+Rain_D.scaled+RH_D.scaled+WS_D.scaled+SO2_D.scaled+NO2_D.scaled
                         +(1+aod_047_mean.scaled|day/metreg)) 

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

#check linearity
x<-gam(PM25~s(road_den.scaled)+s(ndvi.scaled)+s(daily_hpbl.scaled)+s(vc_D.scaled)+s(Pop_dens)+s(SO2_D),data=mod1)
plot(x)
summary(x)

#save
    saveRDS(mod1,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_clean_pred/mod1.AQ.2006.PM25.predm1.rds")
    #save results
    saveRDS(res,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/results/results.mod1.AQ.2006.PM25_Daily.rds")

    library(doSNOW)
    library(foreach)
    library(lme4)
    cl<-makeCluster(4) #change the 2 to your number of CPU cores
    registerDoSNOW(cl)
    
    test_final =  foreach(i=1:10 ,.packages=c("lme4","stats"))  %dopar% {
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
      #saveRDS(mod1.cv,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.CV.rds")
      #table updates
      m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=mod1.cv)
      res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$r.squared)
      res[res$type=="PM25", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[1,1])
      res[res$type=="PM25", 'm1cv.Ise'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[1,2])
      res[res$type=="PM25", 'm1cv.slope'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[2,1])
      res[res$type=="PM25", 'm1cv.slopese'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[2,2])
      #RMSPE
      res[res$type=="PM25", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))
      
      #spatial
      spatialall.cv<-mod1.cv %>%
          group_by(stn) %>%
          summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
      m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
      res[res$type=="PM25", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
      res[res$type=="PM25", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))
      #temporal
      tempoall.cv<-left_join(mod1.cv,spatialall.cv)
      tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
      tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
      mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
      res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

      #save results
      saveRDS(res,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/results/results.mod1.AQ.2006.PM25_Daily.rds")


#local stage
head(mod1)

# Explore svm
library(e1071)
mod1.cv$res.m1<- mod1.cv$PM25-mod1.cv$pred.m1.cv

# first explore variables in linear model
reg=lm(res.m1~ Elev_200m.scaled + Road_den_200m.scaled + Road_dis_200m.scaled +     
         Dis_Railways_200m.scaled +  Sp_NDVI0002_200m.scaled +  Su_NDVI0002_200m.scaled + 
         P_Ur_2004_200m.scaled +P_In_2004_200m.scaled ,data=mod1.cv)

summary(reg)


tryx <-svm(res.m1~ Elev_200m.scaled + Road_dis_200m.scaled +     
             Dis_Railways_200m.scaled +  Su_NDVI0002_200m.scaled + 
             P_In_2004_200m.scaled ,type="nu-regression",cross=10,data=mod1.cv)
mod1.cv$predsvmloc <- predict(object=tryx) 

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
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1.bothsvm, na.rm=TRUE)) 
m1.fit.all.cv.loc.s <- lm(barpm ~ barpred, data=spatialall.cv.loc)
res[res$type=="PM25", 'm1cvloc.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv.loc))$r.squared)
res[res$type=="PM25", 'm1cvloc.R2.time'] <- print(rmse(residuals(m1.fit.all.cv.loc.s)))
       
#temporal
tempoall.loc.cv<-left_join(mod1.cv,spatialall.cv.loc)
tempoall.loc.cv$delpm <-tempoall.loc.cv$PM25-tempoall.loc.cv$barpm
tempoall.loc.cv$delpred <-tempoall.loc.cv$pred.m1.both-tempoall.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempoall.loc.cv)
res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.loc.cv))$r.squared)

saveRDS(res,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/results/results.mod1.AQ.2006.PM25_Daily.rds")
saveRDS(res,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/results/results.mod1.AQ.2006.PM25_Daily_clean.rds")

res_clean=res
res_all=readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/results/results.mod1.AQ.2006.PM25_Daily.rds")
res_all_2=rbind(res_all,res_clean)
saveRDS(res_all_2,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/results/results.mod1.AQ.2006.PM25_Daily_all.rds")


### mod 2 (around 2-4 h)

mod2 <- readRDS("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQ.MAIAC.2006.mod2.rds")
mod2=as.data.table(mod2)
# Add daily hPBL 
hpbl=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/HPBL_Israel/newmodel.2004_2015_dailyavg.csv")
hpbl$year=substr(hpbl$date,1,4)
hpbl=filter(hpbl,hpbl$year=="2006")
hpbl=as.data.table(hpbl)
setnames(hpbl,"PBLid","pblid")
setnames(hpbl,"date","day")
hpbl$day=as.Date(hpbl$day)
hpbl[,c("V1","year"):=NULL]

setkey(mod2,pblid,day)
setkey(hpbl,pblid,day)
mod2 <- merge(mod2,hpbl,all.x = T)
mod2=as.data.table(mod2)

#scale variables
mod2 = mod2 %>% as.data.frame 
names(mod2)
mod2[,c("FS_BS"):=NULL]
names=c("Elev","sp_ndvi_L14", "su_ndvi_L14","dis_inventory","Dis_Mroads","road_den","Pop_dens",
        "Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw", "Dist_WB","P_Ag_2006",
        "P_In_Min_2006","P_OS_2006", "P_OS_Ag_2006","P_Ur_2006","Temp_D",       
        "WS_D", "RH_D","Rain_D","NO2_D","SO2_D", "PM25_D_closest", "PM25_D_mean","PM25_IDW", "pbl_02",       
        "pbl_11", "vc_D", "daily_hpbl")
scaled = mod2[, names] %>% dplyr::mutate_each(funs(scale))

colnames(scaled) = paste0(colnames(scaled), ".scaled")
mod2 = cbind(mod2, scaled)
setnames(mod2,"aod_055","aod_055_mean.scaled")
mod2$pred.m2 = predict(object=mod1fit,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)
head(mod2)
summary(mod2$pred.m2)
mod2 <- select(mod2,day,aodid,PM25_D_mean,long_aod,lat_aod,pred.m2,aod_055_mean.scaled)
gc()

mod2<-filter(mod2,!is.na(PM25_D_mean))
mod2<-filter(mod2,!is.na(pred.m2))
mod2$m = as.numeric(format(mod2$day, "%m")) 
mod2$bimon = (mod2$m + 1) %/% 2
gc()
names(mod2)
saveRDS(mod2,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ.2006.PM.predm2.rds")
#keep(mod2,res,rmse,splitdf, sure=TRUE) 


#check spatial patterns by plotting a map in mod2
out <-mod2 %>%
  group_by(aodid) %>%
  summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod_055_mean.scaled)  )
out<-na.omit(out)
write.csv(out,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/temp/tst_1.csv")



#
library(gamm4)

out<-gamm4(pred.m2 ~ PM25_D_mean +s(long_aod,lat_aod),random=~(1+PM25_D_mean|aodid),data=mod2 )

#run lme regression, this *should* include the thin plate spline yet will not run (computational limitations) thus we break it down into 2 components  
mod2.fit.s2 = lme(pred.m2 ~ meanPM25,random = list(aodid= ~1 + meanPM25),control=lmeControl(opt = "optim"), data= mod2 )
#correlate to see everything from mod2 and the mpm works
mod2$pred.m2.s2 = predict(mod2.fit.s2)
mod2$resid = residuals(mod2.fit.s2)
#check R2 
print(summary(lm(pred.m2~pred.m2.s2,data=mod2))$r.squared)


# Parallel
library(foreach)
library(doMC)
registerDoMC(cores=20)

mod2_split = split(mod2, mod2$bimon)

final = foreach(i = 1:6) %dopar% {
  
  fit = gam(resid ~ s(long_aod,lat_aod),  data= mod2_split[[i]] )
  cbind(mod2_split[[i]], pred.m2.s3 = (mod2_split[[i]]$pred.m2.s2 - fit$fitted))
  
}

final = do.call(rbind, final)
# sort 
final= arrange(final,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
mod2.fit.s4 <- lme(pred.m2.s3 ~ meanPM25 ,random = list(aodid= ~1 + meanPM25 ),control=lmeControl(opt = "optim"),data= final)
mod2$pred.m2.s4 = predict(mod2.fit.s4)
#check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 











#mod 3 (5-8 h)
mod3 <- readRDS("/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod3.AQ.2013.rds")
mod3[, m := as.numeric(format(day, "%m")) ]
mod3 <- select(mod3,day,aodid,m,meanPM25,long_aod,lat_aod)
mod3[, bimon := (m + 1) %/% 2]
setkey(mod3,day, aodid)
mod3<-mod3[!is.na(meanPM25)]
#generate m.3 mix model  predictions 
mod3$pred.m3.mix <-  predict(Final_pred_all,mod3)

#create unique grid
ugrid <-mod3 %>%
    group_by(aodid) %>%
    summarise(long_aod = mean(long_aod, na.rm=TRUE),  lat_aod = mean(lat_aod, na.rm=TRUE)) 

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
saveRDS(mod3,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod3.pred.AQ.2003.rds")
keep(data.m3,mod3,res,rmse, sure=TRUE) 
gc()

#calculate stage 3 R2- CV ten folds approach will take 6 weeks...we don't currently do CV for stage 3.

mod1 <-readRDS("/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod1.AQ.2003.PM25.predm1.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
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
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))

#temporal
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)
saveRDS(res, "/media/NAS/Uni/Projects/P059_SWISS_AOD/work/resALL.AQ.2003.PM25.rds")



#create final prediction data set for use in health outcome studies

#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod2.AQ.2013.PM.predm2.rds")
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
saveRDS(mod3best,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/bestpred.AQ.2003.PM25.rds")
mod3best<-filter(mod3best,!is.na(bestpred))

#save for plotting in QGIS
out <- mod3best %>% group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
out<-na.omit(out)
write.csv(out,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/map.bestpred.AQ.2003.PM25.csv")
#save res
saveRDS(res,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/results.AQ.2003.rds")

keep(rmse,splitdf, sure=TRUE) 
gc()
