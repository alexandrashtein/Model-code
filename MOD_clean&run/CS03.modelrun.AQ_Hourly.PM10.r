##### install packages

# rm(list = ls())
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

res$type <- c("PM10")

#load data

# AQUA data
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Hourly/mod1.AQ.2003.PM10_Hourly.rds")
mod1_p <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Hourly/previous_run/mod1.AQ.2003.PM10_Hourly.rds")
mod1=as.data.table(mod1)

#Raw full correlation
m1.formula <- as.formula(PM10  ~ aod_047)
mod1fit = lm(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res[res$type=="PM10", 'm1.raw'] <- print(summary(lm(PM10~pred.m1,data=mod1))$r.squared)
plot(mod1$PM10~ mod1$aod_047, ylim=c(0,1500), xlim=c(0,4))

#based mixed model
m1.formula <- as.formula(PM10 ~ aod_047
                         #temporal
                         +(1+aod_047|day))  

#stage 1
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res[res$type=="PM10", 'm1.R2']=print(summary(lm(PM10~pred.m1,data=mod1))$r.squared)

#RMSPE
res[res$type=="PM10", 'm1.rmspe'] <- print(rmse(residuals(mod1fit)))

#spatial
spatialall<-mod1 %>%
  group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM10", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM10", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))

#temporal
#temporal (take out daily PM from yearly mean)
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM10-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM10", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

## add weights
#  mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM10_Daily.rds")
#  
#  mod3 = mod3 %>% as.data.frame
#  scaled = mod3[,c("Elev","Temp_D","daily_hpbl")] %>% dplyr::mutate_each(funs(scale))
#  colnames(scaled) = paste0(colnames(scaled), ".s")
#  mod3 = cbind(mod3, scaled)
#  names(mod3)
#  
# mod3=as.data.table(mod3)
# # ##calculate weights
# mod3[, m := as.numeric(format(day, "%m")) ]
# mod3<-mod3[,obs:=1]
# mod3[is.na(aod_047), obs:= 0]
# mod3<-dplyr::select(mod3,aodid,obs,Elev.s,daily_hpbl.s,m,Temp_D.s,aodid,day)
#  
# ###to save memory
# gc()
# 
# w1 <- glm(obs ~ Elev.s+Temp_D.s+daily_hpbl.s+as.factor(m),family=binomial,data=mod3)
# mod3$prob <- predict(w1 ,type = c("response"))
# mod3$wt <- 1/mod3$prob
# mod3$normwt <- mod3$wt/mean(mod3$wt)
# mod3<-dplyr::select(mod3,aodid,day,normwt)
# names(mod3)
# saveRDS(mod3, "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM10_Daily_weights.rds")
# mod3=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM10_Daily_weights.rds")
# mod3<-dplyr::select(mod3,aodid,day,normwt)
# mod1$aodid=paste(formatC(round(mod1$lon_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")
# setkey(mod3,aodid,day)
# setkey(mod1,aodid,day)
# mod1=merge(mod1,mod3,all.x = T)
# rm(mod3)
# mod1=filter(mod1,!is.na(mod1$normwt))

## Add more generelized climatic zones

# mod1$aodid=paste(formatC(round(mod1$lon_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")
# cz=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Climatic_zones/5_climatic_zones/unique_grid_met5.csv")
# cz=as.data.table(cz)
# setkey(cz,"aodid")
# setkey(mod1,"aodid")
# 
# mod1=merge(mod1,cz,all.x=T)
# setnames(mod1, "metreg.x","metregb")
# setnames(mod1, "metreg.y","metregs")
# mod1=mod1[!is.na(mod1$metregs),]

# Mixed effects model- last model that was used for PM10 prediction

m1.formula <- as.formula(PM10 ~ aod_047
                         # +aod_047*c +aod_047*FS_BS+FS_BS+stn_type+aod_047*stn_type
                         #spatial
                         +Elev.s +ndvi.s+Pop_dens.s +road_den.s
                         # +Dis_Rd2_2012.s
                         # +dis_inventory.s 
                         # +Dist_Railw.s
                         +Dist_WB.s
                         # +P_In.s 
                         # +P_Ur.s
                         # +P_Ag.s
                         +P_OS.s
                         #temporal
                         +daily_hpbl.s+RH_D.s+NO2_D.s+SO2_D.s
                         # +pbl_02.s+vc_D.s+WS_D.s+O3_D+Temp_D.s+Rain_D.s
                         +(1+aod_047|day/metreg_IA))

mod1fit <- lmer(m1.formula,data=mod1, REML = F)
summary(mod1fit)

# A model without the insignificant variables

m2.formula <- as.formula(PM10 ~ aod_047
                         # +aod_047*c +aod_047*FS_BS+FS_BS+stn_type+aod_047*stn_type
                         #spatial
                         +Elev.s +ndvi.s +road_den.s+Pop_dens.s
                         # +Dis_Rd2_2012.s
                         # +dis_inventory.s 
                         # +Dist_Railw.s
                         +Dist_WB.s
                         # +P_In.s 
                         # +P_Ur.s
                         # +P_Ag.s
                         +P_OS.s
                         #temporal
                         +daily_hpbl.s+RH_D.s+NO2_D.s
                         # +SO2_D.s
                         # +pbl_02.s+vc_D.s+WS_D.s+O3_D+Temp_D.s+Rain_D.s
                         +(1+aod_047|day/metreg_IA))

mod2fit <- lmer(m2.formula,data=mod1, REML = F)
summary(mod2fit)
AIC(mod1fit,mod2fit)

# Find the best fixed effects using lmer convinience function
bic_best <- fitLMER.fnc(mod1fit,ran.effects=c("(1+aod_047|day/metreg_IA)",
                                              "((1+aod_047|day/metreg_IA))"),method="BIC")

bic_best <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/bic_best.rds")

mod3fit <- lmer(bic_best,data=mod1, REML = F)
summary(mod3fit)

AIC(mod3fit,mod1fit)
AIC(mod3fit,mod2fit)

#stage 1
# mod1fit <- lmer(m1.formula,data=mod1,weights=normwt)
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM10~pred.m1,data=mod1))$r.squared)
res[res$type=="PM10", 'm1.R2']=print(summary(lm(PM10~pred.m1,data=mod1))$r.squared)

# Check the normality of predictions:
mod1$resid=mod1$PM10-mod1$pred.m1
summary(mod1$resid)
hist(mod1$resid)
qqnorm(mod1$resid)
# Check the Homoscedasticity of predictions:
plot(fitted(mod1fit),residuals(mod1fit))

#RMSPE
res[res$type=="PM10", 'm1.rmspe'] <- print(rmse(residuals(mod1fit)))

#spatial
spatialall<-mod1 %>%
  group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM10", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM10", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))

#temporal
#temporal (take out daily PM from yearly mean)
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM10-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM10", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

# #check linearity for daily mod1
# 
# x<-gam(PM10~ s(Elev.s)+s(ndvi)+s(Dis_Rd1_2012.s)+ s(Pop_dens.s) +s(P_In.s)+s(P_Ur.s) +s(P_Ag.s)+s(P_OS.s)+s(daily_hpbl.s),data=mod1)
# plot(x)
# summary(x)

#save MOD1 
saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2003_2015.PM10_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2012.PM10.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2012.PM10_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_hourly.AQ.2012.PM10_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_hourly.AQ.2012.PM10.predm1.rds")

# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.TR.2012.PM10.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.TR.2012.PM10_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_hourly.TR.2012.PM10_clean.predm1.rds")
# saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_hourly.TR.2012.PM10.predm1.rds")

# Save AQUA results
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2012/results.mod1.AQ.2012.PM10_Daily.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2012/results.mod1.AQ.2012.PM10_Daily_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2012/results.mod1.AQ.2012.PM10_Hourly_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2012/results.mod1.AQ.2012.PM10_Hourly.rds")  
# Save TERRA results
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2012/results.mod1.TR.2012.PM10_Daily.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2012/results.mod1.TR.2012.PM10_Daily_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2012/results.mod1.TR.2012.PM10_Hourly_clean.rds")
# saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2012/results.mod1.TR.2012.PM10_Hourly.rds")

## Building cross-validation (cv) dataset

library(foreach)
library(doMC)
registerDoMC(cores=10)

test_final = foreach(i = 1:10) %dopar% {
  
  splits_s <- splitdf(mod1)
  test_s <- splits_s$testset
  train_s <- splits_s$trainset
  # out_train_s <- lmer(m1.formula,data =  train_s,weights=normwt )
  out_train_s <- lmer(m1.formula,data =  train_s)
  test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL )
  test_s$iter <- i
  test_s
}

#BIND 1 dataset
mod1.cv = do.call(rbind, test_final)

# Save AQUA
saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.AQ.2003_2015.PM10_Daily_clean_CV.rds")

#table updates
m1.fit.all.cv<-lm(PM10~pred.m1.cv,data=mod1.cv)
res[res$type=="PM10", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=mod1.cv))$r.squared)
res[res$type=="PM10", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=mod1.cv))$coef[1,1])
res[res$type=="PM10", 'm1cv.Ise'] <-print(summary(lm(PM10~pred.m1.cv,data=mod1.cv))$coef[1,2])
res[res$type=="PM10", 'm1cv.slope'] <-print(summary(lm(PM10~pred.m1.cv,data=mod1.cv))$coef[2,1])
res[res$type=="PM10", 'm1cv.slopese'] <-print(summary(lm(PM10~pred.m1.cv,data=mod1.cv))$coef[2,2])
#RMSPE
res[res$type=="PM10", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))

# spatial cv.R2 & RMSPE
spatialall.cv<-mod1.cv %>%
  dplyr::group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="PM10", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="PM10", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))

# temporal cv.R2 & RMSPE
tempoall.cv<-left_join(mod1.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$PM10-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="PM10", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

# Save AQUA results
saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2003_2015/results.mod1.AQ.2003_2015.PM10_Daily_clean.rds")

#local stage
head(mod1)

# Explore svm
library(e1071)
mod1.cv$res.m1<- mod1.cv$PM10-mod1.cv$pred.m1.cv

# Scale local variables
names=c("Elev_200m" ,"Pop_dens_200m","Road_dis_200m","sp_ndvi_L_200m","Dis_Railways_200m","P_In_200m","P_Ur_200m")
mod1.cv = mod1.cv %>% as.data.frame 
scaled = mod1.cv[,names] %>% dplyr::mutate_each(funs(scale))
colnames(scaled) = paste0(colnames(scaled), ".s")
mod1.cv= cbind(mod1.cv, scaled)
names(mod1.cv)
mod1.cv=as.data.table(mod1.cv)
mod1.cv<-filter(mod1.cv,!is.na(mod1.cv$Pop_dens_200m.s))

tryx <-svm(res.m1~ Elev_200m.s + Pop_dens_200m.s+Road_dis_200m.s +sp_ndvi_L_200m.s+
             Dis_Railways_200m.s +P_In_200m.s+P_Ur_200m.s ,
           type="nu-regression",cross=10,data=mod1.cv)

mod1.cv$predsvmloc <- predict(object=tryx)

## reg
mod1.cv$pred.m1.bothsvm <- mod1.cv$pred.m1 + mod1.cv$predsvmloc
res[res$type=="PM10", 'm1cvloc.R2'] <- print(summary(lm(PM10~pred.m1.bothsvm,data=mod1.cv))$r.squared)
res[res$type=="PM10", 'm1cvloc.Ise'] <-print(summary(lm(PM10~pred.m1.bothsvm,data=mod1.cv))$coef[1,2])
res[res$type=="PM10", 'm1cvloc.slope'] <-print(summary(lm(PM10~pred.m1.bothsvm,data=mod1.cv))$coef[2,1])
res[res$type=="PM10", 'm1cvloc.slopese'] <-print(summary(lm(PM10~pred.m1.bothsvm,data=mod1.cv))$coef[2,2])

#RMSPE
m1.fit.all.loc<-lm(PM10~pred.m1.bothsvm,data=mod1.cv)
res[res$type=="PM10", 'm1cvloc.rmspe'] <- print(rmse(residuals(m1.fit.all.loc)))

# spatial cv.R2 & RMSPE after local stage
spatialall.cv.loc<-mod1.cv %>%
  dplyr::group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1.bothsvm, na.rm=TRUE)) 
m1.fit.all.cv.loc.s <- lm(barpm ~ barpred, data=spatialall.cv.loc)
res[res$type=="PM10", 'm1cvloc.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv.loc))$r.squared)
res[res$type=="PM10", 'm1cvloc.R2.time'] <- print(rmse(residuals(m1.fit.all.cv.loc.s)))

# temporal cv.R2 & RMSPE after local stage
tempoall.loc.cv<-left_join(mod1.cv,spatialall.cv.loc)
tempoall.loc.cv$delpm <-tempoall.loc.cv$PM10-tempoall.loc.cv$barpm
tempoall.loc.cv$delpred <-tempoall.loc.cv$pred.m1.both-tempoall.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempoall.loc.cv)
res[res$type=="PM10", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.loc.cv))$r.squared)

# Save AQUA mod1.cv

saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_CV/mod1.AQ.2003_2015.PM10_Daily_clean_CV.rds")

### Run mod 2 

mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM10_Daily_Re.rds")
mod2=as.data.table(mod2)

# Data filtering
# mod2<-filter(mod2,RelAZ < 90)
mod2<-filter(mod2,UN < 0.04 & UN > 0)
mod2<-filter(mod2,aod_047 < 1.2)
mod2$c=substr(mod2$day,1,4)

# Predict PM for mod2 from formula craeted in the calibration stage
mod2$pred.m2 = predict(object=mod1fit,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)
summary(mod2$pred.m2)
mod2 <- mod2[,c("day","aodid","lon_aod","lat_aod","pred.m2","aod_047","PM10_IDW","c")]

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
names(mod2)

saveRDS(mod2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily_clean.2003_2015.PM10.predm2.rds")
#keep(mod2,res,rmse,splitdf, sure=TRUE) 

#check spatial patterns by plotting a map in mod2
out <-mod2 %>%
  dplyr::group_by(aodid) %>%
  dplyr::summarise(x=mean(lon_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod_047))
out<-na.omit(out)
out=as.data.table(out)

mod2=as.data.table(mod2)

# Plot the PM2.5 Prediction map
bad_aod <- out[out$predm2>60]
out=out[out$predm2<100,]

# Remove very high observations
bad_aod <- out=out[out$predm2>100,]

ggplot() +  geom_point(data=out, aes(x=x, y=y,color=predm2),alpha=1, size=1)+
  geom_point(data=bad_aod, aes(x=x, y=y),alpha=1, size=1)+
  scale_color_gradient("PM10 prediction",low="lightblue2",high="red")+ggtitle("PM10 prediction for 2003-2015")
write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/pm10_tst_2003_2015_3.csv")

mod2.fit.s2 = lme(pred.m2 ~ PM10_IDW,random = list(aodid= ~1 + PM10_IDW),control=lmeControl(opt = "optim"), data= mod2 )
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
mod2_split = split(mod2, mod2$day) #split the data to daily periods, in each year 

# for each period produce a fit of a gam model (interpolation of residualds) 
library(mgcv)

fits = foreach(i = 1:length(mod2_split)) %dopar% {
  
  gam(resid ~ s(lon_aod,lat_aod),  data= mod2_split[[i]] )
  
}

library(foreach)
library(doMC)
registerDoMC(cores=30)

# use the fits (from the previous stage) for removing the residuals from mod2 prediction

final = foreach(i = 1:length(mod2_split)) %dopar% {
  
  cbind(mod2_split[[i]], pred.m2.s3 = (mod2_split[[i]]$pred.m2.s2 - fits[[i]]$fitted))
  
}

final = do.call(rbind, final)

## sort 

final= arrange(final,day, aodid)

## rerun the lme on the predictions including the spatial spline (smooth)
mod2.fit.s4 <- lme(pred.m2.s3 ~PM10_IDW ,random = list(aodid= ~1 + PM10_IDW),control=lmeControl(opt = "optim"),data= final)
# mod2.fit.s4 <- lme(pred.m2.s3 ~PM10_IDW ,random = list(aodid= ~1 + PM10_IDW+m ),control=lmeControl(opt = "optim"),data= final)
mod2$pred.m2.s4 = predict(mod2.fit.s4)
## check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 

## real mod3 starts here :)
#mod 3 (5-8 h)
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily_Re2.rds")
# mod3$c=substr(mod3$day,1,4)
# mod3 <- dplyr::select(mod3,day,aodid,PM10_IDW,lon_aod,lat_aod,RelAZ,c)
# mod3[, m := as.numeric(format(day, "%m")) ]
# mod3$bimon = (mod3$m + 1) %/% 2
# mod3$bimon = paste(mod3$bimon,mod3$c,sep=".")
# saveRDS(mod3, "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily_Re3.rds")
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

# Parallel process 

library(foreach)
library(doMC)
registerDoMC(cores=30)

mod3_split = split(mod3, mod3$bimon) #split back into bimons to include the gam prediction in final prediction        
uniq_gid_bimon <- ugrid

final_mod3 = foreach(i = 1:length(mod3_split)) %dopar% {
  uniq_gid_bimon$gpred <- predict.gam(fits[[i]],uniq_gid_bimon) # Use the fits from stage 2 (explaining the residuals using thin plate splines)
  setkey(uniq_gid_bimon,aodid)
  mod3_split[[i]]=as.data.table(mod3_split[[i]])
  setkey(mod3_split[[i]],aodid)
  merge(mod3_split[[i]], uniq_gid_bimon[,list(aodid,gpred)], all.x = T)
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

saveRDS(mod3,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM10.pred.AQ_Daily_clean.2003_2015.rds")

# keep(data.m3,mod3,res,rmse, sure=TRUE) 
gc()

#calculate stage 3 R2- CV ten folds approach will take 6 weeks...we don't currently do CV for stage 3.

mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2003_2015.PM10_clean.predm1.rds")
mod1$aodid=paste(formatC(round(mod1$lon_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")

mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]
#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
mod1 <- merge(mod1,mod3[, list(day,aodid,pred.m3,pred.m3.mix)], all.x = T)
m3.fit.all<- summary(lm(PM10~pred.m3,data=mod1))
# m3.fit.all<- summary(lm(PM10~pred.m3.mix,data=mod1))
# res[res$type=="PM10", 'm3.R2'] <- print(summary(lm(PM10~pred.m3.mix,data=mod1))$r.squared)    
res[res$type=="PM10", 'm3.I'] <-print(summary(lm(PM10~pred.m3,data=mod1))$coef[1,1])
res[res$type=="PM10", 'm3.Ise'] <-print(summary(lm(PM10~pred.m3,data=mod1))$coef[1,2])
res[res$type=="PM10", 'm3.slope'] <-print(summary(lm(PM10~pred.m3,data=mod1))$coef[2,1])
# res[res$type=="PM10", 'm3.slope'] <-print(summary(lm(PM10~pred.m3.mix,data=mod1))$coef[2,1])
res[res$type=="PM10", 'm3.slopese'] <-print(summary(lm(PM10~pred.m3,data=mod1))$coef[2,2])
#RMSPE
res[res$type=="PM10", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))

#spatial
###to check
spatialall<-mod1 %>%
  dplyr::group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM10", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM10", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))

#temporal
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM10-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM10", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2003_2015/results.mod1.AQ.2003_2015.PM10_Daily_clean.rds")

#create final prediction data set for use in health outcome studies

#import mod2
mod2<- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily_clean.2003_2015.PM10.predm2.rds")
mod2=as.data.table(mod2)
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, lon_aod, lat_aod, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1[,list(aodid,day,pred.m1,PM10)], all.x = T,allow.cartesian = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
summary(mod3best$bestpred)
mod3best[bestpred < 0 , bestpred  := 0.5]
mod3best<-dplyr::select(mod3best,day,aodid,lon_aod,lat_aod,bestpred)
#save
saveRDS(mod3best,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred.AQ_Daily_clean.2003_2015.PM10.rds")
saveRDS(mod3best,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred.no_GAM.AQ_Daily_clean.2003_2015.PM10.rds")

#save res
saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2003_2015/results.mod1.AQ.2003_2015.PM10_Daily_clean.rds")
write.csv(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/summary_table/2003_2015/results.mod1.AQ.2003_2015.PM10_Daily_clean.csv")


#### Visually explore the prediction

mod3best<-filter(mod3best,!is.na(bestpred))
#save for plotting in QGIS
out <- mod3best %>% group_by(aodid) %>%
  summarise(lon=mean(lon_aod, na.rm=TRUE), lat =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
out<-na.omit(out)
write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/map.bestpred.AQ_Daily_clean.2003_2015.PM10.csv")
write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/map.bestpred.no_GAM.AQ_Daily_clean.2003_2015.PM10.csv")

# Plot the PM2.5 Prediction map
# out=out[out$bestpred<60,]
tiff(width = 800, height = 960,res=150,filename="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/Maps/mod3bet_PM10_pred_2003_2015.tiff")
ggplot() +  geom_point(data=out, aes(x=lon, y=lat,color=bestpred),alpha=1, size=1)+
  scale_color_gradient("PM10 estimation",low="lightblue",high="red")+ ggtitle("Average PM10 estimation for 2003-2015")
dev.off()

############################## Explore spatial patterns per year

mod3best$year=year(mod3best$day)

years=unique(mod3best$year)

for (i in 1:length(years))
{
  mod3best_s=filter(mod3best, year==years[i]) # filter the data per spesific year
  # average the data annualy
  out <- mod3best_s %>% group_by(aodid) %>%
  summarise(lon=mean(lon_aod, na.rm=TRUE), lat =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
  out<-na.omit(out)
  
  # plot the data
  mypath <- file.path("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm10_prediction_graphs",paste("pred_",years[i] , ".jpeg", sep = ""))
  g = ggplot() +  geom_point(data=out, aes(x=lon, y=lat,color=bestpred),alpha=1, size=1)+
  scale_color_gradient("PM10 prediction",low="lightgreen",high="red")+ ggtitle(paste("PM10 prediction for", years[i], sep = " "))
  ggsave(filename = mypath, plot = g, width = 5, height = 6)
}


# Plotting time series of certain stations

mod3best=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred.AQ_Daily_clean.2003_2015.PM10.rds")
setkey(mod1,day,aodid)
setkey(mod3,day,aodid)
mod1 <- merge(mod1,mod3best[, list(day,aodid,bestpred)], all.x = T)
stn=unique(mod1$stn)
print(summary(lm(PM10~bestpred,data=mod1))$r.squared)

for (i in 1:length(stn))
{
  mod1_f=filter(mod1, stn== stn[i])
  mypath <- file.path("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/","Graphs_by_station",paste("PM2.5_", stn[i], ".png", sep = ""))
  png(file=mypath)
  plot(mod1_f$day,mod1_f$PM10,type= "p",ylim=c(0,100),main=paste(stn[i]),cex=.5, ylab="PM2.5 concentration",xlab="date")
  points(mod1_f$day,mod1_f$bestpred,type= "p",ylim=c(0,100),main=paste(stn[i]),cex=.5,col="red")
  legend((max(mod1_f$day)-2*365),90,c("Obs","Pred"),pch=c(1,1),col=c("black","red"))
  dev.off()
}

library(ggplot2)

ggplot(mod1, aes(x = bestpred, y = PM10)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

fit <- lm(PM10 ~ bestpred, data = mod1)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 2),
                       " P = ",signif(summary(fit)$coef[2,4], 4)))
}

ggplotRegression(fit1)

# Visualise missing AOD per year

mod3_full=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily.rds")
mod3_full$year=year(mod3_full$day)

## For each aodid point calculate the precentage of missingness from all period (13 years)
miss <- mod3_s %>% group_by(aodid) %>%
  summarise(lon=mean(lon_aod, na.rm=TRUE), lat =mean(lat_aod, na.rm=TRUE), missing=sum(is.na(aod_047_f)),non_miss=sum(!is.na(aod_047_f)))
miss$per_miss=round(miss$missing/(365*13), 2)
## Save the points with overall missing values higher than 0.9
bad_aod=miss[miss$per_miss>0.9,]
## Exclude these points (bad_aod) from the PM10 best prediction dataset:

mod3_best_f=mod3best[!mod3best$aodid %in% bad_aod$aodid,]

# Save the results
saveRDS(mod3_best_f,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred_2.AQ_Daily_clean.2003_2015.PM10.rds")

#save for plotting in QGIS
out <- mod3_best_f %>% group_by(aodid) %>%
  summarise(lon=mean(lon_aod, na.rm=TRUE), lat =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
out<-na.omit(out)
write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/map.bestpred.AQ_Daily_clean_filtered.2003_2015.PM10.csv")

### Plot the data
mod3best=mod3_best_f
mod3best$year=year(mod3best$day)

years=unique(mod3best$year)

for (i in 1:length(years))
{
  mod3best_s=filter(mod3best, year==years[i]) # filter the data per spesific year
  # average the data annualy
  out <- mod3best_s %>% group_by(aodid) %>%
    summarise(lon=mean(lon_aod, na.rm=TRUE), lat =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
  out<-na.omit(out)
  
  # plot the data
  mypath <- file.path("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/pm10_prediction_graphs",paste("pred_",years[i] , ".jpeg", sep = ""))
  g = ggplot() +  geom_point(data=out, aes(x=lon, y=lat,color=bestpred),alpha=1, size=1)+
    scale_color_gradient("PM10 prediction",low="lightgreen",high="red")+ ggtitle(paste("PM10 prediction for", years[i], sep = " "))
  ggsave(filename = mypath, plot = g, width = 5, height = 6)
}

## How many missing data after filtering aod data? assign NA to all filtered data.
mod3_full$aod_047_f<- ifelse(mod3_full$aod_047<1.2, mod3_full$aod_047, NA)
mod3_full$aod_047_f<- ifelse(mod3_full$UN < 0.04 & mod3_full$UN > 0, mod3_full$aod_047, NA)

years=c(2003:2015)

for (i in 1:length(years))
{
mod3_s=filter(mod3_full, year==years[i])

miss <- mod3_s %>% group_by(aodid) %>%
  summarise(lon=mean(lon_aod, na.rm=TRUE), lat =mean(lat_aod, na.rm=TRUE), missing=sum(is.na(aod_047_f)),non_miss=sum(!is.na(aod_047_f)))
miss$per_miss=round(miss$missing/365, 2)

mypath <- file.path("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/missing_aod_percent/after_filtering",paste("missing AOD for",years[i] , ".jpeg", sep = ""))
g=ggplot() +  geom_point(data=miss, aes(x=lon, y=lat,color=per_miss),alpha=1, size=1)+
  scale_color_gradient("MISSING AOD RATIO",low="lightblue",high="black")+ ggtitle(paste("missing AOD for", years[i], sep = " "))
  ggsave(filename = mypath, plot = g, width = 5, height = 6)
}


## Create a detailed prediction map
library(sp)
library(rgdal)
library(rgeos)
library(maptools)

# Load the data
mod3best=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred.AQ_Daily_clean.2003_2015.PM10.rds")

out <- mod3best %>% dplyr::group_by(aodid) %>%
  summarise(lon=mean(lon_aod, na.rm=TRUE), lat =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
out<-na.omit(out)
out=as.data.frame(out)

# convert to spatial object
coordinates(out) <- ~lon+lat
proj4string(out) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Save prediction shapefile
rgdal::writeOGR(out,dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp",layer="PM10_average_prediction_2003_2015",driver = "ESRI Shapefile")

# Load voronoi polygons
pol=rgdal::readOGR("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/","PM10_prediction_2003_2015_voronoi_qgis")

# Load addtional layers
border=readOGR("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/","Project_border")
pol=spTransform(pol,CRS(proj4string(border)))

# Load main cities shapfile
cities= readOGR("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Main_cities/","Main_cities_lat_long")
  
# clip the thiessen polygons with the border
library(raster)
pol_c <-raster::intersect(pol, border) 

library(ggplot2)
#convert the spatial object to table
border_f = fortify(border)
pol_f = fortify(pol_c, region = "aodid")
pol_f$aodid= pol_f$id
pol_f = plyr::join(pol_f, pol_c@data, "aodid")
cities_f=fortify(cities,group=ID)

  
ggplot() +
  geom_polygon(data = pol_f, aes(x = long, y = lat, group = group, fill = bestpred))+
  # geom_polygon(data = border_f, aes(x = long, y = lat), color="black", fill=NA) +
  # geom_point(data=cities_f, aes(x=x, y=y),alpha=1, size=1)+
  # +scale_fill_gradient(low = "blue", high = "red") 
  scale_fill_gradient(low='lightblue', high='red')+
  scale_color_gradient("PM10 concentration",low="white",high="red")+ ggtitle("PM10 average concentration for 2003-2015")
 