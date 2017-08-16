##### install packages
# install.packages("foreach")
# install.packages(doSNOW)
# install.packages("e1071")
# install.packages("MASS")
# install.packages("MuMIn")
# install.packages("gamm4")

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

#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("PM25")

#load data

# AQUA data
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_re.rds")

#scale variables

names=c("Elev","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","Temp_D","P_In","P_OS","P_Ur","P_Ag",
        "WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11","vc_D","Elev_200m","Pop_dens_200m",
        "Road_den_200m" ,"Road_dis_200m","Dis_Railways_200m","P_In_200m","P_Ur_200m",
        "dist_inven_200m","ndvi","daily_hpbl","su_ndvi_L_200m","sp_ndvi_L_200m")

mean(names %in% names(mod1))
a=names %in% names(mod1)
b=names
b[which(a==FALSE)]

mod1 = mod1 %>% as.data.frame 
scaled = mod1[,names] %>% dplyr::mutate_each(funs(scale))
colnames(scaled) = paste0(colnames(scaled), ".scaled")
mod1 = cbind(mod1, scaled)
names(mod1)

#Raw full correlation
m1.formula <- as.formula(PM25  ~ aod_047_mean)
mod1fit = lm(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res[res$type=="PM25", 'm1.raw'] <- print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

# Raw cleaned correlation
mod1=as.data.table(mod1)
mod1<-filter(mod1,RelAZ < 90)
mod1<-filter(mod1,UN < 0.04 & UN > 0)
mod1<-filter(mod1,aod_047_mean < 2.5)

# check massimo clean

#massimos thresholds
x<-dplyr::select(mod1,aod_047_mean,stn)
x$c<-1
x <- x %>%
  dplyr::group_by (stn) %>%
  dplyr::summarise(saod=sum(c))

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
mod1<-mod1[saod < 20 , exobs := 5]

#take out bad exobs
mod1<-filter(mod1,exobs==0)

mod1fit = lm(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res[res$type=="PM25", 'm1.raw'] <- print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)

mod1=as.data.table(mod1)
#based mixed model
m1.formula <- as.formula(PM25 ~ aod_047_mean
#temporal
+(1+aod_047_mean|day))  

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

mod1=mod1[!is.na(mod1$NO2_D.scaled),]


m1.formula <- as.formula(PM25 ~ aod_047_mean
                         # #spatial
                         # +aod_047_mean*FS_BS
                         # +FS_BS
                         +Elev.scaled+ndvi 
                         # +dis_inventory.scaled 
                           +Dis_Rd1_2012.scaled
                         # +road_den.scaled
                         # +Dist_WB.scaled
                         + Pop_dens.scaled
                         # +Dist_Railw.scaled
                         +P_In.scaled+P_Ur.scaled
                         +P_Ag.scaled+P_OS.scaled
                         #temporal
                         +daily_hpbl.scaled
                         +pbl_02.scaled
                         # +vc_D.scaled
                         #met
                         + Temp_D.scaled+Rain_D.scaled+RH_D.scaled+WS_D.scaled+SO2_D.scaled+NO2_D.scaled
                         +(1+aod_047_mean|day/metreg))

#stage 1
# mod1fit <- lmer(m1.formula,data=mod1,weights=normwt)
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

#save MOD1 
saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/mod1_Daily.AQ.2003_2015.PM25_clean.predm1.rds")

## Building cross-validation (cv) dataset

 # Running the paraller process in Linux
 
    # The doMC library is not available in Windows
    library(foreach)
    library(doMC)
    registerDoMC(cores=4)

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

      #save
# AQUA
saveRDS(mod1.cv,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/mod1.AQ.2003_2015.PM25_Daily_clean_CV.rds")

#table updates
      m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=mod1.cv)
      res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$r.squared)
      res[res$type=="PM25", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[1,1])
      res[res$type=="PM25", 'm1cv.Ise'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[1,2])
      res[res$type=="PM25", 'm1cv.slope'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[2,1])
      res[res$type=="PM25", 'm1cv.slopese'] <-print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$coef[2,2])
      #RMSPE
      res[res$type=="PM25", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))
      
      library(dplyr)
      
      #spatial
      spatialall.cv<-mod1.cv %>%
        dplyr::group_by(stn) %>%
        dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
      m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
      res[res$type=="PM25", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
      res[res$type=="PM25", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))
      
      #temporal
      tempoall.cv<-left_join(mod1.cv,spatialall.cv)
      tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
      tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
      mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
      res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

# Save AQUA results
saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/results.mod1.AQ.2003_2015.PM25_Daily_clean.rds")
      
#local stage
head(mod1)

# Explore svm
library(e1071)
mod1.cv$res.m1<- mod1.cv$PM25-mod1.cv$pred.m1.cv
mod1.cv<-filter(mod1.cv,!is.na(mod1.cv$Pop_dens_200m.scaled))

tryx <-svm(res.m1~ Elev_200m.scaled + Pop_dens_200m.scaled+Road_dis_200m.scaled +sp_ndvi_L_200m.scaled+
             Dis_Railways_200m.scaled +P_In_200m.scaled+P_Ur_200m.scaled ,
           type="nu-regression",cross=10,data=mod1.cv)

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
    dplyr::group_by(stn) %>%
    dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1.bothsvm, na.rm=TRUE)) 
m1.fit.all.cv.loc.s <- lm(barpm ~ barpred, data=spatialall.cv.loc)
res[res$type=="PM25", 'm1cvloc.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv.loc))$r.squared)
res[res$type=="PM25", 'm1cvloc.R2.time'] <- print(rmse(residuals(m1.fit.all.cv.loc.s)))
       
#temporal
tempoall.loc.cv<-left_join(mod1.cv,spatialall.cv.loc)
tempoall.loc.cv$delpm <-tempoall.loc.cv$PM25-tempoall.loc.cv$barpm
tempoall.loc.cv$delpred <-tempoall.loc.cv$pred.m1.both-tempoall.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempoall.loc.cv)
res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.loc.cv))$r.squared)

# Save AQUA results 
saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/results.mod1.AQ.2003_2015.PM25_Daily_clean.rds")

### mod 2 (around 2-4 h)

mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")
mod2=as.data.table(mod2)

#scale variables
mod2 = mod2 %>% as.data.frame 
names(mod2)
names=c("Elev","sp_ndvi_L", "su_ndvi_L","dis_inventory","Dis_Mroads","road_den","Pop_dens",
        "Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw", "ndvi",
        "P_Ag","P_In","P_OS","P_Ur",
        "Temp_D","WS_D", "RH_D","Rain_D","NO2_D","SO2_D", "PM25_D_closest", "PM25_D_mean","PM25_IDW",
        "pbl_02","daily_hpbl")

all(names %in% names(mod2))
mean(names %in% names(mod2))
a=names %in% names(mod2)
b=names
b[which(a==FALSE)]

scaled = mod2[, names] %>% dplyr::mutate_each(funs(scale))
colnames(scaled) = paste0(colnames(scaled), ".scaled")
mod2 = cbind(mod2, scaled)
setnames(mod2,"aod_047","aod_047_mean")
mod2=as.data.table(mod2)
names(mod2)

# Data filtering
mod2<-filter(mod2,RelAZ < 90)
mod2<-filter(mod2,UN < 0.04 & UN > 0)
mod2<-filter(mod2,aod_047_mean < 2)
mod2$c=substr(mod2$day,1,4)

# Predict PM for mod2 from formula craeted in the calibration stage
mod2$pred.m2 = predict(object=mod1fit,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)
head(mod2)
summary(mod2$pred.m2)
mod2 <- select(mod2,day,aodid,PM25_D_mean,long_aod,lat_aod,pred.m2,aod_047_mean,PM25_D_closest,PM25_IDW,c)
gc()

mod2<-filter(mod2,!is.na(pred.m2))
mod2$m = as.numeric(format(mod2$day, "%m")) 
mod2$bimon = (mod2$m + 1) %/% 2
mod2$bimon=paste(mod2$bimon,mod2$c,sep=".")
gc()
names(mod2)
saveRDS(mod2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/mod2.AQ_Daily_clean.2012.PM.predm2.rds")
#keep(mod2,res,rmse,splitdf, sure=TRUE) 

#check spatial patterns by plotting a map in mod2
out <-mod2 %>%
  dplyr::group_by(aodid) %>%
  dplyr::summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod_047_mean))
out<-na.omit(out)
write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/tst_2003_2015.csv")

#run lme regression, this *should* include the thin plate spline yet will not run (computational limitations) thus we break it down into 2 components  
mod2.fit.s2 = lme(pred.m2 ~ PM25_IDW,random = list(aodid= ~1 + PM25_IDW),control=lmeControl(opt = "optim"), data= mod2 )
#correlate to see everything from mod2 and the mpm works
mod2$pred.m2.s2 = predict(mod2.fit.s2)
mod2$resid = residuals(mod2.fit.s2)
#check R2 
print(summary(lm(pred.m2~pred.m2.s2,data=mod2))$r.squared)

# Parallel (Running Parallel process in Linux (the doMC library is not available in Windows))
mod2= arrange(mod2,day, aodid)

library(foreach)
library(doMC)
registerDoMC(cores=30)


mod2_split = split(mod2, mod2$bimon)

#v2

fits = foreach(i = 1:length(mod2_split)) %dopar% {
  
  gam(resid ~ s(long_aod,lat_aod),  data= mod2_split[[i]] )
  
}

gc()

final = foreach(i = 1:length(mod2_split)) %dopar% {
  
  cbind(mod2_split[[i]], pred.m2.s3 = (mod2_split[[i]]$pred.m2.s2 - fits[[i]]$fitted))
  
}

final = do.call(rbind, final)
## sort 
final= arrange(final,day, aodid)
 
## rerun the lme on the predictions including the spatial spline (smooth)
mod2.fit.s4 <- lme(pred.m2.s3 ~PM25_IDW ,random = list(aodid= ~1 + PM25_IDW ),control=lmeControl(opt = "optim"),data= final)
mod2$pred.m2.s4 = predict(mod2.fit.s4)
## check correlations
print(summary(lm(pred.m2 ~ pred.m2.s4,data=mod2))$r.squared) 

## real mod3 starts here :)
#mod 3 (5-8 h)
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3/AQUA/mod3.AQ.2003_2015.PM25_Daily.rds")
mod3[, m := as.numeric(format(day, "%m")) ]
mod3 <- dplyr::select(mod3,day,aodid,m,PM25_D_mean,PM25_IDW,long_aod,lat_aod,RelAZ,c)
mod3$bimon = (mod3$m + 1) %/% 2
mod3$bimon = paste(mod3$bimon,mod3$c,sep=".")
mod3= arrange(mod2,day, aodid)

#generate m.3 mix model  predictions 
mod3$pred.m3.mix <-  predict(mod2.fit.s4,mod3)

#create unique grid
ugrid <-mod3 %>%
    dplyr::group_by(aodid) %>%
    dplyr::summarise(long_aod = mean(long_aod, na.rm=TRUE),  lat_aod = mean(lat_aod, na.rm=TRUE)) 

ugrid<-as.data.table(ugrid)

# Running Parallel process in Linux (the doMC library is not available in Windows)

library(foreach)
library(doMC)
registerDoMC(cores=50)

mod3_split = split(mod3, mod3$bimon) #split back into bimons to include the gam prediction in final prediction        
uniq_gid_bimon <- ugrid


final_mod3 = foreach(i = 1:length(mod2_split)) %dopar% {
  uniq_gid_bimon$gpred <- predict.gam(fits[[i]],uniq_gid_bimon)
  setkey(uniq_gid_bimon,aodid)
  setkey(mod3_split[[i]],aodid)
  merge(mod3_split[[i]], uniq_gid_bimon[,list(aodid,gpred)], all.x = T)
}

final_mod3 = do.call(rbind, final_mod3)
## sort 
final_mod3= arrange(final_mod3,day, aodid)

mod3=final_mod3

# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
summary(mod3$pred.m3)

saveRDS(mod3,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/mod3.pred.AQ_Daily_clean.2003_2015.rds")
  
keep(data.m3,mod3,res,rmse, sure=TRUE) 
gc()

#calculate stage 3 R2- CV ten folds approach will take 6 weeks...we don't currently do CV for stage 3.
 mod1 <-readRDS("/media/qnap//Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2003_2015.PM25_clean.predm1.rds")
 
##fix
mod1$aodid=paste(formatC(round(mod1$long_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")
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
    dplyr::group_by(stn) %>%
    dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))

#temporal
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/results.mod1.AQ.2003_2015.PM25_Daily_clean.rds")

#create final prediction data set for use in health outcome studies

#import mod2
mod2<- readRDS( "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily_clean.2012.PM.predm2.rds")
mod2=as.data.table(mod2)
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
saveRDS(mod3best,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/bestpred.AQ_Daily_clean.2003_2015.PM25.rds")
mod3best<-filter(mod3best,!is.na(bestpred))

#save for plotting in QGIS
out <- mod3best %>% group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
out<-na.omit(out)
 write.csv(out,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/map.bestpred.AQ_Daily_clean.2003_2015.PM25.csv")

#save res
saveRDS(res,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/check/results.mod1.AQ.2003_2015.PM25_Daily_clean.rds")
 
keep(rmse,splitdf, sure=TRUE) 
gc()



