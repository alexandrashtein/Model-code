
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

res=c()
obs=c()
res_raw=c()
res_raw2=c()

#load data

mod1_all <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003_2015.PM25_Daily_re.rds")
mod1_all$year=substr(mod1_all$year,1,4)
# creating a filter field of the forward scattering (FS=1) and the backward scaterring (BS=0 or else)
mod1_all$FS_BS=1
# # First option for data devision be Azimuth angle:
mod1_all <- mod1_all[RelAZ> 90, FS_BS := 0]

y=c(2003:2015)

# AQUA data
for (i in y)
  
{
mod1=filter(mod1_all, year==i)
mod1=as.data.table(mod1)

### ADD ventilation coefficient
mod1$vc_D=c(mod1$WS_D/(mod1$daily_hpbl*1000))

mod1$Ur_Ru_1 <- NA
mod1$Ur_Ru_1<- mod1$P_Ur
mod1$Ur_Ru_1[mod1$Ur_Ru_1<=20] <-1 
mod1$Ur_Ru_1[20<= mod1$Ur_Ru_1 & mod1$Ur_Ru_1<=40] <-2
mod1$Ur_Ru_1[40<= mod1$Ur_Ru_1 & mod1$Ur_Ru_1<=60] <-3
mod1$Ur_Ru_1[60<= mod1$Ur_Ru_1 & mod1$Ur_Ru_1<=80] <-4
mod1$Ur_Ru_1[mod1$Ur_Ru_1>=80] <-5

#scale variables

names=c("Elev","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","Temp_D","P_In","P_OS","P_Ur","P_Ag","Dist_WB",
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
res1<- summary(lm(PM25~pred.m1,data=mod1))$r.squared
res_raw=c(res_raw,res1)

# Raw cleaned correlation
mod1=as.data.table(mod1)
mod1<-filter(mod1,RelAZ < 90)
mod1<-filter(mod1,UN < 0.04 & UN > 0)
mod1<-filter(mod1,aod_047_mean < 3)

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
mod1<-mod1[saod < 20 , exobs := 5]

#take out bad exobs
mod1<-filter(mod1,exobs==0)

mod1fit = lm(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
res2<- summary(lm(PM25~pred.m1,data=mod1))$r.squared
res_raw2=c(res_raw2,res2)

mod1=as.data.table(mod1)
#based mixed model
m1.formula <- as.formula(PM25 ~ aod_047_mean
                         #temporal
                         +(1+aod_047_mean|day))  

#stage 1
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)
# 
# #RMSPE
# res[res$type=="PM25", 'm1.rmspe'] <- print(rmse(residuals(mod1fit)))

#spatial
spatialall<-mod1 %>%
  group_by(stn) %>%
  dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
# res[res$type=="PM25", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
# res[res$type=="PM25", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))

#temporal
#temporal (take out daily PM from yearly mean)
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
# res[res$type=="PM25", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

mod1=mod1[!is.na(mod1$NO2_D.scaled),]

m1.formula <- as.formula(PM25 ~ aod_047_mean
                         # +aod_047_mean*c
                         # #spatial
                         # +aod_047_mean*FS_BS
                         # +FS_BS
                         # +stn_type
                         # +aod_047_mean*stn_type
                         +Elev.scaled+ndvi 
                         # +dis_inventory.scaled 
                         # +road_den.scaled
                         +Dist_WB.scaled
                         # +Dist_Railw.scaled
                         +Dis_Rd1_2012.scaled
                          # +road_den.scaled
                          +Dist_WB.scaled
                         + Pop_dens.scaled
                         # +Dist_Railw.scaled
                          +P_In.scaled+P_Ur.scaled+P_Ag.scaled+P_OS.scaled
                         #temporal
                         +daily_hpbl.scaled
                         +pbl_02.scaled
                         # +vc_D.scaled
                         #met
                         # + Temp_D.scaled
                         +Rain_D.scaled
                         +RH_D.scaled
                         # +WS_D.scaled
                         +SO2_D.scaled
                         +NO2_D.scaled
                         +(1+aod_047_mean|day/metreg))

#stage 1
# mod1fit <- lmer(m1.formula,data=mod1,weights=normwt)
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
R2=summary(lm(PM25~pred.m1,data=mod1))$r.squared
res=c(res,R2)
n=nrow(mod1)
obs=c(obs,n)
}

all_years_FS_BS=data.frame(R2_all=res,y,obs_all=obs)
all_years_FS=data.frame(R2=res,obs)

results=cbind(all_years_FS,all_years_FS_BS)

for (i in 1:length(names))
  {
  mod1_f=filter(mod1,stn==names[i]) 
  mod1_s=rbind(mod1_s,mod1_f)
}

mod1$month <- as.numeric(substr(mod1$day,6,7))
#1-winter, 2-spring,3-summer,4-autum
mod1$season<-car::recode(mod1$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")

raw_r2=c()
m1.formula <- as.formula(PM25  ~ aod_047_mean)

for (i in 1:4)
  {
  mod1_s=filter(mod1,season==i)
  mod1fit = lm(m1.formula,data=mod1_s)
  mod1_s$pred.m1 <- predict(mod1fit)
  r2=print(summary(lm(PM25~pred.m1,data=mod1_s))$r.squared)
  raw_r2=c(raw_r2,r2)
  }