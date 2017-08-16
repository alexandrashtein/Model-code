###############
#LIBS
###############
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
source("/media/qnap/Data/code/R_functions/CV_splits.r") 

#load data

# AQUA data
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily/mod1.AQ.2003_2015.PM10_Daily_re.rds")
mod1=as.data.table(mod1)
nrow(mod1)
plot(mod1$PM10~ mod1$aod_047, xlim=c(0,4))

neveruse <- c("EHA","MOD","SAF")
mod1 <-mod1[!stn %in% neveruse]

############################# Explore which treshold to use for aod #################
# res=c()
# c=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4)
# ls=list()
# 
# for (i in 1:13)
# {
#   mod1_s <- filter(mod1,aod_047 < c[i])
#   
#   # m1.formula <- as.formula(PM10 ~ aod_047+(1+aod_047|day))  
#   m1.formula <- as.formula(PM10 ~ aod_047)
#   
#   for (j in 1:10)
#   {
#     splits_s <- splitdf(mod1_s)
#     test_s <- splits_s$testset
#     train_s <- splits_s$trainset
#     # out_train_s <- lmer(m1.formula,data =  train_s,weights=normwt )
#     # out_train_s <- lmer(m1.formula,data =  train_s)
#     out_train_s <- lm(m1.formula,data =  train_s)
#     test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL )
#     test_s$iter <- j
#     ls[[j]]=test_s
#   }
#   
#   #BIND 1 dataset
#   mod1.cv = do.call(rbind, ls)
#   
#   #table updates
#   r2=summary(lm(PM10~pred.m1.cv,data=mod1.cv))$r.squared
#   res=c(res,r2)
#   
# }
# 
# results_lmer=data.frame(tr=c,r2=round(res,2))
# results_lm=data.frame(tr=c,r2=round(res,2))

#############

# Raw cleaned correlation
mod1<-filter(mod1,PM10 > 0)
# mod1<-filter(mod1,RelAZ < 90)
mod1<-filter(mod1,UN < 0.04 & UN > 0)
mod1<-filter(mod1,aod_047 < 3.5) 
# mod1<-filter(mod1,aod_047 < 1.2) 

plot(mod1$PM10~ mod1$aod_047, xlim=c(0,4))

# Compure raw correlation
m1.formula <- as.formula(PM10 ~ aod_047)
mod1fit = lm(m1.formula,data=mod1)
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM10~pred.m1,data=mod1))$r.squared)     

## Building cross-validation (cv) dataset

library(foreach)
library(doMC)
registerDoMC(cores=10)

test_final = foreach(i = 1:10) %dopar% {
  
  splits_s <- splitdf(mod1)
  test_s <- splits_s$testset
  train_s <- splits_s$trainset
  # out_train_s <- lmer(m1.formula,data =  train_s,weights=normwt )
  mod1fit = lm(m1.formula,data=mod1)
  out_train_s <- lm(m1.formula,data =  train_s)
  test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL )
  test_s$iter <- i
  test_s
}

#BIND 1 dataset
mod1.cv = do.call(rbind, test_final)

#table updates
print(summary(lm(PM10~pred.m1.cv,data=mod1.cv))$r.squared)

# Use Massimos thresholds to remove PM~ aod with located in oposite quantiles

x<-dplyr::select(mod1,aod_047,stn)
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
mod1<-mod1[aod_047 < quantile(aod_047, c(.50)) & PM10 >  quantile(PM10, c(.90)), exobs := 2]
mod1<-mod1[aod_047 > quantile(aod_047, c(.90)) & PM10 <  quantile(PM10, c(.50)), exobs := 3]

# mod1<-mod1[saod < 10 , exobs := 5]

#take out bad exobs
mod1<-filter(mod1,exobs==0)

#based mixed model
m1.formula <- as.formula(PM10 ~ aod_047
                         #temporal
                         +(1+aod_047|day))  

#stage 1
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM10~pred.m1,data=mod1))$r.squared)
plot(mod1$PM10~mod1$aod_047)

## Filter outliers - this was not included in the final cleaned file, because it did not improve the model 
## Except for 2004

# Cleaning problematic values for 2004
mod1$year=year(mod1$day)
m1.2004=dplyr::filter(mod1,year=="2004")
m1.2004=dplyr::filter(m1.2004,PM10<500)
m1.2004=dplyr::filter(m1.2004,aod_047<1)

mod1=filter(mod1,year!=2004)
mod1=rbind(mod1,m1.2004)

plot(mod1$PM10~ mod1$aod_047, xlim=c(0,4))

## Remove observations with missing data
mod1=mod1[!is.na(mod1$NO2_D.s),]

#take out station with wildly diff PM from surrounding stations (taken from Meytar's code)
mod1=as.data.table(mod1)

## cleaning mod1 - by meytar -These procesures were not implemented due to low observations left after applying them

#take out station with wildly diff PM from surrounding stations

# take out stn with co located PM10/25 with very high ratios
#calculate meanPM per grid per day to each station (excluding first station)
PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(date, "%d/%m/%Y")]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
PM25<-PM25[PM25 > 0.000000000001 & PM25 < 900 ]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")

#calculate meanPM per grid per day to each station (excluding first station)
PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(date, "%d/%m/%Y")]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
PM10<-PM10[PM10 > 0.000000000001 & PM10 <  2000 ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")

#clear non continous stations
setkey(PM10,stn,day)
setkey(PM25,stn,day)
PM.j=merge(PM10,PM25,by=c("stn","day"))
#leave only stations with both PM2.5 and PM 10 measurements
PM.j=na.omit(PM.j)
PM.j$ratio=PM.j[,PM10]/PM.j[,PM25]
PM.j[,badstn := paste(stn,day,sep="-")]
#################BAD STN
mod1[,badstn := paste(stn,day,sep="-")]
PM.j<- PM.j[ratio > 0.95]
####Take out bad stations
mod1 <- mod1[!(mod1$badstn %in% PM.j$badstn), ] 

################# clean BAD STN PM10 and check if improved model?
mod1$m=month(mod1$day)

raWDaf <- ddply(mod1, c("stn","m"), 
                function(x) {
                  mod1 <- lm(PM10 ~ aod_047, data=x)
                  data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                             nsamps = length(summary(mod1)$resid))
                })
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.1]
bad[,badid := paste(stn,m,sep="-")]

#################BAD STN

mod1[,badid := paste(stn,m,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily/mod1.AQ.2003_2015.PM10_Daily_Re_Clean.rds")


