#install libraries 
install.packages("lme4")
install.packages("reshape")
install.packages("foreign") 
install.packages("ggplot2")
install.packages("plyr")
install.packages("data.table")
install.packages("Hmisc")
install.packages("mgcv")
install.packages("gdata")
install.packages("car")
install.packages("dplyr")
install.packages("ggmap")
install.packages("broom")
install.packages("splines")
install.packages("DataCombine")
install.packages("readr")
install.packages("bit64")
install.packages("devtools")
install_github("allanjust/aodlur")


#load libraries 
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
library(readr)
library(bit64)
library(devtools)
install_github("allanjust/aodlur")
library("aodlur")


#load clipped/LU grid 
# lu<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid.csv")
lu<-fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid.csv")

#load aod data
aqua<-readRDS("/media/qnap/Projects/P019.IL.Israel.MAIAC.PM/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua<-readRDS("N:/Projects/P019.IL.Israel.MAIAC.PM/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% lu$aodid, ] 
aqua<- aqua[yr == "2005"]

# #fix a bug, convert first from dplyr format to data.frame
# aqua.2004<-as.data.frame(aqua.2004)
# #convert to data.table
# aqua.2004<-as.data.table(aqua.2004)


#create single aod point per aodid per day 
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),yr=mean(yr) )


#create full LU-aod TS
days<-seq.Date(from = as.Date("2004-01-01"), to = as.Date("2004-12-31"), 1)
#create date range
days2004 <- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
days2004$aodid<-as.character(days2004$aodid)
#merge
setkey(aqua,aodid,day)
setkey(days2004 ,aodid,day)
db2004 <- merge(days2004,aqua, all.x = T)
#add land use data
setkey(db2004,aodid)
setkey(lu,aodid)
db2004 <- merge(db2004, lu, all.x = T)
head(db2004)
gc()
#get rid of duplicate names to cut down on DB size
db2004<-dplyr::select(db2004,-lat_aod.y, -long_aod.y)
gc()

#add season
db2004$month <- as.numeric(format(db2004$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
db2004$season<-recode(db2004$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
db2004$seasonSW<-recode(db2004$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#add ndvi
##join first ndviid to db2004 then join by aodid and month

########### import datasets
#import NDVI
ndvi<-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/ndvi_2000_2015.rds")
# add ndviid to db2004
#join actual NDVI to aod
setkey(ndvi, ndviid, m )
setkey(db2004,  ndviid, m)
db2004 <- merge(db2004, ndvi, all.x = T)

#add Pbl
pbl<-fread("N:/Data/Israel/HPBL_Israel/ecmwf_hpbl_israel_2003_2016_new.csv")
#create single pbl point per day
pbld <-pbl %>%
    group_by(pblid,day) %>%
    summarise(lon_pbl=mean(lon),lat_pbl=mean(lat),pbl=mean(hpbl) )

#join actual NDVI to aod
setkey(pbl, pblid, day )
setkey(db2004,  pblid, day)
db2004 <- merge(db2004, ndvi, all.x = T)



################ add TEMPORAL Variables
#add Temp
#change to latest path israelV2
Temp <- fread("/media/qnap/Projects/P019.IL.Israel.MAIAC.PM/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']
Temp <- Temp[c == 2004]


                      
jointo.pt <- makepointsmatrix(datatable = db2004, 
                             xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinfrom.pt <- makepointsmatrix(datatable = Temp, 
                               xvar = "X", yvar = "Y", idvar = "stn") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                       jointo = db2004, joinfrom = Temp, 
                       jointovarname = "aodid", joinfromvarname = "stn", 
                       joinprefix = "nearest", valuefield = "Temp", 
                       knearest = 15, maxdistance = 60000, 
                       nearestmean = FALSE, verbose = T)

setkey(db2004,aodid,day)
setkey(joinout,aodid,day)
db2004 <- merge(db2004, joinout[,list(day,Temp,aodid)], all.x = T)
head(db2004)
summary(db2004$Temp)



#MEAN PM25
PM25 <- fread("/media/qnap/Projects/P019.IL.Israel.MAIAC.PM/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2004<- PM25[c==2004]

for(i in unique(db2004$day)) {

x<-pmall2004[pmall2004$day==i, ]
y= db2004[db2004$day==i, ]
##########
# calculate IDW
library(gstat)
#defaults to idw (gstat)
library(sp)
coordinates(x) = ~ x_stn_ITM + y_stn_ITM
coordinates(y) = ~ x_aod_ITM.y + y_aod_ITM.y
#location statment uneeded since we defined coordinates
inter = gstat(formula = PM25 ~ 1,  data =x)
z<-predict(object = inter, newdata = y)
# head(z)
db2004$pred[db2004$day==i] = z$var1.pred
# spplot(z, "var1.pred", at = 0:100)
}



pm.m <- makepointsmatrix(pmall2004, "x_stn_ITM", "y_stn_ITM", "stn")

aod.m <- makepointsmatrix(db2004[db2004[,unique(aodid)], list(x_aod_ITM.y, y_aod_ITM.y, aodid), mult = "first"], "x_aod_ITM.y", "y_aod_ITM.y", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            db2004, pmall2004 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = NA, nearestmean = T)


#create single aod point per aodid per day 
x <-closestaodse %>%
    group_by(aodid,day) %>%
    summarise(meanpm25=mean(closestmean) )


#join to DB
setkey(x,aodid,day)
setkey(db2004,aodid,day)
xx <- merge(db2004,x,all.x = T)

summary(lm(pred~meanpm25,data=xx))

#### ADD PM10 and PM25



#take out uneeded
#save mod3 
gc()
saveRDS(db2004,"/media/qnap/Projects/Tr.PM25.2013.mod3.rds")
x1db2004<- readRDS("/media/qnap/Projects/P059_SWISS_AOD/work/mod3.AQ.2004.rds")

#calculate weights
x1db2004[, m := as.numeric(format(day, "%m")) ]
x1db2004<-x1db2004[,obs:=1]
x1db2004[is.na(aod), obs:= 0]
ws.2004<-select(x1db2004,obs,elev,pbl,m,temp,aodid,day)
#to save memory
gc()

w1 <- glm(obs ~ elev+temp+BLH+as.factor(m),family=binomial,data=ws.2004)
ws.2004$prob <- predict(w1 ,type = c("response"))  
ws.2004$wt <- 1/ws.2004$prob
#ws.2004$normwt <- ws.2004$wt/mean(ws.2004$wt)
#tray scaled and compare
ws.2004$normwt <- scale(ws.2004$wt)
ws.2004[, c("prob", "wt","obs","elev", "BLH" , "m","temp"  ) := NULL]
gc()

setkey(x1db2004,aodid,day)
setkey(ws.2004,aodid,day)
x1db2004 <- merge(x1db2004,ws.2004,all.x = T)


#create mod 2 file
db2004.m2 <- db2004[!is.na(aod)]
#rm db2004
rm(x1db2004)
gc()
#save mod2
saveRDS(x1db2004.m2,"/media/qnap/Projects/P059_SWISS_AOD/work/TR.XXXXX.rds")
gc()



#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
#x1db2004days <- sort(unique(db2004.m2$day))
# ADD PM2.5
jointo.pt <- makepointsmatrix(datatable = pmall2004, 
                             xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2004, 
                               xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                       jointo = pmall2004 , joinfrom = db2004, 
                       jointovarname = "stn", joinfromvarname = "aodid", 
                       joinprefix = "nearest", valuefield = "aod", 
                       knearest = 9, maxdistance = 1700, 
                       nearestmean = FALSE, verbose = T)


setkey(pmall2004,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(pmall2004, joinout, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]


joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                       jointo = pmall2004 , joinfrom = db2004, 
                       jointovarname = "stn", joinfromvarname = "aodid", 
                       joinprefix = "aodmean", valuefield = "aod", 
                       knearest = 9, maxdistance = 3000, 
                       nearestmean = TRUE, verbose = T)

setkey(PM25.m1,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(PM25.m1, joinout[,list(stn,day,aodmeanmean)], all.x = T)


#save mod 1
saveRDS(PM25.m1,"/media/qnap/Projects/P059_SWISS_AOD/work/mod1.AQ.2004.PM.rds")

