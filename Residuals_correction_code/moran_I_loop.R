### This code explores the residuals correction\interpolation in days where 
### we find significant positive auto correlation in the residuals of stage 3

### June 2017
rm(list=ls())
## Load libraries
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
library(splines)
library(data.table)
library(rgdal)
library("devtools")
install_github("michaeldorman/geobgu")
library(geobgu)
library(gstat)
library(spdep) ## this library allow to calculate Spatial neighbors 
library("sp")
library("rgdal")
library("rgeos")
library(ncf)
library("spatstat")

# Source file
source("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/code/code_examples/multiplot_fun.R")
source("/media/qnap/Data/code/R_functions/rmspe.r")

## Load stations data

PM25_Daily  <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM25_D.csv")
PM25_Daily<-PM25_Daily[PM25 > 0.000000000001 & PM25 < 1000 ]
PM25=PM25_Daily
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(date, "%d/%m/%Y")]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-as.data.table(as.data.frame(PM25)[!is.na(PM25$PM25), ]) 
PM25 <- PM25[day>= "2003-01-01",]

## Load prediction from MOD3

## Several prediction where created, each time leaving one station out (Overall three MOD3 predictions where created, each time without another stations)

# A MOD3 prediction that includs all stations
# mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.pred.AQ_Daily_clean.2003_2015.rds")

# A prediction that excludes BSV\ASS\AFU station
# mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.2015_without_ASS_station.rds")
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.2015_without_AFU_station.rds")
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/mod3_pred_without_AFU.rds")
# mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.2015_without_BSV_station.rds")

mod3 <- as.data.table(mod3)

## Add projected coordinates for mod3
mod3$X=mod3$lon_aod
mod3$Y=mod3$lat_aod

## Convert to ITM
coordinates(mod3) = ~ X + Y
proj4string(mod3) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
newProj=CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs")
mod3 = spTransform(mod3, newProj)
mod3=as.data.table(mod3)

## Create Unique grid of AOD grid
grid=mod3[!duplicated(aodid)]
grid=grid[,list(aodid,lon_aod,lat_aod,X,Y)]

## Join grid aodid to PM stations
## Convert to spatial object
coordinates(grid) = ~ X + Y
proj4string(grid) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"

coordinates(PM25) = ~ X + Y
proj4string(PM25) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"

## Add to each point the aodid from the nearest neighbor
PM25_s2=overNN(PM25,grid)
PM25=as.data.table(PM25)
PM25=cbind(PM25,PM25_s2)
PM25[ , c("lon_aod","lat_aod") := NULL]

# Save PM2.5 data with aodid
# saveRDS(PM25,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/PM25_2003_2015.rds")
PM25=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/PM25_2003_2015.rds")

## Join mod3 prediction to stations data
PM25=as.data.table(PM25)
setkey(PM25,aodid,day)
mod3_t=mod3[,list(day,aodid,pred.m3.mix)]
setkey(mod3_t,aodid,day)
PM25=merge(PM25,mod3_t,all.x=TRUE)

## Compute the difference between observed and predicted
PM25$diff=PM25$PM25-PM25$pred.m3.mix

### Calculate Moran I for all days

df=data.frame(day=numeric(0),sign=numeric(0), moran_stat=numeric(0))
class(df$day) <- "Date"

## We will explore one year (2015) for a strat, and exclude the tested station
# PM25_2015=PM25[c==2015]
# PM25_2015=PM25_2015[stn!="ASS"]
PM25_2015=PM25_2015[stn!="AFU"]
# PM25_2015=PM25_2015[stn!="BSV"]

PM25_2015 <- PM25_2015[day>="2003-01-01",]
days=unique(PM25_2015$day)

## The following loop calculates Moran I for the residuals of each day

days <- unique(PM25_2015$day)
## Create a data.frame that will store the results from the Moran I loop
df=data.frame(day=numeric(0),sign=numeric(0), moran_stat=numeric(0))
class(df$day) <- "Date"

## Paraller Moran I
registerDoMC(cores=10)

df.res = foreach(i = 1:length(days),.packages=c('spdep','sp')) %dopar% {
  
  PM25_s <- PM25_2015[day==days[i]] 
  
  ## Convert monitors to spatial dataframe
  coordinates(PM25_s) <-  ~ X + Y
  proj4string(PM25_s) <-  "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
  
  ######## First method to Define weights
  w <- 1/as.matrix(dist(coordinates(PM25_s)))
  diag(w) <- 0
  # res=moran.test(PM25_s$diff,mat2listw(w))
  
  ## A simulation- based approach 
  
  res=moran.mc(PM25_s$diff,mat2listw(w),10000) 
  
  df[1,"day"] <- days[i]
  df[1,"sign"] <- as.numeric(res$p.value < 0.05)
  df[1,"moran_stat"] <- round(as.numeric(paste(res$statistic[1])),2) # For simulation based approach
  df
  }

df <- do.call(rbind,df.res)
rm(df.res)

# Find the days where there is positive spatial auto-correlation in the residuals
df2=df[df$sign==1 & df$moran_stat > 0,]
days=unique(df2$day) ## Only in these days residuals correction will be carried out. 

## The following loop adds the residuals interpolation for these days.
## first we will take one of the test station out

# PM25_2015=PM25_2015[stn!="ASS"]
 PM25_2015=PM25_2015[stn!="AFU"]
# PM25_2015=PM25_2015[stn!="BSV"]

library(foreach)
library(doMC)
registerDoMC(cores=10)

test_final = foreach(i = 1:length(days)) %dopar% {

  mod3_s=mod3[day==days[i]]
  PM25_s=PM25_2015[day==days[i]]
  
  ## Use IDW interpolation for residuals interpolation from monitors
  coordinates(PM25_s) = ~ X + Y
  proj4string(PM25_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
  coordinates(mod3_s) = ~ X + Y
  proj4string(mod3_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
  inter = gstat(formula = diff ~ 1,  data =PM25_s)
  mod3_s$resid_IDW <-predict(object = inter, newdata = mod3_s)$var1.pred
  mod3_s=as.data.table(mod3_s)
  mod3_s
}

# Convert back the list into a data.table
mod3_s = do.call(rbind, test_final)

## Merge back to MOD3 (adding the days with the correction to the rest to the data)

# Keep in a separate file all the days that the IDW interpolation of resid was not applied
mod3_s2=mod3[!day %in% days]
# Create this column
mod3_s2$resid_IDW <-  0
# Create the full MOD3 for 2015
mod3_final=rbind(mod3_s,mod3_s2)

mod3_final$pred.m3.mix.resid=mod3_final$pred.m3.mix+mod3_final$resid_IDW

##################### Evaluate the improvment

## Compute R2 and RMSE for 2015
PM25_2015=PM25[c==2015]
PM25_2015=as.data.table(PM25_2015)
setkey(PM25_2015,aodid,day)
mod3_s=mod3_final[c=='2015']
mod3_s=mod3_final[,list(day,aodid,pred.m3.mix.resid)]
setkey(mod3_s,aodid,day)

PM25_s=merge(PM25_2015,mod3_s)

print(summary(lm(PM25~pred.m3.mix,data=PM25_s))$r.squared)
print(summary(lm(PM25~pred.m3.mix.resid,data=PM25_s))$r.squared)

rmse(PM25_ASS$diff_Pred.ms)
rmse(PM25_ASS$diff_Pred.ms.resid)

## Plot residuals before and after correction in one station
PM25_ASS=PM25_s[stn=='ASS']
PM25_ASS=PM25_s[stn=='AFU']
PM25_ASS=PM25_ASS[day %in% days]
# PM25_ASS=PM25_ASS[stn=='BSV']

PM25_ASS$diff_Pred.ms= PM25_ASS$PM25-PM25_ASS$pred.m3.mix
PM25_ASS$diff_Pred.ms.resid= PM25_ASS$PM25-PM25_ASS$pred.m3.mix.resid

# RMSE
round(sqrt(mean(abs(PM25_ASS$diff_Pred.ms)^2)),2)
round(sqrt(mean(abs(PM25_ASS$diff_Pred.ms.resid)^2)),2)

print(summary(lm(PM25~pred.m3.mix,data=PM25_ASS))$r.squared)
print(summary(lm(PM25~pred.m3.mix.resid,data=PM25_ASS))$r.squared)

#################### Plot the results - time series plots

plot(abs(PM25_ASS$diff_Pred.ms.resid), type="p", col="blue", ylim=c(0,20),ylab="Absolute Differnce", xlab="Day")

points(abs(PM25_ASS$diff_Pred.ms), type="p", pch=22, lty=2, col="red")

# title(main="Residual Differnce for AFU station", col.main="red", font.main=4)# title(main="Residual Differnce for AFU station", col.main="red", font.main=4)
 title(main="Residual Differnce for ASS station", col.main="red", font.main=4)# title(main="Residual Differnce for AFU station", col.main="red", font.main=4)
# title(main="Residual Differnce for BSV station", col.main="red", font.main=4)

g_range <- range(0, PM25_ASS$diff_Pred.ms.resid)
legend(1, 20, c("Diff - current model","Diff- resid correction"), cex=0.8, 
       col=c("red","blue"), pch=21:22, lty=1:2)

##################### Plotting- Map overlay #####################################

## Load border polygon
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
# Convert to ITM projection:
pol = spTransform(pol, newProj)
pol@data$id = rownames(pol@data)
pol.points = fortify(pol, region="id")
pol.df = join(pol.points, pol@data, by="id")

## Convert to table for plotting
PM25=as.data.table(PM25)

PM25_mean <-mod3_final %>%
  dplyr::group_by(aodid) %>%
  dplyr::summarise(x=mean(X, na.rm=TRUE), y =mean(Y, na.rm=TRUE), pred.m3.mix=mean(pred.m3.mix, na.rm=TRUE), pred.m3.mix.resid=mean(pred.m3.mix.resid))
PM25_mean=as.data.table(PM25_mean)

mod3_s=mod3_final[day=="2015-03-11"]
grid=as.data.table(grid)

setwd("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/GAM_residuals_smoothing")
tiff("try3.tiff",width = 867, height = 458, res="200",units = "px")

## Plot PM prediction before resid correction
p1=ggplot() + 
  geom_point(data=PM25_mean, aes(x,y,color=pred.m3.mix),alpha=1, size=1)+
  geom_polygon(data=pol.df,aes(long,lat,group=group),colour="black", fill=NA)+
  scale_color_gradient("PM25 concentration",low="lightgreen",high="red")+ ggtitle("PM2.5 estimation for 2015-03-11")
multiplot(p1,p2,cols = 2)

## Plot the PM25 estimation after considering resid interpolation 
p2=ggplot() + coord_equal()+
  geom_point(data=PM25_mean, aes(x,y,color=pred.m3.mix.resid),alpha=1, size=2)+
  geom_polygon(data=pol.df,aes(long,lat,group=group),colour="black", fill=NA)+
  scale_color_gradient("Prediction concentration",low="lightgreen",high="red")+ ggtitle("Prediction Error for 2015-03-11")

multiplot(p1,p2,cols = 2)

dev.off()