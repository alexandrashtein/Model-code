### Load libraries
library(sp)
library(data.table)

## Load mod3 bestpred
mod3=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.pred.AQ_Daily_clean.2003_2015.rds")

## Load monitors data that were not included in the calibration
PM25.m1.na=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/PM25.m1.2015.na.rds")
PM25.m1.na=PM25.m1.na[,1:7]
PM25.m1.na=as.data.frame(PM25.m1.na)
coordinates(PM25.m1.na) = ~ x_stn_ITM + y_stn_ITM
proj4string(PM25.m1.na) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
  
# Create unique grid
grid=mod3[,c("aodid","lon_aod","lat_aod")]
grid=as.data.table(grid)
grid=grid[!duplicated(aodid)]
grid$lat_aod_2=grid$lat_aod
grid$lon_aod_2=grid$lon_aod

# Convert to ITM
# Convert to data.frame
coordinates(grid) = ~ lon_aod + lat_aod
proj4string(grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grid=spTransform(grid, CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"))
# grid=as.data.frame(grid)
head(grid)

## Spatial Join

# install.packages("devtools")
library("devtools")
install_github("michaeldorman/geobgu")
library(geobgu)

PM25.m1.na2=overNN(PM25.m1.na,grid)
?overNN
?overNNf


     
### Check the correlation with measurents that were not included in the calibration stage

mod1<-mod1[,c("aodid","day","PM25","pred.m1","stn"),with=FALSE]
#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
mod1 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=mod1))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=mod1))$r.squared)    


