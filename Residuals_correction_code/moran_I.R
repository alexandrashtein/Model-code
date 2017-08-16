# This code explores spatial auto-correllation of monitors residuals using Moran I

## Load libraries
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
library(rgdal)
library("devtools")
install_github("michaeldorman/geobgu")
library(geobgu)
library(gstat)

# Source file
source("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/code/code_examples/multiplot_fun.R")

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
# PM25_s=PM25[day=="2015-03-01"]
PM25_s=PM25[day=="2015-03-15"]

## Convert to layer
coordinates(PM25_s) = ~ X + Y
proj4string(PM25_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"

## Create Unique grid of AOD grid
grid=mod3[!duplicated(aodid)]
grid=grid[,list(aodid,lon_aod,lat_aod,long_aod,lat_aod)]
grid$X=grid$lon_aod
grid$Y=grid$lat_aod

## Convert to ITM
newProj=CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs")
coordinates(grid) = ~ X + Y
proj4string(grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grid = spTransform(grid, newProj)

## Join grid to PM stations
PM25_s2=overNN(PM25_s,grid)
PM25_s=as.data.table(PM25_s)
PM25_s=cbind(PM25_s,PM25_s2)

## Load border
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
pol = spTransform(pol, newProj)
pol@data$id = rownames(pol@data)
pol.points = fortify(pol, region="id")
pol.df = join(pol.points, pol@data, by="id")

## Load prediction from MOD3
mod3 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.pred.AQ_Daily_clean.2003_2015.rds")
mod3 <- as.data.table(mod3)

# Use one day only data
# mod3_s=mod3[day=="2015-03-01"]
mod3_s=mod3[day=="2015-03-15"]
mod3_s=mod3_s[,list(aodid,pred.m3.mix,pred.m3,lon_aod,lat_aod)]

# Add ITM coordinates to MOD3
mod3_s$X=mod3_s$lon_aod
mod3_s$Y=mod3_s$lat_aod

coordinates(mod3_s) = ~ X + Y
proj4string(mod3_s) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mod3_s = spTransform(mod3_s, newProj)
mod3_s=as.data.table(mod3_s)

## Join mod3 prediction to stations data
setkey(PM25_s,aodid)
setkey(mod3_s,aodid)
PM25_s=merge(PM25_s,mod3_s)

## Compute the difference between observed and predicted
PM25_s$diff=PM25_s$PM25-PM25_s$pred.m3.mix

## Interpolate the residuals
grid=as.data.table(grid)
setnames(grid,"lon_aod","X")
setnames(grid,"lat_aod","Y")

## Use IDW interpolation for residuals interpolation from monitors
  coordinates(PM25_s) = ~ X + Y
  proj4string(PM25_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
  coordinates(grid) = ~ X + Y
  proj4string(grid) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
  inter = gstat(formula = diff ~ 1,  data =PM25_s)
  grid$pred<-predict(object = inter, newdata = grid)$var1.pred

##################### Plotting #####################################
  
## Convert to table for plotting
PM25_s=as.data.table(PM25_s)
grid=as.data.table(grid)

setwd("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/results/GAM_residuals_smoothing")
tiff("maps_15032015.tiff",width = 874, height = 497, res="200")
    
## Plot stations
p1=ggplot() + 
  geom_point(data=PM25_s, aes(X,Y,color=PM25),alpha=1, size=2)+
  geom_polygon(data=pol.df,aes(long,lat,group=group),colour="black", fill=NA)+
  scale_color_gradient("PM25 concentration",low="lightgreen",high="red")+ ggtitle("PM2.5 concentration for 2015-03-15")

## Plot PM prediction for that day
p2=ggplot() + 
  geom_point(data=mod3_s, aes(X,Y,color=pred.m3.mix),alpha=1, size=1)+
  geom_polygon(data=pol.df,aes(long,lat,group=group),colour="black", fill=NA)+
  scale_color_gradient("PM25 concentration",low="lightgreen",high="red")+ ggtitle("PM2.5 estimation for 2015-03-15")
multiplot(p1,p2,cols = 2)

## Plot Prediction error
p3=ggplot() +
  geom_point(data=PM25_s, aes(X,Y,color=diff),alpha=1, size=2)+
  geom_polygon(data=pol.df,aes(long,lat,group=group),colour="black", fill=NA)+
  scale_color_gradient("Prediction Error",low="blue",high="red")+ ggtitle("Prediction Error for 2015-03-15")

## Plot the interpolated residuals
p4=ggplot() +
  geom_point(data=grid, aes(X,Y,color=pred),alpha=1, size=2)+
  geom_polygon(data=pol.df,aes(long,lat,group=group),colour="black", fill=NA)+
  scale_color_gradient("Prediction Error",low="blue",high="red")+ ggtitle("Prediction Error for 2015-03-15")

multiplot(p1,p3,p2,p4,cols = 2)

dev.off()

## Use Moran I to see if there is spatial auto-correlations

library(spdep) ## this library allow to calculate Spatial neighbors 
library("sp")
library("rgdal")
library("rgeos")
library(ncf)
library("spatstat")

## Convert monitors to spatial dataframe
coordinates(PM25_s) = ~ X + Y
proj4string(PM25_s) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"

######## First method to Define weights

## This syntax was taken from:https://rstudio-pubs-static.s3.amazonaws.com/79757_3462f2bba8d745378545f0ea5bc38ee1.html
w <- 1/as.matrix(dist(coordinates(PM25_s)))
diag(w) <- 0
res=moran.test(PM25_s$diff,mat2listw(w))

## Is there a spatial auto-correlation?
res$statistic > 0 
res$p.value < 0.05 

# plot weights
w.cols <- 1:42
w.rows <- 1:42
image(w.cols, w.rows, w)

# creates a moran plot
moran <- moran.plot(PM25_s$diff, listw = nb2listw(w, style = "W"))

PM25I <- spline.correlog(x=coordinates(PM25_s)[,1], y=coordinates(PM25_s)[,2],
                         z=PM25_s$diff, resamp=100, quiet=TRUE)
plot(density(PM25_s$diff))
plot(PM25I)

######### Another method to define weights

## Difine weights- distance based neighbors- choose the k nearest points as neighbors
w <- knn2nb(knearneigh(PM25_s,k=8))
res =print(moran.test(PM25_s$PM25,nb2listw(w)))

## Is there a spatial auto-correlation?
res$statistic > 0 # TRUE
res$p.value < 0.05 # TRUE
