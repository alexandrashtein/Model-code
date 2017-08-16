# Reading CHIMERE's NetCDF output files

install.packages("RNetCDF")

# install.packages("ncdf4")
# install.packages("gstat")
# install.packages("proj4")

# Load libraries: ####
library(RNetCDF)
library(ncdf4)
library(rgdal)
library(raster)
library(gstat)
library(proj4)

# clear workspace: ###
rm(list=ls())
gc()             # Garbage Collection (clears the computer's memory)

#define projections:
WGS84_Projection = "+proj=longlat +datum=WGS84 +no_defs"
ITM_Projection   = "+proj=tmerc +lat_0=31.73439361 +lon_0=35.20451694 +k=1.00000670  +x_0=219529.58400000 +y_0=626907.39000000"
UTM36_Projection = "+proj=utm +zone=36 +datum=WGS84"

#set inputs: ####
setwd("D:\\CHIMERE")
#NCFileName = 'C:\\Data\\MOE\\CHIMERE\\CF_OUTPUT_2014_March_LcS_Day2.nc'

NCFileName = 'D:\\CHIMERE\\CF_OUTPUT_2012_December_LcS_Day1.nc'
#AQ_locations_Filename = 'C:\\Data\\MOE\\CHIMERE\\Sites_Full_Out.csv'
AQ_pollution_Filename = 'C:\\CHIMERE\\Q_Avg_PM25_ByYear.csv'
    
#read input data:
NCFile = nc_open(NCFileName) 
AQ_locations = read.csv(file = AQ_locations_Filename,stringsAsFactors=FALSE,header=T)
AQ_pollution = read.table(file = AQ_pollution_Filename, sep = '',stringsAsFactors=FALSE,header=T)

# Define pollutants: ####
My_Variables    = data.frame(
                  name        = c('PM10ant'   ,'PM10bio'   ,'PM10'      ,'PM25ant'   ,'PM25bio'   ,'PM25'      ,'CO'      ,'O3'       ,'NO'        ,'NO2'       ,'SO2'      ,'C2H4'    ),
                  units       = c('ug/m3'     ,'ug/m3'     ,'ug/m3'     ,'ug/m3'     ,'ug/m3'     ,'ug/m3'     , 'ppb vol','ppb vol'  ,'ppb vol'   ,'ppb vol'   ,'ppb vol'  ,'ppb vol' ),
                  Zmax        = NA,
                  stringsAsFactors = FALSE)

#extract CHIMERE data and convert to spatial raster:
lat  = NCFile$dim$lat$vals
lon  = NCFile$dim$lon$vals
ExtentLat = range(lat)
ExtentLon = range(lon)
rm(lat,lon)

TimeStart = 14
TimeLength   = 1
#read data from nc file:
nc.PM25ant = get.var.ncdf(NCFile , varid = 'PM25ant', start=c(1,1,1,TimeStart), count=c(-1,-1,1,TimeLength))
nc.PM10 = ncvar_get(NCFile , varid = 'PM10', start=c(1,1,1,TimeStart), count=c(-1,-1,1,TimeLength))
#convert to raster and flip:
nc.PM25ant = flip(t(raster(nc.PM25ant)), direction='y')
nc.PM10 = flip(t(raster(nc.PM10)), direction='y')

# Give it lat/lon coords for 36-37?E, 3-2?S
extent(nc.PM25ant) = c(ExtentLon,ExtentLat)
extent(nc.PM10) = c(ExtentLon,ExtentLat)
# ... and assign a projection
projection(nc.PM10) <- CRS(WGS84_Projection)
plot(nc.PM10)

# convert AQ data to spatial:
AQ_PM25 = AQ_pollution[which(AQ_pollution$YEAR == 2011),]
AQ_PM25 = merge(AQ_PM25,AQ_locations[,c('Code','Name_Eng','X_ITM','Y_ITM','Long','Lat')],by.x = 'code',by.y = 'Code')
coordinates(AQ_PM25) = ~Long + Lat
plot(AQ_PM25,add=T,pch=19)

#extract CHIMERE values at AQ locations:
PM25_Delta = extract(nc.PM25ant,AQ_PM25,sp=TRUE)
PM25_Delta$Delta = PM25_Delta$AVG_PM25 - PM25_Delta$layer
crs(PM25_Delta) = WGS84_Projection
