# Reading CHIMERE's NetCDF output files 

# install.packages("RNetCDF")

# install.packages("ncdf4")
# install.packages("gstat")
# install.packages("proj4")

# Load libraries: ####
# library(RNetCDF)
library(ncdf4)
library(rgdal)
library(raster)
library(gstat)
library(proj4)
library(magrittr)
library(sp)

# clear workspace: ###
rm(list=ls())
gc()             # Garbage Collection (clears the computer's memory)

#define projections:
WGS84_Projection = "+proj=longlat +datum=WGS84 +no_defs"
ITM_Projection   = "+proj=tmerc +lat_0=31.73439361 +lon_0=35.20451694 +k=1.00000670  +x_0=219529.58400000 +y_0=626907.39000000"
UTM36_Projection = "+proj=utm +zone=36 +datum=WGS84"

#set inputs: ####
setwd("D:\\CHIMERE")
NCFileName = 'CF_OUTPUT_2015_March_LcS_Day1.nc'
AQ_locations_Filename = 'PM10_statioms.csv'

# AQ_pollution_Filename = 'C:\\CHIMERE\\Q_Avg_PM25_ByYear.csv'
    
#read input data:
NCFile = nc_open(NCFileName) 

# Read variable
x = ncvar_get(NCFile, "PM10")

# Get the geographical data from nc files (lat,lon) and the time (Times) 
lon = ncvar_get(NCFile, "lon")[,,1] %>% raster
lat = ncvar_get(NCFile, "lat")[,,1] %>% raster
times = ncvar_get(NCFile, "Times")

# Convert the variavle into raster brick
r = ncvar_get(NCFile, "PM10") %>% brick 
r

# Choosed only the 8-9 morning time data
times_selected = grepl("08:00:00", times, fixed = TRUE) %>% which
r = r[[times_selected]]
times = times[times_selected]

r = stack(r, lon, lat)
names(r) = c(times, "lon", "lat")
dat = as.data.frame(r)

coordinates(dat) = ~ lon + lat
proj4string(dat) = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

# Change the name of the data frame- fix this!
dat = dat[, 1:10]
names(dat) = letters[1:10]

# Converting the data frame into a shapefile 
writeOGR(
  dat,
  ".",
  "dat_test",
  driver = "ESRI Shapefile",
  overwrite_layer = TRUE
)

dat = readOGR(".", "dat_test")

# Spatial join in R

# Read PM10 stations shapefile 
stn = readOGR(".", "pmstn10_geo")
# Calculate matrix of distance between the two shapefiles
dist = spDists(dat, stn)
# finds out which point is the nearest neighbor in each case
nearest_dat = apply(dist, 2, which.min)
# assigns the values of the closest dat point to the stations
stn$nn = dat$a[nearest_dat]

plot(dat)
plot(stn, add = TRUE)

writeOGR(
  stn,
  ".",
  "pmstn10_geo_nn",
  driver = "ESRI Shapefile",
  overwrite_layer = TRUE
)



# Read the CHIMERE data shapefile 

# read square grid 1 km maiac
setwd("N:/Projects/P060_IL_MAIAC_V2/work/Qgis/Grid_for_LU")
s_grid = readOGR(".", "IL_MAIAC_grid_1km_squars")
S_grid_geo = spTransform(s_grid, CRS(proj4string(dat)))

plot(dat)





# Method 2
lon = ncvar_get(NCFile, "lon")[,,1] %>% raster
lat = ncvar_get(NCFile, "lat")[,,1] %>% raster
times = ncvar_get(NCFile, "Times")
r = ncvar_get(NCFile, "O3") %>% brick %>% t %>% flip(direction = 'y')
extent(r) = extent(c(range(lon[]), range(lat[])))
proj4string(r) = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

writeRaster(r[[1]], "test.tif")

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
