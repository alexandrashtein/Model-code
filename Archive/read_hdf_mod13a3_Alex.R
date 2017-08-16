library(magrittr)
library(raster)
library(rgdal)
library(gdalUtils)
library(rgeos)
library(reshape2)
library(plyr)
library(sp)


# system("python /media/michael/Elements/modis_download.py -r -p MOD11A1.005 -t h20v05,h20v06,h21v05,h21v06 -f 2012-10-04 -e 2012-10-06 /media/michael/Elements/terra2")

# Define list of layers
layers = c(1)

# Set working directory
setwd("C:/")

# START

# Prepare files list
files = list.files(pattern = "\\.hdf$") # Vector of all HDF files in WD
files_split = strsplit(files, "\\.") # Split by .
dates = sapply(files_split, "[", 2) # Select 2nd filename component
years = substr(dates, 2, 5) # Characters 2-5 = Year

# # Dates with less than 4 tiles
# x = dates %>% substr(2, 8) %>% as.Date(format = "%Y%j")
# x_tab = table(x)
# x_tab[x_tab != 4]
# 
# # Missing dates
# x_seq = seq(as.Date("2000-01-01"), as.Date("2016-01-01"), by = 1)
# setdiff(x_seq, x) %>% as.Date(origin = as.Date("1970-01-01"))
# 
# # Verify that all years have 365 days
# x = table(years) / 4
# x

# Create Extent object for cropping
il = readOGR("D:/Hdf_convert", "israel_borders")
mod_proj = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
il = spTransform(il, mod_proj)
il_ext = extent(il) + 15000

###############################################################################################
# STEP 1
###############################################################################################

# # Test
# y = 2014
# d = "A2014001"
# l = 1

for(y in 2014:2015) { # Loop 1: for each year

  # Empty list for collecting rasters
  result = list(`1` = stack())
  
  for(d in unique(dates[years == y])) { # Loop 2: each day
  
  # Current 4-files group
  current_files = files[dates == d] # 'files' subset for given day
  
    for(l in layers) { # Loop 3: each layer
      
      # Empty list for 4 rasters
      r = list() 
      
      # Reading 4 rasters (tiles)
      for(k in current_files) { # Loop 4: each tile (usually 4 files)
      
        # Test
        # system("gdal_translate -of GTiff C:\\test\\MOD13A3_ndvi_2015.tif C:\\test\\MOD13A3_ndvi_2015a.tif")
        
        sds = get_subdatasets(k) # Read current file
        x = strsplit(sds[1], ":")[[1]]
        x = paste0(x[1], ":", x[2], ':"', x[3], '":', x[4], ":", x[5])
        system(
          paste0(
            "gdal_translate -of GTiff ",
            "\"", 
            x,
            "\" C:\\test\\",
            gsub(".hdf", ".tif", k, fixed = TRUE)
            )
          )
        
        
        r[[k]] = raster(paste0("C:\\test\\", gsub(".hdf", ".tif", k, fixed = TRUE))) # Convert to raster
        
      }
      
      # Save 1st tile to temporary object 
      tmp = r[[1]]
      
      # If there is more than 1 tile...
      if(length(current_files) > 1) {
      
        # Loop through remaining 2, 3..., n tiles and mosaic
        for(m in 2:length(current_files)) {
          
          tmp = mosaic(tmp, r[[m]], fun = "mean") # Mosaic
        
        }
        
    }
      
      r = crop(tmp, il_ext) # Crop
      names(r) = d # Set layer name
      
      # x = resample(r, result[[1]], )
      
      result[[as.character(l)]] = stack(result[[as.character(l)]], r) # Save result
      
      }
    
  }

#   for(l in layers) {
#     
#     writeRaster(
#       result[[as.character(l)]], 
#       paste0("MOD11A1_", y, "_", l, ".rds")
#       )
#     write.csv(
#       names(result[[as.character(l)]]), 
#       paste0("MOD11A1_dates_", y, "_", l, ".csv")
#       )
#       
#   }
  
  # Save whole year list
  saveRDS(result[[1]], paste0("C:\\test\\MYD13A3_ndvi_", y, ".rds"))
  #writeRaster(result[[1]], paste0("C:\\test\\MYD13A3_ndvi_", y, ".tif"))
  # writeRaster(result[[2]], paste0("MOD13A3_evi_", y, ".tif"))
}

# for 2014
setwd("C:/test")
ndvi_2014=readRDS("MOD13A3_ndvi_2014.RDS")
ndvi_2014= rasterToPoints(ndvi_2014, spatial = TRUE)
ndvi_2014= spTransform(ndvi_2014, CRSobj = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs"))
ndvi_2014=as.data.frame(ndvi_2014)
ndvi_2014=melt(ndvi_2014,id.vars=c("x","y"))
# Scaling NDVI
ndvi_2014$value=ndvi_2014$value*0.0001*0.0001
# Creating ndviid
ndvi_2014 = rename(ndvi_2014, c("x" = "long_ndvi", "y" = "lat_ndvi", "variable"="date","value"="ndvi"))
# Round coords
ndvi_2014$long_ndvi = round(ndvi_2014$long_ndvi, 4)
ndvi_2014$lat_ndvi = round(ndvi_2014$lat_ndvi, 4)
#Creating ndviid
ndvi_2014$ndviid = paste0(ndvi_2014$long_ndvi, ndvi_2014$lat_ndvi)
# Convert to date
ndvi_2014$date = ndvi_2014$date%>% as.character %>% substr(2, 8) %>% as.Date(format = "%Y%j")
ndvi_2014$c = format(ndvi_2014$date, "%Y")
ndvi_2014$m = format(ndvi_2014$date, "%m")

ndvi_2014 = ndvi_2014[, c("c", "m", "lat_ndvi", "long_ndvi", "ndvi", "ndviid")]

saveRDS(ndvi_2014, paste0("C:\\test\\ndvi_2014_table", ".rds"))

# for 2015
setwd("C:/test")
ndvi_2015=readRDS("MOD13A3_ndvi_2015.RDS")
ndvi_2015= rasterToPoints(ndvi_2015, spatial = TRUE)
ndvi_2015= spTransform(ndvi_2015, CRSobj = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs"))
ndvi_2015=as.data.frame(ndvi_2015)
ndvi_2015=melt(ndvi_2015,id.vars=c("x","y"))
# Scaling NDVI
ndvi_2015$value=ndvi_2015$value*0.0001*0.0001
# Creating ndviid
ndvi_2015 = rename(ndvi_2015, c("x" = "long_ndvi", "y" = "lat_ndvi", "variable"="date","value"="ndvi"))
# Round coords
ndvi_2015$long_ndvi = round(ndvi_2015$long_ndvi, 4)
ndvi_2015$lat_ndvi = round(ndvi_2015$lat_ndvi, 4)
#Creating ndviid
ndvi_2015$ndviid = paste0(ndvi_2015$long_ndvi, ndvi_2015$lat_ndvi)
# Convert to date
ndvi_2015$date = ndvi_2015$date%>% as.character %>% substr(2, 8) %>% as.Date(format = "%Y%j")
ndvi_2015$c = format(ndvi_2015$date, "%Y")
ndvi_2015$m = format(ndvi_2015$date, "%m")

ndvi_2015 = ndvi_2015[, c("c", "m", "lat_ndvi", "long_ndvi", "ndvi", "ndviid")]

saveRDS(ndvi_2015, paste0("C:\\test\\ndvi_2015_table", ".rds"))

ndvi_2000_2013<-readRDS("N:/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.RDS")

final = rbind(ndvi_2000_2013, ndvi_2014,ndvi_2015)

saveRDS(final, paste0("C:\\test\\ndvi_2000_2015", ".rds"))


###############################################################################################
# STEP 2
###############################################################################################

# grid = readOGR("/home/michael/Dropbox/BGU/Adar/reading_modis_hdf", "grid")
# grid = readOGR("D:\\Users\\adarro\\Desktop\\research\\gisdata5", "voronoi_density_ndvi_water_final_finalll_GEO_002a")
# grid = grid[, c("long_lst", "lat_lst")]

final = NULL

for(y in 2004:2014) {
  
  result = readRDS(paste0("MOD13A3_", y, ".rds"))
  # result = lapply(result, `[[`, 1) # Subset for testing
  
  result = result[[1]]
  result = result * 0.0001 * 0.0001

  result = rasterToPoints(result, spatial = TRUE)
  result = spTransform(result, CRSobj = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs"))
  
#   # Correct according to grid
#   for(i in 1:length(result)) {
#     result[[i]]@data = cbind(result[[i]]@data, over(result[[i]], grid))
#     result[[i]] = result[[i]][!is.na(result[[i]]$long_lst) & !is.na(result[[i]]$lat_lst), ]
#   }
  result = as.data.frame(result)
  # result = lapply(result, `[<-`, "x", value = NULL)
  # result = lapply(result, `[<-`, "y", value = NULL)
  result = rename(result, c("x" = "long_ndvi", "y" = "lat_ndvi"))
  
  # print(paste("Processing", y))
  
  # Melt
  result = melt(result, id.vars = c("long_ndvi", "lat_ndvi"), value.name = "ndvi")
  
#   # Set variable names
#   result[["1"]]$var = "d.tempc"
#   result[["5"]]$var = "n.tempc"
#   result[["9"]]$var = "emissivity"
#   # result[["10"]]$var = "Emis_32"
#   
#   # Combine 4 tables
#   result = do.call(rbind, result)
#   
#   # NaN to NA
#   result$value[is.nan(result$value)] = NA
  
  # Convert to date
  result$variable = result$variable %>% as.character %>% substr(2, 8) %>% as.Date(format = "%Y%j")
  result$c = format(result$variable, "%Y")
  result$m = format(result$variable, "%m")
  
  # Cast
  # result = dcast(result, ... ~ var, value.var = "value")
  
  # Remove empty rows
  # all_missing = 
  #   result[, c("Emis_31","Emis_32","d.tempc","n.tempc")] %>% 
  #   apply(1, function(x) all(is.na(x)))
  # result = result[!all_missing, ]
  
  # To Celsius
  # result$d.tempc = result$d.tempc - 273.15
  # result$n.tempc = result$n.tempc - 273.15
  
  # Round coords
  result$long_ndvi = round(result$long_ndvi, 4)
  result$lat_ndvi = round(result$lat_ndvi, 4)
  
  # ID
  result$ndviid = paste0(result$long_ndvi, result$lat_ndvi)
  
  # Rename
  # result = plyr::rename(result, c("variable" = "day"))
  
  # Calculate year
  # result$Year = format(result$day, format = "%Y")
  
  # Columns order
  result = result[, c("c", "m", "lat_ndvi", "long_ndvi", "ndvi", "ndviid")]
  
  # write.csv(result, paste0("MOD11A1_", y, ".csv"), row.names = FALSE)
  saveRDS(result, paste0("result_", y, ".rds"))
  
  final = rbind(final, result)
  
}

saveRDS(final, "result_2004_2014.rds")

# Test
# x = readRDS("ndvi.rds")
# x = as.data.frame(x)
# x = x[x$c == 2004, ]
# x$long_ndvi = round(x$long_ndvi, 4)
# x$lat_ndvi = round(x$lat_ndvi, 4)
# coordinates(x) = ~ long_ndvi + lat_ndvi
# proj4string(x) = "+proj=longlat +ellps=WGS84 +no_defs"
# writeOGR(x, ".", "test1", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# 
# x = result
# x = x[x$c == 2004, ]
# x$long_ndvi = round(x$long_ndvi, 4)
# x$lat_ndvi = round(x$lat_ndvi, 4)
# coordinates(x) = ~ long_ndvi + lat_ndvi
# proj4string(x) = "+proj=longlat +ellps=WGS84 +no_defs"
# writeOGR(x, ".", "test2", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Test
# x = grid@data
# names(x) = c("x", "y")
# coordinates(x) = ~ x + y
# x$long_lst = coordinates(x)[, 1]
# x$lat_lst = coordinates(x)[, 2]
# proj4string(x) = "+proj=longlat +ellps=WGS84 +no_defs"
# writeOGR(x, "P:\\2.work", "grid_test2", driver = "ESRI Shapefile")

# Correcting Adar's NDVI aodid
NDVI=readRDS("N:/Projects/P060_IL_MAIAC_V2/work/NDVI/MODIS/result_2004_2014.rds")

# Correcting ndviid from Adar's ndvi data
NDVI=readRDS("N:/Projects/P060_IL_MAIAC_V2/work/NDVI/MODIS/result_2004_2014.rds")
head(NDVI)
NDVI$ndviid= paste0(formatC(NDVI$long_ndvi,format="f",digits=3),paste0("-"),formatC(NDVI$lat_ndvi,format="f",digits=3))
setwd("N:/Projects/P060_IL_MAIAC_V2/work/NDVI/MODIS")
saveRDS(NDVI,"NDVI_2000_2014_new2.rds")
NDVI_all=readRDS("N:/Projects/P060_IL_MAIAC_V2/work/NDVI/MODIS/ndvi_2000_2015.rds")
NDVI_all$ndviid_new= paste0(formatC(NDVI_all$long_ndvi,format="f",digits=3),paste0("-"),formatC(NDVI_all$lat_ndvi,format="f",digits=3))
NDVI_all[(nrow(NDVI_all)-1000):nrow(NDVI_all),]

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS")
dat=readRDS("ndvi_2000_2015.rds")
dat=dat[c==2001 & m==1,]
write.csv(dat, "ndvi_grid.csv")
dat=as.data.frame(dat)
coordinates(dat) = ~ long_ndvi + lat_ndvi
proj4string(dat) = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')

writeOGR(
  dat1,
  ".",
  "NDVI_grid",
  driver = "ESRI Shapefile",
  overwrite_layer = TRUE
)

