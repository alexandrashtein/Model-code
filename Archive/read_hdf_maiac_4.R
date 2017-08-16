library(raster)
library(rgdal)
library(rgeos)
library(gdalUtils)
library(plyr)
library(readxl)
library(leaflet)

# Original HDF files location
# smb://132.72.152.204/uni/Projects/P040.US.6city.PM10-25

# Input Directories
aod_dir = "/media/michael/Elements/MAIAC_AOD"
ref_grid_dir = "/media/michael/Elements/MAIAC_lon_lat"
xl_dir = "/home/michael/Dropbox/BGU/Meredith/MAIAC_AOD"

# Define USA cities clipping regions
pnt = read_excel(file.path(xl_dir, "Centroids of MESA Cities.xlsx"))
pnt = as.data.frame(pnt)
coordinates(pnt) = ~ Centroid_Long + Centroid_Lat
proj4string(pnt) = CRS("+init=epsg:4326")
pnt = spTransform(pnt, CRS("+init=epsg:2163")) # To 'US National Atlas' projection
pol = gBuffer(pnt, width = 60000, byid = TRUE)
pol = spTransform(pol, CRS("+init=epsg:4326"))

# Read HDF files list from AOD directory
setwd(aod_dir)
files = list.files(pattern = "\\.hdf$", recursive = TRUE)
tiles = sapply(strsplit(files, "\\."), `[`, 2)
years = sapply(strsplit(files, "/"), `[`, 2)

###################################################################
# STEP 1 - Reading HDF files
# writeOGR(r, ".", "grid_h07v08", driver = "ESRI Shapefile")

# for(t in unique(tiles)) {

t = "h06v07"
  
  # Read 'lon' and 'lat' rasters from static grid
  grid = 
    get_subdatasets(
      paste0(
        ref_grid_dir, 
        "/MAIACLatlon.",
        t,
        ".hdf"
        )
    )
  lon = grid[2] %>% readGDAL %>% raster
  lat = grid[1] %>% readGDAL %>% raster
  
  # Creare 'row' and 'col' rasters
  row = lon
  row[] = rowFromCell(lon, 1:ncell(lon))
  col = lon
  col[] = colFromCell(lon, 1:ncell(lon))
  
  # Combine to multi-band raster
  grid = stack(row, col, lon, lat)
  names(grid) = c("row", "col", "lon", "lat")
  
  # Convert to data.frame
  grid = as.data.frame(grid)
  
  #####################
  # Write grid table
  
  coordinates(grid) = ~ lon + lat
  proj4string(grid) = CRS("+init=epsg:4326")
  grid$MESA_Study_Site = over(grid, pol)$MESA_Study_Site
  grid = grid[!is.na(grid$MESA_Study_Site), ]
  # grid = grid[pol, ]
  
  # Map
  # leaflet() %>% addTiles %>% addCircles(data = grid, opacity = 0.1)
  
  grid = as.data.frame(grid)
  head(grid)
  # write.csv(grid, paste0(t, "_grid.csv"), row.names = FALSE)
  
  #####################
  
  
  # for(y in unique(years[tiles == t])) {
  for(y in c(2006, 2008:2009)) {
    
    result = list()

    for(f in files[tiles == t & years == y]) {
    
    # Read data
    sds = get_subdatasets(f)
    Optical_Depth_Land = sds[1] %>% readGDAL %>% raster
    AOT_Uncertainty = sds[2] %>% readGDAL %>% raster
    AOT_QA = sds[6] %>% readGDAL %>% raster
    row = Optical_Depth_Land
    row[] = rowFromCell(Optical_Depth_Land, 1:ncell(Optical_Depth_Land))
    col = Optical_Depth_Land
    col[] = colFromCell(Optical_Depth_Land, 1:ncell(Optical_Depth_Land))
    r = stack(row, col, Optical_Depth_Land, AOT_Uncertainty, AOT_QA)
    names(r) = c("row", "col", "Optical_Depth_Land", "AOT_Uncertainty", "AOT_QA")
    r = as.data.frame(r)
        
    # Join with 'grid'
    r = join(r, grid, c("row", "col"))
    r = r[!is.na(r$lon) & !is.na(r$lat), ]
    
    # Add filename
    r$file = f
    
    # Combine results
    result[[f]] = r
    
    }
    
    result = do.call(rbind.fill, result)
    
    write.csv(result, paste0("MAIACTAOT_", t, "_", y, ".csv"), row.names = FALSE)
    
    gc()
          
  }
  
# }


###################################################################
# STEP 2 - Processing

library(plyr)

aod_dir = "/media/michael/Elements/MAIAC_AOD"
setwd(aod_dir)

t = "h06v07"

files = 
  list.files(pattern = paste0("MAIACTAOT_", t, "_.*\\.csv$"))

for(i in files) {
 
  dat = read.csv(i, stringsAsFactors = FALSE)
  dat$date = 
    dat$file %>% 
    strsplit("\\.") %>% 
    sapply("[", 3) %>% 
    substr(1, 7) %>% 
    as.Date(format = "%Y%j")
  
  # dat$row = NULL
  # dat$col = NULL
  
  cols = c(
    "MESA_Study_Site", 
    "row", 
    "col", 
    "lon", 
    "lat", 
    "date",
    "Optical_Depth_Land", 
    "AOT_Uncertainty", 
    "AOT_QA", 
    "file"
    )
  dat = dat[, cols]
  
  y = 
    i %>% 
    strsplit("_") %>% 
    sapply("[", 3) %>% 
    strsplit("\\.") %>% 
    sapply("[", 1)
  
  write.csv(
    dat, 
    paste0("./final/MAIACTAOT_", t, "_", y, "_final.csv"), 
    row.names = FALSE
    )
    
}

###################################################################













