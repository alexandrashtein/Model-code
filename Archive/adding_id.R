library(magrittr)
library(h5)
library(raster)
library(rgdal)
library(plyr)
library(reshape2)


setwd("D:\\IL_200m_unique_id")
setwd("N:/Projects/P060_IL_MAIAC_V2/work/Qgis/IL_200m.grid/IL_200m_grid_squares")
grid = readOGR(".", "grid_200m_squares_id", stringsAsFactors = FALSE)

grid$X_Y=paste0(grid$Avg_POINT_, "-",grid$Avg_POIN_1)

coordinates(final) = ~ lon + lat
proj4string(grid) = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'


writeOGR(grid,".","grid_200m_squares_id2",driver="ESRI Shapefile")

