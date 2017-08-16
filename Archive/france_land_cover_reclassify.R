# This code runs spatial calculation based on a grid of points and a raster
# for example calculating the percentage of forested\open\developed pixels within a grid cell
# Credit: Michael Dorman, update: August 2016


##########################################################################
# Extract France land cover

library(magrittr)
library(raster)
library(rgdal)

setwd("/media/michael/Elements/france_ndvi")
# setwd("~/france")

# Read data
grid = readOGR(".", "grid200_clip_aggregate4_ndvi_elev_buffer200", stringsAsFactors = FALSE)
lc = raster("/media/michael/Elements/france_corine/g100_06.tif")
legend = read.csv("/media/michael/Elements/france_corine/clc_legend.csv", stringsAsFactors = FALSE)
legend = legend[!is.na(legend$type), ]

# Test
# r = lc[25000:25100, 35000:35100, drop = FALSE]
# plot(r)
# r2 = r %in% 1:3
# r[r2] %>% range

# Rasters for each class
developed = lc %in% legend$GRID_CODE[legend$type == "developed"]
open = lc %in% legend$GRID_CODE[legend$type == "open"]
forest = lc %in% legend$GRID_CODE[legend$type == "forest"]

writeRaster(
  stack(developed, open, forest),
  "/media/michael/Elements/france_corine/g100_06_developed_open_forest.tif"
)


##########################################################################
# Extract France land cover

library(magrittr)
library(raster)
library(rgdal)

setwd("~/france")

grid = readOGR(".", "grid200_clip_aggregate4_ndvi_elev", stringsAsFactors = FALSE)
lc = brick("g100_06_developed_open_forest.tif")

# Focal filter
m = matrix(c(NA, NA, 1, NA, NA, NA, 1, 1, 1, NA, 1, 1, 1, 1, 1, 
             NA, 1, 1, 1, NA, NA, NA, 1, NA, NA), nrow = 5, ncol = 5)
m = m/13
m[is.na(m)] = 0
lc[[1]] = focal(lc[[1]], w = m)
lc[[2]] = focal(lc[[2]], w = m)
lc[[3]] = focal(lc[[3]], w = m)

grid$developed = extract(lc[[1]], grid)
grid$open = extract(lc[[2]], grid)
grid$forest = extract(lc[[3]], grid)

# Write Shapefile
writeOGR(
  grid,
  ".",
  "grid200_clip_aggregate4_ndvi_elev_lc1",
  driver = "ESRI Shapefile",
  overwrite_layer = TRUE
  )




# ##########################################################################
# i = 0
# start = 1000000 * i + 1
# end = 1000000 * (i + 1)
# 
# grid1 = grid[start:end, ]
# 
# grid1@data = cbind(
#   grid1@data,
#   extract(lc, grid1, fun = mean, na.rm = TRUE, buffer = 200, df = TRUE)
# )

# ##########################################################################
# # R version - 1
# 
# library(magrittr)
# library(raster)
# library(rgdal)
# 
# # setwd("/media/michael/Elements/france_ndvi")
# setwd("~/france_ndvi")
# 
# ndvi_1 = raster("l8_france_2013_2014_2015_jun_aug_20cl_1.tif")
# ndvi_2 = raster("l8_france_2013_2014_2015_jun_aug_20cl_2.tif")
# ndvi_3 = raster("l8_france_2013_2014_2015_jun_aug_20cl_3.tif")
# 
# grid = readOGR("./grid200", "grid200_clip", stringsAsFactors = FALSE)
# # grid = grid[sample(1:nrow(grid), 10), ]
# 
# tmp = spTransform(grid, proj4string(ndvi_1))
# writeOGR(tmp, ".", "grid200_clip_ndvi_1", driver = "ESRI Shapefile")
# grid$ndvi_1 = extract(ndvi_1, tmp)
# tmp = spTransform(grid, proj4string(ndvi_2))
# writeOGR(tmp, ".", "grid200_clip_ndvi_2", driver = "ESRI Shapefile")
# grid$ndvi_2 = extract(ndvi_2, tmp)
# tmp = spTransform(grid, proj4string(ndvi_3))
# writeOGR(tmp, ".", "grid200_clip_ndvi_3", driver = "ESRI Shapefile")
# grid$ndvi_3 = extract(ndvi_3, tmp)
# 
# grid$ndvi_final = apply(grid@data[, paste0("ndvi_", 1:3)], 1, max, na.rm = TRUE)
# 
# writeOGR(grid, ".", "grid200_clip_ndvi", driver = "ESRI Shapefile")
# 
# ##########################################################################
# # R version - 2
# 
# library(magrittr)
# library(raster)
# library(rgdal)
# 
# # setwd("/media/michael/Elements/france_ndvi")
# setwd("~/france_ndvi")
# 
# ndvi_1 = raster("l8_france_2013_2014_2015_jun_aug_20cl_1.tif")
# ndvi_2 = raster("l8_france_2013_2014_2015_jun_aug_20cl_2.tif")
# ndvi_3 = raster("l8_france_2013_2014_2015_jun_aug_20cl_3.tif")
# 
# ndvi_1 = aggregate(ndvi_1, 4)
# ndvi_2 = aggregate(ndvi_2, 4)
# ndvi_3 = aggregate(ndvi_3, 4)
# 
# grid = readOGR("./grid200", "grid200_clip", stringsAsFactors = FALSE)
# 
# tmp = spTransform(grid, proj4string(ndvi_1))
# grid$ndvi_1 = extract(ndvi_1, tmp)
# 
# tmp = spTransform(grid, proj4string(ndvi_2))
# grid$ndvi_2 = extract(ndvi_2, tmp)
# 
# tmp = spTransform(grid, proj4string(ndvi_3))
# grid$ndvi_3 = extract(ndvi_3, tmp)
# 
# grid$ndvi_final = apply(grid@data[, paste0("ndvi_", 1:3)], 1, max, na.rm = TRUE)
# grid$ndvi_final[apply(is.na(grid@data[, paste0("ndvi_", 1:3)]), 1, all)] = NA
# 
# writeOGR(grid, ".", "grid200_clip_aggregate4_ndvi", 
#          driver = "ESRI Shapefile", overwrite_layer = TRUE)
# 
# 
# ##########################################################################
# # QGIS version
# 
# library(magrittr)
# library(raster)
# library(rgdal)
# 
# # setwd("/media/michael/Elements/france_ndvi")
# setwd("~/france_ndvi")
# 
# grid = readOGR("./grid200", "grid200_clip", stringsAsFactors = FALSE)
# 
# tmp = readOGR(".", "grid200_clip_ndvi_1_final", stringsAsFactors = FALSE)
# grid$ndvi_1 = tmp$ndvi_1
#  
# grid$ndvi_final = apply(grid@data[, paste0("ndvi_", 1:3)], 1, max, na.rm = TRUE)
# grid$ndvi_final[apply(is.na(grid@data[, paste0("ndvi_", 1:3)]), 1, all)] = NA
# 
# writeOGR(grid, ".", "grid200_clip_ndvi_final", driver = "ESRI Shapefile")
# 




