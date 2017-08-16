## This code converts PBL data from nc format to csv file

library(magrittr)
library(ncdf4)
library(raster)
library(reshape2)

setwd("~/Downloads")

filename = "_grib2netcdf-atls15-95e2cf679cd58ee9b4db4dd119a05a8d-6JOEWD.nc"
nc = nc_open(filename)

# Read time data
time = ncvar_get(nc, "time")
origin = as.POSIXct("1900-01-01 00:00:0.0", tz = "GMT")
diffs = as.difftime(time, format = "%H", units = "hours")
time = origin + diffs

# Read values data
r = brick(filename, varname = "blh")

# To 'data.frame'
dat = as.data.frame(r, xy = TRUE)
names(dat) = c("lon", "lat", as.character(time, usetz = TRUE))
dat = melt(
  dat, 
  id.vars = c("lon", "lat"), 
  variable.name = "time", 
  value.name = "hpbl"
)

# Round & Remove rows with no 'hpbl' value
dat$hpbl = round(dat$hpbl)
dat = dat[!is.na(dat$hpbl), ]

# Filter time frame
dat$time = as.POSIXct(dat$time, tz = "GMT")
dat = dat[dat$time >= as.POSIXct("2003-01-01 00:00:00 GMT"), ]

# Write CSV
write.csv(dat, "ecmwf_hpbl_israel_2003_2016_new.csv", row.names = FALSE)







