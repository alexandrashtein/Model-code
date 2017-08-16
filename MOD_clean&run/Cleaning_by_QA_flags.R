
#load libraries 
library(reshape)
library(foreign) 
library(plyr)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(broom)
library(DataCombine)
library(readr)
library(bit64)
library(sp)
library(rgdal)
library(stringi)
library(data.table)

# Load the data (RAW MAIAC data per each yer, y=year)
maiac=fread(sprintf("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/MAIAC_data_082016/AQUA/MAIACAAOT_Israel_%s.csv", y))
maiac = as.data.table(maiac)

## set names abbreviations
setnames(maiac,"AOT_Uncertainty","UN")
setnames(maiac,"AOT_QA","QA")
setnames(maiac,"Optical_Depth_047","aod_047")
setnames(maiac,"Optical_Depth_055","aod_055")
setnames(maiac,"date","day")
setnames(maiac,"lon","long_aod")
setnames(maiac,"lat","lat_aod")

## Use the QA data to remove problematic observations

system.time(maiac[, CloudMask := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
system.time(maiac[, MaskLandWaterSnow := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
system.time(maiac[, MaskAdjacency := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
system.time(maiac[, CloudDetection := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[9:12]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
system.time(maiac[, GlintMask := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[13]), collapse = "")}, mc.cores = 15) %>% simplify2array)])
system.time(maiac[, AerosolModel := as.factor(parallel::mclapply(QA, function(x){paste(rev(as.integer(intToBits(x))[14:15]), collapse = "")}, mc.cores = 15) %>% simplify2array)])

# Make sure that the values that were created are reasonable (compare to the values in the table called "AOT_QA definition (16-bit unsigned integer)" MAIAC data specification document)
# summary(maiac$CloudMask)
# summary(maiac$MaskLandWaterSnow)
# summary(maiac$CloudDetection)
# summary(maiac$AerosolModel)
# summary(maiac$GlintMask)
# summary(maiac$MaskAdjacency)

# remove cloudy QA
maiac=filter(maiac,CloudMask!="011") # cloudy
maiac=filter(maiac,CloudMask!="010") # possibly cloudy
maiac=filter(maiac,CloudMask!="101") # cloud shadow
maiac=filter(maiac,MaskAdjacency!="010") # remove observations surrounded  by more than 8 cloudy pixels QA
## remove Adjacent to snow QA
maiac=filter(maiac,MaskAdjacency!="100") #snow
## remove water QA
maiac=filter(maiac,MaskLandWaterSnow!="01") # water
maiac=filter(maiac,MaskLandWaterSnow!="10") # snow