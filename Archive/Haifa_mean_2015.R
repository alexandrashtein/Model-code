library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
library(readr)
library(bit64)
library(devtools)
library(sp)
library(rgdal)
library(stringi)

mod3best=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/Best_pred/bestpred.AQ_Daily_clean.2003_2015.PM25.rds")
mod3best<-filter(mod3best,!is.na(bestpred))
mod3best$year=substr(mod3best$day,1,4)
mod3best<-filter(mod3best,year=="2015")

#add season
mod3best$month <- as.numeric(substr(mod3best$day,6,7))
#1-winter, 2-spring,3-summer,4-autum
mod3best$season<-car::recode(mod3best$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
mod3best$seasonSW<-car::recode(mod3best$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

### Plot only 2015 year
mod3best=as.data.table(mod3best)

### read polygons 
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/cities_polygons","Haifa_City")
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/cities_polygons","Haifa area")
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/cities_polygons","Rishon_lezion_city")
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/cities_polygons","Rehovot")

# Convert to data.frame
mod3best = as.data.frame(mod3best)
# Spatial subset
coordinates(mod3best) = ~ lon_aod + lat_aod
proj4string(mod3best) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mod3best = mod3best[pol, ]
# Convert back to data.table
mod3best = as.data.table(mod3best)

seanonal_mean_Haifa=ddply(mod3best,~season,summarize,mean(bestpred))
seanonal_mean_Haifa_SW=ddply(mod3best,~seasonSW,summarize,mean(bestpred))
  
out <- mod3best_2015 %>% group_by(aodid) %>%
  summarise(lon=mean(lon_aod, na.rm=TRUE), lat =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))

ggplot() +  geom_point(data=out, aes(x=lon, y=lat,color=bestpred),alpha=1, size=1)+
  scale_color_gradient("PM25 prediction",low="lightblue2",high="red")+ ggtitle("mean PM25 prediction for 2015")
