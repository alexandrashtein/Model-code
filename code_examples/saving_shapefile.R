library(sp)
coordinates(out) = ~ lon+ lat
proj4string(out) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
out_pnt = spTransform(
 out, 
 
 "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
)
library(rgdal)
writeOGR(out,dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/",
         layer="PM10_annual_bestpred_no_GAM_2003_2015", driver="ESRI Shapefile")


out$x = coordinates(out_pnt)[, 1]
out$y = coordinates(out_pnt)[, 2]

out=as.data.table(out)
names(out)