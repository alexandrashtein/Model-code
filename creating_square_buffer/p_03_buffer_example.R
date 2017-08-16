library(sp)
library(rgeos)

# Load grid
grid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid.csv")
grid=data.frame(lon=grid$lon,lat=grid$lat,aodid=grid$aodid)
grid$aodid=paste(formatC(round(grid$long_aod,3),format='f',3),formatC(round(grid$lat_aod,3),format='f',3),sep="-")
coordinates(grid) = ~ lon + lat
proj4string(grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grid = spTransform(grid, "+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs")
plot(grid)
# Buffer
b = gBuffer(grid[1:12443, ], byid = TRUE, width = 500, capStyle = "SQUARE")
plot(b, border = rainbow(5))

writeOGR(b,dsn="D:/Temp", layer="square_1km_grid_NEW4", driver="ESRI Shapefile")
