## This code joins all the spatial data into one keytable that will be used for the Israeli model
library(data.table)
library(dplyr)
library(sp)
library(rgdal)

grid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200_m_clean_keytable/200_m_clean_keytable.csv")
grid$X_Y=paste(grid$POINT_X,grid$POINT_Y,sep="-")
# grid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
# grid$V1=NULL
grid$ID=NULL
grid$il2014=NULL
setnames(grid,"POINT_X","X_ITM")
setnames(grid,"POINT_Y","Y_ITM")

# Subseting the 200 m grid to the project area only

grid=as.data.table(grid)
setkey(grid,X_Y)
grid_200=grid[!duplicated(X_Y)]
coordinates(grid_200) = ~ X_ITM + Y_ITM
proj4string(grid_200) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
# read project border shapefile
pol=readOGR(dsn="N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
newProj = CRS("+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs")
pol = spTransform(pol, newProj)
grid_200 = grid_200[pol, ]

plot(grid_200,col="RED")
plot(pol,add=TRUE)
# Convert back to data.table
grid = as.data.table(grid_200)

# setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins")

# Join Elevation data
elev_new=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/mean_Elev_200m_new.csv")
# elev_new$X_ITM=substr(elev_new$X_Y,1,11)
# elev_new$Y_ITM=substr(elev_new$X_Y,13,25)
# elev_new$X_ITM=as.numeric(elev_new$X_ITM)
# elev_new$Y_ITM=as.numeric(elev_new$Y_ITM)
# coordinates(elev_new) = ~ X_ITM + Y_ITM
# proj4string(elev_new) = "+proj=tmerc +lat_0=31.7343936111111 +lon_0=35.2045169444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-24.002400,-17.103200,-17.844400,-0.330090,-1.852690,1.669690,5.424800 +units=m +no_defs"
# plot(elev_new,add=TRUE)

grid=left_join(grid,elev_new,by="X_Y")
grid=as.data.table(grid)
grid$OBJECTID=NULL
setnames(grid,"MEAN","Elev")

# Join population density data
Pop_dens=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/zonal_mean_pop_den_200m_new2.csv")
grid=left_join(grid,Pop_dens,by="X_Y")
grid$Pop_dens=NULL
setnames(grid,"MEAN","Pop_dens")
grid=as.data.table(grid)
grid
summary(grid)

# Join Road density data
Road_den=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/mean_road_density_200m.csv")
grid=left_join(grid,Road_den,by="X_Y")
grid=as.data.table(grid)
setnames(grid,"MEAN","Road_den")
grid
summary(grid)

# Join Road distance data
Road_dis=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/mean_road_dis_200m_grid.csv")
grid=left_join(grid,Road_dis,by="X_Y")
grid=as.data.table(grid)
setnames(grid,"MEAN","Road_dis")
grid
summary(grid)

# Join distance to railways data
Dist_railways=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/Mean_dist_Railways_200m_new.csv")
grid=left_join(grid,Dist_railways,by="X_Y")
grid=as.data.table(grid)
setnames(grid,"MEAN","Dis_Railways")
grid
summary(grid)

# Join percent industrial area (2004) data
P_In=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/P_In_200m.csv")
grid=left_join(grid,P_In,by="X_Y")
grid=as.data.table(grid)
grid
setnames(grid,"Percent","P_In_2004")
summary(grid)

# Join percent urban area (2004) data
P_Urban=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/P_Urban_200m.csv")
grid=left_join(grid,P_Urban,by="X_Y")
grid=as.data.table(grid)
grid=as.data.table(grid)
grid
setnames(grid,"Percent","P_Ur_2004")
summary(grid)

# Join spring 2000-2002 Landsat ndvi data
Sp_NDVI0002=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/spring_0002_mean_NDVI.csv")
grid=left_join(grid,Sp_NDVI0002,by="X_Y")
grid=as.data.table(grid)
grid
setnames(grid,"MEAN","Sp_NDVI0002")
summary(grid)

# Join spring 2014 Landsat ndvi data
Sp_NDVI14=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/spring_14_mean_NDVI_new.csv")
Sp_NDVI14$X_Y=paste(substr(Sp_NDVI14$X_Y,1,11),substr(Sp_NDVI14$X_Y,13,25),sep = "-")
grid=left_join(grid,Sp_NDVI14,by="X_Y")
grid=as.data.table(grid)
setnames(grid,"MEAN","Sp_NDVI14")
summary(grid)

# Join summer 2000-2002 Landsat ndvi data
Su_NDVI0002=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/summer_0002_mean_NDVI.csv")
Su_NDVI0002[MEAN>1,MEAN:=NA]
grid=left_join(grid,Su_NDVI0002,by="X_Y")
grid=as.data.table(grid)
setnames(grid,"MEAN","Su_NDVI0002")
summary(grid)

# Join summer 2014 Landsat ndvi data
Su_NDVI14=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/summer_14_mean_NDVI_new2.csv")
setnames(Su_NDVI14,"Unique","X_Y")
Su_NDVI14$X_Y=paste(substr(Su_NDVI14$X_Y,1,11),substr(Su_NDVI14$X_Y,13,25),sep = "-")
Su_NDVI14[MEAN>1,MEAN:=NA]
grid=left_join(grid,Su_NDVI14,by="X_Y")
grid=as.data.table(grid)
setnames(grid,"MEAN","Su_NDVI14")
summary(grid)

# Join MODIS NDVI id data
NDVIid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/200m_grid_NDVIid/200_m_grid_NDVIid.csv")
grid=left_join(grid,NDVIid,by="X_Y")
grid=as.data.table(grid)
summary(grid)

# Add 2014 land use for 200m grid

# Join percent agricultural areas 2014 data
P_ag=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse2014/LU_zonal_stat_tables/200m_grid/ag_pr.csv")
P_ag[,c("COUNT","SUM")]=NULL
grid=left_join(grid,P_ag,by="X_Y")
grid=as.data.table(grid)
setnames(grid,"ag_pr","P_Ag_2014")
summary(grid)

# Join percent industrial and mining areas 2014 data
P_In_Min=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse2014/LU_zonal_stat_tables/200m_grid/In_Min_pr.csv")
P_In_Min[,c("COUNT","SUM")]=NULL
grid=left_join(grid,P_In_Min,by="X_Y")
grid=as.data.table(grid)
grid
setnames(grid,"In_Min_Pr","P_In_Min_2014")
summary(grid)

# Join percent open space and agricultural areas 2014 data
P_os_ag=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse2014/LU_zonal_stat_tables/200m_grid/os_ag_pr.csv")
P_os_ag[,c("COUNT","SUM","V5","V6","V7")]=NULL
grid=left_join(grid,P_os_ag,by="X_Y")
grid=as.data.table(grid)
grid
setnames(grid,"os_ag_pr","P_OS_Ag_2014")
summary(grid)

# Join percent open space areas 2014 data
P_os=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse2014/LU_zonal_stat_tables/200m_grid/os_pr.csv")
P_os[,c("COUNT","SUM")]=NULL
grid=left_join(grid,P_os,by="X_Y")
grid=as.data.table(grid)
grid
setnames(grid,"os_pr","P_OS_2014")
summary(grid)

# Join percent urban areas 2014 data
P_ur=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse2014/LU_zonal_stat_tables/200m_grid/ur_pr.csv")
P_ur[,c("COUNT","SUM")]=NULL
grid=left_join(grid,P_ur,by="X_Y")
grid=as.data.table(grid)
grid
setnames(grid,"Ur_pr","P_Ur_2014")
summary(grid)

# Add PBL id to the 200 m grid
grid= fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
PBLid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/200m_grid_PBLid/200m_grid_PBLid.csv")
PBLid$pblid=paste(PBLid$lon,PBLid$lat, sep="-")
grid=left_join(grid,PBLid,by="X_Y")
grid=as.data.table(grid)
grid
summary(grid)

# Add Distance to emission points 
grid= fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
emission=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/200m_grid_emission/200m_grid_emission.csv")
grid=left_join(grid,emission,by="X_Y")
grid=as.data.table(grid)
grid
summary(grid)

write.csv(grid, "N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data_fixed.csv")

# Add 2004 land use for 1km grid
grid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid.csv")
grid=as.data.table(grid)
grid[,V1:=NULL]
P_Ur=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse_2004/Landuse_Tables_by_squares/P_Ur_2004_new.csv")
P_Ur=data.frame(aodid=P_Ur$AODID,P_Ur_2004=P_Ur$P_Ur_2004)

mean(P_Ur$aodid %in% grid$aodid)
P_Ur=as.data.table(P_Ur)
P_Ur$aodid=as.character(P_Ur$aodid)
grid=left_join(grid,P_Ur,by="aodid")
grid=as.data.table(grid)
grid
summary(grid)

write.csv(grid,"N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid_new.csv")

# Add 2014 land use for 1km grid

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid")
grid_1km = read.csv("1km_MAIAC_grid.csv")
head(grid_1km)

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Landuse/Landuse2014/LU_zonal_stat_tables/1km_grid")
P_ag=read.csv("ag_pr.csv")
P_ag=P_ag[,-3]
P_ag=P_ag[,-2]
P_In_Min=read.csv("In_Min_pr.csv")
P_In_Min=P_In_Min[,-3]
P_In_Min=P_In_Min[,-2]
P_os_ag=read.csv("os_ag_pr.csv")
P_os_ag=P_os_ag[,-3]
P_os_ag=P_os_ag[,-2]
P_os=read.csv("os_pr.csv")
P_os=P_os[,-3]
P_os=P_os[,-2]
P_ur=read.csv("Ur_pr.csv")
P_ur=P_ur[,-3]
P_ur=P_ur[,-2]

head(P_ag)
head(P_os_ag)
head(P_ur)
head(P_os)
head(P_In_Min)

all_LU_1Km=join(P_ag,P_In_Min,by="AODID")
all_LU_1Km=join(all_LU_1Km,P_os,by="AODID")
all_LU_1Km=join(all_LU_1Km,P_os_ag,by="AODID")
all_LU_1Km=join(all_LU_1Km,P_ur,by="AODID")
head(all_LU_1Km)

names(all_LU_1Km)=c("aodid", "ag_pr14","In_Min_Pr14","os_pr14","os_ag_pr14","Ur_pr14")

grid_1km=join(grid_1km,all_LU_1Km,by="aodid")

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid")
write.csv(grid_1km,"1km_MAIAC_grid.csv")

# adding PBL and MODIS ndvi ID

all_LU_1Km=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid_new.csv")
NDVIid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS/NDVI_join/1_km_MAIAC_grid_ndviid.csv")
setnames(NDVIid,"AODID","aodid")

# Add PBLid- correct the id
PBLid=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/lu_pbl.csv")
PBLid$pblid=paste(formatC(round(PBLid$POINT_X,3),format='f',3),formatC(round(PBLid$POINT_Y,3),format='f',3),sep="-")
lu=fread("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid_new.csv")
lu[,pblid:=NULL]
mean(PBLid$aodid %in% lu$aodid)

setkey(lu,aodid)
setkey(PBLid,aodid)
lu=merge(lu,PBLid,all.x = T)
setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid")
write.csv(lu,"1km_MAIAC_grid_fixed.csv")

# correct pblid

cr=fread("D:/Temp/empty_join2.csv")
cr$aodid_2=paste(formatC(round(cr$long_aod,3),format='f',3),formatC(round(cr$lat_aod,3),format='f',3),sep="-")
cr=data.table(aodid=cr$aodid,aodid_2=cr$aodid_2)
PBLid=left_join(PBLid,cr,by="aodid_2")
PBLid$aodid_3= as.numeric(!is.na(PBLid$aodid_2))

PBLid= data.frame(aodid=PBLid$aodid,pblid=PBLid$aodid)
PBLid=as.data.table(PBLid)
PBLid$aodid=as.character(PBLid$aodid)
all_LU_1Km=left_join(all_LU_1Km,NDVIid,by="aodid")
all_LU_1Km=left_join(all_LU_1Km,PBLid,by="aodid")
all_LU_1Km=as.data.table(all_LU_1Km)
all_LU_1Km[,c("POINT_X_1","POINT_Y_1"):=NULL]

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid")
write.csv(all_LU_1Km,"1km_MAIAC_grid_fixed.csv")

setwd("N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/NDVI/MODIS")
NDVI=readRDS("ndvi_2000_2014.RDS")

