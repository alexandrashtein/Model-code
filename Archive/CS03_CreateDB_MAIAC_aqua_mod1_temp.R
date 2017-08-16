############################################### Create MOD2 ##########################################

# Scale mod2

db2003.m2=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/Data_buliding_Dec16/AQ.MAIAC.2003.mod2.rds")

names=c("daily_hpbl","ndvi","Elev","dis_inventory","Dis_Mroads","road_den","Pop_dens","Dis_Rd1_2012","Dis_Rd2_2012","Dist_Railw",
        "Dist_WB","Temp_D","P_In_Min_2014","P_OS_2014","P_Ur_2014","P_Ag_2014", "P_In_2004","P_OS_2004","P_Ur_2004","P_Ag_2004",
        "WS_D","RH_D","Rain_D","NO2_D" ,"SO2_D","pbl_02","pbl_11")


# all(names %in% names(db2003.m2))
# mean(names %in% names(db2003.m2))
# a=names %in% names(db2003.m2)
# b=names
# b[which(a==FALSE)]


db2003.m2 = db2003.m2 %>% as.data.frame 
scaled = db2003.m2[,names] %>% dplyr::mutate_each(funs(scale))
colnames(scaled) = paste0(colnames(scaled), ".s")
db2003.m2= cbind(db2003.m2, scaled)
names(db2003.m2)
summary(db2003.m2)
db2003.m2=as.data.table(db2003.m2)

#save mod2
# saveRDS(db2003.m2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/TR.MAIAC.2003.mod2.rds")
saveRDS(db2003.m2,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/AQ.MAIAC.2003.mod2.rds")
gc()

############################################# building model 1 -MOD1##################################

### Create daily PM2.5 mod1
# daily database
PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM25_D.csv")

PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y",tz="GMT"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
PM25<-PM25[PM25 > 0.000000000001 & PM25 < 1000 ]

# Add field classification (General or transportation)
PM_Type <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM_monitors_classification/PM_monitors.csv")
setnames(PM_Type,"Code","stn")
PM_Type$stn=substr(PM_Type$stn, 2, 4)
PM25=dplyr::left_join(PM25,PM_Type,by="stn")
PM25=as.data.table(PM25)
PM25[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM25$stn_type<-0
PM25[Type=="'Gener'",stn_type:=1]
PM25[Type=="'Trans'",stn_type:=0]
PM25[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2003<- PM25[c==2003]
setnames(pmall2003,"X.x","x_stn_ITM")
setnames(pmall2003,"Y.x","y_stn_ITM")

# ADD AOD 055 to PM25 mod 1
jointo.pt <- makepointsmatrix(datatable = pmall2003, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2003.m2, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2003 , joinfrom =db2003.m2, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = TRUE, verbose = T)

setkey(pmall2003,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(pmall2003, joinout, all.x = T)

PM25.m1<-PM25.m1[!is.na(aod_055)]
setnames(PM25.m1,"nearestmean", "aod_055_mean")

PM25.m1[,nearestknn:=NULL]
PM25.m1[,nearestnobs:=NULL]
PM25.m1[,c.y:=NULL]
setnames(PM25.m1,"c.x", "year")

# ADD AOD 047
db2003.m2_s=db2003.m2[, c("aodid","x_aod_ITM","y_aod_ITM","aod_047","day"), with = FALSE]

jointo.pt <- makepointsmatrix(datatable = PM25.m1, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable =db2003.m2_s, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = PM25.m1 , joinfrom = db2003.m2_s, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = TRUE, verbose = T)

setkey(PM25.m1,stn,day)
setkey(joinout,stn,day)
PM25.m1<- merge(PM25.m1, joinout, all.x = T)

setnames(PM25.m1,"nearestmean", "aod_047_mean")
setnames(PM25.m1,"aod_047.x", "aod_047")
setnames(PM25.m1,"x_aod_ITM.x", "x_aod_ITM")
setnames(PM25.m1,"y_aod_ITM.x", "y_aod_ITM")
PM25.m1[,c("nearest.x","nearestknn","nearestnobs","x_aod_ITM.y", "y_aod_ITM.y", "aod_047.y","nearest.y"):=NULL]

# Join 200 m spatial variables
# add 200 m key field to the database
key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

setkey(PM25.m1,stn)
setkey(key_field,stn)
PM25.m1= merge(PM25.m1,key_field,all.x = T)

lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")

setkey(lu_200m,Key200_id)
setkey(PM25.m1,Key200_id)
PM25.m1 <- merge(PM25.m1, lu_200m, all.x = T)
PM25.m1[,c("X_ITM_200m","Y_ITM_200m","V1_200m"):=NULL]
setnames(PM25.m1,"aod_047.x" ,"aod_047")

PM25.m1_D=PM25.m1
# # delete hourly meteorological variables
PM25.m1_D[,c("Temp_H","WS_H" ,"RH_H","Rain_H","NO2_H" ,"SO2_H","PM25_D_closest","PM25_D_mean","PM25_IDW","PM10_D_closest","PM10_IDW","PM10_H_mean","vc_H","PM25_H_mean"):=NULL]

 summary(PM25.m1_D)
# Save RDS files
saveRDS(PM25.m1_D,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/mod1.AQ.2003.PM25_Daily.rds")

##### Create Hourly PM2.5 mod1

# hourly terra database
# PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/PM25_H.csv")
# hourly aqua database
PM25 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/PM25_H.csv")

PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y",tz="GMT"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
PM25<-PM25[PM25 > 0.000000000001 & PM25 < 1000 ]

# Add field classification (General or transportation)
PM_Type <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM_monitors_classification/PM_monitors.csv")
setnames(PM_Type,"Code","stn")
PM_Type$stn=substr(PM_Type$stn, 2, 4)
PM25=dplyr::left_join(PM25,PM_Type,by="stn")
PM25=as.data.table(PM25)
PM25[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM25$stn_type<-0
PM25[Type=="'Gener'",stn_type:=1]
PM25[Type=="'Trans'",stn_type:=0]
PM25[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2003<- PM25[c==2003]
setnames(pmall2003,"X.x","x_stn_ITM")
setnames(pmall2003,"Y.x","y_stn_ITM")

# ADD AOD 055 to PM25 mod 1
jointo.pt <- makepointsmatrix(datatable = pmall2003, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2003.m2, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2003 , joinfrom = db2003.m2, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = TRUE, verbose = T)

setkey(pmall2003,stn,day)
setkey(joinout,stn,day)
PM25.m1 <- merge(pmall2003, joinout, all.x = T)

PM25.m1<-PM25.m1[!is.na(aod_055)]
setnames(PM25.m1,"nearestmean", "aod_055_mean")

PM25.m1[,nearestknn:=NULL]
PM25.m1[,nearestnobs:=NULL]
PM25.m1[,c.y:=NULL]
setnames(PM25.m1,"c.x", "year")

# ADD AOD 047
db2003.m2_s=db2003.m2[, c("aodid","x_aod_ITM","y_aod_ITM","aod_047","day"), with = FALSE]

jointo.pt <- makepointsmatrix(datatable = PM25.m1, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2003.m2_s, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = PM25.m1 , joinfrom = db2003.m2_s, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = TRUE, verbose = T)

setkey(PM25.m1,stn,day)
setkey(joinout,stn,day)
PM25.m1<- merge(PM25.m1, joinout, all.x = T)

setnames(PM25.m1,"nearestmean", "aod_047_mean")
setnames(PM25.m1,"aod_047.x", "aod_047")
setnames(PM25.m1,"x_aod_ITM.x", "x_aod_ITM")
setnames(PM25.m1,"y_aod_ITM.x", "y_aod_ITM")
PM25.m1[,c("nearest.x","nearestknn","nearestnobs","x_aod_ITM.y", "y_aod_ITM.y", "aod_047.y","nearest.y"):=NULL]

# Join 200 m spatial variables
# add 200 m key field to the database
key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

setkey(PM25.m1,stn)
setkey(key_field,stn)
PM25.m1= merge(PM25.m1,key_field,all.x = T)

lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")

setkey(lu_200m,Key200_id)
setkey(PM25.m1,Key200_id)
PM25.m1 <- merge(PM25.m1, lu_200m, all.x = T)
PM25.m1[,c("X_ITM_200m","Y_ITM_200m","V1_200m"):=NULL]
setnames(PM25.m1,"aod_047.x" ,"aod_047")

PM25.m1_H=PM25.m1
# delete daily meteorological variables
PM25.m1_H[,c("c.y","Temp_D","WS_D" ,"RH_D","Rain_D","NO2_D","SO2_D","PM25_D_closest","PM25_D_mean","PM25_IDW","PM10_D_closest","PM10_IDW","PM10_H_mean","vc_D"):=NULL ] 
names(PM25.m1_H)
# summary(PM25.m1_H)

# Save RDS files
saveRDS(PM25.m1_H,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Hourly/mod1.AQ.2003.PM25_Hourly.rds")
# saveRDS(PM25.m1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2003.PM25_Daily.rds")
# saveRDS(PM25.m1,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2003.PM25_Hourly.rds")

# Create mod 1 for PM10

### PM10 mod1 Daily
# daily database
PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM10_D.csv")

PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y",tz="GMT"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
PM10<-PM10[PM10 > 0.000000000001 & PM10 <  2000 ]

# Add field classification (General or transportation)
PM_Type <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM_monitors_classification/PM_monitors.csv")
setnames(PM_Type,"Code","stn")
PM_Type$stn=substr(PM_Type$stn, 2, 4)
PM10=left_join(PM10,PM_Type,by="stn")
PM10=as.data.table(PM10)
PM10[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM10$stn_type<-0
PM10[Type=="'Gener'",stn_type:=1]
PM10[Type=="'Trans'",stn_type:=0]
PM10[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2003<- PM10[c==2003]
setnames(pmall2003,"X.x","x_stn_ITM")
setnames(pmall2003,"Y.x","y_stn_ITM")

# ADD AOD 055 to MOD1
jointo.pt <- makepointsmatrix(datatable = pmall2003, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2003.m2, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2003 , joinfrom = db2003.m2, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = TRUE, verbose = T)

setkey(pmall2003,stn,day)
setkey(joinout,stn,day)
PM10.m1 <- merge(pmall2003, joinout, all.x = T)

PM10.m1<-PM10.m1[!is.na(aod_055)]
setnames(PM10.m1,"nearestmean", "aod_055_mean")

PM10.m1[,nearestknn:=NULL]
PM10.m1[,nearestnobs:=NULL]
PM10.m1[,c.y:=NULL]
setnames(PM10.m1,"c.x", "year")

# ADD AOD 047
db2003.m2_s=db2003.m2[, c("aodid","x_aod_ITM","y_aod_ITM","aod_047","day"), with = FALSE]

jointo.pt <- makepointsmatrix(datatable = PM10.m1, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2003.m2_s, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = PM10.m1 , joinfrom = db2003.m2_s, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = TRUE, verbose = T)

setkey(PM10.m1,stn,day)
setkey(joinout,stn,day)
PM10.m1<- merge(PM10.m1, joinout, all.x = T)

setnames(PM10.m1,"nearestmean", "aod_047_mean")
setnames(PM10.m1,"aod_047.x", "aod_047")
PM10.m1[,c("nearest.x","nearestknn","nearestnobs","x_aod_ITM.y", "y_aod_ITM.y", "aod_047.y","nearest.y"):=NULL]

# Join 200 m spatial variables
# add 200 m key field to the database
key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

setkey(PM10.m1,stn)
setkey(key_field,stn)
PM10.m1= merge(PM10.m1,key_field,all.x = T)

lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")

setkey(lu_200m,Key200_id)
setkey(PM10.m1,Key200_id)
PM10.m1 <- merge(PM10.m1, lu_200m, all.x = T)
PM10.m1[,c("X_ITM_200m","Y_ITM_200m"):=NULL]
setnames(PM10.m1,"aod_047.x" ,"aod_047")

PM10.m1_D=PM10.m1
# delete unneeded variables from daily database
PM10.m1_D[,c("aod_047.x","aod_055","nearest.x","Temp_H","WS_H" ,"RH_H","Rain_H","NO2_H" ,"SO2_H","PM25_D_closest","PM25_D_mean","PM25_IDW","PM25_H_mean","PM10_H_mean","PM10_D_closest","PM10_IDW","vc_H","V1_200m","c.y","lon_200m","lat_200m","m"):=NULL] 
setnames(PM10.m1_D,"c.x","c")
names(PM10.m1_D)
# summary(PM10.m1_D)

# Save RDS files
saveRDS(PM10.m1_D,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Daily/mod1.AQ.2003.PM10_Daily.rds")
# saveRDS(PM10.m1_D,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2003.PM10_Daily.rds")

### PM10 mod1 Hourly

# hourly terra database
# PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16/PM10_H.csv")
# hourly aqua database
PM10 <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/AQUA_Hourly_data_May16/PM10_H.csv")

PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y",tz="GMT"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
PM10<-PM10[PM10 > 0.000000000001 & PM10 <  2000 ]


# Add field classification (General or transportation)
PM_Type <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM_monitors_classification/PM_monitors.csv")
setnames(PM_Type,"Code","stn")
PM_Type$stn=substr(PM_Type$stn, 2, 4)
PM10=left_join(PM10,PM_Type,by="stn")
PM10=as.data.table(PM10)
PM10[,c("Name","Region","X.y","Y.y","Long","Lat","HASL","HAGL","Parameters"):=NULL]
PM10$stn_type<-0
PM10[Type=="'Gener'",stn_type:=1]
PM10[Type=="'Trans'",stn_type:=0]
PM10[Type=="'NaN'",stn_type:=2]

#clear non continous stations
pmall2003<- PM10[c==2003]
setnames(pmall2003,"X.x","x_stn_ITM")
setnames(pmall2003,"Y.x","y_stn_ITM")

#--------->mod1
#PM10
# ADD AOD 055
jointo.pt <- makepointsmatrix(datatable = pmall2003, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2003.m2, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = pmall2003 , joinfrom = db2003.m2, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_055", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = TRUE, verbose = T)

setkey(pmall2003,stn,day)
setkey(joinout,stn,day)
PM10.m1 <- merge(pmall2003, joinout, all.x = T)

PM10.m1<-PM10.m1[!is.na(aod_055)]
setnames(PM10.m1,"nearestmean", "aod_055_mean")

PM10.m1[,nearestknn:=NULL]
PM10.m1[,nearestnobs:=NULL]
PM10.m1[,c.y:=NULL]
setnames(PM10.m1,"c.x", "year")

# ADD AOD 047

jointo.pt <- makepointsmatrix(datatable = PM10.m1, 
                              xvar = "x_stn_ITM", yvar = "y_stn_ITM", idvar = "stn") 

joinfrom.pt <- makepointsmatrix(datatable = db2003.m2_s, 
                                xvar = "x_aod_ITM", yvar = "y_aod_ITM", idvar = "aodid") 

joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
                        jointo = PM10.m1 , joinfrom = db2003.m2_s, 
                        jointovarname = "stn", joinfromvarname = "aodid", 
                        joinprefix = "nearest", valuefield = "aod_047", 
                        knearest = 9, maxdistance = 1500, 
                        nearestmean = TRUE, verbose = T)

setkey(PM10.m1,stn,day)
setkey(joinout,stn,day)
PM10.m1<- merge(PM10.m1, joinout, all.x = T)

setnames(PM10.m1,"nearestmean", "aod_047_mean")
setnames(PM10.m1,"aod_047.x", "aod_047")
PM10.m1[,c("nearest.x","nearestknn","nearestnobs","x_aod_ITM.y", "y_aod_ITM.y", "aod_047.y","nearest.y"):=NULL]

# Join 200 m spatial variables
# add 200 m key field to the database
key_field=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/Joins/PM25_stn_200m_keytable_id/PM25_stn_200m_keytable_id.csv")
key_field=as.data.table(key_field)
key_field$Key200_id <- paste0(key_field$POINT_X,"-",key_field$POINT_Y)
setnames(key_field, "Code","stn")
key_field$stn=substr(key_field$stn,2,4)
key_field=key_field[,.(stn,Key200_id)]

setkey(PM10.m1,stn)
setkey(key_field,stn)
PM10.m1= merge(PM10.m1,key_field,all.x = T)

lu_200m=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/200m_grid/200m_grid_spatial_Data.csv")
setnames(lu_200m,"X_Y","Key200_id")
lu_200m$V1=NULL
colnames(lu_200m) <- paste(colnames(lu_200m),"200m", sep = "_")
setnames(lu_200m,"Key200_id_200m","Key200_id")

setkey(lu_200m,Key200_id)
setkey(PM10.m1,Key200_id)
PM10.m1 <- merge(PM10.m1, lu_200m, all.x = T)
PM10.m1[,c("X_ITM_200m","Y_ITM_200m"):=NULL]
setnames(PM10.m1,"aod_047.x" ,"aod_047")

PM10.m1_H=PM10.m1
# delete unneeded variables from hourly database
PM10.m1_H[,c("aod_047.x","aod_055","nearest.x","Temp_D","WS_D" ,"RH_D","Rain_D","NO2_D","SO2_D","PM25_D_closest","PM25_D_mean","PM25_IDW","PM25_H_mean","PM10_H_mean","PM10_D_closest","PM10_IDW","vc_D")] =NULL 
summary(PM10.m1_H)

saveRDS(PM10.m1_H,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM10_Hourly/mod1.AQ.2003.PM10_Hourly.rds")
# saveRDS(PM10.m1_H,"/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.TR.2003.PM10_Hourly.rds")

summary(PM10.m1_H)
summary(PM10.m1_D)
summary(PM25.m1_H)
summary(PM25.m1_D)
