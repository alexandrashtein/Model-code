############################# Load PM25 data ################################################

# Add Daily PM2.5
PM25_Daily  <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM25_D.csv")

maiac=readRDS(paste0("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/raw_data/AOD_after_QA.AQ.",2015,".RDS"))

############################# Load 1 km Spatial data ########################################

lu<-fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Keytables/1km_grid/1km_MAIAC_grid.csv")
lu$V1=NULL
# Project border layer
pol=readOGR(dsn="/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Qgis/General/Project_border/Project_aoi","Project_border_latlon")
# Grid for correcting aodid
replace=fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Temp/correcting_grid/aod_to_replace.csv")
# load clipped/LU grid 
## replacing aodids not indetical to the lu aodids
all(replace$aodid %in% lu$aodid)
setkey(lu,aodid)
setkey(replace,aodid)
lu=merge(lu,replace,all.x = T)
lu$correct[is.na(lu$correct)] =lu$aodid[is.na(lu$correct)]
lu=as.data.table(lu)
lu[,aodid:=NULL]
setnames(lu,"correct","aodid")


###################################################################### Create db ################################################

# Make sure
all(lu$aodid %in% maiac$aodid)
mean(lu$aodid %in% maiac$aodid)
maiac <- maiac[maiac$aodid %in% lu$aodid, ]

#create full LU-aod TS
days<-seq.Date(from = as.Date(paste0(y,"-01-01")), to = as.Date(paste0(y,"-12-31")), 1)
#create date range
# days <- data.table(expand.grid(aodid = lu[, unique(aodid)], day = days))
days <- data.table(expand.grid(aodid = lu$aodid, day = days))
days$aodid<-as.character(days$aodid)

#merge maiac data
setkey(maiac,aodid,day)
setkey(days,aodid,day)
db <- merge(days,maiac, all.x = T)

# precentage of NA in the data
sum(is.na(db$aod_055))*100/length(db$aod_055)

#add land use data
setkey(db,aodid)
setkey(lu,aodid)
db<- merge(db, lu, all.x = T)
summary(db)
gc()

db[,.(aodid,long_aod,lon,lat_aod,lat)]
db[,c("long_aod","lat_aod"):=NULL]
setnames(db,"lon","lon_aod")
setnames(db,"lat","lat_aod")

summary(db)
    
##################################################################### Add IDW PM2.5 - PARALLER 

PM25=PM25_Daily 
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(date, "%d/%m/%Y")]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-as.data.table(as.data.frame(PM25)[!is.na(PM25$PM25), ]) 
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall<- PM25[c==year]

library(foreach)
library(doMC)
library(gstat)
registerDoMC(cores=50)

final = foreach(i = unique(db$day)) %dopar% {
  x<-pmall[pmall$day==i, ]
  prd= db[db$day==i, ]
  #defaults to idw (gstat)
  # library(sp)
  coordinates(x) = ~ x_stn_ITM + y_stn_ITM
  coordinates(prd) = ~ x_aod_ITM + y_aod_ITM
  #location statment uneeded since we defined coordinates
  inter = gstat(formula = PM25 ~ 1,  data =x)
  prd$pred<-predict(object = inter, newdata = prd)
  prd=as.data.frame(prd)
  #prd@data - this try did not work
}
# final = lapply(final, as.data.table)
db = do.call(rbind, final)
db = as.data.table(db)
setnames(db,"pred","PM25_IDW") # change name
