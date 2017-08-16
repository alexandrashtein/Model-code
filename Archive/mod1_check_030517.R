
# Add Daily PM2.5
mod1  <- fread("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Daily_Data/Pollution_stn_May16/PM25_D.csv")
mod1$date<-paste(mod1$Day,mod1$Month,mod1$Year,sep="/")
mod1[, day:=as.Date(date, "%d/%m/%Y")]

library(data.table)
library(dplyr)

## How many stations in each days? Before cleaning

stn_num <- mod1 %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(num=length(unique(stn)))

stn_num =as.data.table(stn_num)
summary(stn_num)
hist(stn_num$num)
nrow(stn_num[num==1,])
nrow(stn_num[num==1,])/nrow(stn_num)

PM25_Daily<-PM25_Daily[PM25 > 0.000000000001 & PM25 < 1000 ]
