library(dplyr)

# Load mod3
mod3=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod3_pred/mod3.PM25.pred.AQ_Daily_clean.2003_2015.rds")
mod3=as.data.table(mod3)
mod3$year=year(mod3$day)
# mod3=dplyr::filter(mod3, year==2003) # Take out one year
mod3$bimon = (mod3$m + 1) %/% 2
mod3$bimon=paste(mod3$bimon,mod3$year,sep=".")

# Load mod2 
mod2<- readRDS( "/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2_pred/mod2.AQ_Daily_clean.2003_2015.PM25.predm2.rds")
mod2=as.data.table(mod2)
mod2$year=year(mod2$day)
# mod2=filter(mod2, year==2003) # Take out one year
mod2$m=month(mod2$day)
# mod2=as.data.table(mod2)
mod2<-mod2[,c("aodid","day","pred.m2","m","year"),with=FALSE]
mod2$bimon = (mod2$m + 1) %/% 2
mod2$bimon=paste(mod2$bimon,mod2$year,sep=".")

# Load mod1
mod1 <-readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1_pred/mod1_Daily.AQ.2003_2015.PM25_clean.predm1.rds")
mod1=as.data.table(mod1)
mod1$aodid=paste(formatC(round(mod1$lon_aod,3),format='f',3),formatC(round(mod1$lat_aod,3),format='f',3),sep="-")
mod1$year=year(mod1$day)  
mod1$m=month(mod1$day)  
# mod1=filter(mod1, year==2003) # Take out one year
mod1=as.data.table(mod1)
mod1<-mod1[,c("aodid","day","PM25","pred.m1","stn","m","year"),with=FALSE]
mod1$bimon = (mod1$m + 1) %/% 2
mod1$bimon=paste(mod1$bimon,mod1$year,sep=".")

## Create results table

#-------------------->> RES TABLE
res <- matrix(nrow=10, ncol=78)
res <- data.frame(res)
rownames(res) <- c("m3.R2.GAM","m3.R2.mix","m3.RMSE.GAM","m3.RMSE.mix","m3.spatial.R2.GAM","m3.spatial.R2.MIX","m3.spatial.RMSE.GAM","m3.spatial.RMSE.MIX","m3.temporal.R2.GAM","m3.temporal.R2.MIX")

## In each bimon calculate three performance measures
mod3_split = split(mod3, mod3$bimon) #split the data to bi-monthly periods, in each year (overall 78 periods)
mod2_split = split(mod2, mod2$bimon) #split the data to bi-monthly periods, in each year (overall 78 periods)
mod1_split = split(mod1, mod1$bimon) #split the data to bi-monthly periods, in each year (overall 78 periods)

# final=list()
for (i in 1:78) # calculate model performance mesures for each bimon
{
  mod3_s=mod3_split[[i]] # Filter mod3 to a certain year and bimon
  mod1_s=mod1_split[[i]] # Filter mod1 to a certain year and bimon

  setkey(mod3_s,day,aodid)
  setkey(mod1_s,day,aodid)
  mod1_s <- merge(mod1_s,mod3_s[, list(day,aodid,pred.m3,pred.m3.mix)], all.x = T)
  #R2.m3
  res['m3.R2.GAM',i] <- round(summary(lm(PM25~pred.m3,data=mod1_s))$r.squared,2) 
  res['m3.R2.mix',i] <- round(summary(lm(PM25~pred.m3.mix,data=mod1_s))$r.squared,2)   
  m1.fit.all<- lm(PM25~pred.m3,data=mod1_s)
  res['m3.RMSE.GAM',i] <- round(rmse(residuals(m1.fit.all)),2) 
  m1.fit.all<- lm(PM25~pred.m3.mix,data=mod1_s)
  res['m3.RMSE.mix',i] <- round(rmse(residuals(m1.fit.all)),2) 
  
  #spatial.R2.m3
  spatialall<-mod1_s %>%
    dplyr::group_by(stn) %>%
    dplyr::summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE),barpred.mix = mean(pred.m3.mix, na.rm=TRUE)) 
  
  ### Using GAM
  m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
  res['m3.spatial.R2.GAM',i] <-  round(summary(lm(barpm ~ barpred, data=spatialall))$r.squared,2) 
  res['m3.spatial.RMSE.GAM',i] <- round(rmse(residuals(m1.fit.all.spat)),2)
  ### Not Using GAM
  m1.fit.all.spat<- lm(barpm ~ barpred.mix, data=spatialall)
  res['m3.spatial.R2.MIX',i] <-  round(summary(lm(barpm ~ barpred.mix, data=spatialall))$r.squared, 2)
  res['m3.spatial.RMSE.MIX',i] <- round(rmse(residuals(m1.fit.all.spat)),2)

  # temporal
  tempoall<-left_join(mod1_s,spatialall)
  tempoall$delpm <-tempoall$PM25-tempoall$barpm
  tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
  mod_temporal <- lm(delpm ~ delpred, data=tempoall)
  res['m3.temporal.R2.GAM',i] <-round(summary(lm(delpm ~ delpred, data=tempoall))$r.squared, 2)

  # temporal r2 for prediction without gam resid correction
  tempoall$delpm <-tempoall$PM25-tempoall$barpred
  tempoall$delpred <-tempoall$pred.m3.mix-tempoall$barpred.mix
  mod_temporal <- lm(delpm ~ delpred, data=tempoall)
  res['m3.temporal.R2.MIX',i] <-round(summary(lm(delpm ~ delpred, data=tempoall))$r.squared, 2)
}

years=rep(2003:2015,each=6)
res[11,]=years

# calculate annual means
final <- matrix(nrow=10, ncol=13)
final=as.data.frame(final)
rownames(final) <- c("m3.R2.GAM","m3.R2.mix","m3.RMSE.GAM","m3.RMSE.mix","m3.spatial.R2.GAM","m3.spatial.R2.MIX","m3.spatial.RMSE.GAM","m3.spatial.RMSE.MIX","m3.temporal.R2.GAM","m3.temporal.R2.MIX")

years=c(2003:2015)

for (i in 1:13)
{
 temp=res[,res[11,]==years[i]]
 temp=round(rowMeans(temp),2)
 temp=temp[1:10]
 final[,i]=temp
}

colnames(final) <- c(2003:2015)
final
