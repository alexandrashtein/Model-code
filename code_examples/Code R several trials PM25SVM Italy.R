
#######################################
#### conventional approach (stage 1)
#######################################


############ First try: use the full model as in the published paper for PM10 (M5FULL)

m5full.formula <- as.formula(pm25svm ~ aod+as.factor(season)
                             # OMI data
                             +uvai+visai
                             # meteo
                             +temp.c.s+speed.ms.s+visib.km.s+rh.s
                             # other spatio-temporal  
                             +pblnew.s+ndvi.s+as.factor(dust)
                             # spatial 1: population, elevation, isa
                             +r.mean.restot.s+diff.restot.s+elevation.s+isa.s
                             # spatial 2: point emissions and areal emissions
                             +near.emip.inv.s+co.2010p.s+nh3.2010a.s+pm10.2010a.s
                             # spatial 4: land coverage vars.
                             +pct.deciduous.s+pct.evergreen.s+pct.crop.s+pct.pasture.s+pct.shrub.s+pct.high.dev.s+pct.low.dev.s
                             # spatial 5: streets vars.
                             +near.a1.inv.s+near.a2.inv.s+r.mean.length.a1.s+r.mean.length.a23.s+r.mean.length.oth.s
                             # spatial 5: other proximity variables
                             +near.sea.inv.s+near.airport.inv.s
                             # random component
                             +(1+aod+temp.c.s|day/cod_reg2))
m5full <- lmer(m5full.formula,data=ita,weights=normwt)
ita[,pred.m5full.st1 := NULL]
ita$pred.m5full.st1 <- predict(m5full)


##### NON-CV RESULTS

# Overall
m1.fit.all  <- lm(pm25svm~pred.m5full.st1, data=ita)
r2.all      <- summary(m1.fit.all)$r.squared
rmspe       <- rmse(residuals(m1.fit.all))
inter       <- summary(m1.fit.all)$coef[1,1]
slope       <- summary(m1.fit.all)$coef[2,1]
# spatial
spatialall     <- ita %>% group_by(site) %>% summarise(barpm = mean(pm25svm, na.rm=TRUE), barpred = mean(pred.m5full.st1, na.rm=TRUE))
m1.fit.all.s   <- lm(barpm ~ barpred, data=spatialall)
r2.spatial     <- summary(m1.fit.all.s)$r.squared
rmse.spatial   <- rmse(residuals(m1.fit.all.s))
spatial.inter  <- summary(m1.fit.all.s)$coef[1,1]
spatial.slope  <- summary(m1.fit.all.s)$coef[2,1]
# temporal
tempoall         <- left_join(ita,spatialall)
tempoall$delpm   <- tempoall$pm25svm-tempoall$barpm
tempoall$delpred <- tempoall$pred.m5full.st1-tempoall$barpred
mod_temporal     <- lm(delpm ~ delpred, data=tempoall)
r2.temporal      <- summary(mod_temporal)$r.squared
rmse.temporal    <- rmse(residuals(mod_temporal))
temporal.inter   <- summary(mod_temporal)$coef[1,1]
temporal.slope   <- summary(mod_temporal)$coef[2,1]
#results
non.cv.results <<- rbind(r2.all,      rmspe,         inter,          slope,
                         r2.spatial,  rmse.spatial,  spatial.inter,  spatial.slope, 
                         r2.temporal, rmse.temporal, temporal.inter, temporal.slope)
print(non.cv.results)
#### full model
# r2.all          7.586192e-01
# rmspe           5.470134e+00
# inter          -4.813969e-01
# slope           1.028792e+00
# r2.spatial      6.754004e-01
# rmse.spatial    2.679550e+00
# spatial.inter  -3.142616e-01
# spatial.slope   1.020650e+00
# r2.temporal     7.757302e-01
# rmse.temporal   4.824811e+00
# temporal.inter  2.551637e-16
# temporal.slope  1.031148e+00


##### CV RESULTS: NOT DONE TO SAVE TIME...


############ Second try: only AOD and TEMP (M5DITA)

m5dita.formula <- as.formula(pm25svm ~ aod + temp.c.s +(1+aod+temp.c.s|day/cod_reg2))

m5dita <- lmer(m5dita.formula,data=ita,weights=normwt)
ita[,pred.m5dita.st1 := NULL]
ita$pred.m5dita.st1 <- predict(m5dita)

##### NON-CV RESULTS

# r2.all          7.400716e-01
# rmspe           5.676406e+00
# inter          -5.102654e-01
# slope           1.036824e+00
# r2.spatial      5.918011e-01
# rmse.spatial    3.004857e+00
# spatial.inter  -3.636714e+00
# spatial.slope   1.221101e+00
# r2.temporal     7.707615e-01
# rmse.temporal   4.877965e+00
# temporal.inter  1.440936e-16
# temporal.slope  1.019101e+00


##### CV RESULTS: 10-fold by monitoring sites
cv10.sites.pm25svm(ita,m5dita.formula)
cv10.sites.dataset <- mod1.cv
cv10.sites.results <- results
# cv.r2.all          6.579475e-01
# cv.rmspe           6.511679e+00
# cv.inter           4.878001e-01
# cv.slope           9.744845e-01
# cv.r2.spatial      4.404263e-01
# cv.rmse.spatial    3.518168e+00
# cv.spatial.inter  -1.207468e+00
# cv.spatial.slope   1.073361e+00
# cv.r2.temporal     6.955354e-01
# cv.rmse.temporal   5.621643e+00
# cv.temporal.inter -2.041461e-16
# cv.temporal.slope  9.628828e-01


##### Generate residuals (varible RES) and put it into dataset ITA

ita$res = residuals(m5dita)



#######################################
#### GAM and SVM MODELS ON RESIDUALS
#######################################


# try1=gam(res~te(xcoord_idcell,ycoord_idcell)+te(xcoord_idcell,ycoord_idcell,by=jd),data=ita)
# try2=gam(res~te(xcoord_idcell,ycoord_idcell)+te(xcoord_idcell,ycoord_idcell,aod,by=jd),data=ita)
# plot(try2,pers=TRUE)
library(e1071)
try3 <- svm(res~aod+uvai+visai+temp.c.s+I(1/speed.ms.s)+rh.s+I(1/pblnew.s)+ndvi.s+
                r.mean.restot.s+elevation.s+isa.s+pct.deciduous.s+pct.evergreen.s+pct.crop.s+pct.pasture.s+pct.shrub.s+
                near.a1.inv.s+near.a2.inv.s+r.mean.length.a1.s+r.mean.length.a23.s+r.mean.length.oth.s+
                near.sea.inv.s, type="nu-regression",data=ita)
summary(try3)
# Parameters:
#   SVM-Type:  nu-regression 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.04545455 
# nu:  0.5 
# 
# Number of Support Vectors:  32494

pred.svm     <- predict(try3)
pred.m5dita  <- predict(m5dita)
pred.sum     <- pred.svm + pred.m5dita

res2 <- ita$pm25svm-pred.sum

# try1=gam(res2~te(xcoord_idcell,ycoord_idcell)+te(xcoord_idcell,ycoord_idcell,by=jd),data=ita)
# summary(try1)
try2=gam(res2~te(xcoord_idcell,ycoord_idcell)+te(xcoord_idcell,ycoord_idcell,aod,by=jd),data=ita)
summary(try2)

pred.sum.new = pred.sum + predict(try2)

r2           <- summary(lm(pm25svm~pred.sum.new,data=ita))$r.squared
rmspe        <- rmse(residuals(lm(pm25~pred.svm2,data=data)))
inter        <- summary(lm(pm25~pred.svm2,data=data))$coef[1,1]
slope        <- summary(lm(pm25~pred.svm2,data=data))$coef[2,1]
results <<- rbind(r2, rmspe, inter, slope)
print(results)
# We haven't reported them...


#### In the following, we cross-validate the previous models

#### only aod and temp
# m5dita.formula <- as.formula(pm25svm ~ aod + temp.c.s +(1+aod+temp.c.s|day/cod_reg2))

final =  foreach (i=1:10, .packages = c("data.table", "lme4","e1071","mgcv") ) %dopar% {
  test                  <- subset(ita, split==i)
  train                 <- subset(ita, split!=i)
  out_train             <- lmer(m5dita.formula, data = train, weights=normwt)
  res=residuals(out_train)
  try3 <- svm(res~aod+uvai+visai+temp.c.s+I(1/speed.ms.s)+rh.s+I(1/pblnew.s)+ndvi.s+as.factor(zona)+as.factor(dust)+
                  r.mean.restot.s+elevation.s+isa.s+pct.deciduous.s+pct.evergreen.s+pct.crop.s+pct.pasture.s+pct.shrub.s+pct.high.dev.s+pct.low.dev.s+
                  near.a1.inv.s+near.a2.inv.s+r.mean.length.a1.s+r.mean.length.a23.s+r.mean.length.oth.s+
                  near.sea.inv.s, type="nu-regression",data=train)
  res2=residuals(try3)
  
  try2=gam(res2~te(xcoord_idcell,ycoord_idcell)+te(xcoord_idcell,ycoord_idcell,aod,by=jd),data=train)
  test$pred.m1.cv <- predict(object=out_train, newdata=test, allow.new.levels=TRUE, re.form=NULL ) +
                     predict(try3,newdata=test) + predict(try2,newdata=test)
  test$itercv <- i
  # export these results
  test[, list(day, site, pm25svm, pred.m1.cv, itercv)]
}

## Bind the list into one table 
mod1.cv = do.call(rbind, final)

m1.fit.all  <- lm(pm25svm~pred.m1.cv, data=mod1.cv)
r2.all      <- summary(m1.fit.all)$r.squared
rmspe       <- rmse(residuals(m1.fit.all))
inter       <- summary(m1.fit.all)$coef[1,1]
slope       <- summary(m1.fit.all)$coef[2,1]
#spatial
spatialall     <- mod1.cv %>% group_by(site) %>% summarise(barpm = mean(pm25svm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE))
m1.fit.all.s   <- lm(barpm ~ barpred, data=spatialall)
r2.spatial     <- summary(m1.fit.all.s)$r.squared
rmse.spatial   <- rmse(residuals(m1.fit.all.s))
spatial.inter  <- summary(m1.fit.all.s)$coef[1,1]
spatial.slope  <- summary(m1.fit.all.s)$coef[2,1]
#temporal
tempoall         <- left_join(mod1.cv,spatialall)
tempoall$delpm   <- tempoall$pm25svm-tempoall$barpm
tempoall$delpred <- tempoall$pred.m1.cv-tempoall$barpred
mod_temporal     <- lm(delpm ~ delpred, data=tempoall)
r2.temporal      <- summary(mod_temporal)$r.squared
rmse.temporal    <- rmse(residuals(mod_temporal))
temporal.inter   <- summary(mod_temporal)$coef[1,1]
temporal.slope   <- summary(mod_temporal)$coef[2,1]

#results
non.cv.results <<- rbind(r2.all,      rmspe,         inter,          slope,
                         r2.spatial,  rmse.spatial,  spatial.inter,  spatial.slope, 
                         r2.temporal, rmse.temporal, temporal.inter, temporal.slope)
print(non.cv.results)
# r2.all          6.448586e-01
# rmspe           6.635097e+00
# inter           2.467233e-01
# slope           9.882225e-01
# r2.spatial      2.413982e-01
# rmse.spatial    4.096329e+00
# spatial.inter   4.557016e-01
# spatial.slope   9.719233e-01
# r2.temporal     7.137375e-01
# rmse.temporal   5.451012e+00
# temporal.inter -2.309578e-16
# temporal.slope  9.839933e-01



#######################################
#### GLMNET APPROACH
#######################################

names(ita)
library(doParallel)
registerDoParallel(8)
install.packages("glmnet")
library(glmnet)


##### Work on the residuals from previous model M5DITA

ita$site = as.numeric(ita$site)
y_train <- ita$res

# First try
x_temp  <- with(ita, data.frame(aod,uvai,visai,temp.c.s,I(1/speed.ms.s),rh.s,I(1/pblnew.s),ndvi.s,
                                r.mean.restot.s,elevation.s,isa.s,pct.deciduous.s,pct.evergreen.s,pct.crop.s,pct.pasture.s,pct.shrub.s,
                                near.a1.inv.s,near.a2.inv.s,r.mean.length.a1.s,r.mean.length.a23.s,r.mean.length.oth.s,
                                near.sea.inv.s,split))
x_train <- sparse.model.matrix(~ aod+uvai+visai+temp.c.s+I(1/speed.ms.s)+rh.s+I(1/pblnew.s)+ndvi.s+
                                 r.mean.restot.s+elevation.s+isa.s+pct.deciduous.s+pct.evergreen.s+pct.crop.s+pct.pasture.s+pct.shrub.s+
                                 near.a1.inv.s+near.a2.inv.s+r.mean.length.a1.s+r.mean.length.a23.s+r.mean.length.oth.s+
                                 near.sea.inv.s, data=x_temp)

# Second try
x_temp<- with(ita, data.frame(aod,speed.ms.s,pblnew.s,split))
x_train <- sparse.model.matrix(~ aod+I(1/speed.ms.s)+I(1/pblnew.s), data=x_temp)

# Third try (on observed PM25SVM, not on the reiduals!)
y_train <- ita$pm25svm
x_temp<- with(ita, data.frame(aod,uvai,visai,temp.c.s,speed.ms.s,rh.s,pblnew.s,ndvi.s,jd,cod_reg2,
                              r.mean.restot.s,elevation.s,isa.s,pct.deciduous.s,pct.evergreen.s,pct.crop.s,pct.pasture.s,pct.shrub.s,
                              near.a1.inv.s,near.a2.inv.s,r.mean.length.a1.s,r.mean.length.a23.s,r.mean.length.oth.s,near.sea.inv.s,split))
x_train <- sparse.model.matrix(~ aod+uvai+visai+temp.c.s+speed.ms.s+rh.s+pblnew.s+ndvi.s+
                                 r.mean.restot.s+elevation.s+isa.s+pct.deciduous.s+pct.evergreen.s+pct.crop.s+pct.pasture.s+pct.shrub.s+
                                 near.a1.inv.s+near.a2.inv.s+r.mean.length.a1.s+r.mean.length.a23.s+r.mean.length.oth.s+near.sea.inv.s+ jd:cod_reg2+ temp.c.s*jd*cod_reg2 + aod*jd*cod_reg2, data=x_temp)


# CV of simple glmnet model (only aod, day, metreg)

cv.fit <- cv.glmnet(x=x_train, y=y_train, family='gaussian', type.measure = "mse",alpha=0, 
                    foldid = x_temp$split, parallel=FALSE)
#plot(cv.fit)
#summary(cv.fit$lambda)
#coef(cv.fit)
mse.min <- cv.fit$cvm[cv.fit$lambda == cv.fit$lambda.min]
mse.min
ita$pred.gl.cv = predict(cv.fit,ita)
summary(lm(pm25svm~pred.gl.cv,data=ita))



################################################
##### Code for parallelizing mixed models in CV
################################################

final =  foreach (i=1:10, .packages = c("data.table", "lme4") ) %dopar% {
  test                  <- subset(ita, split==i)
  train                 <- subset(ita, split!=i)
  out_train             <- lmer(m5eita.formula, data = train, weights=normwt)
  test$pred.m1.cv       <- predict(object=out_train, newdata=test, allow.new.levels=TRUE, re.form=NULL )
  test$itercv <- i
  # export these results
  test[, list(day, site, pm25svm, pred.m1.cv, itercv)]
}

## Bind the list into one table 
mod1.cv = do.call(rbind, final)
head(mod1.cv)
m2.fit.all.cv<-lm(pm25svm~pred.m1.cv, data=mod1.cv)

cv.r2.all <- summary(m2.fit.all.cv)$r.squared
summary(lm(pm25svm~pred.m1.cv, data=mod1.cv))



#######################################
#### k-means pluS random-effects models
#### CORRECT: use of meanPM10 from buffer
#######################################

# Select sub-dataset from the original ITA
itamod= select(ita,meanPM10,temp.c.s,pblnew.s,elevation.s,restot.s,rh.s)

# Apply k-means clustering and define ten clusters
clus= kmeans(itamod,10)

# Add derived variable, CLUS2, into ITA dataset
ita$clus2= clus$cluster

# Define M5EITA formula as simple random int. and slope by cluster (no day effect)
m5eita.formula <- pm25svm ~ aod + temp.c.s + (1 + aod + temp.c.s | clus2)


##### NON CV: run model and get fitting stats.

mod             <- lmer(m5eita.formula, data = ita, weights=normwt)
ita$pred.non.cv <- predict(object=mod)
m1.non.cv       <- lm(pm25svm~pred.non.cv, data=ita)
r2.all          <- summary(m1.non.cv)$r.squared
r2.all
# 0.7180611


##### CV: run model and get fitting stats with cross-validation

for (i in 1:10)
{
  test                  <- subset(ita, split==i)
  train                 <- subset(ita, split!=i)
  out_train             <- lmer(m5eita.formula, data = train, weights=normwt)
  test$pred.m1.cv       <- predict(object=out_train, newdata=test, allow.new.levels=TRUE, re.form=NULL )
  assign(paste("test_s",i,sep=""),test)
  rm("test","train","out_train")
}
#BIND 1 dataset
mod1.cv <<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9,test_s10))
rm(list = ls(pattern = "test_"))
#table updates
m1.fit.all.cv <- lm(pm25svm~pred.m1.cv, data=mod1.cv)
r2.all        <- summary(m1.fit.all.cv)$r.squared
r2.all
# 0.7153017



#######################################
#### k-means pluS random-effects models
#### WRONG: use of PM25SVM obs. data
#######################################


# Select sub-dataset from the original ITA
itamod= select(ita,pm25svm,temp.c.s,pblnew.s,elevation.s,restot.s,rh.s)

# Apply k-means clustering and define ten clusters
clus= kmeans(itamod,10)
head(clus)

# Add derived variable, CLUS, into ITA dataset
ita$clus= clus$cluster
tapply(ita$aod,ita$clus,mean)
head(ita)


##### Define M5DITA formula as simple random int. and slope by day, nested by cluster

m5dita.formula <- as.formula(pm25svm ~ aod +temp.c.s +(1+aod+temp.c.s|day/clus))


##### NON CV: run model and get fitting stats.

m5dita <- lmer(m5dita.formula,data=ita,weights=normwt)
ita[,pred.m5dita.st1 := NULL]
ita$pred.m5dita.st1 <- predict(m5dita)
m1.fit.all  <- lm(pm25svm~pred.m5dita.st1, data=ita)
r2.all      <- summary(m1.fit.all)$r.squared
r2.all
# 9.798215e-01


##### CV: run model and get fitting stats with cross-validation

cv10.sites.pm25svm(ita,m5dita.formula)
cv10.sites.dataset <- mod1.cv
cv10.sites.results <- results
# cv.r2.all          9.556761e-01



##### Code from ITAI to get maps of sample locations

# head(ita)
# library(dplyr)
# 
# ex = ita %>%
#   dplyr::group_by(aodid) %>%
#   dplyr::summarize(x = mean(xcoord_idcell, na.rm = TRUE),y = mean(ycoord_idcell, na.rm = TRUE), clus = median(clus, na.rm = TRUE)  )
# ex=as.data.frame(ex)
# str(ex)
# library(foreign)
# write.dbf(ex, "F:\\ex4.dbf")
# 
# 
# day <- subset(ita,jd==1)
# write.dbf(day, "F:\\day2.dbf")
# 
# ex = ita %>%
#   dplyr::group_by(aodid) %>%
#   dplyr::summarize(x = mean(xcoord_idcell, na.rm = TRUE),y = mean(ycoord_idcell, na.rm = TRUE), clus = median(clus, na.rm = TRUE)  )
# ex=as.data.frame(ex)
# str(ex)
# library(foreign)
# write.dbf(ex, "F:\\ex4.dbf")
# 



#######################################
#### BAYESIAN KERNEL MACHINE REGRESSION
#######################################


library(bkmr)
set.seed(111)
attach(ita)
Z=cbind(aod,uvai,visai,temp.c.s,speed.ms.s,rh.s,pblnew.s,ndvi.s,
        r.mean.restot.s,elevation.s,isa.s,pct.deciduous.s,pct.evergreen.s,pct.crop.s,pct.pasture.s,pct.shrub.s,
        near.a1.inv.s,near.a2.inv.s,r.mean.length.a1.s,r.mean.length.a23.s,r.mean.length.oth.s,
        near.sea.inv.s)
detach(ita)


fitkm <- kmbayes(y = res, Z = Z, iter = 10000, verbose = FALSE, varsel = TRUE)
# Did not run


