## For mixed effects modeling
#add all packages
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
library(splines)
library(data.table)

#sourcing
source("/media/qnap/Data/code/R_functions/CV_splits.r") 
source("/media/qnap/Data/code/R_functions/rmspe.r")

# Load data
setwd("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily")
mod1=readRDS("mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")
mod1=as.data.table(mod1)

# Compute the square root of the PM2.5

mod1$PM25.sq=sqrt(mod1$PM25)

# Mixed effects model Before transformation:

m1.formula <- as.formula(PM25 ~ aod_047
                         # +aod_047_mean*c +aod_047_mean*FS_BS+FS_BS+stn_type+aod_047_mean*stn_type
                         #spatial
                         +Elev.s +ndvi.s+Pop_dens.s+Dis_Rd1_2012.s
                         # +dis_inventory.s 
                         # +road_den.s
                         # +Dist_Railw.s
                         # +Dist_WB.s
                         +P_In.s 
                         +P_Ur.s
                         +P_Ag.s+P_OS.s
                         #temporal
                         +daily_hpbl.s+Temp_D.s+Rain_D.s+RH_D.s+NO2_D.s
                         # +pbl_02.s+vc_D.s+WS_D.s+O3_D+SO2_D.s
                         +(1+aod_047|day/metreg))


#stage 1
# mod1fit <- lmer(m1.formula,data=mod1,weights=normwt)
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1 <- predict(mod1fit)
print(summary(lm(PM25~pred.m1,data=mod1))$r.squared)
# Check the Homoscedasticity of predictions:
plot(fitted(mod1fit),residuals(mod1fit))

## Cross validation

library(foreach)
library(doMC)
registerDoMC(cores=10)

test_final = foreach(i = 1:10) %dopar% {
  
  splits_s <- splitdf(mod1)
  test_s <- splits_s$testset
  train_s <- splits_s$trainset
  # out_train_s <- lmer(m1.formula,data =  train_s,weights=normwt )
  out_train_s <- lmer(m1.formula,data =  train_s)
  test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL )
  test_s$iter <- i
  test_s
}

#BIND 1 dataset
mod1.cv = do.call(rbind, test_final)

m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=mod1.cv)
print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$r.squared)
print(rmse(residuals(m1.fit.all.cv)))

# Mixed effects model after transformation:

m1.formula <- as.formula(PM25.sq ~ aod_047
                         # +aod_047_mean*c +aod_047_mean*FS_BS+FS_BS+stn_type+aod_047_mean*stn_type
                         #spatial
                         +Elev.s +ndvi.s+Pop_dens.s+Dis_Rd1_2012.s
                         # +dis_inventory.s 
                         # +road_den.s
                         # +Dist_Railw.s
                         # +Dist_WB.s
                         +P_In.s 
                         +P_Ur.s
                         +P_Ag.s+P_OS.s
                         #temporal
                         +daily_hpbl.s+Temp_D.s+Rain_D.s+RH_D.s+NO2_D.s
                         # +pbl_02.s+vc_D.s+WS_D.s+O3_D+SO2_D.s
                         +(1+aod_047|day/metreg))

#stage 1
# mod1fit <- lmer(m1.formula,data=mod1,weights=normwt)
mod1fit <- lmer(m1.formula,data=mod1)
summary(mod1fit)
mod1$pred.m1.sq<- predict(mod1fit)
print(summary(lm(PM25.sq~pred.m1.sq,data=mod1))$r.squared)
mod1$pred.m1.sq2=mod1$pred.m1.sq*mod1$pred.m1.sq
print(summary(lm(PM25~pred.m1.sq2,data=mod1))$r.squared)
# Check the Homoscedasticity of predictions:
plot(fitted(mod1fit),residuals(mod1fit))

## Cross validation

library(foreach)
library(doMC)
registerDoMC(cores=10)

test_final = foreach(i = 1:10) %dopar% {
  
  splits_s <- splitdf(mod1)
  test_s <- splits_s$testset
  train_s <- splits_s$trainset
  # out_train_s <- lmer(m1.formula,data =  train_s,weights=normwt )
  out_train_s <- lmer(m1.formula,data =  train_s)
  test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL )
  test_s$pred.m1.cv.2 <- test_s$pred.m1.cv*test_s$pred.m1.cv
    test_s$iter <- i
  test_s
}

#BIND 1 dataset
mod1.cv = do.call(rbind, test_final)

m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=mod1.cv)
print(summary(lm(PM25~pred.m1.cv.2,data=mod1.cv))$r.squared)
print(rmse(residuals(m1.fit.all.cv)))


#### Glmnet MSE for transformed data

# Define glmnet inputs for full model (adding the spatial and temporal variables)

y <- mod1$PM25.sq # compute the sqaure root of the pm2.5
mod1$day=as.factor(mod1$day)
mod1$metreg=as.factor(mod1$metreg)

x_temp<- with(mod1, data.frame(aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

## Define train and test dataset

set.seed(1)
train=sample(1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

cv.fit <- cv.glmnet(x=x[train,], y=y[train], alpha=0,family='gaussian', type.measure = "mse", parallel=TRUE)
plot(cv.fit)
cv.fit$cvm[which(cv.fit$lambda==cv.fit$lambda.min)] %>% sqrt()

ridge.pred=predict(cv.fit ,s=cv.fit$lambda.min ,newx=x[test ,])

# MSE after back transformation

ridge.pred.bts=ridge.pred*ridge.pred
y.test.bts=y.test*y.test
mean((ridge.pred.bts -y.test.bts)^2)%>% sqrt() ## 15.5
resid.bts=ridge.pred.bts -y.test.bts
plot(resid.bts~ridge.pred.bts)

#### Glmnet MSE for oringial PM2.5 data

# Define glmnet inputs for full model (adding the spatial and temporal variables)

y <- mod1$PM25 # compute the sqaure root of the pm2.5
mod1$day=as.factor(mod1$day)
mod1$metreg=as.factor(mod1$metreg)

x_temp<- with(mod1, data.frame(aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

## Define train and test dataset

set.seed(1)
train=sample(1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

cv.fit <- cv.glmnet(x=x[train,], y=y[train], alpha=0,family='gaussian', type.measure = "mse", parallel=TRUE)
plot(cv.fit)
cv.fit$cvm[which(cv.fit$lambda==cv.fit$lambda.min)] %>% sqrt()

ridge.pred=predict(cv.fit ,s=cv.fit$lambda.min ,newx=x[test ,])
mean((ridge.pred -y.test)^2)%>% sqrt() # 14
resid=ridge.pred -y.test
plot(resid~ridge.pred)

