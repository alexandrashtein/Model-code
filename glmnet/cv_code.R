source("/media/qnap/Data/code/R_functions/CV_splits.r")
source("/media/qnap/Data/code/R_functions/rmspe.r")

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
library(glmnet)

#### Load mod1 data

mod1=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily/previous_run/mod1.AQ.2003_2015.PM25_Daily_filt04_clean.rds")
mod1$day=as.factor(mod1$day)
mod1$metreg=as.factor(mod1$metreg)

#### Check how many stations are in each metreg

num <-mod1 %>%
  group_by(metreg) %>%
  dplyr::summarise(s_num = length(unique(stn)))

days <-mod1 %>%
  group_by(metreg) %>%
  dplyr::summarise(s_num = length(unique(day)))

days <-mod1 %>%
  group_by(metreg,stn) %>%
  dplyr::summarise(s_num = length(unique(day)))

days=as.data.frame(days)

### Define formula for calibration

m1.formula= m1.formula <- as.formula(PM25 ~ aod_047_mean+(1+aod_047_mean|day/metreg))

####  cross-validation (cv) for mixed effects modeling

# This loop splits the dataset into 10 different folds of 90% training data and 10% test data
mod1_s=list()
for (i in 1:10)
{
  splits_s <- splitdf(mod1)
  mod1_s[[i]]=splits_s
}

library(foreach)
library(doMC)
registerDoMC(cores=10) # parallel processing 

# This stage creates a list (test_final) where each compoenent of the list contains the test_s table  
test_final = foreach(i = 1:10) %dopar% { 
  
  splits_s <- mod1_s[[i]] # split the dataset into 90% training data abnd 10% test data
  test_s <- splits_s$testset # the test dataset (10% of the data)
  train_s <- splits_s$trainset # the training dataset (90% of the data)
  out_train_s <- lmer(m1.formula,data =  train_s) # m1.formula is the formulation of the mixed effects modeling: m1.formula <- as.formula(PM25 ~ aod_047_mean +...+(1+aod_047_mean|day/metreg)) 
  test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL ) # predict for the test period using the coefficients produced from running the model on the training period
  test_s$iter <- i
  test_s
}

# Bind the dataset
mod1.cv = do.call(rbind, test_final) # bind the list into one table (mod1.cv) that containes the predicted values for all the different test periods and the obsreved data (PM25)
print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$r.squared) # compute r2 
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=mod1.cv)
print(rmse(residuals(m1.fit.all.cv)))

#### Cross validation using glment

# The following code uses the same structure of the cv code above with adjustments for the glmnet code structure 

library(foreach)
library(doMC)
registerDoMC(cores=10)

test_final = foreach(i = 1:10) %dopar% {
  
  splits_s <- mod1_s[[i]]
  
  #define test dataset
  test_s <- splits_s$testset
  x_temp_test<- with(test_s, data.frame(aod_047_mean, day, metreg))
  x_train_test <- sparse.model.matrix(~ aod_047_mean+ day:metreg + aod_047_mean:day:metreg , data=x_temp_test)
  
  #define training dataset
  train_s <- splits_s$trainset
  x_temp_s<- with(train_s, data.frame(aod_047_mean, day, metreg))
  x_train_s <- sparse.model.matrix(~ aod_047_mean+ day:metreg + aod_047_mean:day:metreg, data=x_temp_s)
  y_train_s <- train_s$PM25
  out_train_s <- glmnet(x=x_train_s, y=y_train_s, lambda=0, family='gaussian')
  
  # predict for test period
  test_s$pred.m1.cv <- predict(object=out_train_s ,newx=x_train_test)
  test_s$iter <- i
  test_s
}

# Bind the dataset

mod1.cv = do.call(rbind, test_final)
print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$r.squared)

#### glmnet cv

y_train <- mod1$PM25

x_temp<- with(mod1, data.frame(aod_047_mean, day, metreg, stnm))

x_train <- sparse.model.matrix(~ aod_047_mean+ day:metreg + aod_047_mean:day:metreg, data=x_temp)

mod1$stnm = as.numeric(mod1$stn_f)

# CV of simple glmnet model 
cv.fit <- cv.glmnet(x=x_train, y=y_train, family='gaussian', type.measure = "mse",
                   foldid = x_temp$stnm, parallel=TRUE)
plot(cv.fit)
mod1$pred.gl.cv = predict(cv.fit,x_train)

m1.gm= lm(PM25~pred.gl.cv,data=mod1)

# Evaluate  glmnet prediction

summary(lm(PM25~pred.gl.cv,data=mod1)) # R2=0.66

print(rmse(residuals(m1.gm))) # RMSE=9.15

# glmnet cv full (adding the spatial and temporal variables)

y_train <- mod1$PM25
x_temp<- with(mod1, data.frame(stnm,aod_047_mean, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x_train<- sparse.model.matrix(~ aod_047_mean+ day:metreg + aod_047_mean:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

# mod1$stnm = as.numeric(mod1$stn_f)

cv.fit <- cv.glmnet(x=x_train, y=y_train, family='gaussian', type.measure = "mse",
                    foldid = x_temp$stnm, parallel=TRUE)
plot(cv.fit)
mod1$pred.gl.cv = predict(cv.fit,x_train)
summary(lm(PM25~pred.gl.cv,data=mod1)) #0.67
m1.gm= lm(PM25~pred.gl.cv,data=mod1)
print(rmse(residuals(m1.gm))) #8.9

# glmnet cv full 

y_train <- mod1$PM25
x_temp<- with(mod1, data.frame(stnm,aod_047_mean, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x_train<- sparse.model.matrix(~ aod_047_mean+ day:metreg + aod_047_mean:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

cv.fit <- cv.glmnet(x=x_train, y=y_train, family='gaussian', type.measure = "mse", alpha = 0,  foldid = x_temp$stnm, parallel=TRUE)
plot(cv.fit)
mod1$pred.gl.cv = predict(cv.fit,x_train)
summary(lm(PM25~pred.gl.cv,data=mod1)) # 0.83
m1.gm= lm(PM25~pred.gl.cv,data=mod1)
print(rmse(residuals(m1.gm)))  # 6.45

# glmnet non cv 

y_train <- mod1$PM25
x_temp<- with(mod1, data.frame(stnm,aod_047_mean, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x_train<- sparse.model.matrix(~ aod_047_mean+ day:metreg + aod_047_mean:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

gm.fit <- glmnet(x=x_train, y=y_train, family='gaussian',  alpha = 0)
mod1$pred.gm = predict(gm.fit,x_train)
summary(lm(PM25~pred.gm ,data=mod1))
m1.gm= lm(PM25~pred.gm ,data=mod1)
print(rmse(residuals(m1.gm)))