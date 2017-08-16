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
setwd("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily")
mod1=readRDS("mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")
mod1$day=as.factor(mod1$day)
mod1$metreg=as.factor(mod1$metreg)
mod1$stn_f=as.factor(mod1$stn)
mod1$stnm = as.numeric(mod1$stn_f)

#### Define glmnet inputs -simple model (only aod, day, metreg)

y_train <- mod1$PM25

x_temp<- with(mod1, data.frame(aod_047, day, metreg, stnm))

x_train <- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg, data=x_temp)


# CV of simple glmnet model (only aod, day, metreg)

cv.fit <- cv.glmnet(x=x_train, y=y_train, family='gaussian', type.measure = "mse",
                    foldid = x_temp$stnm, parallel=TRUE) # foldid defines leave one station out

plot(cv.fit)

mod1$pred.gl.cv = predict(cv.fit,x_train)

# mod1$pred.gl.cv = predict(cv.fit,x_test)

m1.gm= lm(PM25~pred.gl.cv,data=mod1)

# Evaluate glmnet prediction for simple model

summary(lm(PM25~pred.gl.cv,data=mod1)) # R2=0.71

print(rmse(residuals(m1.gm))) # RMSE=8.89

# Define glmnet inputs for full model (adding the spatial and temporal variables)
  
y_train <- mod1$PM25
x_temp<- with(mod1, data.frame(stnm,aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x_train<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

cv.fit <- cv.glmnet(x=x_train, y=y_train, family='gaussian', type.measure = "mse",
                    foldid = x_temp$stnm, parallel=TRUE)
plot(cv.fit)
cv.fit$cvm[which(cv.fit$lambda==cv.fit$lambda.min)] %>% sqrt()

mod1$pred.gl.cv = predict(cv.fit,x_train)

# Add new column stn identifier- for each stan a code will be given for each half

mod1=mod1

mod1<-mod1 %>% 
  group_by (stn) %>%
  mutate(t1 = length(stnm)) %>% # Calculate the length of each station vector
  mutate(t2 = round(0.5*t1)) %>% # Calculate half of the length
  mutate(t3 = c(1:length(stnm))) %>% # Index the vector of each station
  mutate(t4 = ifelse(t3 > t2 , 1 , 2)) %>% # If the index is higher than half of length of vector assign 1, else assign 2
  mutate(t5 = paste(t4, stnm, sep=".")) %>% # Add the station identifier to the unique half station id
  as.data.table

mod1$t6=as.numeric(as.factor(mod1_t$t5)) # Convert to numeric

# Define glmnet inputs for full model (adding the spatial and temporal variables) using half data test (half data of station)

y_train <- mod1$PM25
x_temp<- with(mod1, data.frame(t6,aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x_train<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

cv.fit <- cv.glmnet(x=x_train, y=y_train, family='gaussian', type.measure = "mse",
                    foldid = x_temp$t6, parallel=TRUE)
plot(cv.fit)
cv.fit$cvm[which(cv.fit$lambda==cv.fit$lambda.min)] %>% sqrt()



# Evaluate glmnet prediction for full model
summary(lm(PM25~pred.gl.cv,data=mod1)) # R2=0.73
m1.gm= lm(PM25~pred.gl.cv,data=mod1)
print(rmse(residuals(m1.gm))) #rmse=8.5

# Define glmnet inputs for full model (adding the spatial and temporal variables)
# Same variables as above, however different alpha was defined

y_train <- mod1$PM25
x_temp<- with(mod1, data.frame(stnm,aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x_train<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

cv.fit <- cv.glmnet(x=x_train, y=y_train, family='gaussian', type.measure = "mse", alpha = 0,  foldid = x_temp$stnm, parallel=TRUE)
plot(cv.fit)
cv.fit$cvm[which(cv.fit$lambda==cv.fit$lambda.min)] %>% sqrt()


mod1$pred.gl.cv = predict(cv.fit,x_train)

# Evaluate glmnet prediction for full model with alpha=0
summary(lm(PM25~pred.gl.cv,data=mod1)) # R2=0.87
m1.gm= lm(PM25~pred.gl.cv,data=mod1)
print(rmse(residuals(m1.gm)))  # rmse=6.01

## Run full model and Predict for stage 2

y_train <- mod1$PM25
x_temp<- with(mod1, data.frame(aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x_train<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)

glmfull <- glmnet(x=x_train, y=y_train, alpha =0, family='gaussian')
mod1$pred.full = predict(glmfull , newx = x_train)

### Predict mod2
mod2 <- readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod2/AQUA/mod2.AQ.2003_2015.PM25_Daily_Re.rds")
mod2=as.data.table(mod2)
# Data filtering
# mod2<-filter(mod2,RelAZ < 90)
mod2<-filter(mod2,UN < 0.04 & UN > 0)
mod2<-filter(mod2,aod_047 < 3.5)
mod2$c=substr(mod2$day,1,4)

# Predict PM for mod2 from formula craeted in the calibration stage
mod2$day=as.factor(mod2$day)
mod2$metreg=as.factor(mod2$metreg)
x_temp<- with(mod2, data.frame(aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
x_train<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp)
mod2$pred.gl.cv = predict(cv.fit,x_train)

####### Check how many stations are in each metreg
num
days <-mod1 %>%
  group_by(metreg) %>%
  dplyr::summarise(s_num = length(unique(day)))

days <-mod1 %>%
  group_by(metreg,stn) %>%
  dplyr::summarise(s_num = length(unique(day)))

days=as.data.frame(days)

### Leave one station out cv for mixed effects modeling

m1.formula <- as.formula(PM25 ~ aod_047
                         +Elev.s +ndvi.s+Pop_dens.s+Dis_Rd1_2012.s
                         +P_In.s 
                         +P_Ur.s
                         +P_Ag.s+P_OS.s
                         #temporal
                         +daily_hpbl.s+Temp_D.s+Rain_D.s+RH_D.s+NO2_D.s+SO2_D.s+WS_D.s
                         # +pbl_02.s+vc_D.s+O3_D
                         +(1+aod_047|day/metreg))

# repeated leave x monitors out CV
#neveruse <- c("PER")
neveruse <- c("")
mons <- unique(mod1[!stn %in% neveruse, stn]); length(mons)
xout <- 1 # number of monitors to hold out
# how many combinations if we pull out xout mons
ncol(combn(mons, xout))
n.iter <- 48
# we will compute mean of the other monitors using all monitoring data
setkey(mod1, stnm)

# list to store scheme
cvscheme <- list()
cvout <- list()
# set seed for reproducibility
set.seed(20150112)

# cross-validation in parallel

library(doParallel)
library(doRNG)

registerDoParallel(14)
# use a proper reproducible backend RNG
registerDoRNG(1234)
system.time({
  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dorng% {
  #system.time(for(i in 1:n.iter){
  #mons.test <- mons[sample(length(mons), xout)]
  mons.test <- combn(mons, xout)[,i]
  cvscheme[[i]] <- mons.test
  test <- mod1[stn %in% mons.test, ]
  train<- mod1[!stn %in% mons.test, ]
  # fit the model
  print(paste("iteration #", i, "testing set is monitor", paste(unique(test$stn), collapse = ","), ",", nrow(test), "records from", paste(format(range(test$day), "%Y-%m-%d"), collapse = " to ")))
  print(paste("training on", nrow(train), "records"))
  trainmod <-  lmer(m1.formula, data =  train)
  test$predcv <- predict(object=trainmod,newdata=test,allow.new.levels=TRUE,re.form=NULL )
  test$itercv <- i
  # export these results
  test[, list(day, stn, PM25, predcv, itercv)]
}# end of cross-validation loop
})
summary(lm(PM25 ~ predcv, data = iter.out))
# compute root mean squared error
iter.out[, sqrt(mean((PM25 - predcv)^2))]

iter.out$resid=iter.out$PM25-iter.out$predcv

print(rmse(iter.out$resid))

# cross-validation in parallel with doMC

library(foreach)
library(doMC)
registerDoMC(cores=20)

  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dopar%  {
    #system.time(for(i in 1:n.iter){
    #mons.test <- mons[sample(length(mons), xout)]
    mons.test <- combn(mons, xout)[,i]
    cvscheme[[i]] <- mons.test
    test <- mod1[stn %in% mons.test, ]
    train<- mod1[!stn %in% mons.test, ]
    # fit the model
    print(paste("iteration #", i, "testing set is monitor", paste(unique(test$stn), collapse = ","), ",", nrow(test), "records from", paste(format(range(test$day), "%Y-%m-%d"), collapse = " to ")))
    print(paste("training on", nrow(train), "records"))
    trainmod <-  lmer(m1.formula, data =  train)
    test$predcv <- predict(object=trainmod,newdata=test,allow.new.levels=TRUE,re.form=NULL )
    test$itercv <- i
    # export these results
    test[, list(day, stn, PM25, predcv, itercv)]
  }# end of cross-validation loop

summary(lm(PM25 ~ predcv, data = iter.out))
# compute root mean squared error
iter.out[, sqrt(mean((PM25 - predcv)^2))]

iter.out$resid=iter.out$PM25-iter.out$predcv

print(rmse(iter.out$resid))


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
  out_train_s <- lmer(m1.formula,data =  train_s) # m1.formula is the formulation of the mixed effects modeling: m1.formula <- as.formula(PM25 ~ aod_047 +...+(1+aod_047|day/metreg)) 
  test_s$pred.m1.cv <- predict(object=out_train_s ,newdata=test_s,allow.new.levels=TRUE,re.form=NULL ) # predict for the test period using the coefficients produced from running the model on the training period
  test_s$iter <- i
  test_s
}

# Bind the dataset
mod1.cv = do.call(rbind, test_final) # bind the list into one table (mod1.cv) that containes the predicted values for all the different test periods and the obsreved data (PM25)
print(summary(lm(PM25~pred.m1.cv,data=mod1.cv))$r.squared) # compute r2 
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=mod1.cv)
print(rmse(residuals(m1.fit.all.cv)))


