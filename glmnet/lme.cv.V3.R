## For mixed effects modeling

# Load data
setwd("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily")
mod1=readRDS("mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")
mod1=as.data.table(mod1)
# Create a numeric station number
mod1$stn_f=as.factor(mod1$stn)
mod1$stnm = as.numeric(mod1$stn_f)
mod1=as.data.table(mod1)
mod1=filter(mod1, aod_047<1.2)

mons <- unique(mod1[, stn]) # mons is a vector of the unique stations names (stn= abbreviation for monitoring stations)

xout <- 1 # number of monitors to hold out in each fold
n.iter <- length(mons) # number of iterations (equal to the number of stations)
setkey(mod1, stnm) # sort the data by the stations column 

mons.test.all <- combn(mons, xout) # Generate all combinations of stations according to xout 

m1.formula <- as.formula(PM25 ~ aod_047 +Elev.s +ndvi.s+Pop_dens.s+Dis_Rd1_2012.s 
                         +P_In.s+P_Ur.s  +P_Ag.s+P_OS.s +daily_hpbl.s+Temp_D.s+Rain_D.s+RH_D.s+NO2_D.s
                        +(1+aod_047|day/metreg))

# Cross-validation in parallel 

library(foreach)
library(doMC)
registerDoMC(cores=20)

  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dopar% {
    mons.test <- mons.test.all[,i] 
    test <- mod1[stn %in% mons.test, ]
    half <- round(0.5*nrow(test))
    add_t <- test[half:nrow(test),]
    test <- test[1:half,]
    
    # The test data includes the data from the 1 station that was is left fot testing the model prediction ability
    train<- mod1[!stn %in% mons.test, ] # The train data includes data from all the stations except the one that is left fot testing
    train <- rbind(train,add_t)
    # fit the model
    trainmod <-  lmer(m1.formula, data =  train) # Train the model on the train data-set
    test$predcv <- predict(object=trainmod, newdata=test, allow.new.levels=TRUE, re.form=NULL ) # Perdict for the test station
    
    test$itercv <- i # Save the iteration number
    test[, list(day, stn, PM25, predcv, itercv)] # export these results
  } # end of cross-validation loop

iter.out[, sqrt(mean((PM25 - predcv)^2))] # Compute the RMSE

iter.out$resid=iter.out$PM25-iter.out$predcv # Create residuals column

# Check the normality of predictions:
hist(iter.out$resid)
qqnorm(iter.out$resid)

# Check the Homoscedasticity of predictions:

plot(iter.out$predcv,iter.out$resid, ylab = "Residuals", xlab="Prediction")
a=rep(0, nrow(iter.out))
points(a, add=TRUE,col="red")

## For glmnet

# Load data
setwd("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Daily")
mod1=readRDS("mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")
# Create a numeric station number
mod1$stn_f=as.factor(mod1$stn)
mod1$stnm = as.numeric(mod1$stn_f)
mod1$day=as.factor(mod1$day)
mod1$metreg=as.factor(mod1$metreg)

mons <- unique(mod1[, stn]) # mons is a vector of the unique stations names (stn= abbreviation for monitoring stations)

xout <- 1 # number of monitors to hold out in each fold
n.iter <- length(mons) # number of iterations (equal to the number of stations)
setkey(mod1, stnm) # sort the data by the stations column 

mons.test.all <- combn(mons, xout) # Generate all combinations of stations according to xout 

# Cross-validation in parallel 

library(foreach)
library(doMC)
registerDoMC(cores=20)

iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dopar% {
  mons.test <- mons.test.all[,i] 
  test <- mod1[stn %in% mons.test, ]
  half <- round(0.5*nrow(test))
  add_t <- test[half:nrow(test),]
  test <- test[1:half,]

  # The test data includes the data from the 1 station that was is left fot testing the model prediction ability
  train<- mod1[!stn %in% mons.test, ] # The train data includes data from all the stations except the one that is left fot testing
  train <- rbind(train,add_t)
  
  # Define glmnet input for test 
  y_test <- test$PM25
  x_temp_test<- with(test, data.frame(aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
  x_test<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp_test)
  
  # Define glmnet input for train 
  y_train <- train$PM25
  x_temp_train<- with(train, data.frame(aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
  x_train<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp_train)
  
  # fit the model
  glmfull <- glmnet(x=x_train, y=y_train,  lambda=0, family='gaussian') # Train the model on the train data-set
  test$predcv <-  predict(object=glmfull , newx = x_test) # Perdict for the test station
  
  test$itercv <- i # Save the iteration number
  test[, list(day, stn, PM25, predcv, itercv)] # export these results

  } # end of cross-validation loop

iter.out[, sqrt(mean((PM25 - predcv)^2))] # Compute the RMSE


# Cross-validation in parallel 

library(foreach)
library(doMC)
registerDoMC(cores=20)

iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dopar% {
  mons.test <- mons.test.all[,i] 
  test <- mod1[stn %in% mons.test, ]
  
  # The test data includes the data from the 1 station that was is left fot testing the model prediction ability
  train<- mod1[!stn %in% mons.test, ] # The train data includes data from all the stations except the one that is left fot testing

  # Define glmnet input for test 
  y_test <- test$PM25
  x_temp_test<- with(test, data.frame(aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
  x_test<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp_test)
  
  # Define glmnet input for train 
  y_train <- train$PM25
  x_temp_train<- with(train, data.frame(aod_047, day, metreg, Elev.s, ndvi.s, Dis_Rd1_2012.s, Pop_dens.s, P_In.s, P_Ur.s ,P_Ag.s, P_OS.s, daily_hpbl.s, Temp_D.s, Rain_D.s, RH_D.s, WS_D.s, SO2_D.s, NO2_D.s))
  x_train<- sparse.model.matrix(~ aod_047+ day:metreg + aod_047:day:metreg + Elev.s+ ndvi.s+ Dis_Rd1_2012.s+Pop_dens.s+ P_In.s + P_Ur.s+ P_Ag.s+ P_OS.s + daily_hpbl.s+ Temp_D.s+ Rain_D.s+ RH_D.s+ WS_D.s + SO2_D.s+ NO2_D.s, data=x_temp_train)
  
  # fit the model
  glmfull <- glmnet(x=x_train, y=y_train,  lambda=0, family='gaussian') # Train the model on the train data-set
  test$predcv <-  predict(object=glmfull , newx = x_test) # Perdict for the test station
  
  test$itercv <- i # Save the iteration number
  test[, list(day, stn, PM25, predcv, itercv)] # export these results
  
} # end of cross-validation loop

iter.out[, sqrt(mean((PM25 - predcv)^2))] 

