
# Load data
mod1=readRDS("mod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")

mons <- unique(mod1[, stn]) # mons is a vector of the unique stations names (stn= abbreviation for monitoring stations)

xout <- 1 # number of monitors to hold out in each fold
n.iter <- length(mons) # number of iterations (equal to the number of stations)
setkey(mod1, stn) # sort the data by the stations column 

mons.test.all <- combn(mons, xout) # Generate all combinations of stations according to xout 

# Cross-validation in parallel 

library(foreach)
library(doMC)
registerDoMC(cores=20)

  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dopar% {
    mons.test <- mons.test.all[,i] 
    test <- mod1[stn %in% mons.test, ] # The test data includes the data from the 1 station that was is left fot testing the model prediction ability
    train<- mod1[!stn %in% mons.test, ] # The train data includes data from all the stations except the one that is left fot testing
    
    # fit the model
    trainmod <-  lmer(m1.formula, data =  train) # Train the model on the train data-set
    test$predcv <- predict(object=trainmod, newdata=test, allow.new.levels=TRUE, re.form=NULL ) # Perdict for the test station
    
    test$itercv <- i # Save the iteration number
    test[, list(day, stn, PM25, predcv, itercv)] # export these results
  } # end of cross-validation loop

iter.out[, sqrt(mean((PM25 - predcv)^2))] # Compute the RMSE
