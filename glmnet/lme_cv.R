#### Load mod1 data

mod1=readRDS("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/work/RDS_files/mod1/mod1.AQ.2003_2015.PM25_Dailymod1.AQ.2003_2015.PM25_Daily_Re_Clean.rds")


### Leave one station out cv for mixed effects modeling

m1.formula <- as.formula(PM25 ~ aod_047
                         +Elev.s+ndvi.s+Pop_dens.s+Dis_Rd1_2012.s+P_In.s+P_Ur.s+P_Ag.s+P_OS.s+daily_hpbl.s+Temp_D.s+Rain_D.s+RH_D.s+NO2_D.s+SO2_D.s+WS_D.s
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
