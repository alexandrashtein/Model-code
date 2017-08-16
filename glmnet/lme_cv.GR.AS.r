mons <- unique(mod1[!stn %in% neveruse, stn]) # JR: what is stn? AS: Stn is abbreviation for monitoring stations

xout <- 1 # number of monitors to hold out
n.iter <- 48
setkey(mod1, stnm) 

cvscheme <- list()
cvout <- list()
set.seed(20150112)


library(doParallel)
library(doRNG)

registerDoParallel(14)
# use a proper reproducible backend RNG #JR: I don't think you need it.
registerDoRNG(1234) 
system.time({ #JR: I suspcet the timing may slow the computation (not sure). AS: Ok This part can be removed.
  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dorng% {
    #JR: do you really need %dorng% and not %dopar% ? AS: I got this code from Itai, so I am not sure what were the considerations for that, I will have to read more about the doRNG to see if there is any advantage in using it here. It works fine also with dopar
    
    mons.test <- combn(mons, xout)[,i] # faster to do the splitting outside the loop. OK I will implement that
    cvscheme[[i]] <- mons.test #JR: do you really need cvscheme? AS: No
    test <- mod1[stn %in% mons.test, ] 
    train<- mod1[!stn %in% mons.test, ]
    
    # fit the model
    trainmod <-  lmer(m1.formula, data =  train)
    test$predcv <- predict(object=trainmod, newdata=test, allow.new.levels=TRUE, re.form=NULL )
    
    test$itercv <- i
    # export these results
    test[, list(day, stn, PM25, predcv, itercv)] #JR: I did not know you can extract with a list object :-)
  }# end of cross-validation loop
})

iter.out[, sqrt(mean((PM25 - predcv)^2))] #JR: I am confused-- why is the RMSE used for extracting columns? AS: This part calculates the RMSE, it is the same as: iter.out$resid=iter.out$PM25-iter.out$predcv, print(rmse(iter.out$resid)). This part gives the result of 8.5
