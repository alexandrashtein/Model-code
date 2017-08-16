forward.lmer <- function(
  start.model, blocks,
  max.iter=1, sig.level=FALSE,
  zt=FALSE, print.log=TRUE)
{
  
  # forward.lmer: a function for stepwise regression using lmer mixed effects models
  # Author: Rense Nieuwenhuis
  
  # Initialysing internal variables
  log.step <- 0
  log.LL <- log.p <- log.block <- zt.temp <- log.zt <- NA
  model.basis <- start.model
  
  # Maximum number of iterations cannot exceed number of blocks
  if (max.iter > length(blocks)) max.iter <- length(blocks)
  
  # Setting up the outer loop
  for(i in 1:max.iter)
  {
    
    models <- list()
    
    # Iteratively updating the model with addition of one block of variable(s)
    # Also: extracting the loglikelihood of each estimated model
    for(j in 1:length(blocks))
    {
      models[[j]] <- update(model.basis, as.formula(paste(". ~ . + ", blocks[j])))
    }
    
    LL <- unlist(lapply(models, logLik))
    
    # Ordering the models based on their loglikelihood.
    # Additional selection criteria apply
    for (j in order(LL, decreasing=TRUE))
    {
      
      ##############
      ############## Selection based on ANOVA-test
      ##############
      
      if(sig.level != FALSE)
      {
        if(anova(model.basis, models[[j]])[2,7] < sig.level)
        {
          
          model.basis <- models[[j]]
          
          # Writing the logs
          log.step <- log.step + 1
          log.block[log.step] <- blocks[j]
          log.LL[log.step] <- as.numeric(logLik(model.basis))
          log.p[log.step] <- anova(model.basis, models[[j]])[2,7]
          
          blocks <- blocks[-j]
          
          break
        }
      }
      
      ##############
      ############## Selection based significance of added variable-block
      ##############
      
      if(zt != FALSE)
      {
        b.model <- summary(models[[j]])@coefs
        diff.par <- setdiff(rownames(b.model), rownames(summary(model.basis)@coefs))
        if (length(diff.par)==0) break
        sig.par <- FALSE
        
        for (k in 1:length(diff.par))
        {
          if(abs(b.model[which(rownames(b.model)==diff.par[k]),3]) > zt)
          {
            sig.par <- TRUE
            zt.temp <- b.model[which(rownames(b.model)==diff.par[k]),3]
            break
          }
        }
        
        if(sig.par==TRUE)
        {
          model.basis <- models[[j]]
          
          # Writing the logs
          log.step <- log.step + 1
          log.block[log.step] <- blocks[j]
          log.LL[log.step] <- as.numeric(logLik(model.basis))
          log.zt[log.step] <- zt.temp
          blocks <- blocks[-j]
          
          break
        }
      }
    }
  }
  
  ## Create and print log
  log.df <- data.frame(log.step=1:log.step, log.block, log.LL, log.p, log.zt)
  if(print.log == TRUE) print(log.df, digits=4)
  
  ## Return the 'best' fitting model
  return(model.basis)
}