# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("BB","BH","DBI","DEoptimR","DataCombine","FNN","Formula","Hmisc","Lahman","MEMSS","MatrixModels",
              "PKPDmodels","R.matlab","R.methodsS3","R.oo","R.utils","R6","RColorBrewer","RCurl","RJSONIO",
              "RSQLite","RUnit","Rcgmin","Rcpp","RcppEigen","RgoogleMaps","Rvmmin","SparseM","TH.data","XML",
              "abind","acepack","alr4","aodlur","assertthat","automap","bibtex","biglm","bit","bit64","bitops",
              "brew","broom","car","caret","celestial","chemometrics","chron","coda","colorspace","corrplot",
              "curl","data.table","devtools","dfoptim","dichromat","digest","doBy","doMC","doParallel","doRNG",
              "doRedis","dplyr","e1071","effects","evaluate","fastmatch","foreach","formatR","gam","gamm4",
              "gdata","geepack","geosphere","ggmap","ggplot2","git2r","glmnet","gridExtra","gstat","gsubfn",
              "gtable","gtools","hexbin","highr","htmltools","htmlwidgets","httr","intervals","iterators",
              "itertools","jpeg","jsonlite","kernlab","knitr","labeling","lars","latticeExtra","lazyeval",
              "leaps","lfe","lme4","lmtest","lubridate","magrittr","manipulate","mapproj","maps","maptools",
              "markdown","mclust","memoise","mice","microbenchmark","mime","minqa","mlmRev","mnormt","moonBook",
              "multcomp","munsell","mvtnorm","nlme","nloptr","nortest","numDeriv","nycflights13","optextras",
              "optimx","pbkrtest","pcaPP","pkgmaker","pls","plyr","png","polspline","proto","psych","quadprog",
              "quantreg","randomForest","raster","rbenchmark","readr","registry","reshape","reshape2","rgdal",
              "rgl","rjson","rms","rngtools","robustbase","roxygen2","rredis","rstudio","rstudioapi","rversions",
              "sandwich","sas7bdat","scales","season","setRNG","som","sp","spacetime","sparcl","sqldf","stargazer",
              "stringi","stringr","survey","survival","svUnit","tables","testthat","tidyr","ucminf","whisker","xtable",
              "xts","yaml","zoo","ztable")

ipak(packages)
