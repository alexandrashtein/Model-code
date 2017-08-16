# ADD PM2.5 IDW prediction to db

db=readRDS() # Load the needed database
pmall=readRDS()# Load the PM data
pm=filter(pmall, year=y)# Filter the data according to the needed year

for(i in unique(db$day)) { # db is the data base you want to add the results from PM IDW
  
  x<-pm[pm$day==i, ] 
  y= db[db$day==i, ]
  library(gstat)
  library(sp)
  coordinates(x) = ~ x + y 
  coordinates(y) = ~ x_aod + y_aod
  inter = gstat(formula = PM25 ~ 1,  data =x)
  z<-predict(object = inter, newdata = y)
  db$pred[db$day==i] = z$var1.pred
  
}

setnames(db,"pred","PM25_IDW")