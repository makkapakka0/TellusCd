library(randomForest)
library(data.table)
library(foreach)
library(doParallel)

df<-read.csv('D:\\telluscd\\all.csv')

df<-df[,c('Cd','x','y','tp','tem','quarry','industry','road','PIPP','soiltype','lc','br500')]

folds<-sample(1:10,size=nrow(df),replace=TRUE)

results<-data.frame(fold=integer(10),performance=numeric(10))

for (i in 1:10){
  df_train<-df[folds!=i,]
  df_test<-df[folds==i,]
  
  rf_g<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br500,
                     ntree=400,
                     mtry=2,
                     data=df_train)
  
  pred<-predict(rf_g,df_test)
  
  rmse<-sqrt(sum((pred-df_test$Cd)^2)/nrow(df_test))
  
  results$fold[i]<-i
  results$performance[i]<-rmse
  
}

rmse_g<-mean(results$performance)

setDT(df)
setkey(df, x, y)

results2<-data.frame(fold=integer(10),performance=numeric(10))

numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

for (i in 1:10){
  df_train<-df[folds!=i]
  df_test<-df[folds==i]
  
  results3 <- foreach(i = 1:nrow(df_test), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
    row <- df_test[i]
    long1 <- row$x
    lat1 <- row$y
    
    window_data <- df_train[sqrt((x-long1)^2+(y-lat1)^2)<5500]
    sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/5500)^2)^2
    
    local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br500,
                          ntree=400,
                          mtry=2,
                          weights=sample_weight,
                          data = window_data)
    
    pred<-predict(local,row)
    
    data.table(long = row$x, lat = row$y, pred = pred)
  }
  
  rmse<-sqrt(sum((results3$pred-df_test$Cd)^2)/nrow(results3))
  
  results2$fold[i]<-i
  results2$performance[i]<-rmse
  
}

stopCluster(cl)

rmse_l<-mean(results2$performance)



