library(randomForest)
library(data.table)
library(foreach)
library(doParallel)
library(ggplot2)
library(sf)
library(spdep)

#Read data
df<-read.csv('D:\\telluscd\\all3.csv')

#10-fold cross validation
folds<-sample(1:10,size=nrow(df),replace=TRUE)

#RF
results<-data.frame(fold=integer(10),performance=numeric(10))
sum<-data.frame()

for (i in 1:10){
  df_train<-df[folds!=i,]
  df_test<-df[folds==i,]
  
  rf_g<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
                     ntree=350,
                     mtry=3,
                     data=df_train)
  pred<-predict(rf_g,df_test)
  
  sum1<-cbind(df_test$Cd,pred)
  sum<-rbind(sum,sum1)
  
  rmse<-sqrt(sum((pred-df_test$Cd)^2)/nrow(df_test))
  
  results$fold[i]<-i
  results$performance[i]<-rmse
}

rmse_g<-mean(results$performance)
R2_g<-1-sum((sum$V1-sum$pred)^2)/sum((sum$V1-mean(sum$V1))^2)



#GWRF
results2<-data.frame(fold=integer(10),performance=numeric(10))
sum<-data.frame()

setDT(df)
setkey(df, x, y)
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
    
    window_data <- df_train[sqrt((x-long1)^2+(y-lat1)^2)<7500]
    
    if (nrow(window_data) < 3) {
      return(data.table(pred = -999))
    }
    
    sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/7500)^2)^2
    
    local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
                          ntree=350,
                          mtry=3,
                          weights=sample_weight,
                          data = window_data)
    pred<-predict(local,row)
    
    data.table(pred = pred)
  }
  
  sum1<-cbind(df_test$Cd,results3)
  sum1<-sum1[sum1$pred>0,]
  sum<-rbind(sum,sum1)

  rmse<-sqrt(sum((sum1$pred-sum1$V1)^2)/nrow(sum1))
  
  results2$fold[i]<-i
  results2$performance[i]<-rmse
  
}

stopCluster(cl)

rmse_l<-mean(results2$performance)
R2_l<-1-sum((sum$V1-sum$pred)^2)/sum((sum$V1-mean(sum$V1))^2)

#GWRF-Z
results_out<-data.frame(fold=integer(10),performance=numeric(10))
sum<-data.frame()

numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

for (i in 1:10){
  df_train<-df[folds!=i]
  df_test<-df[folds==i]
  
  results4 <- foreach(i = 1:nrow(df_test), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
    row <- df_test[i]
    long1 <- row$x
    lat1 <- row$y
    
    window_data <- df_train[sqrt((x-long1)^2+(y-lat1)^2)<40000]
    window_data$z_scores <- scale(window_data$Cd)
    window_data <- window_data[abs(window_data$z_scores) <= 3, ]
    
    if (nrow(window_data) < 3) {
      return(data.table(pred = -999))
    }
    
    sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/40000)^2)^2
    
    local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
                          ntree=350,
                          mtry=3,
                          weights=sample_weight,
                          data = window_data)
    pred<-predict(local,row)
    
    data.table(pred = pred)
  }
  
  sum1<-cbind(df_test$Cd,results4)
  sum1<-sum1[sum1$pred>0,]
  sum<-rbind(sum,sum1)
  
  rmse<-sqrt(sum((sum1$pred-sum1$V1)^2)/nrow(sum1))
  
  results_out$fold[i]<-i
  results_out$performance[i]<-rmse
}

stopCluster(cl)

rmse_l_out<-mean(results_out$performance)
R2_l_out<-1-sum((sum$V1-sum$pred)^2)/sum((sum$V1-mean(sum$V1))^2)

#GWRF-LISA
df<-read.csv('D:\\telluscd\\all3.csv')
df_sf<-st_as_sf(df,coords = c('x','y'),crs=2157)

coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,d1=0,d2=4000)
lw<-nb2listw(nb,style = 'W')
local_moran<-localmoran(df_sf$Cd,lw)

df$Ii<-local_moran[,'Ii']
df$p<-local_moran[,5]

results_lm<-data.frame(fold=integer(10),performance=numeric(10))
sum<-data.frame()

setDT(df)
setkey(df, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

for (i in 1:10){
  df_train<-df[folds!=i]
  
  df_train<-df_train[df_train$Ii>0 | df_train$p>0.05,]
  
  df_test<-df[folds==i]
  
  results4 <- foreach(i = 1:nrow(df_test), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
    row <- df_test[i]
    long1 <- row$x
    lat1 <- row$y
    
    window_data <- df_train[sqrt((x-long1)^2+(y-lat1)^2)<10000]
    
    if (nrow(window_data) < 3) {
      return(data.table(pred = -999))
    }
    
    sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/10000)^2)^2
    
    local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
                          ntree=350,
                          mtry=3,
                          weights=sample_weight,
                          data = window_data)
    pred<-predict(local,row)
    
    data.table(pred = pred)
  }
  
  sum1<-cbind(df_test$Cd,results4)
  sum1<-sum1[sum1$pred>0,]
  sum<-rbind(sum,sum1)
  
  rmse<-sqrt(sum((sum1$pred-sum1$V1)^2)/nrow(sum1))
  
  results_lm$fold[i]<-i
  results_lm$performance[i]<-rmse
  
}

stopCluster(cl)

rmse_lm<-mean(results_lm$performance)
R2_lm<-1-sum((sum$V1-sum$pred)^2)/sum((sum$V1-mean(sum$V1))^2)

#All the results are saved to a csv file
rmse_r2<-read.csv('D:\\telluscd\\RMSER2.csv')
rmse_r2<-na.omit(rmse_r2)

ggplot(rmse_r2,aes(x=band,y=RMSE,group=Group,color=Group))+
  geom_line(size=1.5)+
  geom_point(color='black',size=2)+
  labs(x='Bandwidth (m)',y='RMSE (mg/kg)')+
  ylim(0.54,0.62)+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'),
        legend.position = c(0.8,0.2),
        legend.title = element_text(size=0),
        legend.text = element_text(size=24))+
  ggsave('D:\\telluscd\\maps\\RMSEcom.tiff',dpi=300,width=10,height=6)

ggplot(rmse_r2,aes(x=band,y=R2,group=Group,color=Group))+
  geom_line(size=1.5)+
  geom_point(color='black',size=2)+
  labs(x='Bandwidth (m)',y='R2')+
  ylim(0.35,0.50)+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'),
        legend.position = c(0.8,0.3),
        legend.title = element_text(size=0),
        legend.text = element_text(size=24))+
  ggsave('D:\\telluscd\\maps\\R2com.tiff',dpi=300,width=10,height=6)
