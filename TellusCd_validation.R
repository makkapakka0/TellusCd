library(randomForest)
library(data.table)
library(foreach)
library(doParallel)

df<-read.csv('D:\\telluscd\\all3.csv')

folds<-sample(1:10,size=nrow(df),replace=TRUE)

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



setDT(df)
setkey(df, x, y)

results2<-data.frame(fold=integer(10),performance=numeric(10))



numCores <- detectCores() - 1

sum<-data.frame()
cl <- makeCluster(numCores)
registerDoParallel(cl)

for (i in 1:10){
  df_train<-df[folds!=i]
  df_test<-df[folds==i]
  
  results3 <- foreach(i = 1:nrow(df_test), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
    row <- df_test[i]
    long1 <- row$x
    lat1 <- row$y
    
    window_data <- df_train[sqrt((x-long1)^2+(y-lat1)^2)<45000]
    
    if (nrow(window_data) < 3) {
      return(data.table(pred = -999))
    }
    
    sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/45000)^2)^2
    
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


rmse_r2<-read.csv('D:\\telluscd\\RMSER2.csv')

ggplot(rmse_r2,aes(x=band,y=RMSE))+
  geom_line(color='black',size=1)+
  geom_point(color='black',size=2)+
  labs(x='Bandwidth (m)',y='RMSE (mg/kg)')+
  geom_hline(yintercept=0.616,,linetype='dashed',color='red',size=1)+
  ylim(0.57,0.62)+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\RMSEcom.tiff',dpi=300,width=10,height=6)

ggplot(rmse_r2,aes(x=band,y=R2))+
  geom_line(color='black',size=1)+
  geom_point(color='black',size=2)+
  labs(x='Bandwidth (m)',y='R2')+
  geom_hline(yintercept=0.37,,linetype='dashed',color='red',size=1)+
  ylim(0.36,0.46)+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\R2com.tiff',dpi=300,width=10,height=6)


