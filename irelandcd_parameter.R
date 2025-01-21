library(randomForest)
library(ggplot2)
library(data.table)
library(foreach)
library(doParallel)
library(gstat)


df<-read.csv('D:\\telluscd\\all.csv')
df<-df[,c('Cd','x','y','tp','tem','quarry','industry','road','PIPP','soiltype','lc','br500')]

v<-variogram(Cd~1,data=df,locations=~x+y)
plot(v)

ggplot(data=v,aes(x=dist,y=gamma))+
  geom_jitter(size=4,height=0,width=0,alpha=0.5)+
  geom_smooth(method='loess',span=0.9,se=FALSE,color='red',linetype='solid',linewidth=1.5)+
  ylim(0.3,0.6)+
  xlim(0,125000)+
  xlab('Distance')+
  ylab('Semivariance')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\semi.tiff',dpi=300,width=12,height=6)
  
  
ntree<-c(100,150,200,250,300,350,400,450,500)
mtry<-c(1,2,3,4,5,6)

results<-data.frame()

for (i in ntree) {
  for (k in mtry){
    rf<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br500,
                     ntree=i,
                     mtry=k,
                     data=df)
    r<-data.frame(ntree=i,mtry=k,rmse=sqrt(rf[["mse"]][i]))
    results<-rbind(results,r)
  }
}

ggplot(results,aes(x=ntree,y=mtry,color=rmse,size=rmse))+
  geom_point()+
  scale_color_gradient(low='blue',high='red')+
  scale_size_continuous(range=c(6,10))+
  labs(x='ntree',
       y='mtry',
       color='RMSE',
       size='RMSE')+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=24),
        panel.border = element_rect(colour = 'black',fill=NA),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))+
  ggsave('D:\\telluscd\\maps\\gridsearch.tiff',dpi=300,width = 10, height=6)


setDT(df)

# Add indices on `long` and `lat` columns
setkey(df, x, y)

numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

param<-data.frame()

for (b in seq(10000,60000,5000)){
  results2 <- foreach(i = 1:nrow(df), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
    row <- df[i]
    df2<-df[-i]
    long1 <- row$x
    lat1 <- row$y
    
    window_data <- df2[sqrt((x-long1)^2+(y-lat1)^2)<b]
    sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/b)^2)^2
    
    local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br500,
                          ntree=400,
                          mtry=2,
                          weights=sample_weight,
                          data = window_data)
    
    pred<-predict(local,row)
    
    data.table(pred = pred)
  }
  
  rmse<-sqrt(sum((results2$pred-df$Cd)^2)/nrow(df))
  
  rmse<-data.frame(bandwidth=b,rmse=rmse)
  
  param<-rbind(param,rmse)
}

# Parallel processing with foreach

# Stop the cluster
stopCluster(cl)

band<-read.csv('D:\\telluscd\\band.csv')


ggplot(data=band,aes(x=bandwidth,y=rmse))+
  geom_jitter(size=4,height=0,width=0,alpha=0.5)+
  geom_smooth(method='loess',span=0.3,se=FALSE,color='red',linetype='solid',linewidth=1.5)+
  ylim(0.60,0.65)+
  xlim(0,60000)+
  xlab('Bandwidtth')+
  ylab('RMSE (mg/kg)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\bandwidth.tiff',dpi=300,width=12,height=6)








