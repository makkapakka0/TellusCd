library(randomForest)
library(ggplot2)
library(data.table)
library(foreach)
library(doParallel)
library(gstat)

#Read data
df<-read.csv('D:\\telluscd\\all3.csv')
  
#RF parameters - Grid search and OOBRMSE
ntree<-c(100,150,200,250,300,350,400,450,500)
mtry<-c(1,2,3,4,5,6)

results<-data.frame()

for (i in ntree) {
  for (k in mtry){
    rf<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
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

#GWRF parameters - Leave-one-out cross validation
setDT(df)
setkey(df, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

param<-data.frame()

for (b in seq(4000,40000,1000)){
  results2 <- foreach(i = 1:nrow(df), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
    row <- df[i]
    df2<-df[-i]
    long1 <- row$x
    lat1 <- row$y
    #Local dataset
    window_data <- df2[sqrt((x-long1)^2+(y-lat1)^2)<b]
    #Avoid some models are not properly trained due to small dataset
    if (nrow(window_data) < 3) {
      return(data.table(pred = -999))
    }
    #Sample weight
    sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/b)^2)^2
    #Local model
    local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
                          ntree=350,
                          mtry=3,
                          weights=sample_weight,
                          data = window_data)
    pred<-predict(local,row)
    #Save predicted data
    data.table(pred = pred)
  }
  #Add raw data
  results2$Cd<-df$Cd
  #Remove -999
  results2<-results2[results2$pred>0,]
  #RMSE
  rmse<-sqrt(sum((results2$pred-results2$Cd)^2)/nrow(results2))
  #Save to dataframe
  rmse<-data.frame(bandwidth=b,rmse=rmse)
  param<-rbind(param,rmse)
}

stopCluster(cl)

#The for loop causes memory issue on my PC, thus I do it individually
band<-read.csv('D:\\telluscd\\band.csv')

ggplot(data=band,aes(x=bandwidth,y=rmse))+
  geom_jitter(size=4,height=0,width=0,alpha=0.5)+
  geom_smooth(method='loess',span=0.3,se=FALSE,color='red',linetype='solid',linewidth=1.5)+
  ylim(0.57,0.63)+
  xlim(0,60000)+
  xlab('Bandwidth (m)')+
  ylab('RMSE (mg/kg)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\bandwidth.tiff',dpi=300,width=12,height=6)
