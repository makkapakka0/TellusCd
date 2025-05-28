library(randomForest)
library(ggplot2)
library(fastshap)
library(sf)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)


df<-read.csv('D:\\telluscd\\all3.csv')

rf_g<-randomForest(Cd~tp+tem+road+industry+quarry+PIPP+soiltype+br+lc+LOI+pH+ele,
                   ntree=350,
                   mtry=3,
                   data=df)

predg<-as.data.frame(predict(rf_g,df))
res<-predg-df$Cd
names(res)<-c('res')

densityplot<-data.frame(df$Cd,rf[["predicted"]])
names(densityplot)<-c('Observed','Predicted')

ggplot(densityplot,aes(x=Observed,y=Predicted))+
  geom_hex(bins=100)+
  scale_fill_gradientn(
    colors = c('#4B0082','blue','green','yellow','orange','red'),
    values = scales::rescale(c(0,0.01,0.05,0.2,0.4,1)),
    name='Density'
  )+
  geom_abline(slope = 1,intercept = 0,linetype='dashed',color='red')+
  xlim(0,5)+
  ylim(0,5)+
  xlab('Observed Cd (mg/kg)')+
  ylab('Predicted Cd (mg/kg)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\density.tiff',dpi=300,width=10,height=6)


setDT(df)
setkey(df, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

pred4000 <- foreach(i = 1:nrow(df), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
  row <- df[i]
  long1 <- row$x
  lat1 <- row$y
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<4000]
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/4000)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  
  data.table(pred = pred)
}

stopCluster(cl)

results_p<-cbind(pred4000,pred4500,pred5000,pred5500,pred6000,pred6500,pred7000,
                 pred7500,pred8000,pred8500,pred9000,pred10000,pred15000,pred20000,
                 pred25000,pred30000,pred35000,pred40000)

results_p<-results_p-df$Cd

names(results_p)<-c('pred4000','pred4500','pred5000','pred5500','pred6000',
                    'pred6500','pred7000','pred7500','pred8000','pred8500','pred9000',
                    'pred10000','pred15000','pred20000','pred25000','pred30000',
                    'pred35000','pred40000')

merge<-cbind(df,results_p)

output<-st_as_sf(merge,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\res_g.shp',append=FALSE)

densityplot<-data.frame(df$Cd,results)
names(densityplot)<-c('Observed','Predicted')

ggplot(densityplot,aes(x=Observed,y=Predicted))+
  geom_hex(bins=100)+
  scale_fill_gradientn(
    colors = c('#4B0082','blue','green','yellow','orange','red'),
    values = scales::rescale(c(0,0.01,0.05,0.2,0.4,1)),
    name='Density'
  )+
  geom_abline(slope = 1,intercept = 0,linetype='dashed',color='red')+
  xlim(-3,3)+
  ylim(-3,3)+
  xlab('Observed Cd (mg/kg)')+
  ylab('Predicted Cd (mg/kg)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\densitynstl.tiff',dpi=300,width=10,height=6)
