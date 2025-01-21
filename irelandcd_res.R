library(randomForest)
library(ggplot2)
library(fastshap)
library(sf)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)


df<-read.csv('D:\\telluscd\\all.csv')
df<-df[,c('Cd','x','y','tp','tem','quarry','industry','road','PIPP','soiltype','lc','br500')]


rf<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br500,
                 ntree=400,
                 mtry=2,
                 data=df)

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

merge<-cbind(df,predg)

output<-st_as_sf(merge,coords=c('x','y'),crs=2157)
#save as shapefile
st_write(output,'D:\\telluscd\\working\\resgnst.shp',append=FALSE)

setDT(df)

# Add indices on `long` and `lat` columns
setkey(df, x, y)

numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Parallel processing with foreach
results <- foreach(i = 1:nrow(df), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
  row <- df[i]
  long1 <- row$x
  lat1 <- row$y
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<45000]
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/45000)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br500,
                        ntree=400,
                        mtry=2,
                        weights=sample_weight,
                        data = window_data)
  
  pred<-predict(local,row)
  
  data.table(pred = pred)
}

stopCluster(cl)

merge<-cbind(df,results)

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


output<-st_as_sf(merge,coords=c('x','y'),crs=2157)
#save as shapefile
st_write(output,'D:\\telluscd\\working\\reslnst.shp',append=FALSE)


















