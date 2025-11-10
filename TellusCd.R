library(randomForest)
library(ggplot2)
library(fastshap)
library(sf)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)
library(gstat)
library(spdep)
library(car)
library(corrplot)

#Read Cd data
df<-read.csv('D:\\telluscd\\Cd2.csv')
#Cd distribution
ggplot(df,aes(x=Cd))+
  geom_histogram(bins = 100)+
  xlab('Cd (mg/kg)')+
  ylab('Count')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\Cddistribution.tiff',dpi=300,width=10,height=6)
#Cd distribution 0-2 mg/kg
ggplot(df,aes(x=Cd))+
  geom_histogram(bins = 100)+
  xlim(c(0,2))+
  xlab('Cd (mg/kg)')+
  ylab('Count')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\Cddistribution2.tiff',dpi=300,width=10,height=6)

#log Cd
df<-read.csv('D:\\telluscd\\Cd2.csv')

ggplot(df,aes(x=Cd))+
  geom_histogram(bins = 100)+
  geom_density(aes(y=..density..*nrow(df)/7),color='red',size=1.2)+
  xlab('Cd (mg/kg)')+
  ylab('Count')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\Cddistributionorig.tiff',dpi=300,width=10,height=6)

df$Cdlog<-log(df$Cd)
ggplot(df,aes(x=Cdlog))+
  geom_histogram(bins = 100)+
  geom_density(aes(y=..density..*nrow(df)/15),color='red',size=1.2)+
  xlab('Cd (mg/kg)')+
  ylab('Count')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\Cddistributionlog.tiff',dpi=300,width=10,height=6)

#representative table
df<-read.csv('D:\\telluscd\\by_county.csv')

summary_df<-df%>%
  group_by(county)%>%
  summarise(
    n=n(),
    grassland_ratio=mean(lc %in% c('Pastures','Natural grasslands')),
    cropland_ratio=mean(lc %in% c('Land principally occupied by agriculture, with significant areas of natural vegetation',
                                 'Complex cultivation patterns','Non-irrigated arable land')),
    quarry_med=median(quarry),
    industry_med=median(industry),
    road_med=median(road)
  )

summary_total<-df%>%
  summarise(
    n=n(),
    grassland_ratio=mean(lc %in% c('Pastures','Natural grasslands')),
    cropland_ratio=mean(lc %in% c('Land principally occupied by agriculture, with significant areas of natural vegetation',
                                  'Complex cultivation patterns','Non-irrigated arable land')),
    quarry_med=median(quarry),
    industry_med=median(industry),
    road_med=median(road)
  )

write.csv(summary_df,'D:\\telluscd\\by_county_sum.csv')

#labels
df<-read.csv('D:\\telluscd\\all3.csv')
df%>%count(soiltype,sort = TRUE)
df%>%count(br,sort = TRUE)
df%>%count(lc,sort = TRUE)

#Read processed Cd data
df<-read.csv('D:\\telluscd\\all3.csv')

model<-lm(Cd~tp+tem+quarry+industry+road+pH+LOI+ele+factor(PIPP)+factor(soiltype)+
            factor(lc)+factor(br),
          data=df)

vif<-vif(model)
print(vif)

#split data
index<-sample(1:nrow(df),size=0.90*nrow(df))

train<-df[index,]
test<-df[-index,]

#parameter RF OOB RMSE
ntree<-c(100,150,200,250,300,350,400,450,500)
mtry<-c(1,2,3,4,5,6)

results<-data.frame()

for (i in ntree) {
  for (k in mtry){
    rf<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
                     ntree=i,
                     mtry=k,
                     data=train)
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
setDT(train)
setkey(train, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

param<-data.frame()

for (b in seq(4000,40000,500)){
  results2 <- foreach(i = 1:nrow(train), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
    row <- train[i]
    train2<-train[-i]
    long1 <- row$x
    lat1 <- row$y
    
    window_data <- train2[sqrt((x-long1)^2+(y-lat1)^2)<b]
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
  results2$Cd<-train$Cd
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
  xlim(0,41000)+
  xlab('Bandwidth (m)')+
  ylab('RMSE (mg/kg)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\bandwidth.tiff',dpi=300,width=12,height=6)

#validation RF
rf_g<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+LOI+pH+ele,
                   ntree=350,
                   mtry=3,
                   data=train)
pred<-predict(rf_g,test)
rmse_g<-sqrt(sum((pred-test$Cd)^2)/nrow(test))
R2_g<-1-sum((test$Cd-pred)^2)/sum((test$Cd-mean(test$Cd))^2)

#validation GWRF
setDT(train)
setkey(train, x, y)
setDT(test)
setkey(test, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

results3 <- foreach(i = 1:nrow(test), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
  row <- test[i]
  long1 <- row$x
  lat1 <- row$y
  
  window_data <- train[sqrt((x-long1)^2+(y-lat1)^2)<7500]
  
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

stopCluster(cl)

rmse_l<-sqrt(sum((results3$pred-test$Cd)^2)/nrow(test))
R2_l<-1-sum((test$Cd-results3$pred)^2)/sum((test$Cd-mean(test$Cd))^2)

#validation GWRF-Z
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

results4 <- foreach(i = 1:nrow(test), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
  row <- test[i]
  long1 <- row$x
  lat1 <- row$y
  
  window_data <- train[sqrt((x-long1)^2+(y-lat1)^2)<7500]
  window_data$z_scores <- scale(log(window_data$Cd))
  window_data <- window_data[abs(window_data$z_scores) <= 3, ]
  
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

stopCluster(cl)

rmse_lz<-sqrt(sum((results4$pred-test$Cd)^2)/nrow(test))
R2_lz<-1-sum((test$Cd-results4$pred)^2)/sum((test$Cd-mean(test$Cd))^2)

#validation RF-LISA
df_sf<-st_as_sf(train,coords = c('x','y'),crs=2157)

coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,d1=0,d2=7500)
lw<-nb2listw(nb,style = 'W')
local_moran<-localmoran(log(df_sf$Cd),lw)

train$Ii<-local_moran[,'Ii']
train$p<-local_moran[,5]

setDT(train)
setkey(train, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

df_train<-train[train$Ii>0 | train$p>0.05,]

results5 <- foreach(i = 1:nrow(test), .combine = rbind, .packages = c('randomForest', 'data.table')) %dopar% {
  row <- test[i]
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

stopCluster(cl)

rmse_lm<-sqrt(sum((results5$pred-test$Cd)^2)/nrow(test))
R2_lm<-1-sum((test$Cd-results5$pred)^2)/sum((test$Cd-mean(test$Cd))^2)

# Global RF model
rf<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                 ntree=350,
                 mtry=3,
                 data=df)

#SHAP
df_shap<-df[ ,c('tp','tem','quarry','industry','road','PIPP','soiltype','lc','br','pH','LOI','ele')]
shap<-fastshap::explain(rf,
                        X = df_shap,
                        nsim = 100,
                        baseline = 0,
                        adjust = TRUE,
                        pred_wrapper = function(model,newdata){
                          predict(model,newdata)
                        },
                        newdata = df_shap)
shap<-as.data.frame(shap)

#Dominant variable
shap$dom<-colnames(shap[,1:12])[apply(shap[,1:12],1,which.max)]

#Merge raw data and SHAP
merge<-cbind(df,shap,predg)
names(merge)<-c('Cd','x','y','tp','tem','quarry','industry','road','pH','LOI',
                'PIPP','soiltype','lc','br','ele','tp_s','tem_s',
                'quarry_s','industry_s','road_s','PIPP_s',
                'soiltype_s','lc_s','br_s','pH_s','LOI_s','ele_s','dom','pred')

#Transform temperature and precipitation
merge$tem<-merge$tem-273.15
merge$tp<-merge$tp*3600*24*365*1000

#Plots
ggplot(merge,aes(x=tp,y=tp_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1.5)+
  xlim(800,2000)+
  xlab('Annual precipitation (mm/y)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\tpshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=tem,y=tem_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1)+
  xlim(7,11)+
  xlab('Annual mean temperature (°C)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\temshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=quarry,y=quarry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,5)+
  xlab('Quarry density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\quarryshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=industry,y=industry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,30)+
  xlab('Industry density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\industryshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=road,y=road_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,30)+
  xlab('Road density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\roadshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=pH,y=pH_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.25,1.25)+
  xlim(2,8)+
  xlab('pH')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\phshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=LOI,y=LOI_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.25,1.25)+
  xlim(0,100)+
  xlab('LOI (%)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\loishap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=ele,y=ele_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,600)+
  xlab('Elevation (m)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\eleshap.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('PIPP','PIPP_s')]
gd<-group_by(gd,PIPP)
smd<-summarise(gd,mean_value=mean(PIPP_s))
merge$PIPP[merge$PIPP=='PIP Rank 1']<-c('Rank 1')
merge$PIPP[merge$PIPP=='PIP Rank 2']<-c('Rank 2')
merge$PIPP[merge$PIPP=='PIP Rank 3']<-c('Rank 3')
merge$PIPP[merge$PIPP=='PIP Rank 4']<-c('Rank 4')
merge$PIPP[merge$PIPP=='PIP Rank 5']<-c('Rank 5')
merge$PIPP[merge$PIPP=='PIP Rank 6']<-c('Rank 6')
merge$PIPP[merge$PIPP=='PIP Rank 7']<-c('Rank 7')
merge$PIPP[merge$PIPP=='no rank']<-c('No rank')
ggplot(merge,aes(x=PIPP,y=PIPP_s,group=PIPP))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,0.6)+
  xlab('PIP_P')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\PIPPshap.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('soiltype','soiltype_s')]
gd<-group_by(gd,soiltype)
smd<-summarise(gd,mean_value=mean(soiltype_s))
soiltypedata<-merge[merge$soiltype %in% c('Lac','BminSRPT','BminSW','BminDW','BminSP'),]
ggplot(soiltypedata,aes(x=soiltype,y=soiltype_s,group=soiltype))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,1)+
  xlab('Soil type')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\soiltypeshap.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('lc','lc_s')]
gd<-group_by(gd,lc)
smd<-summarise(gd,mean_value=mean(lc_s))
lcdata<-merge[merge$lc %in% c('Inland marshes','Broad-leaved forest',
                              'Complex cultivation patterns','Industrial or commercial units',
                              'Discontinuous urban fabric'),]
lcdata$lc[lcdata$lc=='Inland marshes']<-c('Inland 
marshes')
lcdata$lc[lcdata$lc=='Broad-leaved forest']<-c('Broad-leaved 
forest')
lcdata$lc[lcdata$lc=='Complex cultivation patterns']<-c('Complex
cultivation
patterns')
lcdata$lc[lcdata$lc=='Industrial or commercial units']<-c('Industrial 
or commercial 
units')
lcdata$lc[lcdata$lc=='Discontinuous urban fabric']<-c('Discontinuous
urban
fabric')
ggplot(lcdata,aes(x=lc,y=lc_s,group=lc))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.1,0.5)+
  xlab('Land cover')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\lcshap.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('br','br_s')]
gd<-group_by(gd,br)
smd<-summarise(gd,mean_value=mean(br_s))
brdata<-merge[merge$br %in% c('71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal',
                              '65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale',
                              '62, Waulsortian mudbank; Pale-grey massive limestone',
                              '68, Marginal marine (Meenymore Formation); Mudstone, sandstone & evaporite',
                              '61, Marine shelf & ramp facies; Argillaceous dark-grey bioclastic limestone, subsidiary shale'),]
brdata$br[brdata$br=='71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal']<-71
brdata$br[brdata$br=='65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale']<-65
brdata$br[brdata$br=='62, Waulsortian mudbank; Pale-grey massive limestone']<-62
brdata$br[brdata$br=='68, Marginal marine (Meenymore Formation); Mudstone, sandstone & evaporite']<-68
brdata$br[brdata$br=='61, Marine shelf & ramp facies; Argillaceous dark-grey bioclastic limestone, subsidiary shale']<-61
ggplot(brdata,aes(x=br,y=br_s,group=br))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,1)+
  xlab('Bedrock')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\br500shap.tiff',dpi=300,width=10,height=6)

#Export results as shp
output<-st_as_sf(merge,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\pointshap8.shp',append=FALSE)

#GWRF
df<-read.csv('D:\\telluscd\\all3.csv')

setDT(df)
setkey(df, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

#Parallel
results <- foreach(i = 1:nrow(df), .combine = rbind, .packages = c('randomForest', 'data.table','fastshap')) %dopar% {
  row <- df[i]
  long1 <- row$x
  lat1 <- row$y
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<7500]
  
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/7500)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  
  row<-row[,4:15]
  shap<-fastshap::explain(local,
                          X = window_data[,4:15],
                          nsim = 100,
                          baseline = 0,
                          adjust = TRUE,
                          pred_wrapper = function(model,newdata){
                            predict(local,newdata)
                          },
                          newdata = row)
  
  data.table(pred = pred, shaps=list(shap),localr=local[["rsq"]][350],importance=list(importance(local)))
}

stopCluster(cl)

#Dominant variable
results2<-as.data.frame(do.call(rbind,results$shaps))
results2$dom<-colnames(results2[,1:12])[apply(results2[,1:12],1,which.max)]
names(results2)<-c('tp_s','tem_s','quarry_s','industry_s','road_s','pH_s','LOI_s','PIPP_s',
                   'soiltype_s','lc_s','br_s','ele_s','dom')

#Merge data
merge2<-cbind(df,results2,results$pred,results$localr)

#save as shapefile
output<-st_as_sf(merge2,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\lshapfinal.shp',append=FALSE)

#Transform temperature and precipitation
merge2$tem<-merge2$tem-273.15
merge2$tp<-merge2$tp*3600*24*365*1000

#Plots
ggplot(merge2,aes(x=tp,y=tp_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1.5)+
  xlim(800,2000)+
  xlab('Annual precipitation (mm/y)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\tplshap.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=tem,y=tem_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1)+
  xlim(7,11)+
  xlab('Annual mean temperature (°C)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\temlshap.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=quarry,y=quarry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,5)+
  xlab('Quarry density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\quarrylshap.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=industry,y=industry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,30)+
  xlab('Industry density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\industrylshap.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=road,y=road_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,30)+
  xlab('Road density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\roadlshap.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=pH,y=pH_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.25,1.25)+
  xlim(2,8)+
  xlab('pH')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\phlshap.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=LOI,y=LOI_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.25,1.25)+
  xlim(0,100)+
  xlab('LOI (%)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\loilshap.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=ele,y=ele_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,600)+
  xlab('Elevation (m)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\elelshap.tiff',dpi=300,width=10,height=6)

merge2$PIPP[merge2$PIPP=='PIP Rank 1']<-c('Rank 1')
merge2$PIPP[merge2$PIPP=='PIP Rank 2']<-c('Rank 2')
merge2$PIPP[merge2$PIPP=='PIP Rank 3']<-c('Rank 3')
merge2$PIPP[merge2$PIPP=='PIP Rank 4']<-c('Rank 4')
merge2$PIPP[merge2$PIPP=='PIP Rank 5']<-c('Rank 5')
merge2$PIPP[merge2$PIPP=='PIP Rank 6']<-c('Rank 6')
merge2$PIPP[merge2$PIPP=='PIP Rank 7']<-c('Rank 7')
merge2$PIPP[merge2$PIPP=='no rank']<-c('No rank')
ggplot(merge2,aes(x=PIPP,y=PIPP_s,group=PIPP))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,0.6)+
  xlab('PIP_P')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\PIPPlshap.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('soiltype','soiltype_s')]
gd<-group_by(gd,soiltype)
smd<-summarise(gd,mean_value=mean(soiltype_s))
soiltypedata<-merge2[merge2$soiltype %in% c('AlluvMRL','BminSPPT','AlluvMIN','BminPDPT','Cut'),]
ggplot(soiltypedata,aes(x=soiltype,y=soiltype_s,group=soiltype))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,1)+
  xlab('Soil type')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=22),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\soiltypleshap.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('lc','lc_s')]
gd<-group_by(gd,lc)
smd<-summarise(gd,mean_value=mean(lc_s))
lcdata<-merge2[merge2$lc %in% c('Inland marshes','Sport and leisure facilities',
                              'Green urban areas','Non-irrigated arable land',
                              'Complex cultivation patterns'),]
lcdata$lc[lcdata$lc=='Inland marshes']<-c('Inland 
marshes')
lcdata$lc[lcdata$lc=='Sport and leisure facilities']<-c('Sport 
and 
leisure
facilities')
lcdata$lc[lcdata$lc=='Green urban areas']<-c('Green 
urban
areas')
lcdata$lc[lcdata$lc=='Non-irrigated arable land']<-c('Non-irrigated 
arable 
land')
lcdata$lc[lcdata$lc=='Complex cultivation patterns']<-c('Complex 
cultivation 
patterns')
ggplot(lcdata,aes(x=lc,y=lc_s,group=lc))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.1,0.5)+
  xlab('Land cover')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\lclshap.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('br','br_s')]
gd<-group_by(gd,br)
smd<-summarise(gd,mean_value=mean(br_s))
brdata<-merge2[merge2$br %in% c('62, Waulsortian mudbank; Pale-grey massive limestone',
                              '48, Transgression - regression sequence (Killary Hbr - Joyces Country Succ.); Sandstone, conglomerate, greywacke, mudstone, tuff',
                              '9, Granite, granophyre',
                              '60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite',
                              '65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale'),]
brdata$br[brdata$br=='62, Waulsortian mudbank; Pale-grey massive limestone']<-62
brdata$br[brdata$br=='48, Transgression - regression sequence (Killary Hbr - Joyces Country Succ.); Sandstone, conglomerate, greywacke, mudstone, tuff']<-48
brdata$br[brdata$br=='9, Granite, granophyre']<-9
brdata$br[brdata$br=='60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite']<-60
brdata$br[brdata$br=='65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale']<-65
ggplot(brdata,aes(x=br,y=br_s,group=br))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,1)+
  xlab('Bedrock')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\brlshap.tiff',dpi=300,width=10,height=6)

#GWRF-Z
df<-read.csv('D:\\telluscd\\all3.csv')

setDT(df)
setkey(df, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

#Parallel
results <- foreach(i = 1:nrow(df), .combine = rbind, .packages = c('randomForest', 'data.table','fastshap')) %dopar% {
  row <- df[i]
  long1 <- row$x
  lat1 <- row$y
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<7500]
  
  window_data$z_scores <- scale(log(window_data$Cd))
  window_data <- window_data[abs(window_data$z_scores) <= 3, ]
  
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/7500)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  
  row<-row[,4:15]
  shap<-fastshap::explain(local,
                          X = window_data[,4:15],
                          nsim = 100,
                          baseline = 0,
                          adjust = TRUE,
                          pred_wrapper = function(model,newdata){
                            predict(local,newdata)
                          },
                          newdata = row)
  
  data.table(pred = pred, shaps=list(shap),localr=local[["rsq"]][350],importance=list(importance(local)))
}

stopCluster(cl)

results2<-as.data.frame(do.call(rbind,results$shaps))
results2$dom<-colnames(results2[,1:12])[apply(results2[,1:12],1,which.max)]
names(results2)<-c('tp_s','tem_s','quarry_s','industry_s','road_s','pH_s','LOI_s','PIPP_s',
                   'soiltype_s','lc_s','br_s','ele_s','dom')

#Merge data
merge2<-cbind(df,results2,results$pred,results$localr)

#Save as shp
output<-st_as_sf(merge2,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\lshapoutfff.shp',append=FALSE)

#transform temperature and precipitation
merge2$tem<-merge2$tem-273.15
merge2$tp<-merge2$tp*3600*24*365*1000

#Plots
ggplot(merge2,aes(x=tp,y=tp_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1.5)+
  xlim(800,2000)+
  xlab('Annual precipitation (mm/y)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\tplshapout.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=tem,y=tem_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1)+
  xlim(7,11)+
  xlab('Annual mean temperature (°C)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\temlshapout.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=quarry,y=quarry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,5)+
  xlab('Quarry density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\quarrylshapout.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=industry,y=industry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,30)+
  xlab('Industry density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\industrylshapout.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=road,y=road_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,30)+
  xlab('Road density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\roadlshapout.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=pH,y=pH_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.25,1.25)+
  xlim(2,8)+
  xlab('pH')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\phlshapout.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=LOI,y=LOI_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.25,1.25)+
  xlim(0,100)+
  xlab('LOI (%)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\loilshapout.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=ele,y=ele_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,600)+
  xlab('Elevation (m)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\elelshapout.tiff',dpi=300,width=10,height=6)

merge2$PIPP[merge2$PIPP=='PIP Rank 1']<-c('Rank 1')
merge2$PIPP[merge2$PIPP=='PIP Rank 2']<-c('Rank 2')
merge2$PIPP[merge2$PIPP=='PIP Rank 3']<-c('Rank 3')
merge2$PIPP[merge2$PIPP=='PIP Rank 4']<-c('Rank 4')
merge2$PIPP[merge2$PIPP=='PIP Rank 5']<-c('Rank 5')
merge2$PIPP[merge2$PIPP=='PIP Rank 6']<-c('Rank 6')
merge2$PIPP[merge2$PIPP=='PIP Rank 7']<-c('Rank 7')
merge2$PIPP[merge2$PIPP=='no rank']<-c('No rank')
ggplot(merge2,aes(x=PIPP,y=PIPP_s,group=PIPP))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,0.6)+
  xlab('PIP_P')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\PIPPlshapout.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('soiltype','soiltype_s')]
gd<-group_by(gd,soiltype)
smd<-summarise(gd,mean_value=mean(soiltype_s))
soiltypedata<-merge2[merge2$soiltype %in% c('AlluvMRL','BminPD','AlluvMIN','BminPDPT','Cut'),]
ggplot(soiltypedata,aes(x=soiltype,y=soiltype_s,group=soiltype))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,1)+
  xlab('Soil type')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=22),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\soiltypleshapout.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('lc','lc_s')]
gd<-group_by(gd,lc)
smd<-summarise(gd,mean_value=mean(lc_s))
lcdata<-merge2[merge2$lc %in% c('Sport and leisure facilities',
                                'Green urban areas','Non-irrigated arable land',
                                'Complex cultivation patterns','Discontinuous urban fabric'),]
lcdata$lc[lcdata$lc=='Discontinuous urban fabric']<-c('Discontinuous
urban 
fabric')
lcdata$lc[lcdata$lc=='Sport and leisure facilities']<-c('Sport 
and 
leisure
facilities')
lcdata$lc[lcdata$lc=='Green urban areas']<-c('Green 
urban
areas')
lcdata$lc[lcdata$lc=='Non-irrigated arable land']<-c('Non-irrigated
arable
land')
lcdata$lc[lcdata$lc=='Complex cultivation patterns']<-c('Complex 
cultivation 
patterns')
ggplot(lcdata,aes(x=lc,y=lc_s,group=lc))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.1,0.5)+
  xlab('Land cover')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\lclshapout.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('br','br_s')]
gd<-group_by(gd,br)
smd<-summarise(gd,mean_value=mean(br_s))
brdata<-merge2[merge2$br %in% c('62, Waulsortian mudbank; Pale-grey massive limestone',
                                '48, Transgression - regression sequence (Killary Hbr - Joyces Country Succ.); Sandstone, conglomerate, greywacke, mudstone, tuff',
                                '9, Granite, granophyre',
                                '60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite',
                                '65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale'),]
brdata$br[brdata$br=='62, Waulsortian mudbank; Pale-grey massive limestone']<-62
brdata$br[brdata$br=='48, Transgression - regression sequence (Killary Hbr - Joyces Country Succ.); Sandstone, conglomerate, greywacke, mudstone, tuff']<-48
brdata$br[brdata$br=='9, Granite, granophyre']<-9
brdata$br[brdata$br=='60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite']<-60
brdata$br[brdata$br=='65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale']<-65
ggplot(brdata,aes(x=br,y=br_s,group=br))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,1)+
  xlab('Bedrock')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\brlshapout.tiff',dpi=300,width=10,height=6)

#GWRF-LISA
df<-read.csv('D:\\telluscd\\all3.csv')

df_sf<-st_as_sf(df,coords = c('x','y'),crs=2157)

coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,d1=0,d2=7500)
lw<-nb2listw(nb,style = 'W')
local_moran<-localmoran(log(df_sf$Cd),lw)

df$Ii<-local_moran[,'Ii']
df$p<-local_moran[,5]

setDT(df)
setkey(df, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

results <- foreach(i = 1:nrow(df), .combine = rbind, .packages = c('randomForest', 'data.table','fastshap')) %dopar% {
  row <- df[i]
  long1 <- row$x
  lat1 <- row$y
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<7500]
  
  window_data<-window_data[window_data$Ii>0 | window_data$p>0.05,]
  
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/7500)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  
  row<-row[,4:15]
  shap<-fastshap::explain(local,
                          X = window_data[,4:15],
                          nsim = 100,
                          baseline = 0,
                          adjust = TRUE,
                          pred_wrapper = function(model,newdata){
                            predict(local,newdata)
                          },
                          newdata = row)
  
  data.table(pred = pred, shaps=list(shap),localr=local[["rsq"]][350],importance=list(importance(local)))
}

stopCluster(cl)

#Merge data
results2<-as.data.frame(do.call(rbind,results$shaps))
results2$dom<-colnames(results2[,1:12])[apply(results2[,1:12],1,which.max)]
names(results2)<-c('tp_s','tem_s','quarry_s','industry_s','road_s','pH_s','LOI_s','PIPP_s',
                   'soiltype_s','lc_s','br_s','ele_s','dom')

#Merge data
merge2<-cbind(df,results2,results$pred,results$localr)

#Save as shp
output<-st_as_sf(merge2,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\lshapoutlisafff.shp',append=FALSE)

#transform temperature and precipitation
merge2$tem<-merge2$tem-273.15
merge2$tp<-merge2$tp*3600*24*365*1000

#Plots
ggplot(merge2,aes(x=tp,y=tp_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1.5)+
  xlim(800,2000)+
  xlab('Annual precipitation (mm/y)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\tplshapoutlisa.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=tem,y=tem_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1)+
  xlim(7,11)+
  xlab('Annual mean temperature (°C)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\temlshapoutlisa.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=quarry,y=quarry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,5)+
  xlab('Quarry density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\quarrylshapoutlisa.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=industry,y=industry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,30)+
  xlab('Industry density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\industrylshapoutlisa.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=road,y=road_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,30)+
  xlab('Road density')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\roadlshapoutlisa.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=pH,y=pH_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.25,1.25)+
  xlim(2,8)+
  xlab('pH')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\phlshapoutlisa.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=LOI,y=LOI_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.25,1.25)+
  xlim(0,100)+
  xlab('LOI (%)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\loilshapoutlisa.tiff',dpi=300,width=10,height=6)

ggplot(merge2,aes(x=ele,y=ele_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.2,1)+
  xlim(0,600)+
  xlab('Elevation (m)')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\elelshapoutlisa.tiff',dpi=300,width=10,height=6)

merge2$PIPP[merge2$PIPP=='PIP Rank 1']<-c('Rank 1')
merge2$PIPP[merge2$PIPP=='PIP Rank 2']<-c('Rank 2')
merge2$PIPP[merge2$PIPP=='PIP Rank 3']<-c('Rank 3')
merge2$PIPP[merge2$PIPP=='PIP Rank 4']<-c('Rank 4')
merge2$PIPP[merge2$PIPP=='PIP Rank 5']<-c('Rank 5')
merge2$PIPP[merge2$PIPP=='PIP Rank 6']<-c('Rank 6')
merge2$PIPP[merge2$PIPP=='PIP Rank 7']<-c('Rank 7')
merge2$PIPP[merge2$PIPP=='no rank']<-c('No rank')
ggplot(merge2,aes(x=PIPP,y=PIPP_s,group=PIPP))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,0.6)+
  xlab('PIP_P')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\PIPPlshapoutlisa.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('soiltype','soiltype_s')]
gd<-group_by(gd,soiltype)
smd<-summarise(gd,mean_value=mean(soiltype_s))
soiltypedata<-merge2[merge2$soiltype %in% c('AlluvMRL','BminPD','AlluvMIN','BminPDPT','Cut'),]
ggplot(soiltypedata,aes(x=soiltype,y=soiltype_s,group=soiltype))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,1)+
  xlab('Soil type')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=22),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\soiltypleshapoutlisa.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('lc','lc_s')]
gd<-group_by(gd,lc)
smd<-summarise(gd,mean_value=mean(lc_s))
lcdata<-merge2[merge2$lc %in% c('Sport and leisure facilities',
                                'Green urban areas','Inland marshes',
                                'Complex cultivation patterns','Discontinuous urban fabric'),]
lcdata$lc[lcdata$lc=='Discontinuous urban fabric']<-c('Discontinuous
urban 
fabric')
lcdata$lc[lcdata$lc=='Sport and leisure facilities']<-c('Sport 
and 
leisure
facilities')
lcdata$lc[lcdata$lc=='Green urban areas']<-c('Green 
urban
areas')
lcdata$lc[lcdata$lc=='Inland marshes']<-c('Inland
marshes')
lcdata$lc[lcdata$lc=='Complex cultivation patterns']<-c('Complex 
cultivation 
patterns')
ggplot(lcdata,aes(x=lc,y=lc_s,group=lc))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.1,0.5)+
  xlab('Land cover')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\lclshapoutlisa.tiff',dpi=300,width=10,height=6)

gd<-merge2[,c('br','br_s')]
gd<-group_by(gd,br)
smd<-summarise(gd,mean_value=mean(br_s))
brdata<-merge2[merge2$br %in% c('62, Waulsortian mudbank; Pale-grey massive limestone',
                                '71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal',
                                '9, Granite, granophyre',
                                '60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite',
                                '65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale'),]
brdata$br[brdata$br=='62, Waulsortian mudbank; Pale-grey massive limestone']<-62
brdata$br[brdata$br=='71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal']<-71
brdata$br[brdata$br=='9, Granite, granophyre']<-9
brdata$br[brdata$br=='60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite']<-60
brdata$br[brdata$br=='65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale']<-65
ggplot(brdata,aes(x=br,y=br_s,group=br))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,1)+
  xlab('Bedrock')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\brlshapoutlisa.tiff',dpi=300,width=10,height=6)


#bandwidth interpretability
df<-read.csv('D:\\telluscd\\all3.csv')

setDT(df)
setkey(df, x, y)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

#Parallel
results <- foreach(i = 1:nrow(df), .combine = rbind, .packages = c('randomForest', 'data.table','fastshap')) %dopar% {
  row <- df[i]
  long1 <- row$x
  lat1 <- row$y
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<30000]
  
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/30000)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  
  row<-row[,4:15]
  shap<-fastshap::explain(local,
                          X = window_data[,4:15],
                          nsim = 100,
                          baseline = 0,
                          adjust = TRUE,
                          pred_wrapper = function(model,newdata){
                            predict(local,newdata)
                          },
                          newdata = row)
  #Save data
  data.table(pred = pred, shaps=list(shap),localr=local[["rsq"]][350],importance=list(importance(local)))
}

stopCluster(cl)

#Dominant variable
results2<-as.data.frame(do.call(rbind,results$shaps))
results2$dom<-colnames(results2[,1:12])[apply(results2[,1:12],1,which.max)]
names(results2)<-c('tp_s','tem_s','quarry_s','industry_s','road_s','pH_s','LOI_s','PIPP_s',
                   'soiltype_s','lc_s','br_s','ele_s','dom')

#Merge data
merge2<-cbind(df,results2,results$pred,results$localr)

#save as shapefile
output<-st_as_sf(merge2,coords=c('x','y'),crs=2157)

#how many times each sample used in modelling

df<-read.csv('D:\\telluscd\\all3.csv')

df$ID<-1:nrow(df)

numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)
setDT(df)

#Z-score
indices_list <- foreach(i = 1:nrow(df), .combine = 'c', .packages = 'data.table') %dopar% {
  long1 <- df$x[i]
  lat1 <- df$y[i]
  
  window_data <- df[sqrt((x - long1)^2 + (y - lat1)^2) < 7500]
  window_data$z_scores <- scale(log(window_data$Cd))
  window_data <- window_data[abs(window_data$z_scores) <= 3, ]
  
  list(window_data$ID)
}

total_list <- foreach(i = 1:nrow(df), .combine = 'c', .packages = 'data.table') %dopar% {
  long1 <- df$x[i]
  lat1 <- df$y[i]
  
  window_data <- df[sqrt((x - long1)^2 + (y - lat1)^2) < 7500]
  
  list(window_data$ID)
}

stopCluster(cl)

window_indices <- unlist(indices_list)
window_count <- table(window_indices)

total_indices<-unlist(total_list)
total_count<-table(total_indices)

df$count <- 0
df$count[as.numeric(names(window_count))] <- as.integer(window_count)

df$total<-0
df$total[as.numeric(names(total_count))] <- as.integer(total_count)

df$ratio<-df$count/df$total

output<-st_as_sf(df,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\appearcount.shp',append=FALSE)

ggplot(df,aes(x=Cd,y=count))+
  geom_point(size=0.8,alpha=0.2)+
  ylim(0,180)+
  xlim(0,15)+
  xlab('Cd (mg/kg)')+
  ylab('Count')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\count.tiff',dpi=300,width=10,height=6)

ggplot(df,aes(x=Cd,y=ratio))+
  geom_point(size=0.8,alpha=0.2)+
  ylim(0,1)+
  xlim(0,15)+
  xlab('Cd (mg/kg)')+
  ylab('Ratio')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\count_ratio.tiff',dpi=300,width=10,height=6)

#LISA
df<-read.csv('D:\\telluscd\\all3.csv')
df_sf<-st_as_sf(df,coords = c('x','y'),crs=2157)

coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,d1=0,d2=7500)
lw<-nb2listw(nb,style = 'W')
local_moran<-localmoran(log(df_sf$Cd),lw)

df$Ii<-local_moran[,'Ii']
df$p<-local_moran[,5]

df<-df%>%
  mutate(flag=ifelse(df$Ii>0 | df$p>0.05,1,0))

ggplot(df,aes(x=Cd,y=flag))+
  geom_point(size=0.8,alpha=0.2)+
  ylim(-0.2,1.2)+
  xlim(0,15)+
  xlab('Cd (mg/kg)')+
  ylab('Count')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\count_lisa.tiff',dpi=300,width=10,height=6)

output<-st_as_sf(df,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\appearcount_lisa.shp',append=FALSE)


st_write(output,'D:\\telluscd\\working\\lshapfinal300.shp',append=FALSE)


