library(randomForest)
library(ggplot2)
library(fastshap)
library(sf)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)


df<-read.csv('D:\\telluscd\\all3.csv')
rf<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                 ntree=350,
                 mtry=3,
                 data=df)

predg<-predict(rf,df)
res<-df$Cd-predg
rf[["rsq"]][350]

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
shap$dom<-colnames(shap[,1:12])[apply(shap[,1:12],1,which.max)]

merge<-cbind(df,shap,predg,res)
names(merge)<-c('Cd','x','y','tp','tem','quarry','industry','road','pH','LOI',
                'PIPP','soiltype','lc','br','ele','tp_s','tem_s',
                'quarry_s','industry_s','road_s','PIPP_s',
                'soiltype_s','lc_s','br_s','pH_s','LOI_s','ele_s','dom','pred','res')

merge$tem<-merge$tem-273.15
merge$tp<-merge$tp*3600*24*365*1000

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
#group the data by parent material
gd<-group_by(gd,PIPP)
#get the mean shap value of each type of parent material
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
#group the data by parent material
gd<-group_by(gd,soiltype)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(soiltype_s))
#get the top 5
soiltypedata<-merge[merge$soiltype %in% c('Lac','BminSRPT','BminSW','BminDW','BminSP'),]
#change the id into real name
#plot
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
#group the data by parent material
gd<-group_by(gd,lc)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(lc_s))
#get the top 5
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
#change the id into real name
#plot
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
#group the data by parent material
gd<-group_by(gd,br)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(br_s))
#get the top 5
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

#plot
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

output<-st_as_sf(merge,coords=c('x','y'),crs=2157)
#save as shapefile
st_write(output,'D:\\telluscd\\working\\pointshap8.shp',append=FALSE)



setDT(df)

# Add indices on `long` and `lat` columns
setkey(df, x, y)

numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Parallel processing with foreach
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

# Stop the cluster
stopCluster(cl)

results2<-as.data.frame(do.call(rbind,results$shaps))
results2$dom<-colnames(results2[,1:12])[apply(results2[,1:12],1,which.max)]
names(results2)<-c('tp_s','tem_s','quarry_s','industry_s','road_s','pH_s','LOI_s','PIPP_s',
                   'soiltype_s','lc_s','br_s','ele_s','dom')

res2<-df$Cd-results$pred

merge2<-cbind(df,results2,results$pred,res2,results$localr)

output<-st_as_sf(merge2,coords=c('x','y'),crs=2157)
#save as shapefile
st_write(output,'D:\\telluscd\\working\\lshapfinal.shp',append=FALSE)

merge2$tem<-merge2$tem-273.15
merge2$tp<-merge2$tp*3600*24*365*1000

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
#group the data by parent material
gd<-group_by(gd,soiltype)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(soiltype_s))
#get the top 5
soiltypedata<-merge2[merge2$soiltype %in% c('AlluvMRL','BminSPPT','AlluvMIN','BminPDPT','Cut'),]
#change the id into real name
#plot
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
#group the data by parent material
gd<-group_by(gd,lc)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(lc_s))
#get the top 5
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
#change the id into real name
#plot
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
#group the data by parent material
gd<-group_by(gd,br)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(br_s))
#get the top 5
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

#plot
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



