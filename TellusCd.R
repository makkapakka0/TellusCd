library(randomForest)
library(ggplot2)
library(fastshap)
library(sf)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)

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

#Read processed Cd data
df<-read.csv('D:\\telluscd\\all3.csv')

#Semivariogram
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

# Global RF model
rf<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                 ntree=350,
                 mtry=3,
                 data=df)

#Preparation for residual analysis
predg<-predict(rf,df)
res<-df$Cd-predg
res<-as.data.frame(res)
res<-res%>%
  mutate(bin=cut(res$res,breaks = seq(floor(min(res$res)),ceiling(max(res$res)),by=0.1)))%>%
  count(bin)%>%
  mutate(
    bin_center=as.numeric(sub('\\((.+),.*','\\1',bin))+0.05,
    log_count=log10(n+1)
  )
breaks_raw<-c(1,10,100,1000)
breaks_log<-log10(breaks_raw+1)
#Residual distribution
ggplot(res,aes(x=bin_center,y=log_count))+
  geom_col(width = 0.1,fill='steelblue')+
  xlab('Residuals')+
  ylab('Count')+
  scale_y_continuous(
    breaks = breaks_log,
    labels = breaks_raw
  )+
  theme_minimal()+
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=24),
        panel.border = element_rect(colour = 'black',fill=NA,size=2))+
  ggsave('D:\\telluscd\\maps\\resg.tiff',dpi=300,width=10,height=2)

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
merge<-cbind(df,shap,predg,res)
names(merge)<-c('Cd','x','y','tp','tem','quarry','industry','road','pH','LOI',
                'PIPP','soiltype','lc','br','ele','tp_s','tem_s',
                'quarry_s','industry_s','road_s','PIPP_s',
                'soiltype_s','lc_s','br_s','pH_s','LOI_s','ele_s','dom','pred','res')

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
#GPU
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
  #Local dataset
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<7500]
  #Sample weights
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/7500)^2)^2
  #Local model
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  #SHAP
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
#Residual
res2<-df$Cd-results$pred
#Merge data
merge2<-cbind(df,results2,results$pred,res2,results$localr)

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

#Residual
res<-as.data.frame(res2)
res<-res%>%
  mutate(bin=cut(res$res,breaks = seq(floor(min(res$res)),ceiling(max(res$res)),by=0.1)))%>%
  count(bin)%>%
  mutate(
    bin_center=as.numeric(sub('\\((.+),.*','\\1',bin))+0.05,
    log_count=log10(n+1)
  )
breaks_raw<-c(1,10,100,1000)
breaks_log<-log10(breaks_raw+1)
ggplot(res,aes(x=bin_center,y=log_count))+
  geom_col(width = 0.1,fill='steelblue')+
  xlab('Residuals')+
  ylab('Count')+
  scale_y_continuous(
    breaks = breaks_log,
    labels = breaks_raw
  )+
  theme_minimal()+
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=24),
        panel.border = element_rect(colour = 'black',fill=NA,size=2))+
  ggsave('D:\\telluscd\\maps\\localres.tiff',dpi=300,width=10,height=2)

#GWRF-Z
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
  #Local dataset
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<7500]
  #Z-score
  window_data$z_scores <- scale(window_data$Cd)
  window_data <- window_data[abs(window_data$z_scores) <= 3, ]
  #Sample weights
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/7500)^2)^2
  #Local model
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  #SHAP
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

results2<-as.data.frame(do.call(rbind,results$shaps))
results2$dom<-colnames(results2[,1:12])[apply(results2[,1:12],1,which.max)]
names(results2)<-c('tp_s','tem_s','quarry_s','industry_s','road_s','pH_s','LOI_s','PIPP_s',
                   'soiltype_s','lc_s','br_s','ele_s','dom')
#Residual
res2<-df$Cd-results$pred
#Merge data
merge2<-cbind(df,results2,results$pred,res2,results$localr)

#Save as shp
output<-st_as_sf(merge2,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\lshapout.shp',append=FALSE)

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

#Residual
res<-as.data.frame(res2)
res<-res%>%
  mutate(bin=cut(res$res,breaks = seq(floor(min(res$res)),ceiling(max(res$res)),by=0.1)))%>%
  count(bin)%>%
  mutate(
    bin_center=as.numeric(sub('\\((.+),.*','\\1',bin))+0.05,
    log_count=log10(n+1)
  )
breaks_raw<-c(1,10,100,1000)
breaks_log<-log10(breaks_raw+1)
ggplot(res,aes(x=bin_center,y=log_count))+
  geom_col(width = 0.1,fill='steelblue')+
  xlab('Residuals')+
  ylab('Count')+
  scale_y_continuous(
    breaks = breaks_log,
    labels = breaks_raw
  )+
  theme_minimal()+
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=24),
        panel.border = element_rect(colour = 'black',fill=NA,size=2))+
  ggsave('D:\\telluscd\\maps\\localoutres.tiff',dpi=300,width=10,height=2)

#GWRF-LISA
df<-read.csv('D:\\telluscd\\all3.csv')
#Coordinates
df_sf<-st_as_sf(df,coords = c('x','y'),crs=2157)
#Local Moran's I
coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,d1=0,d2=4500)
lw<-nb2listw(nb,style = 'W')
local_moran<-localmoran(df_sf$Cd,lw)
#Get I and p-value
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
  #Local dataset
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<4500]
  #Remove outliers
  window_data<-window_data[window_data$Ii>0 | window_data$p>0.05,]
  #In case models are not properly trained due to small dataset
  if (nrow(window_data) < 3) {
    return(data.table(pred = -999))
  }
  #Sample weights
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/4500)^2)^2
  #Local model
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  #Save data
  data.table(pred = pred)
}

stopCluster(cl)

#Merge data
merge2<-cbind(df,results$pred)
merge2$res<-merge2$V2-merge2$Cd

#Save as shp
output<-st_as_sf(merge2,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\lshaplisa.shp',append=FALSE)
