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

predg<-predict(rf,df)

rf[["rsq"]][400]

df_shap<-df[ ,c('tp','tem','quarry','industry','road','PIPP','soiltype','lc','br500')]
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
shap$dom<-colnames(shap[,1:9])[apply(shap[,1:9],1,which.max)]
res<-df$Cd-predg

merge<-cbind(df,shap,predg,res)
names(merge)<-c('Cd','x','y','tp','tem','quarry','industry','road',
                'PIPP','soiltype','lc','br500','tp_s','tem_s',
                'quarry_s','industry_s','road_s','PIPP_s',
                'soiltype_s','lc_s','br500_s','dom','pred','res')

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
  ylim(-0.5,1)+
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
  ylim(-0.5,1)+
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
  ylim(-0.5,1)+
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
soiltypedata<-merge[merge$soiltype %in% c('Lac','BminSRPT','BminSW','BminPDPT','BminSP'),]
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

gd<-merge[,c('br500','br500_s')]
#group the data by parent material
gd<-group_by(gd,br500)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(br500_s))
#get the top 5
br500data<-merge[merge$br500 %in% c('71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal',
                                    '65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale',
                                    '62, Waulsortian mudbank; Pale-grey massive limestone',
                                    '68, Marginal marine (Meenymore Formation); Mudstone, sandstone & evaporite',
                                    '60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite'),]
br500data$br500[br500data$br500=='71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal']<-71
br500data$br500[br500data$br500=='65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale']<-65
br500data$br500[br500data$br500=='62, Waulsortian mudbank; Pale-grey massive limestone']<-62

br500data$br500[br500data$br500=='68, Marginal marine (Meenymore Formation); Mudstone, sandstone & evaporite']<-68
br500data$br500[br500data$br500=='60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite']<-60

#plot
ggplot(br500data,aes(x=br500,y=br500_s,group=br500))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0,1)+
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
st_write(output,'D:\\telluscd\\working\\pointshap7.shp',append=FALSE)



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
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<5500]
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/5500)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br500,
                        ntree=400,
                        mtry=2,
                        weights=sample_weight,
                        data = window_data)
  
  pred<-predict(local,row)
  
  row<-row[,c('tp','tem','quarry','industry','road','PIPP','soiltype','lc','br500')]
  
  
  shap<-fastshap::explain(local,
                          X = window_data[,c(4:12)],
                          nsim = 100,
                          baseline = 0,
                          adjust = TRUE,
                          pred_wrapper = function(model,newdata){
                            predict(local,newdata)
                          },
                          newdata = row)
  
  data.table(pred = pred, shaps=list(shap),localr=local[["rsq"]][400])
}

# Stop the cluster
stopCluster(cl)

results2<-as.data.frame(do.call(rbind,results$shaps))
results2$dom<-colnames(results2[,1:9])[apply(results2[,1:9],1,which.max)]
names(results2)<-c('tp_s','tem_s','quarry_s','industry_s','road_s','PIPP_s',
                   'soiltype_s','lc_s','br500_s','dom')

res<-df$Cd-results$pred

merge<-cbind(df,results2,results$pred,res,results$localr)

output<-st_as_sf(merge,coords=c('x','y'),crs=2157)
#save as shapefile
st_write(output,'D:\\telluscd\\working\\lshap55002.shp',append=FALSE)





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
  ggsave('D:\\telluscd\\maps\\tplshap5500.tiff',dpi=300,width=10,height=6)

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
  ggsave('D:\\telluscd\\maps\\temlshap5500.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=quarry,y=quarry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1)+
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
  ggsave('D:\\telluscd\\maps\\quarrylshap5500.tiff',dpi=300,width=10,height=6)


ggplot(merge,aes(x=industry,y=industry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1)+
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
  ggsave('D:\\telluscd\\maps\\industrylshap5500.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=road,y=road_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,1)+
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
  ggsave('D:\\telluscd\\maps\\roadlshap5500.tiff',dpi=300,width=10,height=6)

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
  xlab('PIPP')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\PIPPlshap5500.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('soiltype','soiltype_s')]
#group the data by parent material
gd<-group_by(gd,soiltype)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(soiltype_s))
#get the top 5
soiltypedata<-merge[merge$soiltype %in% c('AlluvMRL','BminSPPT','AlluvMIN','BminPDPT','Cut'),]
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
  ggsave('D:\\telluscd\\maps\\soiltypleshap5500.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('lc','lc_s')]
#group the data by parent material
gd<-group_by(gd,lc)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(lc_s))
#get the top 5
lcdata<-merge[merge$lc %in% c('Inland marshes','Sport and leisure facilities',
                              'Green urban areas','Non-irrigated arable land',
                              'Discontinuous urban fabric'),]
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
  ggsave('D:\\telluscd\\maps\\lclshap5500.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('br500','br500_s')]
#group the data by parent material
gd<-group_by(gd,br500)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(br500_s))
#get the top 5
br500data<-merge[merge$br500 %in% c('62, Waulsortian mudbank; Pale-grey massive limestone',
                                    '48, Transgression - regression sequence (Killary Hbr - Joyces Country Succ.); Sandstone, conglomerate, greywacke, mudstone, tuff',
                                    '71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal',
                                    '60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite',
                                    '65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale'),]

br500data$br500[br500data$br500=='62, Waulsortian mudbank; Pale-grey massive limestone']<-62
br500data$br500[br500data$br500=='48, Transgression - regression sequence (Killary Hbr - Joyces Country Succ.); Sandstone, conglomerate, greywacke, mudstone, tuff']<-48
br500data$br500[br500data$br500=='71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal']<-71
br500data$br500[br500data$br500=='60, Shallow & marginal marine (Navan Group); Dark-grey limestone, mudstone, sandstone, minor evaporite']<-60
br500data$br500[br500data$br500=='65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale']<-65

#plot
ggplot(br500data,aes(x=br500,y=br500_s,group=br500))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(0,1)+
  xlab('Bedrock')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\br500lshap5500.tiff',dpi=300,width=10,height=6)



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
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<5500]
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/5500)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br500,
                        ntree=400,
                        mtry=2,
                        weights=sample_weight,
                        data = window_data)
  
  pred<-predict(local,row)
  
  row<-row[,c('tp','tem','quarry','industry','road','PIPP','soiltype','lc','br500')]
  
  
  shap<-fastshap::explain(local,
                          X = window_data[,c(4:12)],
                          nsim = 100,
                          baseline=mean(window_data$Cd),
                          adjust = TRUE,
                          pred_wrapper = function(model,newdata){
                            predict(local,newdata)
                          },
                          newdata = row)
  
  data.table(pred = pred, shaps=list(shap),localr=local[["rsq"]][400])
}

# Stop the cluster
stopCluster(cl)

results3<-as.data.frame(do.call(rbind,results$shaps))
results3$dom<-colnames(results3[,1:9])[apply(results3[,1:9],1,which.max)]
names(results3)<-c('tp_s','tem_s','quarry_s','industry_s','road_s','PIPP_s',
                   'soiltype_s','lc_s','br500_s','dom')

res<-df$Cd-results$pred

merge<-cbind(df,results3,results$pred,res,results$localr)

output<-st_as_sf(merge,coords=c('x','y'),crs=2157)
#save as shapefile
st_write(output,'D:\\telluscd\\working\\lshap5500A.shp',append=FALSE)





merge$tem<-merge$tem-273.15
merge$tp<-merge$tp*3600*24*365*1000

ggplot(merge,aes(x=tp,y=tp_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,0.5)+
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
  ggsave('D:\\telluscd\\maps\\tplshap5500A.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=tem,y=tem_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,0.5)+
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
  ggsave('D:\\telluscd\\maps\\temlshap5500A.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=quarry,y=quarry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,0.5)+
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
  ggsave('D:\\telluscd\\maps\\quarrylshap5500A.tiff',dpi=300,width=10,height=6)


ggplot(merge,aes(x=industry,y=industry_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,0.5)+
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
  ggsave('D:\\telluscd\\maps\\industrylshap5500A.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=road,y=road_s))+
  geom_jitter(size=0.8,height=0,width=0,alpha=0.2)+
  geom_smooth(method='loess',span=0.1,se=TRUE,color='red',linetype='solid',linewidth=1.5)+
  ylim(-0.5,0.5)+
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
  ggsave('D:\\telluscd\\maps\\roadlshap5500A.tiff',dpi=300,width=10,height=6)

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
  ylim(-0.4,0.4)+
  xlab('PIPP')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\PIPPlshap5500A.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('soiltype','soiltype_s')]
#group the data by parent material
gd<-group_by(gd,soiltype)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(soiltype_s))
#get the top 5
soiltypedata<-merge[merge$soiltype %in% c('AlluvMRL','BminSPPT','AlluvMIN','BminPDPT','Cut'),]
#change the id into real name
#plot
ggplot(soiltypedata,aes(x=soiltype,y=soiltype_s,group=soiltype))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.2,0.2)+
  xlab('Soil type')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\soiltypleshap5500A.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('lc','lc_s')]
#group the data by parent material
gd<-group_by(gd,lc)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(lc_s))
#get the top 5
lcdata<-merge[merge$lc %in% c('Inland marshes','Complex cultivation patterns',
                              'Pastures','Sport and leisure facilities',
                              'Non-irrigated arable land'),]
lcdata$lc[lcdata$lc=='Inland marshes']<-c('Inland 
marshes')
lcdata$lc[lcdata$lc=='Complex cultivation patterns']<-c('Complex 
cultivation 
patterns')
lcdata$lc[lcdata$lc=='Sport and leisure facilities']<-c('Sport 
and 
leisure 
facilities')
lcdata$lc[lcdata$lc=='Non-irrigated arable land']<-c('Non-irrigated 
arable 
land')

#change the id into real name
#plot
ggplot(lcdata,aes(x=lc,y=lc_s,group=lc))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.5,0.5)+
  xlab('Land cover')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\lclshap5500A.tiff',dpi=300,width=10,height=6)

gd<-merge[,c('br500','br500_s')]
#group the data by parent material
gd<-group_by(gd,br500)
#get the mean shap value of each type of parent material
smd<-summarise(gd,mean_value=mean(br500_s))
#get the top 5
br500data<-merge[merge$br500 %in% c('9, Granite, granophyre',
                                    '48, Transgression - regression sequence (Killary Hbr - Joyces Country Succ.); Sandstone, conglomerate, greywacke, mudstone, tuff',
                                    '71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal',
                                    '62, Waulsortian mudbank; Pale-grey massive limestone',
                                    '65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale'),]

br500data$br500[br500data$br500=='62, Waulsortian mudbank; Pale-grey massive limestone']<-62
br500data$br500[br500data$br500=='48, Transgression - regression sequence (Killary Hbr - Joyces Country Succ.); Sandstone, conglomerate, greywacke, mudstone, tuff']<-48
br500data$br500[br500data$br500=='71, Fluvio-deltaic & basinal marine (Turbiditic); Shale, sandstone, siltstone & coal']<-71
br500data$br500[br500data$br500=='9, Granite, granophyre']<-9
br500data$br500[br500data$br500=='65, Marine basinal facies (Tobercolleen & Lucan Fms - "Calp"); Dark-grey argillaceous & cherty limestone & shale']<-65

#plot
ggplot(br500data,aes(x=br500,y=br500_s,group=br500))+
  stat_boxplot(geom='errorbar',width=0.1)+
  geom_boxplot()+
  ylim(-0.5,0.5)+
  xlab('Bedrock')+
  ylab('SHAP value (mg/kg)')+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\telluscd\\maps\\br500lshap5500A.tiff',dpi=300,width=10,height=6)
