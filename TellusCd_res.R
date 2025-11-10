library(randomForest)
library(ggplot2)
library(fastshap)
library(sf)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)

#RF
df<-read.csv('D:\\telluscd\\all3.csv')

rf<-randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                 ntree=350,
                 mtry=3,
                 data=df)

#Preparation for residual analysis
pred<-predict(rf,df)
df$res<-pred-df$Cd

res<-as.data.frame(df$res)

res<-res%>%
  mutate(bin=cut(df$res,breaks = seq(floor(min(df$res)),ceiling(max(df$res)),by=0.1)))%>%
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
  ggsave('D:\\telluscd\\maps\\gres.tiff',dpi=300,width=10,height=2)

output<-st_as_sf(df,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\final\\res_g.shp',append=FALSE)

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
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<9000]
 
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/9000)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  data.table(pred = pred)
}

stopCluster(cl)

#Residual
df$res<-results$pred-df$Cd

df_sf<-st_as_sf(df,coords=c('x','y'), crs=2157)
coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,0,3693)
dist_mat<-nbdists(nb,coords)
inv_dist<-lapply(dist_mat, function(x) 1/x)
lw<-nb2listw(nb,glist = inv_dist,style = 'W', zero.policy = TRUE)
moran_res<-moran.test(df_sf$res,lw,zero.policy = TRUE)
print(moran_res)

res<-as.data.frame(df$res)
res<-res%>%
  mutate(bin=cut(df$res,breaks = seq(floor(min(df$res)),ceiling(max(df$res)),by=0.1)))%>%
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
  ggsave('D:\\telluscd\\maps\\lres.tiff',dpi=300,width=10,height=2)

output<-st_as_sf(df,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\final\\res_l.shp',append=FALSE)




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
  
  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<9000]
  window_data$z_scores <- scale(log(window_data$Cd))
  window_data <- window_data[abs(window_data$z_scores) <= 3, ]
  
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/9000)^2)^2
  
  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  data.table(pred = pred)
}

stopCluster(cl)

df$res<-results$pred-df$Cd

df_sf<-st_as_sf(df,coords=c('x','y'), crs=2157)
coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,0,3693)
dist_mat<-nbdists(nb,coords)
inv_dist<-lapply(dist_mat, function(x) 1/x)
lw<-nb2listw(nb,glist = inv_dist,style = 'W', zero.policy = TRUE)
moran_res<-moran.test(df_sf$res,lw,zero.policy = TRUE)
print(moran_res)

res<-as.data.frame(df$res)

res<-res%>%
  mutate(bin=cut(df$res,breaks = seq(floor(min(df$res)),ceiling(max(df$res)),by=0.1)))%>%
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
  ggsave('D:\\telluscd\\maps\\lzres.tiff',dpi=300,width=10,height=2)

output<-st_as_sf(df,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\final\\res_lz.shp',append=FALSE)


#GWRF-LISA
df<-read.csv('D:\\telluscd\\all3.csv')

df_sf<-st_as_sf(df,coords = c('x','y'),crs=2157)
#Local Moran's I
coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,d1=0,d2=9000)
lw<-nb2listw(nb,style = 'W')
local_moran<-localmoran(log(df_sf$Cd),lw)
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

  window_data <- df[sqrt((x-long1)^2+(y-lat1)^2)<9000]
  window_data<-window_data[window_data$Ii>0 | window_data$p>0.05,]
  
  sample_weight<-(1-(sqrt((window_data$x-long1)^2+(window_data$y-lat1)^2)/9000)^2)^2

  local <- randomForest(Cd~tp+tem+quarry+industry+road+PIPP+soiltype+lc+br+pH+LOI+ele,
                        ntree=350,
                        mtry=3,
                        weights=sample_weight,
                        data = window_data)
  pred<-predict(local,row)
  data.table(pred = pred)
}

stopCluster(cl)

df$res<-results$pred-df$Cd

df_sf<-st_as_sf(df,coords=c('x','y'), crs=2157)
coords<-st_coordinates(df_sf)
nb<-dnearneigh(coords,0,3693)
dist_mat<-nbdists(nb,coords)
inv_dist<-lapply(dist_mat, function(x) 1/x)
lw<-nb2listw(nb,glist = inv_dist,style = 'W', zero.policy = TRUE)
moran_res<-moran.test(df_sf$res,lw,zero.policy = TRUE)
print(moran_res)

res<-as.data.frame(df$res)

res<-res%>%
  mutate(bin=cut(df$res,breaks = seq(floor(min(df$res)),ceiling(max(df$res)),by=0.1)))%>%
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
  ggsave('D:\\telluscd\\maps\\lmres.tiff',dpi=300,width=10,height=2)

output<-st_as_sf(df,coords=c('x','y'),crs=2157)
st_write(output,'D:\\telluscd\\working\\final\\res_lm.shp',append=FALSE)

#Plot
moran<-read.csv('D:\\telluscd\\moran_res.csv')
moran<-na.omit(moran)

ggplot(moran,aes(x=band,y=Z.score,group=Group,color=Group))+
  annotate('rect',xmin=-Inf,xmax=Inf,ymin=-2.58,ymax=2.58,fill='pink',alpha=0.7)+
  geom_line(size=1.5)+
  geom_point(color='black',size=2)+
  labs(x='Bandwidth (m)',y='Global Morans I Z-score')+
  ylim(-8,54)+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'),
        legend.position = c(0.15,0.8),
        legend.title = element_text(size=0),
        legend.text = element_text(size=24))+
  ggsave('D:\\telluscd\\maps\\moranres.tiff',dpi=300,width=10,height=8)

