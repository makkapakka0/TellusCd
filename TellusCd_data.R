library(osmdata)
library(sf)

#Industry
query<-opq(bbox='Ireland')%>%
  add_osm_feature(key='landuse',value='industrial')
osm_data<-osmdata_sf(query)
industrial_points<-osm_data$osm_points
industrial_points<-industrial_points[,c('osm_id','geometry')]
industrial_points<-st_as_sf(industrial_points,coords='geometry',crs=4326)
st_write(industrial_points,'D:\\telluscd\\industrial\\point2.shp',append=FALSE)

#Quarry
query<-opq(bbox='Ireland')%>%
  add_osm_feature(key='landuse',value='quarry')
osm_data<-osmdata_sf(query)
quarry_points<-osm_data$osm_points
quarry_points<-quarry_points[,c('osm_id','geometry')]
quarry_points<-st_as_sf(quarry_points,coords='geometry',crs=4326)
st_write(quarry_points,'D:\\telluscd\\quarry\\point.shp',append=FALSE)
