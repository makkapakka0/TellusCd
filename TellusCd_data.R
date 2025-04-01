library(osmdata)
library(sf)

#industry
query<-opq(bbox='Ireland')%>%
  add_osm_feature(key='landuse',value='industrial')

osm_data<-osmdata_sf(query)

industrial_points<-osm_data$osm_points

industrial_points<-industrial_points[,c('osm_id','geometry')]

industrial_points<-st_as_sf(industrial_points,coords='geometry',crs=4326)
st_write(industrial_points,'D:\\telluscd\\industrial\\point2.shp',append=FALSE)


#mines
query<-opq(bbox='Ireland')%>%
  add_osm_feature(key='man_made',value='mine')

osm_data<-osmdata_sf(query)

mines_points<-osm_data$osm_points
mines_points<-mines_points[,c('osm_id','geometry')]

#abandoned mines
query<-opq(bbox='Ireland')%>%
  add_osm_feature(key='disused',value='mine')

osm_data<-osmdata_sf(query)

disused_mines_points<-osm_data$osm_points


#tailings
query<-opq(bbox='Ireland')%>%
  add_osm_feature(key='man_made',value='tailings')

osm_data<-osmdata_sf(query)

tailings_points<-osm_data$osm_points

#stone
query<-opq(bbox='Ireland')%>%
  add_osm_feature(key='landuse',value='quarry')

osm_data<-osmdata_sf(query)

quarry_points<-osm_data$osm_points
quarry_points<-quarry_points[,c('osm_id','geometry')]

quarry_points<-st_as_sf(quarry_points,coords='geometry',crs=4326)
st_write(quarry_points,'D:\\telluscd\\quarry\\point.shp',append=FALSE)


