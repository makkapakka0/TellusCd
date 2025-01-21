# TellusCd
**R codes modelling Cd in Ireland using Tellus data**

We use 9 environmental variables to model Cadmium (Cd) in the topsoil of Ireland based on Tellus dataset. Global random forest (RF) and local RF are applied. The models are interpreted using SHapley Additive exPlanations (SHAP) values to further explore the relationships between Cd and its factors, and the spatial patterns.

The TellusCd is to model Cd using global RF and local RF, interpret the models using SHAP value, plot figures, and export results to GIS. The TellusCd_data is to collect industry and quarry data. The TellusCd_parameter is to select suitable parameters for the RF models and bandwidth for the local RF model. The TellusCd_validation is to validate the models.

Please note that the codes are not developed by a computer scientist. It may not provide the most efficient and clearest way. If you find anything to improve, please don't hesitate to contact me.

Some figures in the article were produced in ArcGIS Pro. The codes here only show steps in R.

## Workflow

### Data source

Cd: Tellus, Geological Survey Ireland (point, continous, Shapefile)

Annual mean temperature: Copernicus Climate Change Service (C3S) Climate Data Store (CDS) (multidimensional, continuous, nc)

Annual precipitation: Copernicus Climate Change Service (C3S) Climate Data Store (CDS) (multidimensional, continuous, nc)

Bedrock: Geological Survey Ireland (vector, categorical, Shapefile)

Industry: Open Street Map (point, identity, Shapefile)

Road: Open Street Map (line, identity, Shapefile)

Quarry: Open Street Map (point, identity, Shapefile)

PIP_P: Ireland EPA Geoportal (vector, categorical, Shapefile)

Soil tyoe: Ireland EPA Geoportal (vector, categorical, Shapefile)

Land cover: Ireland EPA Geoportal (vector, categorical, Shapefile)


### Preprocessing

The mean values of Annual mean temperature and Annual precipitation were calculated. The kernel densities of Indusrty, Road, and Quarry were calculated. All raster data were resampled to 1km*1km resolution. The environmental variables were extracted to Cd points in ArcGIS Pro and a table was exported. Rows with na value were deleted.

### Select parameters

Grid search and OOB RMSE were used to select suitable parameters for the RF models. Leave-one-out cross-validation was used to select a suitable bandwidth for the local RF model. A semivariogram was also produced.

### Analysis

The RF was fitted using the selected parameters. The local RF was fitted using the selected bandwidth. SHAP values were calculated. The results were merged, plotted, and exported as Shapefiles. 

### Mapping

In ArcGIS Pro, the maps were produced.

## Software and packages

R 4.3.1

RStudio

randomForest 4.7-1.2

ggplot2 3.5.1

fastshap 0.1.0

sf 1.0-19

dplyr 1.1.3

data.table 1.14.8

foreach 1.5.2

doParallel 1.0.17

osmdata 0.2.5

gstat 2.1-2

