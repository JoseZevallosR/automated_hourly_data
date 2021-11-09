library(raster)

source('d:/Proyectos_GitHub/automated_hourly_data/src/Funciones_ETL.R')
#Script para crear la base de datos historica de RS minerve

station_path          <- 'd:/Senamhi_consultoria_2021_2/code/CHILLON_HOR/data/obs/ESTACIONES_CHILLON/shp/ubicacion.shp'
mean_temperature_path <- 'd:/Senamhi_consultoria_2021_2/code/CHILLON_HOR/data/obs/TEMP_CLIMATOLOGY/MEAN_TEMPERATURA.tif'
#Estation location
points=raster::shapefile(station_path)
TEMP=stack(mean_temperature_path)
#Precipitation data
chirilu=raster::stack('d:/Senamhi_consultoria_2021_2/datos/CHIRILUv2.nc')
#points=readRDS('d:/Proyectos_GitHub/hourlyPrecMerge_rcl/data/processed/obs/obs_data_qc_v4.rds')
result=meto_data_rsminerve(chirilu,TEMP,points)
write.table(na.omit(result),'d:/Proyectos_GitHub/automated_hourly_data/data/CHILLON/obs/HISTORICO/Chillon_historic.csv',sep = ',',row.names = F)

