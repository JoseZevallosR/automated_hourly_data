library(raster)
#Script para crear la base de datos historica de RS minerve

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

date_minerve=function(date){
  date=gsub("X", "",date, fixed = TRUE)
  date=strsplit(date,'[.]')[[1]]
  paste(paste(date[3],date[2],date[1],sep = '.'),paste(date[4],date[5],date[6],sep = ':'))
}


meto_data_rsminerve=function(chirilu,TEMP,points){
  
  ##PP
  hourly_data <-data.frame(raster::extract(chirilu, points))
  temp_climatology <- data.frame(raster::extract(TEMP, points))
  
  pp=t(hourly_data)
  
  
  rows=dim(pp)[1]
  fechas=matrix(NA, nrow =rows,1)
  names=row.names(pp)
  for (i in 1:rows){
    fechas[i,1]=date_minerve(names[i])
  }
  #row.names(pp)=NULL
  row.names(pp)=fechas
  
  Station=points$ESTACION
  X=points@coords[,1]#points$xyz@coords[,1]
  Y=points@coords[,2]#points$xyz@coords[,2]
  Z=points$ALT#points$xyz$ALT
  Sensor=rep('Pp',length(X))
  Category=rep('Precipitation',length(X))
  Unit=rep('mm/h',length(X))
  Interpolation=rep('Linear',length(X))
  
  result1=data.frame(rbind(Station,X,Y,Z,Sensor,Category,Unit,Interpolation,pp))
  result1=header.true(result1)
  
  #temperature
  Temp=matrix(data=NA,nrow = dim(pp)[1], ncol = dim(pp)[2] )
  for (i in 1:rows){
    idx=as.numeric(strsplit(as.character(fechas[i]),'[.]')[[1]][2])
    
    Temp[i,]=round(temp_climatology[,idx],2)
  }
  row.names(Temp)=fechas
  Station=points$ESTACION
  X=points@coords[,1]
  Y=points@coords[,2]
  Z=points$ALT
  Sensor=rep('T',length(X))
  Category=rep('Temperature',length(X))
  Unit=rep('C',length(X))
  Interpolation=rep('Linear',length(X))
  
  
  result2=data.frame(rbind(Station,X,Y,Z,Sensor,Category,Unit,Interpolation,Temp))
  result2=header.true(result2)
  cbind(result1,result2)
}

#Estation location
points=raster::shapefile('d:/Senamhi_consultoria_2021_2/code/CHILLON_HOR/data/obs/ESTACIONES_CHILLON/shp/ubicacion.shp')
#Climatological temperature data
tmax=raster::stack(list.files("d:/Senamhi_consultoria_2021_2/datos/temperatura/tmax_resam1km/",full.names = T))
tmin=raster::stack(list.files("d:/Senamhi_consultoria_2021_2/datos/temperatura/tmin_resam1km/",full.names = T))
TEMP=(tmax+tmin)*0.5
TEMP=projectRaster(TEMP,crs='+proj=longlat +datum=WGS84 +no_defs ')
#Precipitation data
chirilu=raster::stack('d:/Senamhi_consultoria_2021_2/datos/CHIRILUv2.nc')
#points=readRDS('d:/Proyectos_GitHub/hourlyPrecMerge_rcl/data/processed/obs/obs_data_qc_v4.rds')


result=meto_data_rsminerve(chirilu,TEMP,points)
write.table(result,'d:/Senamhi_consultoria_2021_2/code/CHILLON_HOR/data/obs/HISTORICO/Chillon_historic.csv',sep = ',')

