
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

tmax=raster::stack(list.files("d:/Senamhi_consultoria_2021_2/datos/temperatura/tmax_resam1km/",full.names = T))
tmin=raster::stack(list.files("d:/Senamhi_consultoria_2021_2/datos/temperatura/tmin_resam1km/",full.names = T))

chirilu=(tmax+tmin)*0.5
chirilu=projectRaster(chirilu,crs=crs(points$xyz))
points=readRDS('d:/Proyectos_GitHub/hourlyPrecMerge_rcl/data/processed/obs/obs_data_qc_v4.rds')


meto_data_rsminerve=function(chirilu,points){
  hourly_data <-data.frame(raster::extract(chirilu, points$xyz))
  
  datos=read.csv('d:/Senamhi_consultoria_2021_2/datos/rs_minerve_data.csv',sep=';')
  ff=datos$Station
  pp=matrix(data=NA,nrow = 45288, ncol = 29 )
  for (i in 8:45295){
    idx=as.numeric(strsplit(as.character(ff[i]),'[.]')[[1]][2])
    
    pp[i-7,]=round(hourly_data[,idx],2)
  }
  
  rows=dim(pp)[1]
  fechas=ff[8:45295]

  #row.names(pp)=NULL
  row.names(pp)=fechas
  
  Station=points$xyz$ESTACION
  X=points$xyz@coords[,1]
  Y=points$xyz@coords[,2]
  Z=points$xyz$ALT
  Sensor=rep('T',length(X))
  Category=rep('Temperature',length(X))
  Unit=rep('C',length(X))
  Interpolation=rep('Linear',length(X))
  
  result=data.frame(rbind(Station,X,Y,Z,Sensor,Category,Unit,Interpolation,pp))
  result=header.true(result)
  
}








write.table(result,'d:/Senamhi_consultoria_2021_2/datos/rs_minerve_data.csv',sep = ',')



