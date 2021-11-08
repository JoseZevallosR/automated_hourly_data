# Este script contiene las funciones necesarias para el procesamiento de la precipitacion horaria 
# para actualizar la base de datos histórica y el pronóstico como input del modelo RS Minerve
# Elaborado por Karen Leon
# karenleonaltuna@gmail.com


###
fill_0=function(x){
  if (x <10){
    return(paste0(0,as.character(x)))
  }else{
    return(as.character(x))
  }
}
###
# Descarga de datos de precip horaria observados
#===============================================
Down_PpHor <- function(Location , File_obs , Time_ini_obs, Time_end_obs){
  
  # Location      <- 'C:/TUMBES/TUMBES_HOR'
  # File_obs      <- 'StationsP.csv'
  # Time_ini_obs  <- paste0(Sys.Date()-1 ,' 01:00')
  # Time_end_obs  <- paste0(as.Date(Time_ini_obs)+1 ,' 00:00')
  # 
  
  library(XML)
  library(httr)
  httr::set_config(config(ssl_verifypeer = FALSE))
  
  setwd(Location)
  
  meta  <- read.table(File_obs, sep=';', header=T)
  cod   <- as.vector(meta$CODIGO)
  lon   <- meta$LON
  lat   <- meta$LAT
  alt   <- meta$ALT
  nom   <- as.vector(meta[[1]])#nombre de estaciones
  
  # Agrupar datos de estaciones
  ns     <-  dim(meta)[1]
  ini    <-  as.POSIXlt(Time_ini_obs)
  end    <-  as.POSIXlt(Time_end_obs)
  subtime <-  seq(ini, end, by="hour")
  subdata <- matrix(NA,length(subtime),ns)
  
  for (i in 1:ns){ 
    ur    <-  paste0('https://www.senamhi.gob.pe/site/lvera/lluvia2.php?DATO1=',cod[i])
    #ur     <-  paste0('https://www.senamhi.gob.pe/site/lvera/lluvia2.php?DATO1=47E323B2')  
    tabs   <-  GET(ur)
    tabla  <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)[[2]]
    
    
    if (dim(tabla)[1]>3){ #Verifica si la estacion está operativa
      
      tabla   <- na.omit(tabla)
      
      tweb    <- na.omit(tabla[,3:5])
      n       <- dim(tweb)[1]
      time.url<- seq(as.POSIXlt(paste0(tweb[1,1],'-',tweb[1,2],'-',tweb[1,3],' 01:00')),
                     as.POSIXlt(paste0(tweb[n,1],'-',tweb[n,2],'-',tweb[n,3],' 00:00'))+3600*24,
                     by="hour")
      
      indx    <- which(time.url==ini)
      indx2   <- which(time.url==end)
      
      if (length(indx)==0 || length(indx2)==0){#Verifica si las fechas selecc están OK
        print("Se seleccionó mal el rango de fecha!!")
      } else {
        subset   <- seq(indx,indx2)
        pp <- as.numeric(t(as.matrix(tabla[,6:29])))
        subdata[,i]  <- pp[subset]
      }
    }
  }
  
  # Exportar base de datos
  if (length(indx)==0 || length(indx2)==0){
    print("Se seleccionó mal el rango de fecha!!")
  } else {
    
    #values <- rbind(nom, lat, lon, alt, subdata)
    #labels <- c('Station', 'Lat', 'Lon', 'Elev', as.character(subtime))
    values <- rbind(cod, subdata)
    labels <- c('Station', as.character(subtime))
    export <- cbind(labels, values)
    write.table(export, file='PpObs_Act.csv', sep=',', row.names=F, col.names=F)
  }
  
}




## Actualizar csv historico
#=================================================
update_historic_data=function(points,Time_ini_obs,Time_end_ob,chiriluv2_path,TEMP,historic_file_path){
  
  date_minerve=function(date){
    
    date=strsplit(date,'[ ]')[[1]]
    ymd=strsplit(date[1],'[-]')[[1]]
    hms=strsplit(date[2],'[-]')[[1]]
    paste(paste(ymd[3],ymd[2],ymd[1],sep = '.'),paste(hms[1],hms[2],hms[3],sep = ':'))
  }
  
  time <- seq(from=as.POSIXct(Time_ini_obs, format="%Y-%m-%d %H:%M", tz="America/Bogota"),
              to=as.POSIXct(Time_end_obs, format="%Y-%m-%d %H:%M", tz="America/Bogota"),
              by="hour")
  time <- format(time, '%Y-%m-%d %H-%M-%S')
  mapas=stack(lapply(time,function(x) paste0(chiriluv2_path,x,'.tif')))
  hourly_data <-t(data.frame(raster::extract(mapas, points)))
  rownames(hourly_data)=lapply(time, date_minerve)
  
  Temp=matrix(data=NA,nrow = dim(hourly_data)[1], ncol = dim(hourly_data)[2] )
  temp_climatology <- data.frame(raster::extract(TEMP, points))
  for (i in 1:dim(hourly_data)[1]){
    idx=as.numeric(strsplit(as.character(lapply(time, date_minerve)[i]),'[.]')[[1]][2])
    
    Temp[i,]=round(temp_climatology[,idx],2)
  }
  row.names(Temp)=lapply(time, date_minerve)
  
  data_update=data.frame(cbind(hourly_data,Temp))
  
  Historic_data=read.csv(historic_file_path)
  names(data_update)=names(Historic_data)
  Historic_data=rbind(Historic_data,data_update)
  
  write.table(Historic_data,historic_file_path,sep = ',')
  
}

## Descarga y extraccion de ETA10 para el dominio de estudio para el día de hoy
#=============================================================================
Downext_ETA10 <- function(Location , file, Domain, nomcuenca){
  
  #Librerias
  library(RCurl)
  library(rgdal)
  library(ncdf4)
  
  setwd(Location)
  date        <- Sys.Date()
  Area        <- readOGR(file.path(Location, 'SHP', Domain))
  
  #Descarga ETA10 horario de SENAMHI
  url         <- paste0('ftp.senamhi.gob.pe/HIDROLOGIA/SMN/ETA10','/',gsub("-","",date),'/')
  filenames   <- getURL(url, ftp.use.epsv = FALSE, ftplistonly=TRUE, 
                        crlf=TRUE,userpwd="senamhiddr:servicioddr")
  filePaths   <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep="")
  n           <- which(grepl(file, filePaths)==T)[1]
  output.path <- paste0(Location,'/',file,'.txt')
  info        <- paste0('ftp://senamhiddr:servicioddr@',filePaths[n])
  download.file(info, output.path, method='libcurl', quiet = F, mode = "w",
                cacheOK = TRUE)
  
  #Extraccion de Zona de estudio en netcdf
  meta    <- read.table(paste0(file,'.txt'), sep="", header=F)
  data    <- meta[meta[,1]>=Area@bbox[2] & meta[,1]<=Area@bbox[4] 
                  & meta[,2]>=Area@bbox[1] & meta[,2]<=Area@bbox[3],]
  lon     <- unique(data[,2])
  lat     <- unique(data[,1])
  time    <- 1:(ncol(data)-2)
  
  pr_array  <- array(NA,c(length(lat),length(lon),length(time)))
  for (t in 1:length(time)){
    pr_array[,,t] <- matrix(data[,-1:-2][,t], nrow = length(lat) , ncol = length(lon), byrow=T)
  }
  
  datanc    <- pr_array[,,7:length(time)]
  timenc    <- 1:dim(datanc)[3]
  
  VarOut    <- 'pr'
  NameVar   <- "Hourly precipitation"
  Units     <- 'mm'
  londim    <- ncdim_def("lon","degrees_east", as.double(lon))
  latdim    <- ncdim_def("lat","degrees_north", as.double(lat))
  timedim   <- ncdim_def("time",paste('Hours since',date), as.double(timenc))
  fillvalue <- NA
  VarDef    <- ncvar_def(VarOut, Units, list(londim,latdim,timedim), fillvalue, NameVar, prec="single")
  
  Name      <- paste0("ETA10_",nomcuenca,"_Act.nc")
  ncout     <- nc_create(Name,list(VarDef), force_v4=T)
  
  ncvar_put(ncout, VarDef, datanc)
  
  ncatt_put(ncout,"lon","axis","X")
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")
  
  nc_close(ncout)
  message('Done!')
  
}


## Descarga y extraccion de GFS para el dominio de estudio para el día de hoy
#=============================================================================
Downext_GFS025 <- function(Location , file, Domain, nomcuenca, type){
  
  #Librerias
  library(RCurl)
  library(rgdal)
  library(ncdf4)
  
  setwd(Location)
  date        <- Sys.Date()
  Area        <- readOGR(file.path(Location, 'SHP', Domain))
  
  #Descarga GFS025 horario de SENAMHI
  url         <- paste0('ftp.senamhi.gob.pe/HIDROLOGIA/SMN/GFS025','/',gsub("-","",date),'/',type,'/')
  filenames   <- getURL(url, ftp.use.epsv = FALSE, ftplistonly=TRUE, 
                        crlf=TRUE,userpwd="senamhiddr:servicioddr")
  filePaths   <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep="")
  n           <- which(grepl(file, filePaths)==T)[1]
  output.path <- paste0(Location,'/',file,'.txt')
  info        <- paste0('ftp://senamhiddr:servicioddr@',filePaths[n])
  download.file(info, output.path, method='libcurl', quiet = F, mode = "w",
                cacheOK = TRUE)
  
  #Extraccion de Zona de estudio en netcdf
  meta    <- read.table(paste0(file,'.txt'), sep="", header=F)
  data    <- meta[meta[,1]>=Area@bbox[2] & meta[,1]<=Area@bbox[4] 
                  & meta[,2]>=Area@bbox[1] & meta[,2]<=Area@bbox[3],]
  lon     <- unique(data[,2])
  lat     <- unique(data[,1])
  time    <- 1:(ncol(data)-2)
  
  pr_array  <- array(NA,c(length(lat),length(lon),length(time)))
  for (t in 1:length(time)){
    pr_array[,,t] <- matrix(data[,-1:-2][,t], nrow = length(lat) , ncol = length(lon), byrow=T)
  }
  
  if(type=='00'){
    h <- 7
  }else{
    h <- 2
  }
  
  datanc    <- pr_array[,,h:length(time)]
  timenc    <- 1:dim(datanc)[3]
  
  VarOut    <- 'pr'
  NameVar   <- "Hourly precipitation"
  Units     <- 'mm'
  londim    <- ncdim_def("lon","degrees_east", as.double(lon))
  latdim    <- ncdim_def("lat","degrees_north", as.double(lat))
  timedim   <- ncdim_def("time",paste('Hours since',date), as.double(timenc))
  fillvalue <- NA
  VarDef    <- ncvar_def(VarOut, Units, list(londim,latdim,timedim), fillvalue, NameVar, prec="single")
  
  Name      <- paste0("GFS025_",type,"_",nomcuenca,"_Act.nc")
  ncout     <- nc_create(Name,list(VarDef), force_v4=T)
  
  ncvar_put(ncout, VarDef, datanc)
  
  ncatt_put(ncout,"lon","axis","X")
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")
  
  nc_close(ncout)
  message('Done!')
  
}



## Actualizar csv pronostico
#=============================================================================
actcsv_pron <- function(Location.copy, Location, patterncsv.from, Location.to, patterncsv.to, Time_ini_eta){
  
  # Location.copy <- './MODEL/HIST'
  # Location <- 'D:/TUMBES/TUMBES_HOR'
  # patterncsv.from <- 'GFS025_00'
  # Location.to <- './MODEL/PRON'
  # patterncsv.to <-'GFS025_00'
  # Time_ini_eta  <- paste0(Sys.Date()   ,' 01:00')
  
   setwd(Location)
  
  # Copia del archivo HIST y renombre a PRON
  nametorename <- list.files(Location.copy,pattern=glob2rx(paste0('*','HIST','*.csv$*')))
  filecsv.to   <- sub('HIST',patterncsv.to,nametorename)
  file.copy(file.path(Location.copy,nametorename),file.path(Location.to,filecsv.to),overwrite = T)
  
  # Extraer datos a copiar (from)
  filecsv.from  <- list.files(Location,pattern=glob2rx(paste0('*',patterncsv.from,'*.csv$*')),full.names = FALSE)
  data.from     <- read.table(filecsv.from,sep=",", header=F)
  data.copy     <- data.from[-1:-8,]
  
  
  for (i in 1:length(filecsv.to)){
    
    data.to         <- read.table(file.path(Location.to,filecsv.to[i]),sep=",", header=F)
    
    #Chequear la fecha
    last.date.from  <- as.character(data.from[9,1])
    last.date.to    <- as.character(data.to[dim(data.to)[1],1])
    
    last.date.from  <- as.POSIXlt(last.date.from, "%d/%m/%Y %H:%M", tz='GMT')
    last.date.to    <- as.POSIXlt(last.date.to, "%d/%m/%Y %H:%M", tz='GMT')
    
    if (as.numeric(last.date.from)-as.numeric(last.date.to)==3600){
      print("La fecha de actualizacion de datos coincide")
      write.table(data.copy, file.path(Location.to,filecsv.to[i]),row.names=F,col.names=F,
                  quote=F, na="", sep = ",", append = T)
    } else {
      print("EROR!!!! La fecha de actualizacion de la base de datos NO coincide")
    }

    
  }
  
}




