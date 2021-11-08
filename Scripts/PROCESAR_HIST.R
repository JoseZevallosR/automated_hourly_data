rm(list=ls())
cat('\f')
"%>%" = magrittr::`%>%`

################################## Input
name          <- 'CHILLON'
Location      <- "C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/data/obs/INSTANTANEO/"#'C:/Scripts_DHI/PRON_HOR/TUMBES_HOR'
setwd(Location)
source('C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/Scripts/Funciones_ETL.R')
imerg.path    <- 'P:/PH_GRID/IMERG_Peru/V06B/early'
Domain        <- 'domain.shp'#CHECK
Subcuencas    <- 'urh_rsmCGS.shp'#CHECK
File_obs      <- 'C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/data/metadata/StationsP.csv'
Filename      <- "PpObs_Act.csv"

################################## Fechas
Dayi          <- Sys.Date()-1
Dayf          <- Sys.Date()

Time_ini_obs  <- paste0(Dayi ,' 01:00') 
Time_end_obs  <- paste0(Dayf   ,' 00:00') 

Time_ini_sat  <- paste0(Dayi ,' 06:00') 
Time_end_sat  <- paste0(Dayf ,' 05:00') 

Time_ini_sat2 <- paste0(Dayi ,' 05:00') 
Time_end_sat2 <- paste0(Dayf   ,' 04:30') 

Time_ini_pron  <- paste0(Sys.Date()   ,' 01:00')
Time_end_pron  <- paste0(Sys.Date()+2 ,' 19:00') 

######################################################################################
# #DESCARGA DE PRECIPITACION HORARIA DE ESTACION 
Down_PpHor(Location , File_obs , Time_ini_obs, Time_end_obs)

setwd("C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/")
source("./src/make_imerg_path.R")
source("./src/operational_qc.R") # Control de calidad
source("./src/points_inside_pixel.R") # Punto-grilla
source("./src/interpolation.R") # Mezcla
library(lubridate)
library(foreach)
library(doParallel)
##PISCO

ppisco_p_max_monthly <- raster::stack("P:/PISCOnc/MAX_PPISCOpd.tif")
## CHIRILU area
chirilu <- raster::extent(c(-77.4, -76, -12.3, -11)) 

obs_data <- read.csv("C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/data/obs/")

obs_master<- read.csv("C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/data/metadata/xyz_crl.csv",sep=',')

for (iter in 1:dim(obs_data)[1]){
  time_step=obs_data[iter,]
  PP=as.numeric(time_step[-1])
  fecha=as.character(time_step$Station)
  #raster_names[k]=fecha
  
  
  n_size=length(colnames(obs_data)[-1])
  NEST=1:n_size
  FECHA=rep(fecha,n_size)
  
  data_ema=cbind(NEST,obs_master[match(colnames(obs_data)[-1],obs_master$CODE),c('CODE','LON','LAT','ALT')],FECHA,PP)
  
  colnames(data_ema) <- c("NSET", "CODE", "LON", "LAT", "ALT", "DATE", "OBS")
  ## GPM-IMERG-Early
  time_imerg <- unique(FECHA) %>% strptime("%Y-%m-%d %H:%M:%S") - 1*60*60*5
  
  sub_folder=paste0(year(time_imerg),month(time_imerg),fill_0(day(time_imerg)))
  file_imerg <- make_imerg_path(date_imerg = time_imerg,
                                dir_path = paste(imerg.path,sub_folder,sep='/'))
  
  if (Reduce(`+`,lapply(file_imerg, file.exists))==4){
    data_imerg <- lapply(file_imerg, function(x) raster::raster(x)) %>%
      raster::brick() %>%
      raster::stackApply(., indices = c(1,1,2,2), fun = sum) %>%
      raster::crop(chirilu)
    
    data_ema=na.omit(data_ema)
    data_ema <- sp::SpatialPointsDataFrame(coords = data_ema[, c("LON", "LAT")],
                                           data = data_ema,
                                           proj4string = sp::CRS(raster::projection(data_imerg)))%>% raster::crop(chirilu)
    
    # Valores maximos y minimos de precipitación diaria para cada mes (PPISCOp)
    
    daily_max_monthly <- data.frame(raster::extract(ppisco_p_max_monthly, data_ema))
    rownames(daily_max_monthly) <- data_ema$CODE
    #Agregar la condicion para ejecutar la mezcla
    
    data_ema <- data_ema %>% .[complete.cases(.@data),]
    
    
    if( length(data_ema@data$OBS) < 10) {
      
      #chirilu_gridded[[k]] <- data_imerg[[1]]
      #raster::values(chirilu_gridded[[k]]) <- NA
      chirilu_gridded <- data_imerg[[1]]
      raster::values(chirilu_gridded) <- NA
      #return(chirilu_gridded)
    }else{
      # Aplicación del QC
      qc_internal_consistency_check(spatial_point = data_ema) %>%
        qc_extreme_check(spatial_point = ., daily_monthly_limits = daily_max_monthly)  %>%
        qc_spatial_consistency(spatial_point = .) -> data_ema
      
      ## EMA-grilla
      
      data_ema <- make_single_point(pts = data_ema,
                                    rgrid = data_imerg[[1]])$xyz %>%
        .[complete.cases(.@data),]
      
      colnames(data_ema@data)[7] <- "obs"
      
      ## Mezcla
      IDW_res <- IDW(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
      OK_res <- OK(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
      RIDW_res <- RIDW(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
      RIDW2_res <- RIDW(gauge_points = data_ema, gridded_cov = data_imerg)
      CM_IDW_res <- CM_IDW(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
      RK_res <- RK(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
      RK2_res <- RK(gauge_points = data_ema, gridded_cov = data_imerg)
      CM_OK_res <- CM_OK(gauge_points = data_ema, gridded_cov = data_imerg[[1]])
      
      chirilu_gridded <- raster::brick(IDW_res, OK_res,
                                       RIDW_res, RIDW2_res, CM_IDW_res,
                                       RK_res, RK2_res, CM_OK_res) %>%
        raster::calc(fun = function(x) sd(x, na.rm = TRUE))
      
      #return(chirilu_gridded)
    }
  }else{
    chirilu_gridded <- data_imerg[[1]]
    raster::values(chirilu_gridded) <- NA
  }

  raster::writeRaster(chirilu_gridded,paste0('P:/CHIRILUv2/',gsub(":", "-", fecha, fixed = TRUE),'.tif'),overwrite=T)
}





