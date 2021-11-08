rm(list=ls())
cat('\f')
"%>%" = magrittr::`%>%`
library(raster)
################################## Input
name          <- 'CHILLON'
Location      <- "C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/data/obs/INSTANTANEO/"#'C:/Scripts_DHI/PRON_HOR/TUMBES_HOR'
setwd(Location)
source('C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/Scripts/Funciones_ETL.R')
imerg.path            <- 'P:/PH_GRID/IMERG_Peru/V06B/early'
File_obs              <- 'C:/Scripts_DHI/PRON_HOR/CHILLON_HOR/data/metadata/StationsP.csv'
Filename              <- "PpObs_Act.csv"
mean_temperature_path <- 'd:/Senamhi_consultoria_2021_2/code/CHILLON_HOR/data/obs/TEMP_CLIMATOLOGY/MEAN_TEMPERATURA.tif'
station_path          <- 'd:/Senamhi_consultoria_2021_2/code/CHILLON_HOR/data/obs/ESTACIONES_CHILLON/shp/ubicacion.shp'
chiriluv2_path        <-"d:/Senamhi_consultoria_2021_2/outputs/chirilu_2020/"
historic_file_path    <- 'd:/Senamhi_consultoria_2021_2/code/CHILLON_HOR/data/obs/HISTORICO/Chillon_historic.csv'


################################## Fechas
Dayi          <- Sys.Date()-1
Dayf          <- Sys.Date()

Dayi <- "2020-01-01"
Dayf <- "2020-01-02"



Time_ini_obs  <- paste0(Dayi ,' 01:00') 
Time_end_obs  <- paste0(Dayf   ,' 00:00') 

Time_ini_sat  <- paste0(Dayi ,' 06:00') 
Time_end_sat  <- paste0(Dayf ,' 05:00') 

Time_ini_sat2 <- paste0(Dayi ,' 05:00') 
Time_end_sat2 <- paste0(Dayf   ,' 04:30') 

Time_ini_pron  <- paste0(Sys.Date()   ,' 01:00')
Time_end_pron  <- paste0(Sys.Date()+2 ,' 19:00') 






#Estation location
points=raster::shapefile(station_path)

#Climatological temperature data
TEMP=stack(mean_temperature_path)


update_historic_data(points,Time_ini_obs,Time_end_obs,chiriluv2_path,TEMP,historic_file_path)



