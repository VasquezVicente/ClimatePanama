## COMPILING DAILY DATA FROM CHELSA 2.1 
library(raster)
library(tiff)
library(terra)
library(ggplot2)
library(tidyterra)
library(readxl)
library(sp)
CHELSADIR <- "data_reanalysis/CHELSA_COMPILE/CHELSA_CLIMATOLOGIES"
panlatlon <-  c(-84,-76, 6, 10)

#load file list
url_dest<- read_excel("data_reanalysis/CHELSA_COMPILE/url_dest.xlsx")

#dowload files
for(i in 13:nrow(url_dest)){
  download.file(url_dest$url[i], url_dest$dest[i], mode="wb")
  plot<-raster::raster(url_dest$dest[i])
  crop<- raster::crop(plot, panlatlon)
  raster::writeRaster(crop,url_dest$dest[i], overwrite=TRUE)
}
#load to stack them and calculate them 
for (i in 1:nrow(url_dest)) {
  filename <- paste0(substring(url_dest$dest[i],72, 73))
  wd <- paste0(url_dest$dest[i])
  assign(filename, raster::raster(wd))
}
#writte them 
precip<- raster::stack(`01`,`02`,`03`,`04`,`05`,`06`,`07`,`08`,`09`,`10`,`11`,`12`)
precip_jfma<- raster::stack(`01`,`02`,`03`,`04`,`05`)
precip_annual<- raster::calc(precip,sum) 
precip_jfma<- raster::calc(precip_jfma,sum)
raster::writeRaster(precip_annual,paste0(CHELSADIR,"/precip_1979_2013_1.tif"))
raster::writeRaster(precip_jfma, paste0(CHELSADIR,"/precipjfma_1979_2013_1.tif"))





