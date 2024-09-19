#data vis
library(rasterVis)
library(ggplot2)
library(readxl)
library("seegSDM", lib.loc="C:/Users/VasquezV/AppData/Local/Programs/R/R-4.2.1/library")
library(raster)
library(dplyr)
library(reshape2)
library(gridExtra)
library(tmap)
library(terra)
library(sf)
#path
out<- "C:/Users/P_pol/repo/ClimatePanama/output/"
#extents
panlatlon <-  c(-84,-76, 6, 10)
extentMap  <- raster::extent(c(-80.2,-79.4, 8.8, 9.5))
nowater<-raster("data_reanalysis/nowater.tif")
nowater<-raster::crop(nowater,c(-80.2,-79.4, 8.8, 9.5))
#load df
acp<-read_excel("tables/ground_data.xlsx")
acp_jfma<-acp

panama_elevation<-raster::raster("data_reanalysis/panama_elevation.tif")
panama_elevation<-raster::crop(panama_elevation,extentMap)
panama<-panama_elevation
values(panama)<- ifelse(values(panama)==0, NA, values(panama))
panama1<-rasterToContour(panama,maxpixels = 500)

panama_elevation<- aggregate(panama_elevation,fact=30)
elevation<-rasterToPoints(panama_elevation)
elevation<- as.data.frame(elevation)
elevation<- elevation%>% rename(z="panama_elevation")
elevation<-elevation %>% filter(z>=0)

###display annual mean precipitation
{
annual_pan<-function(list,extent){
  a<-lapply(list, raster)
  b<-raster::stack(a)
  c<-raster::calc(b,sum)
  d<-raster::crop(c,extent)
  return(d)
}
#map panels with same extent and temporal extent
  #chelsa 1.2 annual precip 0,0083333 degree orig 
  chelsa_orig<-list.files("data_reanalysis/climatologies/chelsa1-orig/",full.names=TRUE)
  chelsa_orig<-annual_pan(chelsa_orig,extentMap)
  #load satelital chips Isai Vasquez
  line<-list.files("data_reanalysis/climatologies/chirps",full.name=TRUE)
  line<-annual_pan(line,extentMap)
  line<-projectRaster(line,chelsa_orig, method='ngb')
  #chelsa 2.1 annual precip 0,0083333 degree orig 
  chelsa2_orig<-list.files("data_reanalysis/climatologies/chelsa2-orig/",full.names=TRUE)
  chelsa2_orig<-annual_pan(chelsa2_orig,extentMap)
  chelsa2_orig<- chelsa2_orig*nowater
  values(chelsa2_orig)<- ifelse(values(chelsa2_orig)==0,NA, values(chelsa2_orig))
  #chp 0.05 degree original 1980-2009 
  chp<- list.files("data_reanalysis/climatologies/chp1980-2009/",full.names=TRUE)
  chp<-annual_pan(chp,extentMap)
  chp<-projectRaster(chp,chelsa_orig, method='ngb')
  chp<-chp*nowater
  values(chp)<-ifelse(values(chp)==0,NA, values(chp))
  #pbcor 0.05 degree origina
  pbcor_chp<- list.files("data_reanalysis/climatologies/pbcor_chp1980-2009/",full.names=TRUE)
  pbcor_chp<-annual_pan(pbcor_chp,extentMap)
  pbcor_chp<-projectRaster(pbcor_chp,chelsa_orig, method='ngb')
  #pbcor 0.05 degree original
  pbcor_chelsa<- list.files("data_reanalysis/climatologies/pbcor_chels1979-2013/",full.names=TRUE)
  pbcor_chelsa<-annual_pan(pbcor_chelsa,extentMap)
  #terra
  terra<- list.files("data_reanalysis/climatologies/terra/",full.names = TRUE)
  terra<-annual_pan(terra,extentMap)
  terra<-projectRaster(terra,chelsa_orig,method = 'ngb')
  #chelsaW5E5
  chelsaw5e5<- list.files("data_reanalysis/climatologies/chelsawe51979-2016", full.names=TRUE)
  chelsaw5e5<- annual_pan(chelsaw5e5,extentMap)
  #Chelsa earthenv 2003-2016
  chelsaearthenv<-list.files("data_reanalysis/climatologies/earthenv2003-2016",full.names = TRUE)
  chelsaearthenv<-annual_pan(chelsaearthenv,extentMap)
  chelsaearthenv<-chelsaearthenv*nowater
  values(chelsaearthenv)<-ifelse(values(chelsaearthenv)==0,NA, values(chelsaearthenv))
  #Pbcorworldclim
  pbcor_worldclim<-list.files("data_reanalysis/climatologies/pbcor_worldclim",full.names = TRUE)
  pbcor_worldclim<-annual_pan(pbcor_worldclim,extentMap)
  #worldclim 
worldclim<-list.files("data_reanalysis/climatologies/worldclim",full.names=TRUE)
worldclim<-lapply(worldclim, raster)
worldclim<-raster::stack(worldclim)
worldclim<-raster::crop(worldclim, panlatlon)
worldclim<- raster::calc(worldclim,sum)
worldclim<-raster::crop(worldclim,extentMap)
  res_stack<-raster::stack(chelsa_orig,chelsa2_orig,chelsaearthenv,pbcor_chelsa,pbcor_chp,pbcor_worldclim,terra,
                           chp,worldclim,line)
  models<-list('layer.1'= "CHELSA 1.2",
  'layer.2'="CHELSA 2.1",
  'layer.3'="CHELSA EarthEnv",
  'layer.4'="PBCOR CHELSA 1.2",
  'layer.5'="PBCOR CHPclim",
  'layer.6'="PBCOR WorldClim",
  'layer.7'="TERRA",
  'layer.8'="CHPclim v1",
  'layer.9'="WorldClim v2",
  'layer.10'="CHIRPS v2")
  model2<- function(variable,value){
    return(models[value])
  }
 jf1<-gplot(res_stack) + 
    geom_raster(aes(fill = value)) +
    facet_wrap(~ variable,labeller=model2,ncol = 5) +
    scale_fill_gradientn(colours = rev(terrain.colors(225)),na.value="white",name=NULL)+
    coord_equal()+theme_void()+theme(strip.text.x = element_text(size = 10, colour = "black"))+
 labs(title = "Total Annual Precipitation (mm)")+ 
   theme(plot.title = element_text(hjust = 0.5,size = 16))
 ggsave(path = out,filename = "graphs_final/annual_maps.tiff",  units="in", width=6, height=6, dpi=500)
}
###jfma precipitation
{
  #map panels with same extent and temporal extent
  #chelsa 1.2 annual precip 0,0083333 degree orig 
  chelsa_orig<-list.files("data_reanalysis/climatologies/chelsa1-orig/",full.names=TRUE)
  chelsa_orig<- chelsa_orig[1:4]
  chelsa_orig<-annual_pan(chelsa_orig,extentMap) 
  #chirps
  line<-list.files("data_reanalysis/climatologies/chirps/",full.names = TRUE)
  line<-line[1:4]
  line<-annual_pan(line,extentMap)
  line<-projectRaster(line,chelsa_orig,method='ngb')
  #chelsa 2.1 annual precip 0,0083333 degree orig 
  chelsa2_orig<-list.files("data_reanalysis/climatologies/chelsa2-orig/",full.names=TRUE)
  chelsa2_orig<- chelsa2_orig[1:4]
  chelsa2_orig<-annual_pan(chelsa2_orig,extentMap)
  chelsa2_orig<- chelsa2_orig*nowater
  values(chelsa2_orig)<- ifelse(values(chelsa2_orig)==0,NA, values(chelsa2_orig))
  #chp 0.05 degree original 1980-2009 
  chp<- list.files("data_reanalysis/climatologies/CHP/",full.names=TRUE)
  chp<- chp[1:4]
  chp<-annual_pan(chp,extentMap)
  chp<-projectRaster(chp,chelsa_orig, method='ngb')
  chp<-chp*nowater
  values(chp)<-ifelse(values(chp)==0,NA, values(chp))
  #pbcor 0.05 degree origina
  pbcor_chp<- list.files("data_reanalysis/climatologies/pbcor_chp1980-2009/",full.names=TRUE)
  pbcor_chp<- pbcor_chp[1:4]
  pbcor_chp<-annual_pan(pbcor_chp,extentMap)
  pbcor_chp<-projectRaster(pbcor_chp,chelsa_orig, method='ngb')
  #pbcor 0.05 degree original
  pbcor_chelsa<- list.files("data_reanalysis/climatologies/pbcor_chels1979-2013/",full.names=TRUE)
  pbcor_chelsa<-pbcor_chelsa[1:4]
  pbcor_chelsa<-annual_pan(pbcor_chelsa,extentMap)
  #terra
  terra<- list.files("data_reanalysis/climatologies/terra/",full.names = TRUE)
  terra<-terra[1:4]
  terra<-annual_pan(terra,extentMap)
  terra<-projectRaster(terra,chelsa_orig,method = 'ngb')
  #chelsaW5E5
  chelsaw5e5<- list.files("data_reanalysis/climatologies/chelsawe51979-2016", full.names=TRUE)
  chelsaw5e5<-chelsaw5e5[1:4]
  chelsaw5e5<- annual_pan(chelsaw5e5,extentMap)
  #Chelsa earthenv 2003-2016
  chelsaearthenv<-list.files("data_reanalysis/climatologies/earthenv2003-2016",full.names = TRUE)
  chelsaearthenv<-chelsaearthenv[1:4]
  chelsaearthenv<-annual_pan(chelsaearthenv,extentMap)
  chelsaearthenv<-chelsaearthenv*nowater
  values(chelsaearthenv)<-ifelse(values(chelsaearthenv)==0,NA, values(chelsaearthenv))
  #Pbcorworldclim
  pbcor_worldclim<-list.files("data_reanalysis/climatologies/pbcor_worldclim",full.names = TRUE)
  pbcor_worldclim<-pbcor_worldclim[1:4]
  pbcor_worldclim<-annual_pan(pbcor_worldclim,extentMap)
  #worldclim 
  worldclim<-list.files("data_reanalysis/climatologies/worldclim",full.names=TRUE)
  worldclim<-worldclim[1:4]
  worldclim<-lapply(worldclim, raster)
  worldclim<-raster::stack(worldclim)
  worldclim<-raster::crop(worldclim, panlatlon)
  worldclim<- raster::calc(worldclim,sum)
  worldclim<-raster::crop(worldclim,extentMap)
  res_stack_jfma<-raster::stack(chelsa_orig,chelsa2_orig,chelsaearthenv,pbcor_chelsa,pbcor_chp,pbcor_worldclim,terra,chp,worldclim,line)
  models<-list('layer.1'= "CHELSA 1.2",
               'layer.2'="CHELSA 2.1",
               'layer.3'="CHELSA EarthEnv",
               'layer.4'="PBCOR CHELSA 1.2",
               'layer.5'="PBCOR CHPclim",
               'layer.6'="PBCOR WorldClim",
               'layer.7'="TERRA",
               'layer.8'="CHPclim v1",
               'layer.9'="WorldClim v2",
               'layer.10'="CHIRPS v2")
  model2<- function(variable,value){
    return(models[value])
  }
 
an1<-gplot(res_stack_jfma) + 
    geom_raster(aes(fill = value)) +
    facet_wrap(~ variable,labeller=model2, ncol = 5) +
    scale_fill_gradientn(colours = rev(terrain.colors(225)),na.value="white",name=NULL) +
    coord_equal()+theme_void()+theme(strip.text.x = element_text(size = 10, colour = "black"))+
  labs(title = "January-April Total Precipitation(mm)")+ 
  theme(plot.title = element_text(hjust = 0.5,size = 16))
ggsave(path = out,filename = "graphs_final/jfma.tiff",  units="in", width=6, height=6, dpi=500)
}
#par row climatologies
{
b<-grid.arrange(jf1,an1,ncol = 1)
b+annotate_figure(b,bottom = text_grob("Figure 1. Compiled climatologies.", color = "black",hjust = 10.7, x = 2, size = 12))
ggsave(b,path =out ,filename = "graphs_final/climatologies.tiff",  units="in", width=10, height=7, dpi=500)
}
##prep
{
df_names<- data.frame(num=c(1:11), name=c("CHELSA 1.2","CHELSA 2.1","CHELSA EarthEnv","PBCOR CHELSA 1.2","PBCOR CHPclim",
                                         "PBCOR WorldClim","TERRA","CHPclim v1","WorldClim","CHIRPS v2","CHELSA W5E5"))
annual_tif<- as.list(res_stack)
for (i in 1:length(annual_tif)){
  annual_tif[[i]]@data@names<- df_names$name[i]
}
jfma_tif<-as.list(res_stack_jfma)
for (i in 1:length(jfma_tif)){
  jfma_tif[[i]]@data@names<- df_names$name[i]}
  
# extract point values at coordinates and looks and find the closest non NA pixel, requires seegSDM package (github)
extract_land<-function(list_object,coordinate, max_search_distance){
  vect<- ifelse(!is.na(raster::extract(list_object,coordinate,na.rm=FALSE)),
                raster::extract(list_object,coordinate,na.rm=FALSE),
                raster::extract(list_object,nearestLand(coordinate, list_object, 4000),na.rm=FALSE))
  return(vect)
}
stats<-function(predicted,observed){
  correlation<-cor.test(observed,predicted)$estimate
  rmse<-sqrt(mean((predicted-observed)^2 , na.rm = TRUE ) )
  bias<- mean((predicted-observed),na.rm=TRUE)
  rs<- summary(lm(predicted~observed))$r.squared
  mae<-  mean(abs(predicted-observed), na.rm = TRUE)
  done<-c(correlation,rmse,bias,mae,rs)
  return(done)
}
}
#loop to extract annual precipitation and jan-apr precip
{
for(i in 1:11){
acp$thismodel<-extract_land(annual_tif[[i]],acp[,7:8])
names(acp)[names(acp)=="thismodel"]<- annual_tif[[i]]@data@names
}
#loop to extract  jfma precipitation 
for(i in 1:11){
  acp_jfma$thismodel<-extract_land(jfma_tif[[i]],acp_jfma[,7:8])
  names(acp_jfma)[names(acp_jfma)=="thismodel"]<- jfma_tif[[i]]@data@names
}
}
#coor plot ANNUAL
{

  acp_melt<-acp[,-c(3,4,6,7,8)]
  acp_melt<- melt(acp_melt,id=c("site","temporal","annualPrecip_acp") )
  #multiple subsets
  a<- acp_melt%>% filter(variable=="CHELSA 1.2"&temporal=="1979-2013")
  b<- acp_melt%>% filter(variable=="CHELSA 2.1"&temporal=="1981-2010")
  c<- acp_melt%>% filter(variable=="CHELSA EarthEnv"&temporal=="2003-2016")
  d<- acp_melt%>% filter(variable=="PBCOR CHELSA 1.2"&temporal=="1979-2013")
  f<- acp_melt%>% filter(variable=="PBCOR CHPclim"&temporal=="1980-2009")
  g<- acp_melt%>% filter(variable=="PBCOR WorldClim"&temporal=="1970-2000")
  h<- acp_melt%>% filter(variable=="TERRA"&temporal=="1981-2010")
  i<- acp_melt%>% filter(variable=="CHPclim v1"&temporal=="1980-2009")
  j<- acp_melt%>% filter(variable=="WorldClim"&temporal=="1970-2000")
  k<- acp_melt%>% filter(variable=="CHIRPS v2"&temporal=="1981-2016")
  

  final<- bind_rows(a,b,c,d,f,g,h,i,j,k)  
final$variable <- factor(final$variable,      # Reordering group factor levels
                    levels = c("CHELSA 1.2", "CHELSA 2.1", "CHELSA EarthEnv",
                               "PBCOR CHELSA 1.2","PBCOR CHPclim","PBCOR WorldClim",
                               "TERRA","CHPclim v1","WorldClim", "CHIRPS v2"))

p<-ggplot(final,aes(x =value ,y=annualPrecip_acp))+geom_smooth(method='lm',se=FALSE)+
geom_point(col=rgb(0,0,0,alpha=0.5))+facet_wrap(vars(variable),ncol = 5)+
  theme_classic()+
  expand_limits(x=c(1000,4500),y=c(1000,4500))+
  geom_abline(slope = 1, intercept = 0, lty= 2)+
  coord_equal()+
  geom_smooth(method='lm',se=FALSE)

dat_text <- data.frame(
  label = c("hola", "df", "yo","yes","te","day" ,"me","dad","ad","ds"),
  variable  = c("CHELSA 1.2", "CHELSA 2.1","CHELSA EarthEnv","PBCOR CHELSA 1.2","PBCOR CHPclim"
                ,"PBCOR WorldClim","TERRA","CHPclim v1","WorldClim","CHIRPS v2")
)
for(i in 1:10){
dat_text$cor[i]<-cor.test(final$annualPrecip_acp[final$variable==dat_text$variable[i]],
                    final$value[final$variable==dat_text$variable[i]],use="complete.obs")$estimate
dat_text$rmse[i]<-rmse(final$value[final$variable==dat_text$variable[i]],final$annualPrecip_acp[final$variable==dat_text$variable[i]])
dat_text$bias[i]<-mean((final$value[final$variable==dat_text$variable[i]]-final$annualPrecip_acp[final$variable==dat_text$variable[i]])/final$annualPrecip_acp[final$variable==dat_text$variable[i]])*100

dat_text$eq[i]<- paste0("y =",round(lm(final$annualPrecip_acp[final$variable==dat_text$variable[i]]~final$value[final$variable==dat_text$variable[i]])$coefficients[1],2),
                        "+",round(lm(final$annualPrecip_acp[final$variable==dat_text$variable[i]]~final$value[final$variable==dat_text$variable[i]])$coefficients[2],2),
                        "x")
dat_text$mae[i]<-mean(abs(final$value[final$variable==dat_text$variable[i]]-final$annualPrecip_acp[final$variable==dat_text$variable[i]]),na.rm=TRUE)
dat_text$r2[i]<-summary(lm(final$value[final$variable==dat_text$variable[i]]~final$annualPrecip_acp[final$variable==dat_text$variable[i]]))$r.squared
}
dat_text$label<- paste0("r=",round(dat_text$cor,3),", RMSE=",round(dat_text$rmse,3),", bias=",round(dat_text$bias,3),"%")
dat_text$variable <- factor(dat_text$variable,      # Reordering group factor levels
                         levels = c("CHELSA 1.2", "CHELSA 2.1", "CHELSA EarthEnv",
                                    "PBCOR CHELSA 1.2","PBCOR CHPclim","PBCOR WorldClim",
                                    "TERRA","CHPclim v1","WorldClim","CHIRPS v2"))

dat_text$r2<- paste0("R^2=",round(dat_text$r2,2))

p<-p+geom_text(
  data    = dat_text,
  mapping = aes(x = 1800, y = 1300, label = eq),
  hjust   = 0,
  vjust   = 1,
  size    = 3
)+labs(title = "Total Annual Precipitation Correlation Plots")+ 
  xlab("Reanalysis mean annual precipitation (mm)") +
  ylab("Observed mean annual precipitation (mm)") 


ggsave(path = out,filename = "graphs_final/correlation_annual.tiff",  units="in", width=7, height=6, dpi=500)

}
#coor plot jfma
{
  acp_melt<-acp_jfma[,-c(2,4,6,7,8)]
  acp_melt<- melt(acp_melt,id=c("site","temporal","jfmaPrecip_acp") )
  #multiple subsets
  a<- acp_melt%>% filter(variable=="CHELSA 1.2"&temporal=="1979-2013")
  b<- acp_melt%>% filter(variable=="CHELSA 2.1"&temporal=="1981-2010")
  c<- acp_melt%>% filter(variable=="CHELSA EarthEnv"&temporal=="2003-2016")
  d<- acp_melt%>% filter(variable=="PBCOR CHELSA 1.2"&temporal=="1979-2013")
  f<- acp_melt%>% filter(variable=="PBCOR CHPclim"&temporal=="1980-2009")
  g<- acp_melt%>% filter(variable=="PBCOR WorldClim"&temporal=="1970-2000")
  h<- acp_melt%>% filter(variable=="TERRA"&temporal=="1981-2010")
  i<- acp_melt%>% filter(variable=="CHPclim v1"&temporal=="1980-2009")
  j<- acp_melt%>% filter(variable=="WorldClim"&temporal=="1970-2000")
  k<- acp_melt%>% filter(variable=="CHIRPS v2"&temporal=="1981-2016")
  final2<- bind_rows(a,b,c,d,f,g,h,i,j,k)  
  final2$variable <- factor(final2$variable,      # Reordering group factor levels
                           levels = c("CHELSA 1.2", "CHELSA 2.1", "CHELSA EarthEnv",
                                      "PBCOR CHELSA 1.2","PBCOR CHPclim","PBCOR WorldClim",
                                      "TERRA","CHPclim v1","WorldClim","CHIRPS v2"))
  
  c<-ggplot(final2,aes(x = value,y=jfmaPrecip_acp))+geom_smooth(method='lm',se=FALSE)+
    geom_point(col=rgb(0,0,0,alpha=0.5))+facet_wrap(vars(variable),ncol=5)+
    theme_classic()+
    expand_limits(x=c(0,800),y=c(0,800))+
    geom_abline(slope = 1, intercept = 0, lty= 2)+
    coord_equal()+
    geom_smooth(method='lm',se=FALSE)
  dat_text <- data.frame(
    label = c("hola", "df", "yo","yes","te","day" ,"me","dad","ad","ds"),
    variable  = c("CHELSA 1.2", "CHELSA 2.1","CHELSA EarthEnv","PBCOR CHELSA 1.2","PBCOR CHPclim"
                  ,"PBCOR WorldClim","TERRA","CHPclim v1","WorldClim","CHIRPS v2")
  )
  for(i in 1:10){
    dat_text$eq[i]<- paste0("y =",round(lm(final2$jfmaPrecip_acp[final2$variable==dat_text$variable[i]]~final2$value[final2$variable==dat_text$variable[i]])$coefficients[1],2),
                            "+",round(lm(final2$jfmaPrecip_acp[final2$variable==dat_text$variable[i]]~final2$value[final2$variable==dat_text$variable[i]])$coefficients[2],2),
                            "x")
  }
  dat_text$variable <- factor(dat_text$variable,      # Reordering group factor levels
                              levels = c("CHELSA 1.2", "CHELSA 2.1", "CHELSA EarthEnv",
                                         "PBCOR CHELSA 1.2","PBCOR CHPclim","PBCOR WorldClim",
                                         "TERRA","CHPclim v1","WorldClim","CHIRPS v2"))
 d<-c + geom_text(
    data    = dat_text,
    mapping = aes(x = 400, y = 50, label = eq),
    hjust   = 0,
    vjust   = 1,
    size    = 3
  )+labs(title = "January-April Correlation Plots")+ 
    xlab("Reanalysis jan-apr precipitation (mm)") +
    ylab("Observed jan-apr precipitation (mm)") 
  ggsave(path = out,filename = "graphs_final/correlation_jfma.tiff",  units="in", width=7, height=6, dpi=500)
}
#map sites
{
  acp2<- distinct(acp,site, .keep_all = TRUE)
  acp2<-acp2[,c("site","long_dd","lat_dd")]
  acp_st<-acp2%>% filter(site %in% c("GALETASTRI", "BCICLEAR"))
  acp_a<-acp2 %>% filter(!site %in% c("GALETASTRI", "BCICLEAR","BALBOAFAA", "BALBOAHTS","ESCANDALOSA","PELUCA","GALETA","DIABLO"))
  acp_b<-acp2 %>% filter(site %in% c("BALBOAFAA","ESCANDALOSA","PELUCA","BALBOAHTS"))
  acp_z<-acp2 %>% filter(site %in% c("GALETA", "DIABLO"))
  panama_elevation<-raster::raster("data_reanalysis/panama_elevation.tif")
  panama<-panama_elevation
  panama<-raster::crop(panama, c(-80.2,-79.4, 8.8, 9.5))
  values(panama)<- ifelse(values(panama)==0, NA, values(panama))
  values(panama)<- ifelse(values(panama)==28, NA, values(panama))
  f<-shapefile("data_reanalysis/Ecological_Coastal_Units__(ECUs)_.shp")
  f<-raster::crop(f,extentMap)
  plot(panama,colNA="lightblue")
  plot(f, lwd=1,col="gray", add=TRUE)
  points(acp_a$long_dd,acp_a$lat_dd, col='red', pch=20, cex=1)
  text(acp_a$long_dd,acp_a$lat_dd-0.01,labels=acp_a$site,cex=0.7)
  points(acp_st$long_dd+0.01,acp_st$lat_dd, col='blue', pch=20, cex=1)
  text(acp_st$long_dd+0.04,acp_st$lat_dd+0.015,labels=acp_st$site,cex=0.7)
  points(acp_b$long_dd+0.01,acp_b$lat_dd, col='red', pch=20, cex=1)
  text(acp_b$long_dd+0.01,acp_b$lat_dd+0.02,labels=acp_b$site,cex=0.7)
  points(acp_z$long_dd,acp_z$lat_dd, col='red', pch=20, cex=1)
  text(acp_z$long_dd+0.02,acp_z$lat_dd-0.015,labels=acp_z$site,cex=0.7)
  text(-80.1,9.4,labels="Caribbean Sea",cex=1)
  text(-79.55,8.85,labels="Pacific Ocean",cex=1)
  
}
fa<-grid.arrange(p,d,nrow=2, ncol=1)
#climatology stats
{
  acp_melt<-acp[,-c(3,4,6,7,8)]
  acp_melt<- melt(acp_melt,id=c("site","temporal","annualPrecip_acp") )
  #multiple subsets
  a<- acp_melt%>% filter(variable=="CHELSA 1.2"&temporal=="1979-2013")
  b<- acp_melt%>% filter(variable=="CHELSA 2.1"&temporal=="1981-2010")
  c<- acp_melt%>% filter(variable=="CHELSA EarthEnv"&temporal=="2003-2016")
  d<- acp_melt%>% filter(variable=="PBCOR CHELSA 1.2"&temporal=="1979-2013")
  f<- acp_melt%>% filter(variable=="PBCOR CHPclim"&temporal=="1980-2009")
  g<- acp_melt%>% filter(variable=="PBCOR WorldClim"&temporal=="1970-2000")
  h<- acp_melt%>% filter(variable=="TERRA"&temporal=="1981-2010")
  i<- acp_melt%>% filter(variable=="CHPclim v1"&temporal=="1980-2009")
  j<- acp_melt%>% filter(variable=="WorldClim"&temporal=="1970-2000")
  k<- acp_melt%>% filter(variable=="CHIRPS v2"&temporal=="1981-2016")
  l<- acp_melt%>% filter(variable=="CHELSA W5E5"&temporal=="1979-2016")
  final<- bind_rows(a,b,c,d,f,g,h,i,j,k,l)  
  acp_melt<-acp_jfma[,-c(2,4,6,7,8)]
  acp_melt<- melt(acp_melt,id=c("site","temporal","jfmaPrecip_acp") )
  #multiple subsets
  a<- acp_melt%>% filter(variable=="CHELSA 1.2"&temporal=="1979-2013")
  b<- acp_melt%>% filter(variable=="CHELSA 2.1"&temporal=="1981-2010")
  c<- acp_melt%>% filter(variable=="CHELSA EarthEnv"&temporal=="2003-2016")
  d<- acp_melt%>% filter(variable=="PBCOR CHELSA 1.2"&temporal=="1979-2013")
  f<- acp_melt%>% filter(variable=="PBCOR CHPclim"&temporal=="1980-2009")
  g<- acp_melt%>% filter(variable=="PBCOR WorldClim"&temporal=="1970-2000")
  h<- acp_melt%>% filter(variable=="TERRA"&temporal=="1981-2010")
  i<- acp_melt%>% filter(variable=="CHPclim v1"&temporal=="1980-2009")
  j<- acp_melt%>% filter(variable=="WorldClim"&temporal=="1970-2000")
  k<- acp_melt%>% filter(variable=="CHIRPS v2"&temporal=="1981-2016")
  l<- acp_melt%>% filter(variable=="CHELSA W5E5"&temporal=="1979-2016")
  final2<- bind_rows(a,b,c,d,f,g,h,i,j,k,l)  
df<-data.frame(dataset=c("correlation"), Pearson=c("cor"),RMSE=c("d"),bias=c("bia"), mae=c("mae"),r=c("r2"))
df[1,]<-c("CHELSA 1.2",stats(final$value[final$variable=="CHELSA 1.2"],final$annualPrecip_acp[final$variable=="CHELSA 1.2"]))
df[2,]<-c("CHELSA 2.1",stats(final$value[final$variable=="CHELSA 2.1"],final$annualPrecip_acp[final$variable=="CHELSA 2.1"]))
df[3,]<-c("CHELSA EarthEnv",stats(final$value[final$variable=="CHELSA EarthEnv"],final$annualPrecip_acp[final$variable=="CHELSA EarthEnv"]))
df[8,]<-c("PBCOR CHELSA 1.2",stats(final$value[final$variable=="PBCOR CHELSA 1.2"],final$annualPrecip_acp[final$variable=="PBCOR CHELSA 1.2"]))
df[9,]<-c("PBCOR CHPclim",stats(final$value[final$variable=="PBCOR CHPclim"],final$annualPrecip_acp[final$variable=="PBCOR CHPclim"]))
df[10,]<-c("PBCOR WorldClim",stats(final$value[final$variable=="PBCOR WorldClim"],final$annualPrecip_acp[final$variable=="PBCOR WorldClim"]))
df[6,]<-c("TERRA",stats(final$value[final$variable=="TERRA"],final$annualPrecip_acp[final$variable=="TERRA"]))
df[5,]<-c("CHPclim v1",stats(final$value[final$variable=="CHPclim v1"],final$annualPrecip_acp[final$variable=="CHPclim v1"]))
df[7,]<-c("WorldClim",stats(final$value[final$variable=="WorldClim"],final$annualPrecip_acp[final$variable=="WorldClim"]))
df[4,]<-c("CHIRPS v2",stats(final$value[final$variable=="CHIRPS v2"],final$annualPrecip_acp[final$variable=="CHIRPS v2"]))
df[11,]<-c("CHELSA W5E5",stats(final$value[final$variable=="CHELSA W5E5"],final$annualPrecip_acp[final$variable=="CHELSA W5E5"]))
df2<-data.frame(dataset=c("correlation"), Pearson=c("cor"),RMSE=c("d"),bias=c("bia"), mae=c("mae"),r=c("r2"))
df2[1,]<-c("CHELSA 1.2",stats(final2$value[final2$variable=="CHELSA 1.2"],final2$jfmaPrecip_acp[final2$variable=="CHELSA 1.2"]))
df2[2,]<-c("CHELSA 2.1",stats(final2$value[final2$variable=="CHELSA 2.1"],final2$jfmaPrecip_acp[final2$variable=="CHELSA 2.1"]))
df2[3,]<-c("CHELSA EarthEnv",stats(final2$value[final2$variable=="CHELSA EarthEnv"],final2$jfmaPrecip_acp[final2$variable=="CHELSA EarthEnv"]))
df2[8,]<-c("PBCOR CHELSA 1.2",stats(final2$value[final2$variable=="PBCOR CHELSA 1.2"],final2$jfmaPrecip_acp[final2$variable=="PBCOR CHELSA 1.2"]))
df2[9,]<-c("PBCOR CHPclim",stats(final2$value[final2$variable=="PBCOR CHPclim"],final2$jfmaPrecip_acp[final2$variable=="PBCOR CHPclim"]))
df2[10,]<-c("PBCOR WorldClim",stats(final2$value[final2$variable=="PBCOR WorldClim"],final2$jfmaPrecip_acp[final2$variable=="PBCOR WorldClim"]))
df2[6,]<-c("TERRA",stats(final2$value[final2$variable=="TERRA"],final2$jfmaPrecip_acp[final2$variable=="TERRA"]))
df2[5,]<-c("CHPclim v1",stats(final2$value[final2$variable=="CHPclim v1"],final2$jfmaPrecip_acp[final2$variable=="CHPclim v1"]))
df2[7,]<-c("WorldClim",stats(final2$value[final2$variable=="WorldClim"],final2$jfmaPrecip_acp[final2$variable=="WorldClim"]))
df2[4,]<-c("CHIRPS v2",stats(final2$value[final2$variable=="CHIRPS v2"],final2$jfmaPrecip_acp[final2$variable=="CHIRPS v2"]))
df2[11,]<-c("CHELSA W5E5",stats(final2$value[final2$variable=="CHELSA W5E5"],final2$jfmaPrecip_acp[final2$variable=="CHELSA W5E5"]))
write.csv(df, "tables/annual.csv")
write.csv(df2, "tables/jfma.csv")
}
#seasonality
{
month<-read_excel("monthly_ground.xlsx")
acp_nyear<-read_excel("acp_corrected.xlsx")
#& monthlyPrecip!="NA"  
AGUACLARA<-month %>% filter(site=="AGUACLARA"    &Year>=1970&Year<=2016)
SANMIGUEL<-month %>% filter(site=="SANMIGUEL"    &Year>=1970&Year<=2016)
BCI<-month %>% filter(site=="BCI"                &Year>=1970&Year<=2016)
PELUCA<-month %>% filter(site=="PELUCA"          &Year>=1970&Year<=2016)
BCICLEAR<-month %>% filter(site=="BCICLEAR"      &Year>=1970&Year<=2016)
GUACHA<-month %>% filter(site=="GUACHA"          &Year>=1970&Year<=2016)
CASCADAS<-month %>% filter(site=="CASCADAS"      &Year>=1970&Year<=2016)
PEDROMIGUEL<-month %>% filter(site=="PEDROMIGUEL"&Year>=1970&Year<=2016)
GAMBOA<-month %>% filter(site=="GAMBOA"          &Year>=1970&Year<=2016)

acp38<-bind_rows(AGUACLARA,SANMIGUEL,BCI,PELUCA,BCICLEAR,GUACHA,CASCADAS,PEDROMIGUEL,GAMBOA)
acp38$monthlyPrecip<-as.numeric(acp38$monthlyPrecip)
temporal1<- acp38%>% filter(Year>=1970&Year<=2000)    #temporal worldclim and pbcor worldclim 
temporal2<-acp38%>% filter(Year>=1979&Year<=2013)     #temporal chelsa 1 and chelsa pbcor 1
temporal3<- acp38%>% filter(Year>=1981&Year<=2010)    #temporal chelsa2 and terra
temporal4<- acp38%>% filter(Year>=2003&Year<=2016)    #temporal chelsa earthenv
temporal5<- acp38%>% filter(Year>=1980&Year<=2009)    #temporal chelsa chp and pbcor chp 
temporal6<- acp38%>% filter(Year>=1979&Year<=2016)    #temporal chelsa w5e5 
temporal7<- acp38%>% filter(Year>=1981&Year<=2016)
    #temporal chirps
temporal1<- temporal1 %>% group_by(site, Month) %>% summarise(mon_mean=mean(monthlyPrecip,na.rm=TRUE))%>%mutate(temporal="1970-2000")
temporal2<- temporal2 %>% group_by(site, Month) %>% summarise(mon_mean=mean(monthlyPrecip,na.rm=TRUE))%>%mutate(temporal="1979-2013")
temporal3<- temporal3 %>% group_by(site, Month) %>% summarise(mon_mean=mean(monthlyPrecip,na.rm=TRUE))%>%mutate(temporal="1981-2010")
temporal4<- temporal4 %>% group_by(site, Month) %>% summarise(mon_mean=mean(monthlyPrecip,na.rm=TRUE))%>%mutate(temporal="2003-2016")
temporal5<- temporal5 %>% group_by(site, Month) %>% summarise(mon_mean=mean(monthlyPrecip,na.rm=TRUE))%>%mutate(temporal="1980-2009")
temporal6<- temporal6 %>% group_by(site, Month) %>% summarise(mon_mean=mean(monthlyPrecip,na.rm=TRUE))%>%mutate(temporal="1979-2016")
temporal7<- temporal7 %>% group_by(site, Month) %>% summarise(mon_mean=mean(monthlyPrecip,na.rm=TRUE))%>%mutate(temporal="1981-2016")

acp38_ag<- bind_rows(temporal1, temporal2, temporal3,temporal4,temporal5,temporal6,temporal7)
acp38_ag$long_dd<- acp_nyear$long_dd[match(acp38_ag$site,acp_nyear$site)]
acp38_ag$lat_dd<- acp_nyear$lat_dd[match(acp38_ag$site,acp_nyear$site)]
chelsa_orig<-list.files("climatologies/chelsa1-orig/",full.names=TRUE)
chelsa_orig<-stack(chelsa_orig) 
chelsa2_orig<-list.files("climatologies/chelsa2-orig/",full.names=TRUE)
chelsa2_orig<-stack(chelsa2_orig)
chelsaw5e5<- list.files("climatologies/chelsawe51979-2016", full.names=TRUE)
chelsaw5e5<- stack(chelsaw5e5)
terra<-list.files("climatologies/terra/",full.names=TRUE)
terra<-stack(terra)
chelsaearth<-list.files("climatologies/earthenv2003-2016/",full.names = TRUE)
chelsaearth<-stack(chelsaearth)
pbcor_chelsa<-list.files("climatologies/pbcor_chels1979-2013/",full.names = TRUE)
pbcor_chelsa<-stack(pbcor_chelsa)
pbcor_chp<- list.files("climatologies/pbcor_chp1980-2009",full.names = TRUE)
pbcor_chp<-stack(pbcor_chp)
chp<-list.files("climatologies/chp1980-2009/",full.names = TRUE)
chp<-stack(chp)
worldclim<-list.files("climatologies/worldclim",full.names=TRUE)
worldclim<-lapply(worldclim, raster)
worldclim<-raster::stack(worldclim)
worldclim<-raster::crop(worldclim, panlatlon)
pbcor_worldclim<-list.files("climatologies/pbcor_worldclim",full.names = TRUE)
pbcor_worldclim<-raster::stack(pbcor_worldclim)
pbcor_worldclim<-raster::crop(pbcor_worldclim,panlatlon)
chirps<-list.files("climatologies/chirps/",full.names=TRUE)
chirps<-stack(chirps) 
##acp38 is for plotting and acp38_ag is for the table

acp38<- acp38 %>% group_by(site, Month) %>% summarise(mon_mean=mean(monthlyPrecip,na.rm=TRUE))
acp38$long_dd<- acp_nyear$long_dd[match(acp38$site,acp_nyear$site)]
acp38$lat_dd<- acp_nyear$lat_dd[match(acp38$site,acp_nyear$site)]
for (i in 1:length(acp38$site)){
  acp38$chelsa1[i]<-extract_land(chelsa_orig[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$chelsa2[i]<-extract_land(chelsa2_orig[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$chirps[i]<-extract_land(chirps[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$chelsaw5e5[i]<-extract_land(chelsaw5e5[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$terra[i]<-extract_land(terra[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$chelsaearthenv[i]<-extract_land(chelsaearth[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$pbcor_chelsa[i]<-extract_land(pbcor_chelsa[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$pbcor_chp[i]<-extract_land(pbcor_chp[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$chp[i]<-extract_land(chp[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$worldclim[i]<-extract_land(worldclim[[acp38$Month[i]]],acp38[i,c(4,5)])
  acp38$pbcor_worldclim[i]<-extract_land(pbcor_worldclim[[acp38$Month[i]]],acp38[i,c(4,5)])
}
for (i in 1:length(acp38$site)){
  acp38$chelsaw5e5[i]<-extract_land(chelsaw5e5[[acp38$Month[i]]],acp38[i,c(4,5)])
}

for (i in 1:length(acp38_ag$site)){
  acp38_ag$chelsa1[i]<-extract_land(chelsa_orig[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$chelsa2[i]<-extract_land(chelsa2_orig[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$chirps[i]<-extract_land(chirps[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$chelsaw5e5[i]<-extract_land(chelsaw5e5[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$terra[i]<-extract_land(terra[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$chelsaearthenv[i]<-extract_land(chelsaearth[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$pbcor_chelsa[i]<-extract_land(pbcor_chelsa[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$pbcor_chp[i]<-extract_land(pbcor_chp[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$chp[i]<-extract_land(chp[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$worldclim[i]<-extract_land(worldclim[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
  acp38_ag$pbcor_worldclim[i]<-extract_land(pbcor_worldclim[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
}
for (i in 1:length(acp38_ag$site)){
  acp38_ag$chelsaw5e5[i]<-extract_land(chelsaw5e5[[acp38_ag$Month[i]]],acp38_ag[i,c(5,6)])
}

#STATS for multiple temporal extents table output
{
df2<-data.frame(dataset=c("correlation"), Pearson=c("cor"),RMSE=c("d"),bias=c("bia"), mae=c("mae"),r=c("r2"))
df2[1,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1979-2013"]))
df2[2,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1981-2010"]))
df2[3,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="2003-2016"]))
df2[4,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1979-2013"]))
df2[5,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1980-2009"]))
df2[6,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1970-2000"]))
df2[7,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1981-2010"]))
df2[8,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1980-2009"]))
df2[9,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1970-2000"]))
df2[10,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1979-2016"]))
df2[11,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="SANMIGUEL"&acp38_ag$temporal=="1981-2016"]))

df2[12,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1979-2013"]))
df2[13,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1981-2010"]))
df2[14,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="2003-2016"]))
df2[15,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1979-2013"]))
df2[16,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1980-2009"]))
df2[17,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1970-2000"]))
df2[18,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1981-2010"]))
df2[19,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1980-2009"]))
df2[20,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1970-2000"]))
df2[21,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1979-2016"]))
df2[22,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="AGUACLARA"&acp38_ag$temporal=="1981-2016"]))

df2[23,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1979-2013"]))
df2[24,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1981-2010"]))
df2[25,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="2003-2016"]))
df2[26,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1979-2013"]))
df2[27,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1980-2009"]))
df2[28,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1970-2000"]))
df2[29,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1981-2010"]))
df2[30,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1980-2009"]))
df2[31,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1970-2000"]))
df2[32,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1979-2016"]))
df2[32,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="PELUCA"&acp38_ag$temporal=="1981-2016"]))

df2[33,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="BCI"&acp38_ag$temporal=="1979-2013"]))
df2[34,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="BCI"&acp38_ag$temporal=="1981-2010"]))
df2[35,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="BCI"&acp38_ag$temporal=="2003-2016"]))
df2[36,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="BCI"&acp38_ag$temporal=="1979-2013"]))
df2[37,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="BCI"&acp38_ag$temporal=="1980-2009"]))
df2[38,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="BCI"&acp38_ag$temporal=="1970-2000"]))
df2[39,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="BCI"&acp38_ag$temporal=="1981-2010"]))
df2[40,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="BCI"&acp38_ag$temporal=="1980-2009"]))
df2[41,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="BCI"&acp38_ag$temporal=="1970-2000"]))
df2[42,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="BCI"&acp38_ag$temporal=="1979-2016"]))
df2[43,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="BCI"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="BCI"&acp38_ag$temporal=="1981-2016"]))

df2[44,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1979-2013"]))
df2[45,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1981-2010"]))
df2[46,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="2003-2016"]))
df2[47,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1979-2013"]))
df2[48,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1980-2009"]))
df2[49,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1970-2000"]))
df2[50,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1981-2010"]))
df2[51,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1980-2009"]))
df2[52,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1970-2000"]))
df2[53,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1979-2016"]))
df2[54,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="BCICLEAR"&acp38_ag$temporal=="1981-2016"]))

df2[55,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1979-2013"]))
df2[56,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1981-2010"]))
df2[57,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="2003-2016"]))
df2[58,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1979-2013"]))
df2[59,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1980-2009"]))
df2[60,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1970-2000"]))
df2[61,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1981-2010"]))
df2[62,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1980-2009"]))
df2[63,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1970-2000"]))
df2[64,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1979-2016"]))
df2[64,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="GUACHA"&acp38_ag$temporal=="1981-2016"]))

df2[65,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1979-2013"]))
df2[66,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1981-2010"]))
df2[67,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="2003-2016"]))
df2[68,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1979-2013"]))
df2[69,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1980-2009"]))
df2[70,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1970-2000"]))
df2[71,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1981-2010"]))
df2[72,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1980-2009"]))
df2[73,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1970-2000"]))
df2[74,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1979-2016"]))
df2[75,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="CASCADAS"&acp38_ag$temporal=="1981-2016"]))

df2[76,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1979-2013"]))
df2[77,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1981-2010"]))
df2[78,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="2003-2016"]))
df2[79,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1979-2013"]))
df2[80,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1980-2009"]))
df2[81,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1970-2000"]))
df2[82,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1981-2010"]))
df2[83,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1980-2009"]))
df2[84,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1970-2000"]))
df2[85,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1979-2016"]))
df2[86,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="GAMBOA"&acp38_ag$temporal=="1981-2016"]))

df2[81,]<-c("CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1979-2013"],acp38_ag$chelsa1[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1979-2013"]))
df2[82,]<-c("CHELSA 2.1",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1981-2010"],acp38_ag$chelsa2[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1981-2010"]))
df2[83,]<-c("CHELSA EarthEnv",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="2003-2016"],acp38_ag$chelsaearthenv[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="2003-2016"]))
df2[84,]<-c("PBCOR CHELSA 1.2",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1979-2013"],acp38_ag$pbcor_chelsa[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1979-2013"]))
df2[85,]<-c("PBCOR CHPclim",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1980-2009"],acp38_ag$pbcor_chp[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1980-2009"]))
df2[86,]<-c("PBCOR WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1970-2000"],acp38_ag$pbcor_worldclim[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1970-2000"]))
df2[87,]<-c("TERRA",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1981-2010"],acp38_ag$terra[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1981-2010"]))
df2[88,]<-c("CHPclim v1",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1980-2009"],acp38_ag$chp[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1980-2009"]))
df2[89,]<-c("WorldClim",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1970-2000"],acp38_ag$worldclim[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1970-2000"]))
df2[90,]<-c("CHELSA W5E5",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1979-2016"],acp38_ag$chelsaw5e5[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1979-2016"]))
df2[90,]<-c("CHIRPS v2",stats(acp38_ag$mon_mean[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1979-2016"],acp38_ag$chirps[acp38_ag$site=="PEDROMIGUEL"&acp38_ag$temporal=="1981-2016"]))

df2$Pearson<-as.numeric(df2$Pearson)
df2$RMSE<-as.numeric(df2$RMSE)
df2$bias<- as.numeric(df2$bias)


df3<-data.frame(dataset=c("correlation"), Pearson=c("cor"),RMSE=c("d"),bias=c("bia"))
df3[1,]<-c("CHELSA 1.2",mean(df2$Pearson[df2$dataset=="CHELSA 1.2"]),mean(df2$RMSE[df2$dataset=="CHELSA 1.2"]),mean(df2$bias[df2$dataset=="CHELSA 1.2"]))
df3[2,]<-c("CHELSA 2.1",mean(df2$Pearson[df2$dataset=="CHELSA 2.1"]),mean(df2$RMSE[df2$dataset=="CHELSA 2.1"]),mean(df2$bias[df2$dataset=="CHELSA 2.1"]))
df3[3,]<-c("CHELSA EarthEnv",mean(df2$Pearson[df2$dataset=="CHELSA EarthEnv"]),mean(df2$RMSE[df2$dataset=="CHELSA EarthEnv"]),mean(df2$bias[df2$dataset=="CHELSA EarthEnv"]))
df3[4,]<-c("PBCOR CHELSA 1.2",mean(df2$Pearson[df2$dataset=="PBCOR CHELSA 1.2"]),mean(df2$RMSE[df2$dataset=="PBCOR CHELSA 1.2"]),mean(df2$bias[df2$dataset=="PBCOR CHELSA 1.2"]))
df3[5,]<-c("PBCOR CHPclim",mean(df2$Pearson[df2$dataset=="PBCOR CHPclim"]),mean(df2$RMSE[df2$dataset=="PBCOR CHPclim"]),mean(df2$bias[df2$dataset=="PBCOR CHPclim"]))
df3[6,]<-c("PBCOR WorldClim",mean(df2$Pearson[df2$dataset=="PBCOR WorldClim"]),mean(df2$RMSE[df2$dataset=="PBCOR WorldClim"]),mean(df2$bias[df2$dataset=="PBCOR WorldClim"]))
df3[7,]<-c("TERRA",mean(df2$Pearson[df2$dataset=="TERRA"]),mean(df2$RMSE[df2$dataset=="TERRA"]),mean(df2$bias[df2$dataset=="TERRA"]))
df3[8,]<-c("CHPclim v1",mean(df2$Pearson[df2$dataset=="CHPclim v1"]),mean(df2$RMSE[df2$dataset=="CHPclim v1"]),mean(df2$bias[df2$dataset=="CHPclim v1"]))
df3[9,]<-c("WorldClim",mean(df2$Pearson[df2$dataset=="WorldClim"]),mean(df2$RMSE[df2$dataset=="WorldClim"]),mean(df2$bias[df2$dataset=="WorldClim"]))
df3[10,]<-c("CHELSA W5E5",mean(df2$Pearson[df2$dataset=="CHELSA W5E5"]),mean(df2$RMSE[df2$dataset=="CHELSA W5E5"]),mean(df2$bias[df2$dataset=="CHELSA W5E5"]))
df3[11,]<-c("CHIRPS v2",mean(df2$Pearson[df2$dataset=="CHIRPS v2"]),mean(df2$RMSE[df2$dataset=="CHIRPS v2"]),mean(df2$bias[df2$dataset=="CHIRPS v2"]))
write.csv(df3,"tables/seasonality.csv")

}
acp3<-acp38
acp3$mon_mean2<- ifelse(acp3$site=="BCICLEAR",acp3$mon_mean,NA)
acp3<- melt(acp3, id=c("site","Month","long_dd","lat_dd"))

acp3[acp3$variable=="mon_mean2"&acp3$site!="BCICLEAR","value"]<-NA
acp3<-acp3 %>% filter(site!="BCI"|variable!="mon_mean2")

acp3$site<- ifelse(acp3$site=="BCICLEAR", "BCI",acp3$site)

acp3$site <- factor(acp3$site,      # Reordering group factor levels
                    levels = c("SANMIGUEL", "AGUACLARA", "PELUCA", "BCI","GUACHA","CASCADAS","GAMBOA","PEDROMIGUEL"))
acp3$variable<- factor(acp3$variable,      # Reordering group factor levels
                    levels = c("chelsaearthenv","chp","terra","worldclim","pbcor_chelsa","pbcor_chp","pbcor_worldclim","chelsaw5e5","chelsa2","chelsa1","chirps","mon_mean2","mon_mean"))
acp3<-acp3[order(acp3$variable),]

ggplot(acp3,aes(x=Month,y=value))+geom_line(aes(color=variable),linewidth=0.2)+
  geom_point(aes(color=variable),size=0.2)+facet_wrap(vars(site),ncol = 4)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+theme_classic()+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1,size=6),strip.text = element_text(size = 5))+ ylab("mm")+
  scale_color_manual(labels = c( "CHELSA EarthEnv","CHP","TERRA","WORLDCLIM","PBCOR CHELSA 1.2",
                                 "PBCOR CHP","PBCOR WORLDCLIM", "CHELSA W5E5","CHELSA 2.1","CHELSA 1.2","CHIRPS v2","Ground manual","Ground")
                     ,values=c('#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#a6cee3','brown',"purple","black"))

ggsave(path = "F:/Vicente",filename = "graphs_final/seasonality_connected.tiff",  units="in", width=7, height=4, dpi=500)

b+ggplot(acp3[acp3$variable=="mon_mean2",],aes(x=Month,y=value))+geom_line(aes(color=variable),linewidth=0.1)+
  geom_point(aes(color=variable),size=0.3)

ggplot(acp3,aes(x=Month,y=value))+geom_point(aes(color=variable),position = position_dodge(width = 1),size=1)+
  facet_wrap(vars(site))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=6),strip.text = element_text(size = 5))
ggsave(path = "F:/Vicente",filename = "graphs_final/seasonality_points.tiff",  units="in", width=7, height=4, dpi=500)

}
##interannual variation, chelsa1, chelsa2 ,chelsaw5e5, terra chelsa earthenv
  ##chelsa2
{
chelsa1<-list.files("chelsa1_annual",full.names = TRUE)
chelsa2<-list.files("chelsa2_annual",full.names = TRUE)
chelsaw5e5<-list.files("chelsaw5e5_annual",full.names= TRUE)
terra<-list.files("terra_annual",full.names = TRUE)
earthenv<-list.files("earthenv_annual",full.names=TRUE)
chirps<-list.files("chirps_annual",full.names = TRUE)
chelsa1<-lapply(chelsa1,raster)
chelsa1<-stack(chelsa1)
chelsa2<-lapply(chelsa2,raster)
chelsa2<-stack(chelsa2)
chelsaw5e5<-lapply(chelsaw5e5, raster)
chelsaw5e5<-stack(chelsaw5e5)
terra<-lapply(terra,raster)
terra<-stack(terra)
earthenv<-lapply(earthenv, raster)
earthenv<-stack(earthenv)
values(earthenv)<-values(earthenv)*0.01
chirps<- lapply(chirps,raster)
chirps<-stack(chirps)
acp38_year<- acp38 %>% group_by(site, Year) %>% summarise(mon_mean=sum(monthlyPrecip))
acp38_year$long_dd<- acp_nyear$long_dd[match(acp38_year$site,acp_nyear$site)]
acp38_year$lat_dd<- acp_nyear$lat_dd[match(acp38_year$site,acp_nyear$site)]
acp38_year<-acp38_year%>% filter(Year>=1979)
years<-data.frame(num=seq(1,40),year=seq(1979,2018))
for (i in 1:length(acp38_year$site)){
acp38_year$num[i]<- years$num[acp38_year$Year[i]==years$year]
}
for (i in 1:length(acp38_year$site)){
   acp38_year$chelsa2[i]<-extract_land(chelsa2[[acp38_year$num[i]]],acp38_year[i,c(4,5)])
   acp38_year$chelsaw5e5[i]<-extract_land(chelsaw5e5[[acp38_year$num[i]]],acp38_year[i,c(4,5)])
   acp38_year$terra[i]<-extract_land(terra[[acp38_year$num[i]]],acp38_year[i,c(4,5)])
}
acp38_year$num3<- ifelse(acp38_year$num>=36,35,acp38_year$num)
for (i in 1:length(acp38_year$site)){
  acp38_year$chelsa1[i]<-extract_land(chelsa1[[acp38_year$num3[i]]],acp38_year[i,c(4,5)])
}
acp38_year$num2<-acp38_year$num-24
acp38_year$num2<-ifelse(acp38_year$num2<=0,NA,acp38_year$num2 )
acp38_year$num2<-ifelse(acp38_year$num2>13,NA,acp38_year$num2 )
acp38_year$num2<-ifelse(is.na(acp38_year$num2),1,acp38_year$num2 )
acp38_year$num4<-acp38_year$num-2
acp38_year$num4<-ifelse(acp38_year$num4<=0,1,acp38_year$num4)
for (i in 1:length(acp38_year$site)){
  acp38_year$earthenv[i]<-extract_land(earthenv[[acp38_year$num2[i]]],acp38_year[i,c(4,5)])
}
for (i in 1:length(acp38_year$site)){
  acp38_year$chirps[i]<-extract_land(chirps[[acp38_year$num4[i]]],acp38_year[i,c(4,5)])
}
acp38_year$earthenv<-ifelse(acp38_year$num2==1&acp38_year$Year!=2003,NA, acp38_year$earthenv)
acp38_year$chelsa1<-ifelse(acp38_year$num3==35&acp38_year$Year>2013,NA,acp38_year$chelsa1)
acp38_year$chirps<-ifelse(acp38_year$num4==1&acp38_year$Year<1981,NA,acp38_year$chirps)
acp38_year<- acp38_year[,-c(6,8,12,14)]
writexl::write_xlsx(acp38_year,"dataframes/acp38_year.xlsx")
acp38_year$mon_mean2<- ifelse(acp38_year=="BCICLEAR",acp38_year$mon_mean,NA)
acp4<- melt(acp38_year, id=c("site","Year","long_dd","lat_dd"))
year<- acp38_year %>% group_by(site) %>% summarise(annual_mean=mean(mon_mean,na.rm=TRUE))
year1<-year[c(3),]
year1[1,1]<-"BCI"
year<-year[-c(3),]
acp4[acp4$site=="BCICLEAR"&acp4$variable=="mon_mean","variable"]<-"mon_mean2"

for(i in 1:length(acp4$site)){
  acp4$mean[i]<- year$annual_mean[year$site==acp4$site[i]]
}
acp4$site<- ifelse(acp4$site=="BCICLEAR","BCI",acp4$site)
acp4$site <- factor(acp4$site,      # Reordering group factor levels
                         levels = c("SANMIGUEL", "AGUACLARA", "PELUCA", "BCI","GUACHA","CASCADAS","GAMBOA","PEDROMIGUEL"))
acp4$variable<-factor(acp4$variable, levels= c("terra","chelsa1","chelsa2","earthenv","chirps","chelsaw5e5","mon_mean2","mon_mean"))
p<-ggplot(acp4,aes(x=Year,y=value))+geom_line(aes(color=variable),size=0.3)+
  geom_point(aes(color=variable),size=0.3)+
  facet_wrap(vars(site),ncol=2)+
  theme_classic()+
  ylab("Annual Total precipitation")+
  labs(title="Interannual Variability",size=20)+
  theme(strip.text = element_text(size = 5))+
  scale_color_manual(labels = c("TERRA","CHELSA 1.2", "CHELSA 2.1", "CHELSA EarthEnv","CHIRPS v2","CHELSA W5E5","Ground manual", "Ground"),
                     values=c( '#a65628','#e7298a', '#ff7f00','#984ea3','#4daf4a','#377eb8','#e41a1c','black'))

year$site <- factor(year$site,      # Reordering group factor levels
                    levels = c("SANMIGUEL", "AGUACLARA", "PELUCA", "BCI","GUACHA","CASCADAS","GAMBOA","PEDROMIGUEL"))
p + geom_text(
  data    = year,
  mapping = aes(x = -Inf, y = 5250, label = paste0("Mean annual precipitation=",round(annual_mean,0))),
  hjust   = -0.1,
  vjust   = -1,
  size=2
)+
  geom_text(
    data    = year1,
    mapping = aes(x = 1994, y = 5250, label = paste0("(electronic), ",round(annual_mean,0),"(manual)")),
    hjust   = -0.1,
    vjust   = -1,
    size=2)
ggsave(path = "F:/Vicente",filename = "graphs_final/interannual_point.tiff",  units="in", width=8, height=5, dpi=500) 
##annual 
df2<-data.frame(dataset=c("correlation"), Pearson=c("cor"),RMSE=c("d"),bias=c("bia"), mae=c("mae"),r=c("r2"))
df2[1,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="SANMIGUEL"],acp38_year$chelsa1[acp38_year$site=="SANMIGUEL"]))
df2[2,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="SANMIGUEL"],acp38_year$chelsa2[acp38_year$site=="SANMIGUEL"]))
df2[3,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="SANMIGUEL"],acp38_year$earthenv[acp38_year$site=="SANMIGUEL"]))
df2[4,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="SANMIGUEL"],acp38_year$terra[acp38_year$site=="SANMIGUEL"]))
df2[5,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="SANMIGUEL"],acp38_year$chelsaw5e5[acp38_year$site=="SANMIGUEL"]))
df2[6,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="SANMIGUEL"],acp38_year$chirps[acp38_year$site=="SANMIGUEL"]))
df2[7,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="AGUACLARA"],acp38_year$chelsa1[acp38_year$site=="AGUACLARA"]))
df2[8,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="AGUACLARA"],acp38_year$chelsa2[acp38_year$site=="AGUACLARA"]))
df2[9,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="AGUACLARA"],acp38_year$earthenv[acp38_year$site=="AGUACLARA"]))
df2[10,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="AGUACLARA"],acp38_year$terra[acp38_year$site=="AGUACLARA"]))
df2[11,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="AGUACLARA"],acp38_year$chelsaw5e5[acp38_year$site=="AGUACLARA"]))
df2[12,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="AGUACLARA"],acp38_year$chirps[acp38_year$site=="AGUACLARA"]))
df2[13,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="PELUCA"],acp38_year$chelsa1[acp38_year$site=="PELUCA"]))
df2[14,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="PELUCA"],acp38_year$chelsa2[acp38_year$site=="PELUCA"]))
df2[15,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="PELUCA"],acp38_year$earthenv[acp38_year$site=="PELUCA"]))
df2[16,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="PELUCA"],acp38_year$terra[acp38_year$site=="PELUCA"]))
df2[17,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="PELUCA"],acp38_year$chelsaw5e5[acp38_year$site=="PELUCA"]))
df2[18,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="PELUCA"],acp38_year$chirps[acp38_year$site=="PELUCA"]))
df2[19,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="BCI"],acp38_year$chelsa1[acp38_year$site=="BCI"]))
df2[20,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="BCI"],acp38_year$chelsa2[acp38_year$site=="BCI"]))
df2[21,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="BCI"],acp38_year$earthenv[acp38_year$site=="BCI"]))
df2[22,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="BCI"],acp38_year$terra[acp38_year$site=="BCI"]))
df2[23,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="BCI"],acp38_year$chelsaw5e5[acp38_year$site=="BCI"]))
df2[24,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="BCI"],acp38_year$chirps[acp38_year$site=="BCI"]))
df2[25,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="GUACHA"],acp38_year$chelsa1[acp38_year$site=="GUACHA"]))
df2[26,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="GUACHA"],acp38_year$chelsa2[acp38_year$site=="GUACHA"]))
df2[27,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="GUACHA"],acp38_year$earthenv[acp38_year$site=="GUACHA"]))
df2[28,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="GUACHA"],acp38_year$terra[acp38_year$site=="GUACHA"]))
df2[29,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="GUACHA"],acp38_year$chelsaw5e5[acp38_year$site=="GUACHA"]))
df2[30,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="GUACHA"],acp38_year$chirps[acp38_year$site=="GUACHA"]))
df2[31,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="BCICLEAR"],acp38_year$chelsa1[acp38_year$site=="BCICLEAR"]))
df2[32,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="BCICLEAR"],acp38_year$chelsa2[acp38_year$site=="BCICLEAR"]))
df2[33,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="BCICLEAR"],acp38_year$earthenv[acp38_year$site=="BCICLEAR"]))
df2[34,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="BCICLEAR"],acp38_year$terra[acp38_year$site=="BCICLEAR"]))
df2[35,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="BCICLEAR"],acp38_year$chelsaw5e5[acp38_year$site=="BCICLEAR"]))
df2[36,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="BCICLEAR"],acp38_year$chirps[acp38_year$site=="BCICLEAR"]))
df2[37,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="CASCADAS"],acp38_year$chelsa1[acp38_year$site=="CASCADAS"]))
df2[38,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="CASCADAS"],acp38_year$chelsa2[acp38_year$site=="CASCADAS"]))
df2[39,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="CASCADAS"],acp38_year$earthenv[acp38_year$site=="CASCADAS"]))
df2[40,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="CASCADAS"],acp38_year$terra[acp38_year$site=="CASCADAS"]))
df2[41,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="CASCADAS"],acp38_year$chelsaw5e5[acp38_year$site=="CASCADAS"]))
df2[42,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="CASCADAS"],acp38_year$chirps[acp38_year$site=="CASCADAS"]))
df2[43,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="GAMBOA"],acp38_year$chelsa1[acp38_year$site=="GAMBOA"]))
df2[44,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="GAMBOA"],acp38_year$chelsa2[acp38_year$site=="GAMBOA"]))
df2[45,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="GAMBOA"],acp38_year$earthenv[acp38_year$site=="GAMBOA"]))
df2[46,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="GAMBOA"],acp38_year$terra[acp38_year$site=="GAMBOA"]))
df2[47,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="GAMBOA"],acp38_year$chelsaw5e5[acp38_year$site=="GAMBOA"]))
df2[48,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="GAMBOA"],acp38_year$chirps[acp38_year$site=="GAMBOA"]))
df2[49,]<-c("CHELSA 1.2",stats(acp38_year$mon_mean[acp38_year$site=="PEDROMIGUEL"],acp38_year$chelsa1[acp38_year$site=="PEDROMIGUEL"]))
df2[50,]<-c("CHELSA 2.1",stats(acp38_year$mon_mean[acp38_year$site=="PEDROMIGUEL"],acp38_year$chelsa2[acp38_year$site=="PEDROMIGUEL"]))
df2[51,]<-c("CHELSA EarthEnv",stats(acp38_year$mon_mean[acp38_year$site=="PEDROMIGUEL"],acp38_year$earthenv[acp38_year$site=="PEDROMIGUEL"]))
df2[52,]<-c("TERRA",stats(acp38_year$mon_mean[acp38_year$site=="PEDROMIGUEL"],acp38_year$terra[acp38_year$site=="PEDROMIGUEL"]))
df2[53,]<-c("CHELSA W5E5",stats(acp38_year$mon_mean[acp38_year$site=="PEDROMIGUEL"],acp38_year$chelsaw5e5[acp38_year$site=="PEDROMIGUEL"]))
df2[54,]<-c("CHIRPS v2",stats(acp38_year$mon_mean[acp38_year$site=="PEDROMIGUEL"],acp38_year$chirps[acp38_year$site=="PEDROMIGUEL"]))
df2$Pearson<-as.numeric(df2$Pearson)
df2$RMSE<-as.numeric(df2$RMSE)
df2$bias<- as.numeric(df2$bias)
df3<-data.frame(dataset=c("correlation"), Pearson=c("cor"),RMSE=c("d"),bias=c("bia"))
df3[1,]<-c("CHELSA 1.2",mean(df2$Pearson[df2$dataset=="CHELSA 1.2"]),mean(df2$RMSE[df2$dataset=="CHELSA 1.2"]),mean(df2$bias[df2$dataset=="CHELSA 1.2"]))
df3[2,]<-c("CHELSA 2.1",mean(df2$Pearson[df2$dataset=="CHELSA 2.1"]),mean(df2$RMSE[df2$dataset=="CHELSA 2.1"]),mean(df2$bias[df2$dataset=="CHELSA 2.1"]))
df3[3,]<-c("CHELSA EarthEnv",mean(df2$Pearson[df2$dataset=="CHELSA EarthEnv"]),mean(df2$RMSE[df2$dataset=="CHELSA EarthEnv"]),mean(df2$bias[df2$dataset=="CHELSA EarthEnv"]))
df3[4,]<-c("TERRA",mean(df2$Pearson[df2$dataset=="TERRA"]),mean(df2$RMSE[df2$dataset=="TERRA"]),mean(df2$bias[df2$dataset=="TERRA"]))
df3[5,]<-c("CHELSA W5E5",mean(df2$Pearson[df2$dataset=="CHELSA W5E5"]),mean(df2$RMSE[df2$dataset=="CHELSA W5E5"]),mean(df2$bias[df2$dataset=="CHELSA W5E5"]))
df3[6,]<-c("CHIRPS v2",mean(df2$Pearson[df2$dataset=="CHIRPS v2"]),mean(df2$RMSE[df2$dataset=="CHIRPS v2"]),mean(df2$bias[df2$dataset=="CHIRPS v2"]))
write.csv(df3,"tables/interannual.csv")



}
#deviation plot
{
  acp38_jmfa<- acp38_ag[acp38_ag$Month==c(1,2,3,4),]
  acp38_jmfa$bias_chelsa<- acp38_jmfa$chelsa1-acp38_jmfa$mon_mean
  acp38_jmfa$bias_chelsa2<- acp38_jmfa$chelsa2-acp38_jmfa$mon_mean
  acp38_jmfa$bias_chelsaw5e5<- acp38_jmfa$chelsaw5e5-acp38_jmfa$mon_mean
  acp38_jmfa$bias_chelsaearthenv<- acp38_jmfa$chelsaearthenv-acp38_jmfa$mon_mean
  acp38_jmfa$bias_terra<- acp38_jmfa$terra-acp38_jmfa$mon_mean
  acp38_jmfa$bias_pbcor_chelsa<- acp38_jmfa$pbcor_chelsa-acp38_jmfa$mon_mean
  acp38_jmfa$bias_pbcor_chp<- acp38_jmfa$pbcor_chp-acp38_jmfa$mon_mean
  acp38_jmfa$bias_chp<- acp38_jmfa$chp-acp38_jmfa$mon_mean

  acp38_jmfa<-acp38_jmfa[,c(1,2,14,15,16,17,18,19,20,21)]
  acp38_jmfa_melt<-melt(acp38_jmfa, id=c("site","Month"))
  acp38_jmfa_melt$site <- factor(acp38_jmfa_melt$site,      # Reordering group factor levels
                      levels = c("SANMIGUEL", "AGUACLARA", "PELUCA", "BCI","GUACHA","BCICLEAR","CASCADAS","PEDROMIGUEL","BALBOAFAA"))
  
 ggplot(data=acp38_jmfa_melt, aes(x=Month, y=value, fill=variable)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme_classic()+facet_wrap(vars(site))+
    scale_x_continuous(breaks=c(1,2,3,4),
                       labels=c("Jan", "Feb", "Mar","Apr"))+
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1,size=10),strip.text = element_text(size = 5))+
   scale_fill_manual(labels = c("CHELSA 1.2","CHELSA 2.1", "CHELSA W5E5", "TERRA","CHELSA EarthEnv","PBCOR CHELSA 1.2","PBCOR CHP","CHP")
     ,values=c("#b2182b", "#2166ac","#1b7837","#762a83","#f768a1","#ef6548","#8c96c6","#b35806"))
                       
  ggsave(path = "F:/Vicente",filename = "graphs_final/fill_deviation_jmfa.tiff",  units="in", width=7, height=4, dpi=500) 
  a<-acp38_jmfa_melt %>% group_by(variable)%>% summarise(mean=mean(value))
  a %>%
    kbl() %>%
    kable_classic_2(full_width = F)

  acp38_jmfa<- acp38_ag[acp38_ag$Month==c(5,6,7,8),]
  b<-acp38_ag[acp38_ag$Month==c(9,10,11,12),]
  acp38_jmfa<-bind_rows(acp38_jmfa,b)
  
  acp38_jmfa$bias_chelsa<- acp38_jmfa$chelsa1-acp38_jmfa$mon_mean
  acp38_jmfa$bias_chelsa2<- acp38_jmfa$chelsa2-acp38_jmfa$mon_mean
  acp38_jmfa$bias_chelsaw5e5<- acp38_jmfa$chelsaw5e5-acp38_jmfa$mon_mean
  acp38_jmfa$bias_chelsaearthenv<- acp38_jmfa$chelsaearthenv-acp38_jmfa$mon_mean
  acp38_jmfa$bias_terra<- acp38_jmfa$terra-acp38_jmfa$mon_mean
  acp38_jmfa$bias_pbcor_chelsa<- acp38_jmfa$pbcor_chelsa-acp38_jmfa$mon_mean
  acp38_jmfa$bias_pbcor_chp<- acp38_jmfa$pbcor_chp-acp38_jmfa$mon_mean
  acp38_jmfa$bias_chp<- acp38_jmfa$chp-acp38_jmfa$mon_mean
  
  acp38_jmfa<-acp38_jmfa[,c(1,2,14,15,16,17,18,19,20,21)]
  acp38_jmfa_melt<-melt(acp38_jmfa, id=c("site","Month"))
  acp38_jmfa_melt$site <- factor(acp38_jmfa_melt$site,      # Reordering group factor levels
                                 levels = c("SANMIGUEL", "AGUACLARA", "PELUCA", "BCI","GUACHA","BCICLEAR","CASCADAS","PEDROMIGUEL","BALBOAFAA"))
  
  ggplot(data=acp38_jmfa_melt, aes(x=Month, y=value, fill=variable)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme_classic()+facet_wrap(vars(site))+
    scale_x_continuous(breaks=c(5,6,7,8,9,10,11,12),
                       labels=c("May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))+
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1,size=10),strip.text = element_text(size = 5))+
    scale_fill_manual(labels = c("CHELSA 1.2","CHELSA 2.1", "CHELSA W5E5", "TERRA","CHELSA EarthEnv","PBCOR CHELSA 1.2","PBCOR CHP","CHP")
                      ,values=c("#b2182b", "#2166ac","#1b7837","#762a83","#f768a1","#ef6548","#8c96c6","#b35806"))
  
  ggsave(path = "F:/Vicente",filename = "graphs_final/fill_deviation_rainy.tiff",  units="in", width=7, height=4, dpi=500)
  
  a<-acp38_jmfa_melt %>% group_by(variable)%>% summarise(mean=mean(value))
  a %>%
    kbl() %>%
    kable_classic_2(full_width = F)
}
##inter monthly variability
{
  View(acp38)
  acp38<- acp38[,-c(1)]
  
  ##load chelsa2 monthly 
  ch2<- list.files("chelsa_crop/",full.names = TRUE)
  ch2<-as.data.frame(ch2)
  ch2$month<- substring(ch2$ch2, 13,14)
  ch2$year<- substring(ch2$ch2, 16,19)
  ch2<- ch2 %>% filter(year<=2016)
  
  ##merge
  ch2$year<-as.numeric(ch2$year)
  ch2$month<- as.numeric(ch2$month)
  monthly<- acp38 %>% inner_join(ch2, 
                                by=c('Year'='year', 'Month'='month'))
  monthly$long_dd<- acp_nyear$long_dd[match(monthly$site,acp_nyear$site)]
  monthly$lat_dd<- acp_nyear$lat_dd[match(monthly$site,acp_nyear$site)]
  
  ##load and extract
  for (i in 1:length(monthly$Year)){
    r<- raster(monthly$ch2[i])
    monthly$ch2[i]<- raster::extract(r,monthly[i,c("long_dd","lat_dd")])
    }
  monthly$ch2<-as.numeric(monthly$ch2)
  monthly$ch2<- monthly$ch2/100
  #load chelsa1
  ch1<- list.files("chelsa_crop1/",full.names = TRUE)
  ch1<-as.data.frame(ch1)
  ch1$month<- substring(ch1$ch1, 19,20)
  ch1$year<- substring(ch1$ch1, 14,17)
  
  ch1$year<-as.numeric(ch1$year)
  ch1$month<- as.numeric(ch1$month)
  temp<- monthly %>% full_join(ch1, 
                                 by=c('Year'='year', 'Month'='month'))
  
  temp<- temp%>% filter(!is.na(ch1))
  
  for (i in 1:length(temp$Year)){
    r<- raster(temp$ch1[i])
    values(r)<-ifelse(values(r)==65535,NA,values(r))
    temp$ch1[i]<- extract_land(r,monthly[i,c("long_dd","lat_dd")])
  }
  
  #load chelsa w5e5
  chw5e5<- list.files("chelsaw5e5_monthly/",full.names = TRUE)
  chw5e5<-as.data.frame(chw5e5)
  chw5e5$month<- substring(chw5e5$chw5e5, 25,26)
  chw5e5$year<- substring(chw5e5$chw5e5, 20,23)
  
  chw5e5$year<-as.numeric(chw5e5$year)
  chw5e5$month<- as.numeric(chw5e5$month)
  monthly<- monthly %>% full_join(chw5e5, 
                               by=c('Year'='year', 'Month'='month'))
  
  for (i in 1:length(monthly$Year)){
    r<- raster(monthly$chw5e5[i])
    monthly$chw5e5[i]<- extract_land(r,monthly[i,c("long_dd","lat_dd")])
  }
  
  #load monthly terra
  terra<- list.files("terra/",full.names = TRUE)
  terra<-as.data.frame(terra)
  terra$year<- substring(terra$terra, 8,11)
  terra$month<- substring(terra$terra, 13,14)  
  terra$year<-as.numeric(terra$year)
  terra$month<- as.numeric(terra$month)
  terra<-terra %>% filter(year>=1979&year<=2016)
  
  

  monthly<- monthly %>% full_join(terra, 
                                  by=c('Year'='year', 'Month'='month'))
  
  for (i in 1:length(monthly$Year)){
    r<- raster(monthly$terra[i])
    monthly$terra[i]<- extract_land(r,monthly[i,c("long_dd","lat_dd")])
  }
  
  ert<- list.files("earthenv_monthly/",full.names = TRUE)
  ert<-as.data.frame(ert)
  ert$month<- substring(ert$ert, 23,24)
  ert$year<- substring(ert$ert, 18,21)
  
  ert$year<-as.numeric(ert$year)
  ert$month<- as.numeric(ert$month)
  temp2<- monthly %>% full_join(ert, 
                               by=c('Year'='year', 'Month'='month'))
  
  temp2<- temp2%>% filter(!is.na(ert))
  
  for (i in 1:length(temp2$Year)){
    r<- raster(temp2$ert[i])
    temp2$ert[i]<- raster::extract(r,temp2[i,c("long_dd","lat_dd")])
  }
  
  temp<-temp[,c(1,2,3,10)]
  temp2<-temp2[,c(1,2,3,10)]
  
  final<- monthly%>% full_join(temp, 
                               by=c('Year'='Year', 'Month'='Month','site'='site'))
  
  final2<-final %>% full_join(temp2, 
                             by=c('Year'='Year', 'Month'='Month','site'='site'))
  final2$ert<-as.numeric(final2$ert)
  
  final2<- final2 %>% select("Year","Month","site","monthlyPrecip.x","ch2.x","chw5e5","terra","ch1","ert")
  final2$chw5e5<-as.numeric(final2$chw5e5)
  final2$terra<-as.numeric(final2$terra)
  final2$ch1<-as.numeric(final2$ch1)
  
  stat<-c("correlation","mae","rmse","bias","r^2")
  df2<-data.frame(stat)
  df2$chelsa1<-stats(final2$ch1,final2$monthlyPrecip.x)
  df2$chelsa2<-stats(final2$ch2.x,final2$monthlyPrecip.x)
  df2$chelsaw5e5<-stats(final2$chw5e5,final2$monthlyPrecip.x)
  df2$terra<-stats(final2$terra,final2$monthlyPrecip.x)
  df2$chelsaearthenv<-stats(final2$ert,final2$monthlyPrecip.x)
  
  
  write.csv(df2,"intramonthly.csv")
}

##daily variability
{
  daily<-read.csv("daily_prec_stri.csv")
  daily<- daily%>% filter(site!="BOCAS")
  daily$year<- as.numeric(substring(daily$date,1,4))
  daily$month<- substring(daily$date,6,7)
  daily$day<- substring(daily$date,9,10)
  
  daily<- daily%>% filter(year>2002&year<2017)
  daily<-daily%>% filter(year!=2016|month!="02")
  daily<-daily%>% filter(year!=2016|month!="03")
  
  daily$link<- paste0("earthenv/",daily$day,"_",daily$month,"_",daily$year,".tif")
    
  for (i in 1:length(daily$year)){
    r<- raster(daily$link[i])
    daily$earthenv[i]<- raster::extract(r,daily[i,c("lon_dd","lat_dd")])
  }
  daily$earthenv<- daily$earthenv*0.01
  
  daily$link2<- paste0("chelsaw5e5_daily/X",daily$year,".",daily$month,".",daily$day,".tif")
  
  for (i in 1:length(daily2$year)){
    r<- raster(daily2$link2[i])
    daily2$chelsaw5e5[i]<- raster::extract(r,daily2[i,c("lon_dd","lat_dd")])
  }
  
  
  stat<-c("correlation","mae","rmse","bias","r^2")
  df3<-data.frame(stat)
  df3$earthenv<-stats(daily$earthenv,daily$sum.ra.)
  df3$chelsaw5e5<-stats(daily$chelsaw5e5,daily$sum.ra.)
  
  ks.test(daily$earthenv,daily$sum.ra.)
  ks.test(daily$chelsaw5e5,daily$sum.ra.)
}

#who captures best the interannual variability
{
  acp5<- acp38_year %>% select("site","Year","mon_mean","chelsa1","chelsa2","chelsaw5e5","terra","earthenv")%>%
                  mutate(chelsa1= chelsa1-mon_mean,
                         chelsa2= chelsa2-mon_mean,
                         chelsaw5e5= chelsaw5e5-mon_mean,
                         terra=terra-mon_mean,
                         earthenv=earthenv-mon_mean
                         )
  acp5<- melt(acp5, id=c("site","Year","mon_mean"))
  best<- acp5 %>% group_by(site,variable)%>% summarise(mean_bias= mean(value,na.rm=TRUE))
best$site<-factor(best$site,
                  levels= c("SANMIGUEL", "AGUACLARA", "PELUCA", "BCI","GUACHA","BCICLEAR","CASCADAS","PEDROMIGUEL","BALBOAFAA"))
ggplot(data=best, aes(x=site, y=mean_bias, fill=variable)) +
  geom_bar(stat="identity",position = position_dodge())+
  ylab("Mean residuals")+
  labs(title="Mean residuals per site",size=25)+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1))+
  scale_fill_brewer(palette="Paired",labels = c("CHELSA 1.2","CHELSA 2.1", "CHELSA W5E5", "TERRA","CHELSA EarthEnv"))

ggsave(path = "F:/Vicente",filename = "graphs_final/mean_residuals.tiff",  units="in", width=7, height=4, dpi=500)


best2<- acp5 %>% group_by(variable)%>% summarise(mean_bias= mean(value,na.rm=TRUE))

best2 %>%
  kbl() %>%
  kable_classic_2(full_width = F)
}

##bias maps
{
acp_bias<- final
  acp_bias$bias<- acp_bias$value-acp_bias$annualPrecip_acp
    nowater<- rast("nowater.tif")
    nowater<-raster::crop(nowater,c(-80.2,-79.4, 8.8, 9.5))
    pr <- as.polygons(nowater > 0)
    s <- sf::st_as_sf(pr)
    acp_long<- acp[,c(1,7,8)]
for (i in 1:length(acp_bias$site)){
  acp_bias$long_dd[i]<- acp_long$long_dd[acp_long$site==acp_bias$site[i]]
}
for (i in 1:length(acp_bias$site)){
  acp_bias$lat_dd[i]<- acp_long$lat_dd[acp_long$site==acp_bias$site[i]]
}
    inter<- SpatialPoints(acp_bias[,7:8],proj4string=CRS("+proj=longlat +datum=WGS84"))
    temp<-as.data.frame(acp_bias[,c(4,6)])
    inter2<-SpatialPointsDataFrame(inter, temp)
  tm_shape(s) + tm_polygons(alpha=0)+
  tm_shape(inter2)+
  tm_facets(by=c("variable"), ncol = 5,as.layers = TRUE)+
  tm_dots(col='bias',title = "Bias(mm)",
          palette = c('#d73027','#f46d43','#fdae61','#fee08b','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850'),
          breaks = c(-1500,-1000,-500,-100,0,100,500,1000,1500),
          size = 0.4, alpha=1,legend.hist=FALSE,midpoint = NA) 
tmap_save( filename = "graphs_final/bias_facets.tiff",width = 7, height = 6,units = "in",dpi = 500)

acp_bias<- final2
acp_bias$bias<- acp_bias$value-acp_bias$jfmaPrecip_acp
nowater<- rast("nowater.tif")
nowater<-raster::crop(nowater,c(-80.2,-79.4, 8.8, 9.5))
pr <- as.polygons(nowater > 0)
s <- sf::st_as_sf(pr)
acp_long<- acp[,c(1,7,8)]
for (i in 1:length(acp_bias$site)){
  acp_bias$long_dd[i]<- acp_long$long_dd[acp_long$site==acp_bias$site[i]]
}
for (i in 1:length(acp_bias$site)){
  acp_bias$lat_dd[i]<- acp_long$lat_dd[acp_long$site==acp_bias$site[i]]
}
inter<- SpatialPoints(acp_bias[,7:8],proj4string=CRS("+proj=longlat +datum=WGS84"))
temp<-as.data.frame(acp_bias[,c(4,6)])
inter2<-SpatialPointsDataFrame(inter, temp)
tm_shape(s) + tm_polygons(alpha=0)+
  tm_shape(inter2)+
  tm_facets(by=c("variable"), ncol = 5,as.layers = TRUE)+
  tm_dots(col='bias',title = "Bias(mm)",
          palette = c('#d73027','#f46d43','#fdae61','#fee08b','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850'),
          breaks = c(-500,-300,-200,-100,-50,0,50,100,200),
          size = 0.4, alpha=1,legend.hist=FALSE,midpoint = NA) 
tmap_save( filename = "graphs_final/biasjfma_facets.tiff",width = 7, height = 6,units = "in",dpi = 500)
}
## distribution fitting and z test 5 models
{
  #chelsa monthly
  acp38
  library(stringr)
  acp38$Month<-str_pad(acp38$Month, 2, pad = "0")
  

  acp38$ch<-paste0("chelsa_crop/", acp38$Month,"_",acp38$Year,"_V.2.1.tif")
  
  chelsa1_monthly_list<- list.files("chelsa_crop/",full.names = TRUE)
  chelsa1_monthly<- lapply(acp38$ch, raster)
  acp38$long_dd<- acp_nyear$long_dd[match(acp38$site,acp_nyear$site)]
  acp38$lat_dd<- acp_nyear$lat_dd[match(acp38$site,acp_nyear$site)]
  
   
   for (i in 1:length(acp38$site)){
     acp38$chelsa1[i]<- extract_land(chelsa1_monthly[[i]],acp38[i,c(7,8)])
   }
  
  acp38<- acp38 %>% rename(chelsa2=chelsa1)
  acp38$chelsa2<- acp38$chelsa2/100
  acp38$ch<- paste0('chelsa_crop1/',acp38$Year,"_",acp38$Month,"_V1.2.1.tif")
 
  acp38_crop<- acp38 %>% subset(Year<= 2013)
  acp38_crop2$ch<- paste0('chelsa_crop1/',acp38_crop2$Year,"_",acp38_crop2$Month,"_V1.2.1.tif")
  monthly_chelsa<- lapply(acp38_crop2$ch,raster)
  for (i in 1:length(monthly_chelsa)){
   values(monthly_chelsa[[i]])<- ifelse(values(monthly_chelsa[[i]])==65535, NA,values(monthly_chelsa[[i]]))
  }
  for (i in 1:length(acp38_crop2$site)){
    acp38_crop2$chelsa1[i]<- extract_land(monthly_chelsa[[i]],acp38_crop2[i,c(7,8)])
  }
  
acp38_crop$ch<-paste0("chelsaw5e5_monthly/",acp38_crop$Year,"_",acp38_crop$Month,".tif")
monthly_chelsaw5e5<- lapply(acp38_crop$ch,raster)

for (i in 1:length(acp38_crop$site)){
  acp38_crop$chelsaw5e5[i]<- extract_land(monthly_chelsaw5e5[[i]],acp38_crop[i,c(7,8)])
}
acp38_crop$ch<-paste0("terra/X",acp38_crop$Year,".",acp38_crop$Month,".01.tif")
monthly_terra<- lapply(acp38_crop$ch,raster)

for (i in 1:length(acp38_crop$site)){
  acp38_crop$terra[i]<- extract_land(monthly_terra[[i]],acp38_crop[i,c(7,8)])
}

acp38_crop2<-acp38_crop %>% subset(Year>=2003&Year<=2015)
View(acp38_crop2)
acp38_crop2$ch<-paste0("earthenv_monthly/",acp38_crop2$Year,"_",acp38_crop2$Month,".tif")
earthenv<-lapply(acp38_crop2$ch, raster)

for (i in 1:length(acp38_crop2$site)){
  acp38_crop2$earthenv[i]<- extract_land(earthenv[[i]],acp38_crop2[i,c(7,8)])
}
  
  test_ground<-plotdist(acp38$monthlyPrecip[acp38$site=="AGUACLARA"], histo = TRUE, demp = TRUE)
  test_chelsa2<-plotdist(acp38$chelsa2[acp38$site=="AGUACLARA"], histo = TRUE, demp = TRUE)
  
  #kurtois skewness plot
  descdist(acp38$monthlyPrecip, boot = 1000)
  descdist(acp38$chelsa2[acp38$site=="AGUACLARA"], boot = 1000)
  
  
  
  fw <- fitdist(acp38$monthlyPrecip, "weibull")
  fg <- fitdist(acp38$monthlyPrecip, "gamma")
  fe <- fitdist(acp38$monthlyPrecip, "exp")
  par(mfrow = c(2, 2))
  plot.legend <- c("Weibull", "gamma", "expo")
  denscomp(fe, legendtext = plot.legend)
  qqcomp(list(fw, fg, fe), legendtext = plot.legend)
  cdfcomp(list(fw, fg, fe), legendtext = plot.legend)
  ppcomp(list(fw, fg, fe), legendtext = plot.legend)
  
  denscomp(list(fw, fg), legendtext = c("Weibull", "gamma"))
  gofstat(list(fw, fg))
  
  Random.Weibull(309.2556, 229.1921)
  
  ## cumulative density function 
 cdf_aguaclara<- ecdf(acp38$monthlyPrecip[acp38$site=="AGUACLARA"])
 plot(cdf_aguaclara)
 cdf_chelsa2_aguaclara<- ecdf(acp38$chelsa2[acp38$site=="AGUACLARA"])
 plot(cdf_chelsa2_aguaclara)
 
 plot(ecdf(acp38$chelsa2),
      xlim = range(c(acp38$chelsa2,acp38$monthlyPrecip)),
      col = "blue")
 plot(ecdf(acp38$monthlyPrecip),
      add = TRUE,
      lty = "dashed",
      col = "red")

 ks.test(acp38$chelsa2,acp38$monthlyPrecip)
hist(acp38$chelsa2)
hist(acp38$monthlyPrecip)
 ks.test(acp38$chelsa2[acp38$site=="BALBOAFAA"],acp38$monthlyPrecip[acp38$site=="BALBOAFAA"])
 
 ggplot(acp38)+geom_histogram(aes(x=monthlyPrecip))
 
 ggplot(acp38, aes(x=chelsa2)) + 
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   geom_density(alpha=.2, fill="#FF6666") +facet_wrap(vars(site))
 
 
 
 acp38_crop2<- final2%>% filter(Year>2002)
 acp38_crop2<- acp38_crop2%>% filter(Year<2014)
 

 

 acp38_f<-melt(acp38_crop2,id=c("site","Month","Year"))
 acp38_f<- acp38_f%>% mutate()
               
 cdf<-ggplot(acp38_f, aes(x=value,color=variable)) +
   stat_ecdf(geom="line",pad = FALSE,size=0.3,position = position_dodge(width = 1))+
   scale_color_manual(labels = c("Ground", "CHELSA 2.1","CHELSA W5E5", "TERRA","CHELSA 1.2","CHELSA EarthEnv")
                      ,values=c('black', '#b2182b', '#2166ac',"#1b7837","#762a83","#f768a1","#ef6548"))+
   facet_wrap(vars(Month))
 
 ggsave(path = "F:/Vicente",filename = "graphs_final/cdf.tiff",  units="in", width=7, height=4, dpi=500)
 
  jan<- acp38_crop2%>%filter(Month=="1")
    paste(ks.test(jan$monthlyPrecip.x,jan$ch2.x)$p.value,ks.test(jan$monthlyPrecip.x,jan$ch2.x)$statistic)
    paste(ks.test(jan$monthlyPrecip.x,jan$chw5e5)$p.value,  ks.test(jan$monthlyPrecip.x,jan$chw5e5)$statistic)
    paste(ks.test(jan$monthlyPrecip.x,jan$ert)$p.value, ks.test(jan$monthlyPrecip.x,jan$ert)$statistic)   
    paste(ks.test(jan$monthlyPrecip.x,jan$ch1)$p.value,ks.test(jan$monthlyPrecip.x,jan$ch1)$statistic)
    paste(ks.test(jan$monthlyPrecip.x,jan$terra)$p.value, ks.test(jan$monthlyPrecip.x,jan$terra)$statistic)
   
    
    kolmogorov <- read_excel("kolmogorov.xlsx")
    
    ggplot(kolmogorov,aes(x=Month,y=`D-statistic`))+geom_line(aes(group=dataset),stat="identity")
 
 ggplot(acp38_f, aes(value)) +
   coord_cartesian(xlim = c(250,1000),ylim = c(0.30,1))+
   stat_ecdf(aes(color=variable),geom = "line", pad = FALSE, position = "jitter",size=0.5)+
   facet_wrap(vars(site))
 ggsave(path = "F:/Vicente",filename = "graphs_final/cumulative2.tiff",  units="in", width=6, height=5, dpi=500)
 
 
 
 
  #best fit is weibull distribution
 ks.test(acp38$chelsa2[acp38$site=="BALBOAFAA"],acp38$monthlyPrecip[acp38$site=="BALBOAFAA"])
  ks.test(acp38$chelsa2[acp38$site=="AGUACLARA"],acp38$monthlyPrecip[acp38$site=="AGUACLARA"])
 ks.test(acp38$chelsa2[acp38$site=="SANMIGUEL"],acp38$monthlyPrecip[acp38$site=="SANMIGUEL"])
 ks.test(acp38$chelsa2[acp38$site=="BCI"],acp38$monthlyPrecip[acp38$site=="BCI"])
 ks.test(acp38$chelsa2[acp38$site=="BCICLEAR"],acp38$monthlyPrecip[acp38$site=="BCICLEAR"])
 ks.test(acp38$chelsa2[acp38$site=="CASCADAS"],acp38$monthlyPrecip[acp38$site=="CASCADAS"])
 ks.test(acp38$chelsa2[acp38$site=="GUACHA"],acp38$monthlyPrecip[acp38$site=="GUACHA"])
 ks.test(acp38$chelsa2[acp38$site=="PELUCA"],acp38$monthlyPrecip[acp38$site=="PELUCA"])
 ks.test(acp38$chelsa2[acp38$site=="PEDROMIGUEL"],acp38$monthlyPrecip[acp38$site=="PEDROMIGUEL"])
 
 ks.test(acp38_crop2$chelsa1, acp38_crop2$monthlyPrecip)[1]
 ks.test(acp38_crop2$chelsa2, acp38_crop2$earthenv)
 ks.test(acp38_crop2$chelsaw5e5, acp38_crop2$monthlyPrecip)[1]
 ks.test(acp38_crop2$terra, acp38_crop2$monthlyPrecip)[1]
 ks.test(acp38_crop2$earthenv, acp38_crop2$monthlyPrecip)[1]
 
rmse(acp38_crop2$chelsa1, acp38_crop2$monthlyPrecip)[1]
rmse(acp38_crop2$chelsa2, acp38_crop2$monthlyPrecip)[1]
rmse(acp38_crop2$chelsaw5e5, acp38_crop2$monthlyPrecip)[1]
rmse(acp38_crop2$terra, acp38_crop2$monthlyPrecip)[1]
rmse(acp38_crop2$earthenv, acp38_crop2$monthlyPrecip)[1]
 
 
 ggplot(acp38_f,aes(value,after_stat(density)))+geom_histogram(binwidth = 30)+facet_wrap(vars(variable))
 ggsave(path = "F:/Vicente",filename = "graphs_final/freq2.tiff",  units="in", width=6, height=3, dpi=500)
 
}
