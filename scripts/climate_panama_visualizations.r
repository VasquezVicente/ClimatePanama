library(ggplot2)
library(raster)
library(rasterVis)
library(dplyr)
library(basemaps)
library(sf)
library(rasterVis)
library(gridExtra)
library(scico)
library(readr)


setwd("C:/Users/VasquezV/repo/ClimatePanama/ClimatePanamaBook")

ACP_MET<-  "../data_ground/met_data/ACP_data/cleanedVV/ACP_met_stations.csv"
STRI_MET<- "../tables/stri_met_stations.csv"

striStation<- read.csv(STRI_MET)
acpStation<- read.csv(ACP_MET)

extentMap  <- raster::extent(c(-80.2,-79.4, 8.8, 9.5))

#FIGURE 1. SITES
#stations wrangling and cleaning
striStation <- striStation %>%
  dplyr::mutate(
    longitude = as.character(longitude),    
    longitude = readr::parse_number(longitude) 
  )
striStation$source<- "STRI"
acpStation$source<-"ACP"
precipitation_data_subset<-read.csv(file="../tables/climatologies.csv")
#sites combined

site<-data.frame(unique(precipitation_data_subset$site))%>%
rename(site=unique.precipitation_data_subset.site.)%>%
left_join(striStation[,c('alias','latitude','longitude','source')], by = c("site" = "alias"))%>%
left_join(acpStation[,c('site','latitude','longitude','source')], by = c("site" = "site"))%>%
mutate(latitude=ifelse(is.na(latitude.x),latitude.y,latitude.x))%>%
mutate(longitude=ifelse(is.na(longitude.x),longitude.y,longitude.x))%>%
mutate(source=ifelse(is.na(source.x),source.y,source.x))%>%
dplyr::select(site,latitude,longitude,source)%>%filter(!is.na(latitude))
site$longitude<- abs(site$longitude)*-1
site_sf <- st_as_sf(site, coords = c("longitude","latitude"), crs = 4326)
site_sf_3857 <- st_transform(site_sf, 3857)
site_coords <- st_coordinates(site_sf_3857)
site_df <- cbind(site, site_coords)

# Plot using the color blind safe terrain color palette
set_defaults(map_service = "esri", map_type = "world_shaded_relief")
bbox <- st_bbox(c(xmin = -80.2, xmax = -79.4, ymin = 8.7, ymax = 9.6), crs = st_crs(4326))

x_labels_4326 <- c(-80.13, -79.95, -79.77, -79.59, -79.41)
y_labels_4326 <- c(8.77, 8.95, 9.12, 9.30, 9.48)

# Add scales with correct labels
basemap_ggplot(bbox)+
  scale_x_continuous(breaks = c(-8920000, -8900000, -8880000, -8860000, -8840000), labels = x_labels_4326) +
  scale_y_continuous(breaks = c(980000, 1000000, 1020000, 1040000, 1060000), labels = y_labels_4326)+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
  

p<-basemap_ggplot(bbox) +geom_point(data = site_df, aes(x = X, y = Y, color = source), size = 2) +
  scale_x_continuous(breaks = c(-8920000, -8900000, -8880000, -8860000, -8840000), labels = x_labels_4326) +
  scale_y_continuous(breaks = c(980000, 1000000, 1020000, 1040000, 1060000), labels = y_labels_4326) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

site_df2<- subset(site_df, !site %in% c("BCIELECT", "BCICLEAR", "BALBOAHTS", "DIABLO","ESCANDALOSA","PELUCA"))
BCIELECT<-subset(site_df, site %in% c("BCIELECT"))
BCICLEAR<-subset(site_df, site %in% c("BCICLEAR"))
BALBOAHTS<-subset(site_df, site %in% c("BALBOAHTS"))
DIABLO<-subset(site_df, site %in% c("DIABLO"))
ESCANDALOSA<-subset(site_df, site %in% c("ESCANDALOSA"))
PELUCA<-subset(site_df, site %in% c("PELUCA"))

new<-p + 
  geom_text(data = site_df2, aes(x = X, y = Y, label = site), color = "black", size = 2, nudge_y = 1000, fontface = "bold") +
  geom_text(data = BCIELECT, aes(x = X, y = Y, label = site), color = "black", size = 2, nudge_x = -3900, fontface = "bold") +
  geom_text(data = BCICLEAR, aes(x = X, y = Y, label = site), color = "black", size = 2, nudge_x = 4250, fontface = "bold") +
  geom_text(data = BALBOAHTS, aes(x = X, y = Y, label = site), color = "black", size = 2, nudge_y = -1000, fontface = "bold") +
  geom_text(data = DIABLO, aes(x = X, y = Y, label = site), color = "black", size = 2, nudge_x = -3000, fontface = "bold") +
  geom_text(data = ESCANDALOSA, aes(x = X, y = Y, label = site), color = "black", size = 2, nudge_x = -3000, nudge_y = 1000, fontface = "bold") +
  geom_text(data = PELUCA, aes(x = X, y = Y, label = site), color = "black", size = 2, nudge_x = -3000, fontface = "bold") +
  labs(color = "Source")

ggsave("sites.tiff", path = "../plots", plot = new, dpi = 900, units = "in")



# Figure 2 Total annual precipitation, excluding the lowest correlation Chelsa w5e5
annual_pan<-function(list,extent){
  a<-lapply(list, raster)
  b<-raster::stack(a)
  c<-raster::calc(b,sum)
  d<-raster::crop(c,extent)
  return(d)
}

#map panels with same extent and temporal extent
#chelsa 1.2 annual precip 0,0083333 degree orig 
chelsa_orig<-list.files("../data_reanalysis/CHELSA 1.2/",full.names=TRUE,pattern = ".tif")
chelsa_orig<-annual_pan(chelsa_orig,extentMap)
plot(chelsa_orig)

#CHIRPS annual precip at 0.05 
chirps<-list.files("../data_reanalysis/CHIRPS 2.0/climatology",full.name=TRUE,pattern=".tif")
chirps<-annual_pan(chirps,extentMap)
chirps<-projectRaster(chirps,chelsa_orig, method='ngb')
values(chirps)<-ifelse(values(chirps)<0,NA,values(chirps))
plot(chirps)

#chelsa 2.1 annual precip 0,0083333 degree orig 
chelsa2_orig<-list.files("../data_reanalysis/CHELSA 2.1",full.names=TRUE,pattern = ".tif")
chelsa2_orig<-annual_pan(chelsa2_orig,extentMap)
plot(chelsa2_orig)

#chp 0.05 degree original 1980-2009 
chp<- list.files("../data_reanalysis/CHPclim",full.names=TRUE,pattern = ".tif")
chp<-annual_pan(chp,extentMap)
chp<-projectRaster(chp,chelsa_orig, method='ngb')
plot(chp)


#pbcor 0.05 degree origina
pbcor_chp<- list.files("../data_reanalysis/PBCOR/CHPclim corrected",full.names=TRUE,pattern = ".tif")
pbcor_chp<-annual_pan(pbcor_chp,extentMap)
pbcor_chp<-projectRaster(pbcor_chp,chelsa_orig, method='ngb')
plot(pbcor_chp)


#pbcor 0.05 degree original
pbcor_chelsa<- list.files("../data_reanalysis/PBCOR/CHELSA 1.2 corrected",full.names=TRUE)
pbcor_chelsa<-annual_pan(pbcor_chelsa,extentMap)
pbcor_chelsa<-projectRaster(pbcor_chelsa,chelsa_orig,method='ngb')
plot(pbcor_chelsa)

#terra
terra<- list.files("../data_reanalysis/TerraClimate",full.names = TRUE,pattern = ".tif")
terra<-annual_pan(terra,extentMap)
terra<-projectRaster(terra,chelsa_orig,method = 'ngb')
plot(terra)


#Chelsa earthenv 2003-2016
chelsaearthenv<-list.files("../data_reanalysis/CHELSA EarthEnv/climatology",full.names = TRUE,pattern = ".tif")
chelsaearthenv<-annual_pan(chelsaearthenv,extentMap)
plot(chelsaearthenv)

#Pbcorworldclim
pbcor_worldclim<-list.files("../data_reanalysis/PBCOR/WorldClim corrected",full.names = TRUE,pattern = ".tif")
pbcor_worldclim<-annual_pan(pbcor_worldclim,extentMap)
pbcor_worldclim<-projectRaster(pbcor_worldclim,chelsa_orig,method="ngb")
plot(pbcor_worldclim)

#worldclim 
worldclim<-list.files("../data_reanalysis/WorldClim 2.1",full.names=TRUE,pattern=".tif")
worldclim<-annual_pan(worldclim,extentMap)
plot(worldclim)

res_stack<-raster::stack(chelsa_orig,chelsa2_orig,chelsaearthenv,pbcor_chelsa,pbcor_chp,pbcor_worldclim,terra,
                         chp,worldclim,chirps)

models<-list('layer.1'="CHELSA 1.2",
             'layer.2'="CHELSA 2.1",
             'layer.3'="CHELSA EarthEnv",
             'layer.4'="PBCOR CHELSA 1.2",
             'layer.5'="PBCOR CHPclim",
             'layer.6'="PBCOR WorldClim",
             'layer.7'="TerraClimate",
             'layer.8'="CHPclim v.1.0",
             'layer.9'="WorldClim v2.1",
             'layer.10'="CHIRPS v2")
model2<- function(variable,value){
  return(models[value])
}


jf1<-gplot(res_stack) + 
  geom_raster(aes(fill = value)) +
  facet_wrap(~ variable,labeller=model2,ncol = 5) +
  scale_fill_scico(palette = 'roma',na.value="white",name = "mm/year")+
  coord_equal()+theme_void()+theme(strip.text.x = element_text(size = 10, colour = "black"))+
  labs(title = "Total Annual Precipitation (mm)")+ 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

ggsave(path = '../plots',filename = "annual_maps.tiff",  units="in", width=10, height=6, dpi=900)

## JFMA precipitation

chelsa_orig<-list.files("../data_reanalysis/CHELSA 1.2/",full.names=TRUE,pattern = ".tif")
chelsa_orig<-chelsa_orig[1:4]
chelsa_orig<-annual_pan(chelsa_orig,extentMap)
plot(chelsa_orig)

#CHIRPS annual precip at 0.05 
chirps<-list.files("../data_reanalysis/CHIRPS 2.0/climatology",full.name=TRUE,pattern=".tif")
chirps<-chirps[1:4]
chirps<-annual_pan(chirps,extentMap)
chirps<-projectRaster(chirps,chelsa_orig, method='ngb')
values(chirps)<-ifelse(values(chirps)<0,NA,values(chirps))
plot(chirps)

#chelsa 2.1 annual precip 0,0083333 degree orig 
chelsa2_orig<-list.files("../data_reanalysis/CHELSA 2.1",full.names=TRUE,pattern = ".tif")
chelsa2_orig<- chelsa2_orig[1:4]
chelsa2_orig<-annual_pan(chelsa2_orig,extentMap)
plot(chelsa2_orig)

#chp 0.05 degree original 1980-2009 
chp<- list.files("../data_reanalysis/CHPclim",full.names=TRUE,pattern = ".tif")
chp<- chp[1:4]
chp<-annual_pan(chp,extentMap)
chp<-projectRaster(chp,chelsa_orig, method='ngb')
plot(chp)


#pbcor 0.05 degree origina
pbcor_chp<- list.files("../data_reanalysis/PBCOR/CHPclim corrected",full.names=TRUE,pattern = ".tif")
pbcor_chp<-pbcor_chp[1:4]
pbcor_chp<-annual_pan(pbcor_chp,extentMap)
pbcor_chp<-projectRaster(pbcor_chp,chelsa_orig, method='ngb')
plot(pbcor_chp)


#pbcor 0.05 degree original
pbcor_chelsa<- list.files("../data_reanalysis/PBCOR/CHELSA 1.2 corrected",full.names=TRUE)
pbcor_chelsa<-pbcor_chelsa[1:4]
pbcor_chelsa<-annual_pan(pbcor_chelsa,extentMap)
pbcor_chelsa<-projectRaster(pbcor_chelsa,chelsa_orig,method='ngb')
plot(pbcor_chelsa)

#terra
terra<- list.files("../data_reanalysis/TerraClimate",full.names = TRUE,pattern = ".tif")
terra<-terra[1:4]
terra<-annual_pan(terra,extentMap)
terra<-projectRaster(terra,chelsa_orig,method = 'ngb')
plot(terra)


#Chelsa earthenv 2003-2016
chelsaearthenv<-list.files("../data_reanalysis/CHELSA EarthEnv/climatology",full.names = TRUE,pattern = ".tif")
chelsaearthenv<-chelsaearthenv[1:4]
chelsaearthenv<-annual_pan(chelsaearthenv,extentMap)
plot(chelsaearthenv)

#Pbcorworldclim
pbcor_worldclim<-list.files("../data_reanalysis/PBCOR/WorldClim corrected",full.names = TRUE,pattern = ".tif")
pbcor_worldclim<-pbcor_worldclim[1:4]
pbcor_worldclim<-annual_pan(pbcor_worldclim,extentMap)
pbcor_worldclim<-projectRaster(pbcor_worldclim,chelsa_orig,method="ngb")
plot(pbcor_worldclim)

#worldclim 
worldclim<-list.files("../data_reanalysis/WorldClim 2.1",full.names=TRUE,pattern=".tif")
worldclim<-worldclim[1:4]
worldclim<-annual_pan(worldclim,extentMap)
plot(worldclim)

res_stack_jfma<-raster::stack(chelsa_orig,chelsa2_orig,chelsaearthenv,pbcor_chelsa,pbcor_chp,pbcor_worldclim,terra,chp,worldclim,chirps)

models<-list('layer.1'="CHELSA 1.2",
             'layer.2'="CHELSA 2.1",
             'layer.3'="CHELSA EarthEnv",
             'layer.4'="PBCOR CHELSA 1.2",
             'layer.5'="PBCOR CHPclim",
             'layer.6'="PBCOR WorldClim",
             'layer.7'="TerraClimate",
             'layer.8'="CHPclim v.1.0",
             'layer.9'="WorldClim v2.1",
             'layer.10'="CHIRPS v2")
model2<- function(variable,value){
  return(models[value])
}

jf2<-gplot(res_stack_jfma) + 
  geom_raster(aes(fill = value)) +
  facet_wrap(~ variable,labeller=model2,ncol = 5) +
  scale_fill_scico(palette = 'roma',na.value="white",name = "mm")+
  coord_equal()+theme_void()+theme(strip.text.x = element_text(size = 10, colour = "black"))+
  labs(title = "January to April Precipitation (mm)")+ 
  theme(plot.title = element_text(hjust = 0.5,size = 16))

ggsave(path = '../plots',filename = "jfma_maps.tiff",  units="in", width=10, height=6, dpi=900)

b<-grid.arrange(jf1,jf2,ncol = 1)
ggsave(b,path='../plots' ,filename = "../plots/climatologies.tiff",  units="in", width=10, height=7, dpi=900)


#figure 3. bias maps
# List of data sources

precipitation_data_subset <- read.csv(file="../tables/climatologies.csv")
data_sources <- c("CHELSA.1.2", "CHELSA.2.1", "CHELSA.EarthEnv", "CHPclim", "WorldClim.2.1", "CHIRPS.2.0", "CHELSA.W5E5v1.0", "TerraClimate", "PBCOR.CHELSA.1.2", "PBCOR.CHPclim", "PBCOR.WorldClim.2.1")
source_names <- c("CHELSA 1.2", "CHELSA 2.1", "CHELSA EarthEnv", "CHPclim", "WorldClim 2.1", "CHIRPS 2.0", "CHELSA-W5E5v1.0", "TerraClimate", "PBCOR CHELSA 1.2", "PBCOR CHPclim", "PBCOR WorldClim 2.1")

bias_frame <- precipitation_data_subset %>% 
  select("site","longitude","latitude","annual","source") %>% 
  mutate(bias=NA)
bias_frame_jfma<- precipitation_data_subset %>% 
  select("site","longitude","latitude","jfma","source") %>% 
  mutate(bias=NA)

for(i in seq_along(data_sources)) {
  bias_frame[bias_frame$source == source_names[i], "bias"] <- precipitation_data_subset[precipitation_data_subset$source == source_names[i], data_sources[i]] - precipitation_data_subset[precipitation_data_subset$source == source_names[i], "annual"]
}
jfma_sources <- paste0(data_sources, "_jfma")
for(i in seq_along(jfma_sources)) {
  bias_frame_jfma[bias_frame_jfma$source == source_names[i], "bias"] <- precipitation_data_subset[precipitation_data_subset$source == source_names[i], jfma_sources[i]] - precipitation_data_subset[precipitation_data_subset$source == source_names[i], "jfma"]
}

#transform coordinate
bias_frame <- bias_frame %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(3857) %>%
  cbind(st_coordinates(.)) %>%
  rename(longitude_3857 = X, latitude_3857 = Y)%>%
  filter(source!="CHELSA-W5E5v1.0")

bias_frame_jfma <- bias_frame_jfma %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(3857) %>%
  cbind(st_coordinates(.)) %>%
  rename(longitude_3857 = X, latitude_3857 = Y)%>%
  filter(source!="CHELSA-W5E5v1.0")

Bias_annual_panels <- basemap_ggplot(bbox) +
  theme_void() +
  geom_jitter(data = bias_frame,
              aes(x = longitude_3857, y = latitude_3857, colour = bias),
              size = 3, width = 0.3, height = 0.3) +
  geom_jitter(data = bias_frame,
              aes(x = longitude_3857, y = latitude_3857),
              size = 3, shape = 21, width = 0.3, height = 0.3) +
  scale_color_scico(palette = "roma", direction = -1) +
  facet_wrap(~source,ncol=5) +
  labs(color = "Bias (mm)") +
  ggtitle("Annual Precipitation Bias")

Bias_jfma_panels <- basemap_ggplot(bbox) +
  theme_void() +
  geom_jitter(data = bias_frame_jfma,
              aes(x = longitude_3857, y = latitude_3857, colour = bias),
              size = 3, width = 0.3, height = 0.3) +
  geom_jitter(data = bias_frame_jfma,
              aes(x = longitude_3857, y = latitude_3857),
              size = 3, shape = 21, width = 0.3, height = 0.3) +
  scale_color_scico(palette = "roma", direction = -1) +
  facet_wrap(~source,ncol = 5) +
  labs(color = "Bias (mm)") +
  ggtitle("January to April Precipitation Bias")

Bias_annual_panels <- Bias_annual_panels +
  theme(strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,size=14),
        plot.background = element_rect(fill = "white"))

Bias_jfma_panels <- Bias_jfma_panels +
  theme(strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,size=14),
        plot.background = element_rect(fill = "white"))

ggsave(Bias_annual_panels,path = '../plots',filename = "annual_jfma_maps.tiff",  units="in", width=10, height=10, dpi=900)
ggsave(Bias_jfma_panels,path = '../plots',filename = "bias_jfma_maps.tiff",  units="in", width=10, height=10, dpi=900)

c<-grid.arrange(Bias_annual_panels,Bias_jfma_panels,ncol = 1)
ggsave(c,path='../plots' ,filename = "../plots/bias_figure.tiff",  units="in", width=20, height=10, dpi=600)
ggsave(c,path='../plots' ,filename = "../plots/bias_figure.jpg",  units="in", width=20, height=10, dpi=300)


#figure 4. correlation plots
precipitation_data_subset <- read.csv(file="../tables/climatologies.csv")
data_sources <- c("CHELSA.1.2", "CHELSA.2.1", "CHELSA.EarthEnv", "CHPclim", "WorldClim.2.1", "CHIRPS.2.0", "CHELSA.W5E5v1.0", "TerraClimate", "PBCOR.CHELSA.1.2", "PBCOR.CHPclim", "PBCOR.WorldClim.2.1")
source_names <- c("CHELSA 1.2", "CHELSA 2.1", "CHELSA EarthEnv", "CHPclim", "WorldClim 2.1", "CHIRPS 2.0", "CHELSA-W5E5v1.0", "TerraClimate", "PBCOR CHELSA 1.2", "PBCOR CHPclim", "PBCOR WorldClim 2.1")

corr_frame <- precipitation_data_subset %>% 
  select("site","longitude","latitude","annual","source") %>% 
  mutate(value=NA)
corr_frame_jfma<- precipitation_data_subset %>% 
  select("site","longitude","latitude","jfma","source") %>% 
  mutate(value=NA)

for(i in seq_along(data_sources)) {
  corr_frame[corr_frame$source == source_names[i], "value"] <- precipitation_data_subset[precipitation_data_subset$source == source_names[i], data_sources[i]]
}
jfma_sources <- paste0(data_sources, "_jfma")
for(i in seq_along(jfma_sources)) {
  corr_frame_jfma[corr_frame_jfma$source == source_names[i], "value"] <- precipitation_data_subset[precipitation_data_subset$source == source_names[i], jfma_sources[i]] 
}



annual_corr<-ggplot(corr_frame[corr_frame$source!="CHELSA-W5E5v1.0",],aes(x = value,y=annual))+
  geom_smooth(method='lm',se=FALSE)+
  geom_point(col=rgb(0,0,0,alpha=0.5))+facet_wrap(vars(source),ncol=5)+
  theme_classic()+
  expand_limits(x=c(1000,4500),y=c(1000,4500))+
  geom_abline(slope = 1, intercept = 0, lty= 2)+
  coord_equal()

dat_text <- data.frame(
  source  = unique(corr_frame[corr_frame$source!="CHELSA-W5E5v1.0","source"])
)

for(i in 1:10){
  model <- lm(value ~ annual, data = corr_frame[corr_frame$source == dat_text$source[i],])
  dat_text$eq[i] <- paste0("y = ", round(coef(model)[1], 2), " + ", round(coef(model)[2], 2), "x")
}
annual_corr<-annual_corr + geom_text(
  data    = dat_text,
  mapping = aes(x = 1800, y = 1300, label = eq),
  hjust   = 0,
  vjust   = 1,
  size    = 3
)+labs(title = "Total Annual Correlation Plots")+ 
  xlab("Gridded products annual precipitation (mm)") +
  ylab("In-situ annual precipitation (mm)") 

ggsave(annual_corr,path='../plots' ,filename = "../plots/correlation_annual.tiff",  units="in", width=8, height=7, dpi=900)



jfma_corr<-ggplot(corr_frame_jfma[corr_frame_jfma$source!="CHELSA-W5E5v1.0",],aes(x = value,y=jfma))+
  geom_smooth(method='lm',se=FALSE)+
  geom_point(col=rgb(0,0,0,alpha=0.5))+facet_wrap(vars(source),ncol=5)+
  expand_limits(x=c(0,800),y=c(0,800))+
  theme_classic()+
  geom_abline(slope = 1, intercept = 0, lty= 2)+
  coord_equal()

dat_text <- data.frame(
  source  = unique(corr_frame_jfma[corr_frame_jfma$source!="CHELSA-W5E5v1.0","source"])
)

for(i in 1:10){
  model <- lm(value ~ jfma, data = corr_frame_jfma[corr_frame_jfma$source == dat_text$source[i],])
  dat_text$eq[i] <- paste0("y = ", round(coef(model)[1], 2), " + ", round(coef(model)[2], 2), "x")
}
jfma_corr<-jfma_corr + geom_text(
  data    = dat_text,
  mapping = aes(x = 400, y = 50, label = eq),
  hjust   = 0,
  vjust   = 1,
  size    = 3
)+labs(title = "January-April Correlation Plots")+ 
  xlab("Gridded products Jan-Apr precipitation (mm)") +
  ylab("In-situ Jan-Apr precipitation (mm)") 

ggsave(jfma_corr,path='../plots' ,filename = "../plots/correlation_jfma.tiff",  units="in", width=8, height=7, dpi=900)

d<-grid.arrange(annual_corr,jfma_corr,ncol = 1)
ggsave(d,path='../plots' ,filename = "../plots/correlation.tiff",  units="in", width=10, height=10, dpi=600)
ggsave(d,path='../plots' ,filename = "../plots/correlation.jpg",  units="in", width=10, height=10, dpi=300)

#figure 5 seasonality plots
acp38_ag<-read.csv("../tables/acp38_ag.csv")
acp38_ag_long <- acp38_ag %>% 
  pivot_longer(cols = c(CHELSA1.2:PBCOR_WorldClim2.1), names_to = "colname", values_to = "value")%>%
  mutate(colname = case_when(
    colname == "CHELSA1.2" ~ "CHELSA 1.2",
    colname == "CHELSA2.1" ~ "CHELSA 2.1",
    colname == "CHELSA_EarthEnv" ~ "CHELSA EarthEnv",
    colname == "CHPclim" ~ "CHPclim",
    colname == "WorldClim2.1" ~ "WorldClim 2.1",
    colname == "CHIRPS2.0" ~ "CHIRPS 2.0",
    colname == "CHELSAW5E5v1.0" ~ "CHELSA-W5E5v1.0",
    colname == "TerraClimate" ~ "TerraClimate",
    colname == "PBCOR_CHELSA1.2" ~ "PBCOR CHELSA 1.2",
    colname == "PBCOR_CHPclim" ~ "PBCOR CHPclim",
    colname == "PBCOR_WorldClim2.1" ~ "PBCOR WorldClim 2.1",
  ))%>%
  filter((dataset_name == colname) | (dataset_name == "Ground" & colname == "CHELSA 1.2"))%>%
  mutate(value = ifelse(dataset_name == "Ground", mon_mean, value)) %>%
  mutate(dataset_name = ifelse(site == "BCICLEAR" & dataset_name == "Ground", "Ground manual", dataset_name))%>%
  filter(!(site == "BCICLEAR" & dataset_name != "Ground manual"))%>%
  mutate(site=ifelse(site=="BCICLEAR","BCI",site))

acp38_ag_long$dataset_name <- factor(acp38_ag_long$dataset_name, levels = c("CHELSA 1.2","CHELSA 2.1","CHELSA EarthEnv", "CHPclim",            
                                                                            "WorldClim 2.1","CHIRPS 2.0","CHELSA-W5E5v1.0","TerraClimate",     
                                                                            "PBCOR CHELSA 1.2","PBCOR CHPclim","PBCOR WorldClim 2.1","Ground",           
                                                                             "Ground manual"))

acp38_ag_long$site <- factor(acp38_ag_long$site,    # Reordering group factor levels
                               levels = c("SANMIGUEL", "AGUACLARA", "PELUCA", "BCI","GUACHA","CASCADAS","GAMBOA","PEDROMIGUEL"))
seasonality_plot<-ggplot(acp38_ag_long,aes(x=Month,y=value))+
  geom_line(aes(color=dataset_name),linewidth=0.2)+
  geom_point(aes(color=dataset_name),size=0.2)+
  facet_wrap(vars(site),ncol = 4)+
  scale_color_manual(labels = c("CHELSA 1.2","CHELSA 2.1","CHELSA EarthEnv", "CHPclim",            
                                "WorldClim 2.1","CHIRPS 2.0","CHELSA-W5E5v1.0","TerraClimate",     
                                "PBCOR CHELSA 1.2","PBCOR CHPclim","PBCOR WorldClim 2.1","Ground",           
                                "Ground manual"),values=c('#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#a6cee3','brown',"black","purple")) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+theme_classic()+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1,size=6),strip.text = element_text(size = 5),)+
  ylab("Precipitation (mm)")+
  labs(color = "Gridded dataset")+
  labs(title="Seasonal Precipitation Patterns")

ggsave(seasonality_plot,path='../plots' ,filename = "../plots/seasonality.tiff",  units="in", width=9, height=6, dpi=600)

#figure 6, interannual variability
acp38_year<-read.csv("../tables/acp38_year.csv")

acp38_year_long<- acp38_year%>%
  pivot_longer(cols=c(annual_precip,CHELSA1.2:TerraClimate), names_to = "colname", values_to = "value")%>%
  mutate(colname = case_when(
    colname == "CHELSA1.2" ~ "CHELSA 1.2",
    colname == "CHELSA2.1" ~ "CHELSA 2.1",
    colname == "CHELSA_EarthEnv" ~ "CHELSA EarthEnv",
    colname == "CHIRPS2.0" ~ "CHIRPS 2.0",
    colname == "CHELSAW5E5v1.0" ~ "CHELSA-W5E5v1.0",
    colname == "TerraClimate" ~ "TerraClimate",
    colname == "annual_precip" ~ "Ground"
  ))%>%
  filter((dataset_name == colname) | (dataset_name == "CHELSA 1.2" & colname == "Ground"))%>%
  mutate(dataset_name= ifelse(dataset_name == "CHELSA 1.2" & colname == "Ground"& site=="BCICLEAR","Ground manual",dataset_name))%>%
  filter(!(site == "BCICLEAR" & dataset_name != "Ground manual"))%>%
  mutate(site=ifelse(site=="BCICLEAR","BCI",site))%>%
  mutate(dataset_name=ifelse(dataset_name=="CHELSA 1.2"&colname=="Ground","Ground",dataset_name))

acp38_year_long$site <- factor(acp38_year_long$site,      # Reordering group factor levels
                    levels = c("SANMIGUEL", "AGUACLARA", "PELUCA", "BCI","GUACHA","CASCADAS","GAMBOA","PEDROMIGUEL"))
acp38_year_long$dataset_name<-factor(acp38_year_long$dataset_name,
                                     levels=c("CHELSA 1.2","CHELSA 2.1", "CHELSA EarthEnv","CHIRPS 2.0","CHLESA-W5E5v1.0","TerraClimate","Ground","Ground manual"))
acp38_year_long <- acp38_year_long %>%
  filter(!is.na(dataset_name))  

interannual_plot<-ggplot(acp38_year_long,aes(x=Year,y=value))+
  geom_line(aes(color=dataset_name),linewidth=0.2)+
  geom_point(aes(color=dataset_name),size=0.2)+
  expand_limits(x=c(1970,2016),y=c(900,5900))+
  facet_wrap(vars(site),ncol=4)+
  theme_classic()+
  ylab("Annual Total precipitation")+
  labs(title="Interannual Variability",size=20)+
  theme(strip.text = element_text(size = 5))+
  labs(color="Gridded dataset")+
  scale_color_manual(values=c('#a65628','#e7298a', '#ff7f00','#984ea3','#4daf4a','black','purple'))

year<- acp38_year_long %>% filter(dataset_name=="Ground")%>%
  group_by(site) %>% summarise(annual_mean=mean(value,na.rm=TRUE))
year2<-acp38_year_long %>% filter(dataset_name=="Ground manual")%>%
  group_by(site)%>% summarise(annual_mean=mean(value,na.rm=TRUE))

interannual_plot<-interannual_plot + geom_text(
  data    = year,
  mapping = aes(x = -Inf, y = 5350, label = paste0("Mean annual precipitation=",round(annual_mean,0))),
  hjust   = -0.1,
  vjust   = -1,
  size=2
)+
  geom_text(
    data    = year2,
    mapping = aes(x = 1992.5, y = 5350, label = paste0("(electronic), ",round(annual_mean,0),"(manual)")),
    hjust   = -0.1,
    vjust   = -1,
    size=2)

##annual
ggsave(interannual_plot,path='../plots' ,filename = "../plots/interannual.tiff",  units="in", width=12, height=6, dpi=600)