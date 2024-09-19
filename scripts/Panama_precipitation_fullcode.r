#Install packages#
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("raster")
install.packages("readxl")
install.packages("writexl")
install.packages("reshape2")
install.packages("kableExtra")

#Import packages#
library(tidyverse)
library(ggplot2)
library(raster)
library(readxl)
library(writexl)
library(reshape2)
library(kableExtra)

#Auxiliary objects#
WORKINGDIRECTORY<-getwd()
extentMap  <- raster::extent(c(-80.2,-79.4, 8.8, 9.5))

#original information comes from a figshare link
METSTATIONS<-"https://smithsonian.figshare.com/ndownloader/files/24995609"
#download.file(METSTATIONS,destfile="metstations.xlsx",mode="wb")


#Elevation is retrived from earthdata.nasa.gov. #The product is ASTER Global Digital Elevation Model V003
#https://search.earthdata.nasa.gov/search?q=ASTER%20Global%20Digital%20Elevation%20Model%20V003
#six granules are downloaded. ASTGTMV003_N08W080, ASTGTM003_N082081, ASTGTM003_N09E080, ASTGTM003_N09W081,ASTGTM003_N09W079, ASTGTM003_N08W079
#You will acquire 12 .tif images

elevation_granules<- list.files("elevation", pattern = "dem.tif",full.names = TRUE)
rasters <- lapply(elevation_granules, raster)
panama_elevation <- do.call(merge, rasters)
plot(panama_elevation)

#Cleaned Data from the original data"
ACP_MET<-  "data_ground/met_data/ACP_data/cleanedHM/met_location_cleaned.xlsx"
STRI_MET<- "data_ground/met_data/STRI_data/stri_met.xlsx"
ACP_DATA<- "data_ground/met_data/ACP_data/cleanedHM/ACPrainDataVert_2022-08-11.xlsx"
STRI_DATA<-"data_ground/met_data/STRI_data/monthly_prec_stri.csv"

striStation<- read_excel(STRI_MET)
acpStation<- read_excel(ACP_MET)

acpDATA<- read_excel(ACP_DATA)
striDATA<- read.csv(STRI_DATA)

#Ground monthly precip
setDT(acpDATA) #neccesary 
monthlyPrecipData <- melt(acpDATA, 
                          id.vars = grep("Year|Mon", colnames(acpDATA), value = TRUE), 
                          variable.name = "site", value.name = "monthlyPrecip")
monthlyPrecipData$monthlyPrecip <- as.numeric(monthlyPrecipData$monthlyPrecip)
monthlyPrecipData<- rbind(monthlyPrecipData, striDATA)

#number of stations per year
nstations<- monthlyPrecipData %>% group_by(Year)%>%filter(!is.na(monthlyPrecip))%>%summarise(count= n_distinct(site))
monthlyPrecipData<-  merge(monthlyPrecipData, nstations, by.x="Year", 
                            by.y="Year", all=TRUE)%>% rename(nstations= count)

#annual precipitation and Jan-April precipitation for years with all months
annualPrecipData <- monthlyPrecipData %>%
    group_by(site, Year) %>%
    filter(n() == 12) %>%
    summarise(annualPrecip = sum(monthlyPrecip),
                        jfmaPrecip = sum(monthlyPrecip[Month < 5]))
                        
#keep only years with response variables 
annualPrecipData <- annualPrecipData[complete.cases(annualPrecipData), ]
#number of years per site between 1970 and 2016
nyears= annualPrecipData %>% filter(Year>=1970 & Year<=2016)%>% group_by(site) %>% tally() %>% rename(nyears= n)
annualPrecipData<- merge(annualPrecipData, nyears, by.x= "site", by.y="site")%>% filter(nyears>=32)

#table with all the sites used in the study, try adding the instrument type
sites_used<-data.frame(unique(annualPrecipData$site))
striStation<- striStation[,c("site","lat_dd","lon_dd")]%>% rename(long_dd= lon_dd,stri_name_fixed= site)
station_info<- bind_rows(striStation,acpStation)
sites_used<- merge(sites_used,station_info, by.x= "unique.annualPrecipData.site.", by.y="stri_name_fixed", all.x=TRUE)

sites_used<-sites_used[,c("unique.annualPrecipData.site.","acp_name","ele_m","lat_dd","long_dd")]
sites_used$ele_m<- ifelse(is.na(sites_used$ele_m),raster::extract(panama_elevation, sites_used[,c("long_dd","lat_dd")]),sites_used$ele_m)

kbl(sites_used)


