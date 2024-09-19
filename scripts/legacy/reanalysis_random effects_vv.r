# compare climate reanalysis products with observed rainfall data for ACP stations ####
# currently includes reanalysis products from CHELSA 1.2, CHELSA 2.1, and PBCOR CHELSA 1.2
remotes::install_github("SEEG-Oxford/seegSDM")
library(readxl)
library(sp)
library(raster)
library(MASS)
library(dplyr)
library(data.table)
library(ggplot2)
library(lme4)
library(tdr)
library(seegSDM)
library(kableExtra)


rm(list=ls())

METLOCATIONFN <- "data_ground/met_data/ACP_data/cleanedHM/ACP_met_location_fixed_2022-08-15.xlsx"
PRECIPDATAFN <- "data_ground/met_data/ACP_data/cleanedHM/ACPrainDataVert_2022-08-11.xlsx"
MAPDIR <- "output/maps/pan_prec_maps"
GRAPHDIR <- "output/graphs"

MINYEAR <- 1981 # only include ground data from 1980 on, to match CHELSA 2.1 dataset 



## open ACP met station data
metStation <- read_excel(METLOCATIONFN,sheet=1)
monthlyPrecipData1 <- read_excel(PRECIPDATAFN,sheet=1)

names(metStation) <- tolower(names(metStation))
metStation <- metStation[order(metStation$stri_name_fixed),]

strisites<- read.csv("data_ground/met_data/STRI_data/monthly_prec_stri.csv")
strisites$month<- substring(strisites$yearmonth, 6.7)
strisites$Month<- as.numeric(strisites$month)
strisites$month2<- ifelse(strisites$month==1,0.000,ifelse(strisites$month==2,0.083,ifelse(strisites$month==3,0.167,ifelse(strisites$month==4,0.250,ifelse(strisites$month==5,0.333,ifelse(strisites$month==6, 0.417, ifelse(strisites$month==7, 0.500, ifelse(strisites$month==8, 0.583,ifelse(strisites$month==9,0.667,ifelse(strisites$month==10, 0.750,ifelse(strisites$month==11,0.833, 0.917)))))))))))
strisites$Year<- as.numeric(strisites$year)
strisites$Yr.Mon<- strisites$year+strisites$month2
strisites<- rename(strisites,monthlyPrecip=ra)
strisites<- strisites[,c("Year","Month", "Yr.Mon","site", "monthlyPrecip")]
##i have reasons to believe 2006 and 2007 of sherman isnt good enough to keep, they were gap fills 
strisites$yearsite<- paste(strisites$Year, strisites$site)
strisites<-subset(strisites, yearsite!= "2006 SHERMAN"&yearsite!="2007 SHERMAN")
strisites<- strisites[,-c(6)]
# drop the old coordinates and other fields not needed 
metStation <- dplyr::select(metStation,-c(lat_n,long_w,lat_n_dms,long_w_dms,stri_name_orig))

metStation <- mutate(metStation,
                     ele_m=as.numeric(ele_m),
                     utm_n=as.numeric(utm_n),
                     utm_e=as.numeric(utm_e),
                     lat_dd=as.numeric(lat_dd),
                     long_dd=as.numeric(long_dd))

# drop rows lacking any coordinates
metStation <- subset(metStation,!is.na(metStation$utm_e))


# put into data.table format 
setDT(metStation)
setDT(monthlyPrecipData1)

# calculate mean annual precipitation at each site
monthlyPrecipData <- melt(monthlyPrecipData1, 
                          id.vars = grep("Year|Mon", colnames(monthlyPrecipData1), value = TRUE), 
                          variable.name = "site", value.name = "monthlyPrecip")

monthlyPrecipData$monthlyPrecip <- as.numeric(monthlyPrecipData$monthlyPrecip)
monthlyPrecipData<- rbind(monthlyPrecipData, strisites)

#number of stations per year
nstations<- monthlyPrecipData %>% group_by(Year)%>%filter(!is.na(monthlyPrecip))%>%summarise(count= n_distinct(site))

# only keep years that have data for all 12 months
monthlyPrecipData[, nmonths := sum(!is.na(monthlyPrecip)), .(site, Year)]

monthlyPrecipData<-  merge(monthlyPrecipData, nstations, by.x="Year", by.y="Year", all=TRUE)%>% rename(nstations= count)

monthly_ground<- monthlyPrecipData[,c(1,2,4,5)]
monthly_ground<- filter(monthly_ground, site!="CABACERA"& site!="BCICLEARM") 
write.csv(monthly_ground, "monthly_ground.csv")

## calculate annual and JFMA precipitation, per year (JFMA = January to April)
annualPrecipData <- monthlyPrecipData[nmonths == 12 & nstations>=12, .(annualPrecip = sum(monthlyPrecip), 
                                                                       jfmaPrecip = sum(monthlyPrecip[Month<5])), .(site, Year)]
nyears<- annualPrecipData %>% group_by(site) %>% tally() %>% rename(nyears= n)
nsites<- annualPrecipData %>% group_by(Year) %>% tally() %>% rename(nsites= n)
annualPrecipData<- merge(annualPrecipData, nyears, by.x= "site", by.y="site")
annualPrecipData<- merge(annualPrecipData, nsites, by.x= "Year", by.y="Year")
#annualPrecipData<- subset(annualPrecipData, nyears== 38)
ggplot(annualPrecipData, aes(y = annualPrecip, x = Year, group=site)) +
  xlab("Years") +
  ylab("annual precipitation") +
  theme_classic(base_size = 15)+ geom_smooth(method = "lm", se = FALSE)

ggplot(annualPrecipData, aes(x= Year, y=annualPrecip, group=site))+
                                                     geom_smooth() +
                                         facet_wrap(~site, nrow=12)+
                                          labs(title="Group One")

annualPrecipData<- filter(annualPrecipData, site!="CABACERA"& site!="BCICLEARM") 
#linear mixed model fro fitted site and random effect in year

model_annual<- lmer(annualPrecip~ site +(1|Year), data=annualPrecipData)
summary(model_annual)
plot(model_annual)
                annualPrecipData$fitted_annual<- fitted(model_annual)
                annualPrecipData$residual_annual<- residuals(model_annual)
                annualPrecipData$predicted_annual<- predict(model_annual)
                ranef<- ranef(model_annual, condVar=TRUE)$Year
                ranef$Year <- dimnames(ranef)[[1]]
                ranef<- rename(ranef, random_effect="(Intercept)")
                annualPrecipData$Year<- as.character(annualPrecipData$Year)
                annualPrecipData<- merge(annualPrecipData,ranef, by.x="Year", by.y= "Year")
                annualPrecipData$fixef_annual<- annualPrecipData$fitted_annual-annualPrecipData$random_effect
model_jfma<- lmer(jfmaPrecip~ site +(1|Year), data=annualPrecipData)
summary(model_jfma)
plot(model_jfma)
                annualPrecipData$fitted_jfma<- fitted(model_jfma)
                annualPrecipData$residual_jfma<- residuals(model_jfma)
                annualPrecipData$predicted_jfma<-  predict(model_jfma)
                ranef<- ranef(model_jfma, condVar=TRUE)$Year
                ranef$Year <- dimnames(ranef)[[1]]
                ranef<- rename(ranef, random_effect_jfma="(Intercept)")
                annualPrecipData$Year<- as.character(annualPrecipData$Year)
                annualPrecipData<- merge(annualPrecipData,ranef, by.x="Year", by.y= "Year")
                annualPrecipData$fixef_jfma<- annualPrecipData$fitted_jfma-annualPrecipData$random_effect_jfma


dfprecip1<- subset(annualPrecipData, Year>= MINYEAR)
dfprecip1<- dfprecip1[, .(annualPrecip_acp = mean(annualPrecip, na.rm = TRUE), 
                                  annualPrecip_adjusted = mean(predicted_annual, na.rm = TRUE),
                                  jfmaPrecip_acp = mean(jfmaPrecip, na.rm = TRUE),
                                  jfmaprecip_adjusted = mean(predicted_jfma, na.rm = TRUE), 
                                  nYears = sum(!is.na(annualPrecip))), .(site)]




##merge the dfprecip with the random effects of sit
# which stations don't match?  
print("The following met station names appear in the met data file but not the location file:")
print(sort(dfprecip1$site[is.na(match(dfprecip1$site,metStation$stri_name_fixed))]))
print("The following met station names appear in the location file but not the met data file:")
print(sort(metStation$stri_name_fixed[is.na(match(metStation$stri_name_fixed,dfprecip1$site))]))


# add coordinates 
metStation_stri<- read.csv("data_ground/met_data/STRI_data/monthly_prec_stri.csv")%>% select(c("site", "lon_dd", "lat_dd"))%>% rename(stri_name_fixed=site, long_dd=lon_dd)
metStation<- rbind(metStation,metStation_stri, fill=TRUE)
metStation<-subset(metStation,!duplicated(metStation$stri_name_fixed))

dfprecip2 <-merge(dfprecip1,metStation[,c("stri_name_fixed","ele_m","long_dd","lat_dd")],
                  all.x=T,by.x="site",by.y="stri_name_fixed")
dfprecip2<- dfprecip2 %>% select(-c("annualPrecip_acp", "jfmaPrecip_acp"))%>%
  rename(annualPrecip_acp= annualPrecip_adjusted, jfmaPrecip_acp=jfmaprecip_adjusted)
dfprecip2 <- dfprecip2[order(dfprecip2$annualPrecip_acp),]

# take a look at where stations are 
win.graph(width=15,height=8)
plot(dfprecip2$long_dd,dfprecip2$lat_dd,cex=base::scale(dfprecip2$annualPrecip_acp,center=F))
text(dfprecip2$long_dd,dfprecip2$lat_dd,labels=dfprecip2$site,cex=0.7)

# get CHELSA 1 and 2 precipitation data
prec1 <- raster::raster(paste0(MAPDIR,"/precann1-maps.tif"))
precja1 <- raster::raster(paste0(MAPDIR,"/precjfma1-maps.tif"))
prec2 <- raster::raster(paste0(MAPDIR,"/precann2-maps.tif"))
precja2 <- raster::raster(paste0(MAPDIR,"/precjfma2-maps.tif"))

# extract values at site coordinates
##CHELSA 1.2 (1979-2013)AND 2.1(1981-2010)
{
dfprecip2$annualPrecip_chelsa1 <- raster::extract(prec1, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_chelsa1 <- raster::extract(precja1, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$annualPrecip_chelsa2 <- raster::extract(prec2, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_chelsa2 <- raster::extract(precja2, dfprecip2[, c("long_dd", "lat_dd")])
}
## WORLD CLIM (1970-2000)
prec_worldclim_annual <- raster::raster(paste0(MAPDIR,"/prec_worldclim_annual.tif"))
prec_worldclim_jfma <- raster::raster(paste0(MAPDIR,"/prec_worldclim_jfma.tif"))

dfprecip2$annualPrecip_worldclim <- raster::extract(prec_worldclim_annual, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_worldclim <- raster::extract(prec_worldclim_jfma, dfprecip2[, c("long_dd", "lat_dd")])

##PBCOR

pbcor_chelsa1 <- raster::raster(paste0(MAPDIR,"/prec_pbcor_chelsa1_annual.tif"))
pbcor_chelsa1_jfma <- raster::raster(paste0(MAPDIR,"/prec_pbcor_chelsa1_jfma.tif"))

pbcor_chp <- raster::raster(paste0(MAPDIR,"/prec_pbcor_chp_annual.tif"))
pbcor_chp_jfma <- raster::raster(paste0(MAPDIR,"/prec_pbcor_chp_jfma.tif"))

pbcor_worldclim<- raster::raster(paste0(MAPDIR,"/prec_pbcor_worldclim_annual.tif"))
pbcor_worldclim_jfma<- raster::raster(paste0(MAPDIR,"/prec_pbcor_worldclim_jfma.tif"))

dfprecip2$annualPrecip_pbcorChelsa1 <- raster::extract(pbcor_chelsa1, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_pbcorChelsa1 <- raster::extract(pbcor_chelsa1_jfma, dfprecip2[, c("long_dd", "lat_dd")])

dfprecip2$annualPrecip_pbcorChp <- raster::extract(pbcor_chp, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_pbcorChp <- raster::extract(pbcor_chp_jfma, dfprecip2[, c("long_dd", "lat_dd")])

dfprecip2$annualPrecip_pbcorWorldclim <- raster::extract(pbcor_worldclim, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_pbcorWorldclim <- raster::extract(pbcor_worldclim_jfma, dfprecip2[, c("long_dd", "lat_dd")])
##terra
terra<- raster::raster(paste0(MAPDIR,"/prec_terra_annual.tif"))
terra_jfma<- raster::raster(paste0(MAPDIR,"/prec_terra_jfma.tif"))

dfprecip2$annualPrecip_terra <- raster::extract(terra, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_terra <- raster::extract(terra_jfma, dfprecip2[, c("long_dd", "lat_dd")])

terra2<- raster::raster(paste0(MAPDIR,"/prec_terra2_annual.tif"))
terra2_jfma<- raster::raster(paste0(MAPDIR,"/prec_terra2_jfma.tif"))

dfprecip2$annualPrecip_terra2 <- raster::extract(terra2, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_terra2 <- raster::extract(terra2_jfma, dfprecip2[, c("long_dd", "lat_dd")])

#chp
chp<- raster::raster(paste0(MAPDIR,"/chp_annual.tif"))
chp_jfma<- raster::raster(paste0(MAPDIR,"/chp_jfma.tif"))
dfprecip2$annual_chp <- raster::extract(chp, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_chp <- raster::extract(chp_jfma, dfprecip2[, c("long_dd", "lat_dd")])
## the adjusted becomes the new acp values
dfprecip2<- rename(dfprecip2, nYears_acp=nYears)

dfprecip <- melt(dfprecip2, id.vars = c("site","ele_m","long_dd", "lat_dd","source"))
dfprecip$source <- tstrsplit(dfprecip$variable, "_")[[2]]
dfprecip$variable <- tstrsplit(dfprecip$variable, "_")[[1]]

temp<- subset(dfprecip, is.na(dfprecip$value))
temp$near_pixel<- TRUE
temp2<- dfprecip%>% subset(!is.na(value))
temp2$near_pixel<- FALSE

for(i in 1:5){
  temp[i,6]<-extract(prec1,nearestLand(temp[i,c(3,4)],prec1,4000))
}
for(i in 6:10){
temp[i,6]<-extract(precja1,nearestLand(temp[i,c(3,4)],precja1,4000))
}
for(i in 11:15){
  temp[i,6]<-extract(prec_worldclim_annual,nearestLand(temp[i,c(3,4)],prec_worldclim_annual,4000))
}
for(i in 16:20){
  temp[i,6]<-extract(prec_worldclim_jfma,nearestLand(temp[i,c(3,4)],prec_worldclim_jfma,4000))
}
for(i in 21:22){
  temp[i,6]<-extract(terra,nearestLand(temp[i,c(3,4)],terra,4000))
}
for(i in 23:24){
  temp[i,6]<-extract(terra_jfma,nearestLand(temp[i,c(3,4)],terra_jfma,4000))
}
for(i in 25:26){
  temp[i,6]<-extract(terra2,nearestLand(temp[i,c(3,4)],terra2,4000))
}
for(i in 27:28){
  temp[i,6]<-extract(terra2_jfma,nearestLand(temp[i,c(3,4)],terra2_jfma,4000))
}
dfprecip<-bind_rows(temp, temp2)

panama_elevation<- raster::raster("data_reanalysis/panama_elevation.tif")

for (i in 1:length(dfprecip$ele_m)){
dfprecip$ele_m[i]<- ifelse(!is.na(dfprecip$ele_m[i]),dfprecip$ele_m[i],extract(panama_elevation, c(dfprecip$long_dd[i],dfprecip$lat_dd[i]))[2])
}


write.csv(dfprecip,"dfprecip.csv")
## melt dfprecip to long format
dfprecip <- melt(dfprecip2[, -c("ele_m","long_dd", "lat_dd")], id.vars = "site")
dfprecip$source <- tstrsplit(dfprecip$variable, "_")[[2]]
dfprecip$variable <- tstrsplit(dfprecip$variable, "_")[[1]]

# make  columns for each source of data and add coordinates 
dfprecip <- dcast(dfprecip, site + variable ~ source, value.var = "value")
dfprecip <-merge(dfprecip,metStation[,c("stri_name_fixed","ele_m","long_dd","lat_dd")],
                 all.x=T,by.x="site",by.y="stri_name_fixed")
dfprecip$devch1 <- dfprecip$chelsa1-dfprecip$acp
dfprecip$devch2 <- dfprecip$chelsa2-dfprecip$acp
dfprecip$devwoc <- dfprecip$worldclim-dfprecip$acp
dfprecip$devpche <- dfprecip$pbcorChelsa1-dfprecip$acp
dfprecip$devpchp <- dfprecip$pbcorChp-dfprecip$acp
dfprecip$devpwor <- dfprecip$pbcorWorldclim-dfprecip$acp

###error statistics tables
errors<- function(x, y,functions = c('MAE', 'MBE', 'RMSE', 'randError',
                                  'proprandError', 'propRMSE', 'propMBE',
                                  'propMAE')){
MAE<- function(x,y){
  a<- length(which(!is.na(x)))
  b<-sum(abs(x-y),na.rm = TRUE)
  mae<- mean(b/a)
  return(mae)
}
MBE<- function(x,y){
  a<- 1/(length(which(!is.na(x))))
  b<-sum((x-y), na.rm = TRUE)
  percent<-(a*b)
  return(percent)
}
RMSE<- function(x,y){
  a<- 1/(length(which(!is.na(x))))
  b<-abs(sum(((x-y)/y)^2, na.rm = TRUE))
  percent<-sqrt(a*b)
  return(percent)
}
randError<-function(x,y){
  a<-1/(length(which(!is.na(x)))-1)
  b<-sum((x-y- tdStats(x, y, functions ="mbe"))^2, na.rm = TRUE)
  randError<-sqrt(a*b)
  return(randError)
}
propRMSE<- function(x,y){
  a<- 1/(length(which(!is.na(x))))
  b<-abs(sum(((x-y)/y)^2, na.rm = TRUE))
  percent<-sqrt(a*b)
  return(percent)
}
propMBE<- function(x,y){
  a<- 1/(length(which(!is.na(x))))
  b<-abs(sum(((x-y)/y), na.rm = TRUE))
  percent<-(a*b)
  return(percent)
}
propMAE<- function(x,y){
  a<- length(which(!is.na(x)))
  b<-sum(abs((x-y)/y),na.rm = TRUE)
  mae<- mean(b/a)
  return(mae)
}
proprandError<-function(x,y){
  a<-1/(length(which(!is.na(x)))-1)
  b<-abs(sum((((x-y)/y)- propMBE(x, y))^2, na.rm = TRUE))
  randError<-sqrt(a*b)
  return(randError)
}
ss <- lapply(functions,
             FUN=function(f) do.call(f, list(x, y)))
ss <- do.call(c, ss)
names(ss) <- functions
ss
}

annual<- subset(dfprecip, variable=="annualPrecip")
jfma<- subset(dfprecip, variable=="jfmaPrecip")

##APPLES TO ORANGES
{
annual_chelsa1<- as.data.frame(errors(annual$chelsa1, annual$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
annual_chelsa2<- as.data.frame(errors(annual$chelsa2, annual$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
jfma_chelsa1<- as.data.frame(errors(jfma$chelsa1, jfma$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
jfma_chelsa2<- as.data.frame(errors(jfma$chelsa2, jfma$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
annual_worldclim<- as.data.frame(errors(annual$worldclim, annual$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
jfma_worldclim<- as.data.frame(errors(jfma$worldclim, jfma$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
###
table1<- cbind(annual_chelsa1,annual_chelsa2,annual_worldclim,jfma_chelsa1,jfma_chelsa2,jfma_worldclim)
dimnames(table1)[[2]]<- c("annual_chelsa1","annual_chelsa2","annual_worldclim","jfma_chelsa1","jfma_chelsa2","jfma_worldclim")
}
#APPLES TO APPLES TAKING OUT NAs
{
  annual2<- subset(annual, !is.na(chelsa1)& !is.na(worldclim))
  jfma2<- subset(jfma, !is.na(chelsa1)& !is.na(worldclim))
  annual_chelsa1<- as.data.frame(errors(annual2$chelsa1, annual2$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  annual_chelsa2<- as.data.frame(errors(annual2$chelsa2, annual2$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  jfma_chelsa1<- as.data.frame(errors(jfma2$chelsa1, jfma2$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  jfma_chelsa2<- as.data.frame(errors(jfma2$chelsa2, jfma2$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  annual_worldclim<- as.data.frame(errors(annual2$worldclim, annual2$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  jfma_worldclim<- as.data.frame(errors(jfma2$worldclim, jfma2$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  ###
  table2<- cbind(annual_chelsa1,annual_chelsa2,annual_worldclim,jfma_chelsa1,jfma_chelsa2,jfma_worldclim)
  dimnames(table2)[[2]]<- c("annual_chelsa1","annual_chelsa2","annual_worldclim","jfma_chelsa1","jfma_chelsa2","jfma_worldclim")
}
#APPLES TO APPLES, ESITMATE CHELSA1 MISSING VALUES TO CLOSEST PIXEL
{
  ##chelsa1 na estimation
  {
  temp<- dfprecip2 %>% subset(is.na(annualPrecip_chelsa1))
  temp<- temp[,c(6,7)]
  a<- nearestLand(temp, prec1, 3000)
  
  temp$annualPrecip_chelsa1<-raster::extract(prec1, a)
  temp$jfmaPrecip_chelsa1<- raster::extract(precja1, a)
  
  dfprecip2<- merge(dfprecip2,temp, by.x="long_dd", by.y="long_dd", all=TRUE)
  dfprecip2<- mutate(dfprecip2, jfmaPrecip_chelsa1=ifelse(!is.na(jfmaPrecip_chelsa1.x), jfmaPrecip_chelsa1.x, jfmaPrecip_chelsa1.y),
                     annualPrecip_chelsa1=ifelse(!is.na(annualPrecip_chelsa1.x), annualPrecip_chelsa1.x, annualPrecip_chelsa1.y))
  dfprecip2<- rename(dfprecip2, lat_dd= lat_dd.x)
  dfprecip2<- dfprecip2[,-c("annualPrecip_chelsa1.x","jfmaPrecip_chelsa1.x","lat_dd.y", "annualPrecip_chelsa1.y","jfmaPrecip_chelsa1.y")]
  attributes(dfprecip2$long_dd)<- NULL
  }
  ##worldclim na estimation
  {
    temp2<- dfprecip2 %>% subset(is.na(annualPrecip_worldclim))
    temp2<- temp2[,c("long_dd","lat_dd")]
    b<- nearestLand(temp2, prec_worldclim_annual, 4000)
    
    temp2$annualPrecip_worldclim<-raster::extract(prec_worldclim_annual, b)
    temp2$jfmaPrecip_worldclim<- raster::extract(prec_worldclim_jfma, b)
    
    dfprecip2<- merge(dfprecip2,temp2, by.x="long_dd", by.y="long_dd", all=TRUE)
    dfprecip2<- mutate(dfprecip2, jfmaPrecip_worldclim=ifelse(!is.na(jfmaPrecip_worldclim.x), jfmaPrecip_worldclim.x, jfmaPrecip_worldclim.y),
                       annualPrecip_worldclim=ifelse(!is.na(annualPrecip_worldclim.x), annualPrecip_worldclim.x, annualPrecip_worldclim.y))
    dfprecip2<- rename(dfprecip2, lat_dd= lat_dd.x)
    dfprecip2<- dfprecip2[,-c("annualPrecip_worldclim.x","jfmaPrecip_worldclim.x","lat_dd.y", "annualPrecip_worldclim.y","jfmaPrecip_worldclim.y")]
    attributes(dfprecip2$long_dd)<- NULL
  }
  ## melt dfprecip to long format
  dfprecip <- melt(dfprecip2[, -c("ele_m","long_dd", "lat_dd")], id.vars = "site")
  dfprecip$source <- tstrsplit(dfprecip$variable, "_")[[2]]
  dfprecip$variable <- tstrsplit(dfprecip$variable, "_")[[1]]
  
  # make  columns for each source of data and add coordinates 
  dfprecip <- dcast(dfprecip, site + variable ~ source, value.var = "value")
  dfprecip <-merge(dfprecip,metStation[,c("stri_name_fixed","ele_m","long_dd","lat_dd")],
                   all.x=T,by.x="site",by.y="stri_name_fixed")
  dfprecip$devch1 <- dfprecip$chelsa1-dfprecip$acp
  dfprecip$devch2 <- dfprecip$chelsa2-dfprecip$acp
  
  annual3<- subset(dfprecip, variable=="annualPrecip")
  jfma3<- subset(dfprecip, variable=="jfmaPrecip")
  
  annual_chelsa1<- as.data.frame(errors(annual3$chelsa1, annual3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  annual_chelsa2<- as.data.frame(errors(annual3$chelsa2, annual3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  jfma_chelsa1<- as.data.frame(errors(jfma3$chelsa1, jfma3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  jfma_chelsa2<- as.data.frame(errors(jfma3$chelsa2, jfma3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  annual_worldclim<- as.data.frame(errors(annual3$worldclim, annual3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  jfma_worldclim<- as.data.frame(errors(jfma3$worldclim, jfma3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  
  annual_pbcorChelsa1<- as.data.frame(errors(annual3$pbcorChelsa1, annual3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  annual_pbcorChp<- as.data.frame(errors(annual3$pbcorChp, annual3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  annual_pbcorWorldclim<- as.data.frame(errors(annual3$pbcorWorldclim, annual3$acp, functions = c("MAE", "MBE", "RMSE", "randError","proprandError", "propRMSE", "propMBE","propMAE")))
  ###
  table3<- cbind(annual_chelsa1,annual_chelsa2,annual_worldclim,annual_pbcorChelsa1,annual_pbcorChp,annual_pbcorWorldclim,jfma_chelsa1,jfma_chelsa2,jfma_worldclim)
  dimnames(table3)[[2]]<- c("annual_chelsa1","annual_chelsa2","annual_worldclim"," annual_pbcorChelsa1"," annual_pbcorChp","annual_pbcorWorldclim","jfma_chelsa1","jfma_chelsa2","jfma_worldclim")
}

table3 %>%
  kbl(caption = "NA's in chelsa 1.2 and worldclim filled with closest non NA cell value") %>%
  kable_styling()
table2 %>%
  kbl(caption = "all rows with NA's in chelsa 1.2 and worldclim removed") %>%
  kable_styling()
table1 %>%
  kbl(caption = "Apple to Pears, only Na's in chelsa 1.2 and worldclim removed") %>%
  kable_styling()

# graphs of scatterplots alone (adapted from code by Camille Piponiot)
thisplotfn <- function(xdata,ydata,xname,yname,maintext) {
  thisdata <- data.frame(x=xdata,y=ydata)
  inc <- !is.na(thisdata$x) & !is.na(thisdata$y)
  rangexy=range(c(xdata,ydata),na.rm=T)
  thisdata <- thisdata[inc,]
  thiscor <- cor.test(xdata,ydata,use="complete.obs")
  textcor <- paste("r =",round(thiscor$estimate,2),", p =",signif(thiscor$p.value,2),", n =",nrow(thisdata))
  thisg <- ggplot(thisdata, aes(x = x,y=y)) +
    geom_abline(slope = 1, intercept = 0, lty= 2) +
    geom_point(col=rgb(0,0,0,alpha=0.5)) +
    theme_classic() +
    expand_limits(x = rangexy, y = rangexy) +
    coord_equal() +
    annotate("text",x=rangexy[1],y=rangexy[2],hjust=0,vjust=1,label=textcor)+
    labs(x = xname, y = yname,
         #title=paste(maintext,"r=",round(thiscor$estimate,2),"p=",signif(thiscor$p.value,2))
         title=maintext
    )
  return(thisg)
} 
# end this plotfn
inca <-dfprecip$variable=="annualPrecip"
g1 <- thisplotfn(dfprecip$acp[inca],dfprecip$chelsa2[inca],xname="ACP meteorological stations", 
                 yname = "CHELSA v 2.1",maintext="Mean Annual Precipitation (mm)")
g2 <- thisplotfn(dfprecip$acp[!inca],dfprecip$chelsa2[!inca],xname="ACP meteorological stations", 
                 yname = "CHELSA v 2.1",maintext="Mean Jan-April Precipitation (mm)")
g3 <- thisplotfn(dfprecip$acp[inca],dfprecip$chelsa1[inca],xname="ACP meteorological stations", 
                 yname = "CHELSA v 1.2",maintext="Mean Annual Precipitation (mm)")
g4 <- thisplotfn(dfprecip$acp[!inca],dfprecip$chelsa1[!inca],xname="ACP meteorological stations", 
                 yname = "CHELSA v 1.2",maintext="Mean Jan-April Precipitation (mm)")
inc1 <- !is.na(dfprecip$chelsa1) # only include sites where CHELSA1 values are available
g5 <- thisplotfn(dfprecip$acp[inca&inc1],dfprecip$chelsa2[inca&inc1],xname="ACP meteorological stations", 
                 yname = "CHELSA v 2.1",maintext="Mean Annual Precipitation (mm)")
g6 <- thisplotfn(dfprecip$acp[!inca & inc1],dfprecip$chelsa2[!inca & inc1],xname="ACP meteorological stations", 
                 yname = "CHELSA v 2.1",maintext="Mean Jan-April Precipitation (mm)")
g7<- thisplotfn(log(dfprecip$acp[inca]),log(dfprecip$chelsa2[inca]),xname="ACP meteorological stations", 
yname = "CHELSA v 2.1",maintext="Mean Annual Precipitation (mm)")

ggpubr::ggarrange(g1, g2, nrow = 1, ncol=2, labels = "AUTO", heights = c(1, 1),align="hv")
ggsave(paste0(GRAPHDIR,"/acp-vs-chelsa2.pdf"), height = 5, width = 8)
ggpubr::ggarrange(g5, g6, nrow = 1, ncol=2, labels = "AUTO", heights = c(1, 1),align="hv")
ggsave(paste0(GRAPHDIR,"/acp-vs-chelsa2-samesitesch1.pdf"), height = 5, width = 8)
ggpubr::ggarrange(g3, g4, nrow = 1, ncol=2, labels = "AUTO", heights = c(1, 1),align="hv")
ggsave(paste0(GRAPHDIR,"/acp-vs-chelsa1.pdf"), height = 5, width = 8)
ggpubr::ggarrange(g1, g2, g3, g4, nrow = 2, ncol=2, labels = "AUTO", heights = c(1, 1),align="hv")
ggsave(paste0(GRAPHDIR,"/acp-vs-chelsa.pdf"), height = 8, width = 8)


# what are the big outliers?
temp <- subset(dfprecip,variable=="jfmaPrecip"&abs(devch2)>200)
temp <- temp[order(temp$devch2),]

# do the outliers line up with elevation?
win.graph()
par(mfrow=c(2,2))
plot(dfprecip$ele_m[inca],dfprecip$devch2[inca],main="Annual Precip, CHELSA2-ACP")
abline(h=0)
plot(dfprecip$ele_m[inca],dfprecip$devch1[inca], main="Annual Precip, CHELSA1-ACP")
abline(h=0)
plot(dfprecip$ele_m[!inca],dfprecip$devch2[!inca],main="JFMA Precip, CHELSA2-ACP")
abline(h=0)
plot(dfprecip$ele_m[!inca],dfprecip$devch1[!inca], main="JFMA Precip, CHELSA1-ACP")
abline(h=0)


###############################################
# now make maps showing deviations (adapted from code by KC Cushman)
extentMap <- raster::extent(c(-80.7,-79.2, 8.2,9.7))
useprec1 <- raster::crop(prec1, extentMap)
useprecja1 <- raster::crop(precja1, extentMap)
useprec2 <- raster::crop(prec2, extentMap)
useprecja2 <- raster::crop(precja2, extentMap)

# turn areas over water to NA (as in CHELSA1) so maps look better
# NOTE - would be even better to crop using an outline of Panama, to keep values for lake perhaps?
notwater <- !is.na(useprec1)   
useprec2 <- useprec2*notwater
useprecja2 <- useprecja2*notwater


rangeannprec1 <- cellStats(useprec1,range,na.rm=T)
rangeannprec2 <- cellStats(useprec2,range,na.rm=T)
rangeannprec <- range(c(rangeannprec1,rangeannprec2))
colBreaksap <- seq(floor(rangeannprec[1]),ceiling(rangeannprec[2]),1)

rangejfmaprec1 <- cellStats(useprecja1,range,na.r=T)
rangejfmaprec2 <- cellStats(useprecja2,range,na.rm=T)
rangejfmaprec <- range(c(rangejfmaprec1,rangejfmaprec2))
colBreaksap <- seq(floor(rangejfmaprec[1]),ceiling(rangejfmaprec[2]),1)

rangeannprecsites <- range(c(dfprecip$acp[inca],dfprecip$chelsa2[inca]),na.rm=T)
rangejfmaprecsites <- range(c(dfprecip$acp[!inca],dfprecip$chelsa2[!inca]),na.rm=T)

dfprecipa <- dfprecip[variable=="annualPrecip",]
dfprecipb <- dfprecip[variable=="jfmaPrecip",]
dfprecipa$cex1 <- scale(abs(dfprecipa$devch1),center=F)
dfprecipa$cex2 <- scale(abs(dfprecipa$devch2),center=F)
dfprecipb$cex1 <- scale(abs(dfprecipb$devch1),center=F)
dfprecipb$cex2 <- scale(abs(dfprecipb$devch2),center=F)
collohi <- c("red","blue")
leglohi <- c("reanalysis < ground","reanalysis > ground")
dfprecipa$col1 <- ifelse(is.na(dfprecipa$devch1),NA,
                         ifelse(dfprecipa$devch1<0,collohi[1],collohi[2]))
dfprecipa$col2 <- ifelse(is.na(dfprecipa$devch2),NA,
                         ifelse(dfprecipa$devch2<0,collohi[1],collohi[2]))
dfprecipb$col1 <- ifelse(is.na(dfprecipb$devch1),NA,
                         ifelse(dfprecipb$devch1<0,collohi[1],collohi[2]))
dfprecipb$col2 <- ifelse(is.na(dfprecipb$devch2),NA,
                         ifelse(dfprecipb$devch2<0,collohi[1],collohi[2]))

# need to do the scatterplots in base R for them to play nice with the raster map

baseplotfn <- function(xdata,ydata,xname,yname,maintext) {
  par(las=1)
  thisdata <- data.frame(x=xdata,y=ydata)
  inc <- !is.na(thisdata$x) & !is.na(thisdata$y)
  rangexy=range(c(xdata,ydata),na.rm=T)
  thisdata <- thisdata[inc,]
  thiscor <- cor.test(xdata,ydata,use="complete.obs")
  textcor <- paste("r =",round(thiscor$estimate,2),", p =",signif(thiscor$p.value,2),", n =",nrow(thisdata))
  plot(thisdata$x,thisdata$y,xlab=xname,ylab=yname,main=maintext,pch=16,
       col=rgb(0,0,0,alpha=0.5),xlim=rangexy,ylim=rangexy)
  text(x=rangexy[1],y=rangexy[2],adj=c(0,1),labels=textcor)
  abline(0,1)
} # end this baseplotfn


#win.graph(width=7,height=7.5)
pdf(paste0(GRAPHDIR,"/acp-vs-chelsa2-wmap---testing.pdf"), width=7, height=7.5)
par(mfrow=c(2,2), oma=c(1,1,1,2),las=1)  # in principle should be possible to do this better with layout
baseplotfn(dfprecipa$acp,dfprecipa$chelsa2,xname="ACP meteorological stations", 
           yname = "CHELSA v 2.1",maintext="Mean Annual Precipitation (mm)")
raster::plot(useprec2,main = 'Annual Precip CHELSA 2.1 (mm)',axes=F,box=F,bty="n")
points(lat_dd~long_dd, data=dfprecipa,cex=cex2,col=col2)
legend("bottomright",legend=leglohi,col=collohi,pch=1)
baseplotfn(dfprecipb$acp,dfprecipb$chelsa2,xname="ACP meteorological stations", 
           yname = "CHELSA v 2.1",maintext="Mean Jan-April Precipitation (mm)")
raster::plot(useprecja2,main = 'JFMA Precip CHELSA 2.1 (mm)',axes=F,box=F,bty="n")
points(lat_dd~long_dd, data=dfprecipb,cex=cex2,col=col2)
legend("bottomright",legend=leglohi,col=collohi,pch=1)
dev.off()




pdf(paste0(GRAPHDIR,"/acp-vs-chelsa1-wmap.pdf"), width=8, height=8)
par(mfrow=c(2,2), oma=c(1,1,1,2),las=1)  # in principle should be possible to do this better with layout
baseplotfn(dfprecipa$acp,dfprecipa$chelsa1,xname="ACP meteorological stations", 
           yname = "CHELSA v 1.2",maintext="Mean Annual Precipitation (mm)")
raster::plot(useprec1,main = 'Annual Precip CHELSA 1.2 (mm)',axes=F,box=F,bty="n")
points(lat_dd~long_dd, data=dfprecipa,cex=cex1,col=col1)
legend("bottomright",legend=leglohi,col=collohi,pch=1)
baseplotfn(dfprecipb$acp,dfprecipb$chelsa1,xname="ACP meteorological stations", 
           yname = "CHELSA v 1.2",maintext="Mean Jan-April Precipitation (mm)")
raster::plot(useprecja1,main = 'JFMA Precip CHELSA 1.2 (mm)',axes=F,box=F,bty="n")
points(lat_dd~long_dd, data=dfprecipb,cex=cex1,col=col1)
legend("bottomright",legend=leglohi,col=collohi,pch=1)
dev.off()


png(paste0(GRAPHDIR,"/acp-vs-chelsa2-wmap.png"), width=7, height=7.5,units="in",res=300)
par(mfrow=c(2,2), oma=c(1,1,1,2),las=1)  # in principle should be possible to do this better with layout
baseplotfn(dfprecipa$acp,dfprecipa$chelsa2,xname="ACP meteorological stations", 
           yname = "CHELSA v 2.1",maintext="Mean Annual Precipitation (mm)")
raster::plot(useprec2,main = 'Annual Precip CHELSA 2.1 (mm)',axes=F,box=F,bty="n")
points(lat_dd~long_dd, data=dfprecipa,cex=cex2,col=col2)
legend("bottomright",legend=leglohi,col=collohi,pch=1)
baseplotfn(dfprecipb$acp,dfprecipb$chelsa2,xname="ACP meteorological stations", 
           yname = "CHELSA v 2.1",maintext="Mean Jan-April Precipitation (mm)")
raster::plot(useprecja2,main = 'JFMA Precip CHELSA 2.1 (mm)',axes=F,box=F,bty="n")
points(lat_dd~long_dd, data=dfprecipb,cex=cex2,col=col2)
legend("bottomright",legend=leglohi,col=collohi,pch=1)
dev.off()

