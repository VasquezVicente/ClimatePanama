# compare climate reanalysis products with observed rainfall data for ACP stations ####
#Libraries
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
#DIRECTORIES
METLOCATIONFN <- "data_ground/met_data/ACP_data/cleanedHM/ACP_met_location_fixed_2022-08-15.xlsx"
PRECIPDATAFN <- "data_ground/met_data/ACP_data/cleanedHM/ACPrainDataVert_2022-08-11.xlsx"
MAPDIR <- "output/maps/pan_prec_maps"
GRAPHDIR <- "output/graphs"
#load
panama_elevation<- raster::raster("data_reanalysis/panama_elevation.tif")
#annual precip data frame and monthly precipitation df 
{
## open ACP met station data
metStation <- read_excel(METLOCATIONFN,sheet=1)            #NAMES AND LOCATIONS ACP
monthlyPrecipData1 <- read_excel(PRECIPDATAFN,sheet=1)     # 74 ACP STATIONS
names(metStation) <- tolower(names(metStation))
metStation <- metStation[order(metStation$stri_name_fixed),]
metStation <- dplyr::select(metStation,-c(lat_n,long_w,lat_n_dms,long_w_dms,stri_name_orig))
metStation <- mutate(metStation,
                     ele_m=as.numeric(ele_m),
                     utm_n=as.numeric(utm_n),
                     utm_e=as.numeric(utm_e),
                     lat_dd=as.numeric(lat_dd),
                     long_dd=as.numeric(long_dd))
metStation <- subset(metStation,!is.na(metStation$utm_e))
#STRI SITES
strisites<- read.csv("data_ground/met_data/STRI_data/monthly_prec_stri.csv")
strisites$month<- substring(strisites$yearmonth, 6.7)
strisites$Month<- as.numeric(strisites$month)
strisites$month2<- ifelse(strisites$month==1,0.000,ifelse(strisites$month==2,0.083,ifelse(strisites$month==3,0.167,ifelse(strisites$month==4,0.250,ifelse(strisites$month==5,0.333,ifelse(strisites$month==6, 0.417, ifelse(strisites$month==7, 0.500, ifelse(strisites$month==8, 0.583,ifelse(strisites$month==9,0.667,ifelse(strisites$month==10, 0.750,ifelse(strisites$month==11,0.833, 0.917)))))))))))
strisites$Year<- as.numeric(strisites$year)
strisites$Yr.Mon<- strisites$year+strisites$month2
strisites<- rename(strisites,monthlyPrecip=ra)
strisites<- strisites[,c("Year","Month", "Yr.Mon","site", "monthlyPrecip")]
##I have reasons to believe 2006 and 2007 of sherman isnt good enough to keep, they were gap fills 
strisites$yearsite<- paste(strisites$Year, strisites$site)
strisites<-subset(strisites, yearsite!= "2006 SHERMAN"&yearsite!="2007 SHERMAN")
strisites<- strisites[,-c(6)]
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
monthlyPrecipData[, nmonths := sum(!is.na(monthlyPrecip)), .(site, Year)]
monthlyPrecipData<-  merge(monthlyPrecipData, nstations, by.x="Year", by.y="Year", all=TRUE)%>% rename(nstations= count)
monthly_ground<- monthlyPrecipData[,c(1,2,4,5)]
monthly_ground<- filter(monthly_ground, site!="CABACERA"& site!="BCICLEARM") 
## calculate annual and JFMA precipitation, per year (JFMA = January to April)
annualPrecipData <- monthlyPrecipData[nmonths == 12 & nstations>=12, .(annualPrecip = sum(monthlyPrecip), 
                                                                       jfmaPrecip = sum(monthlyPrecip[Month<5])), .(site, Year)]
nyears<- annualPrecipData %>% group_by(site) %>% tally() %>% rename(nyears= n)
nsites<- annualPrecipData %>% group_by(Year) %>% tally() %>% rename(nsites= n)
annualPrecipData<- merge(annualPrecipData, nyears, by.x= "site", by.y="site")
annualPrecipData<- merge(annualPrecipData, nsites, by.x= "Year", by.y="Year")
#annualPrecipData<- subset(annualPrecipData, nyears== 38)
annualPrecipData<- filter(annualPrecipData, site!="CABACERA"& site!="BCICLEARM"&site!="BALBOADOCKS"&site!="BOCAS") 
}
#linear mixed model for fitted site and random effect in year/fit the model for complete years
model_annual<- lmer(annualPrecip~ site +(1|Year), data=annualPrecipData)
model_jfma<- lmer(jfmaPrecip~ site +(1|Year), data=annualPrecipData)
annual<- annualPrecipData %>% filter(Year>=1970&Year<=2016)
#DF for 78 sites with all the years between 1970 and 2018, accounts for 3822 observations
{
Year<-seq(1970, 2016)
si<-unique(annual$site)

df<- data.frame(Year)
df$site<-si[1]

df2<-data.frame(Year)
df2$site<-si[2]

df<-rbind(df,df2)

df2<- data.frame(Year)
df2$site<-si[3]

df<-rbind(df,df2)

df2<- data.frame(Year)
df2$site<-si[4]
df<-rbind(df,df2)

for (i in 5:length(si)){
df2<- data.frame(Year)
df2$site<-si[i]
df<-rbind(df,df2)
}
}
#Gap fill the 31 sites and add categorical to know wheter its filled or not
annualPrecip<- merge(annual,df, all=T,by.x=c("Year","site"),by.y=c("Year","site"))
annualPrecip<-annualPrecip[,c(1,2,3,4)]
nyears<- annualPrecip %>% group_by(site) %>% tally(!is.na(annualPrecip)) %>% rename(nyears= n)
nsites<- annualPrecip %>% group_by(Year) %>% tally(!is.na(annualPrecip)) %>% rename(nsites= n)
annualPrecip<- merge(annualPrecip, nyears, by.x="site",by.y="site",all=TRUE)
annualPrecip<- merge(annualPrecip, nsites, by.x="Year",by.y="Year",all=TRUE)
annualPrecip$predicted_annual<- predict(model_annual,newdata=annualPrecip)
annualPrecip$predicted_jfma<- predict(model_jfma,newdata=annualPrecip)
annualPrecip$annual_gap<-ifelse(is.na(annualPrecip$annualPrecip),annualPrecip$predicted_annual,annualPrecip$annualPrecip)
annualPrecip$jfma_gap<-ifelse(is.na(annualPrecip$jfmaPrecip),annualPrecip$predicted_jfma,annualPrecip$jfmaPrecip)
dfprecip1<-annualPrecip 
dfprecip1<- dfprecip1%>%filter(nyears>32)
dfprecip1$fill<- ifelse(is.na(dfprecip1$annualPrecip),TRUE,FALSE)


##annual for 5 different extents, missing the CHIRPS extent
{
  ##temporal 1979-2013
  first<- dfprecip1%>% filter(Year>=1979&Year<=2013)
  first<-first[, .(annualPrecip = mean(annual_gap, na.rm = TRUE),
                   jfmaprecip = mean(jfma_gap, na.rm = TRUE), 
                   nYears = sum(!is.na(annualPrecip))), .(site)]
  first$temporal<-"1979-2013"
  ##temporal 1979-2013
  second<- dfprecip1%>% filter(Year>=2003&Year<=2016)
  second<-second[, .(annualPrecip = mean(annual_gap, na.rm = TRUE),
                     jfmaprecip = mean(jfma_gap, na.rm = TRUE), 
                     nYears = sum(!is.na(annualPrecip))), .(site)]
  second$temporal<-"2003-2016"
  ##temporal 1981-2016
  third<- dfprecip1%>% filter(Year>=1981&Year<=2016)
  third<-third[, .(annualPrecip = mean(annual_gap, na.rm = TRUE),
                     jfmaprecip = mean(jfma_gap, na.rm = TRUE), 
                     nYears = sum(!is.na(annualPrecip))), .(site)]
  third$temporal<-"1981-2016"
  ##temporal 1980-2010
  fourth<- dfprecip1%>% filter(Year>=1980&Year<=2009)
  fourth<-fourth[, .(annualPrecip = mean(annual_gap, na.rm = TRUE),
                     jfmaprecip = mean(jfma_gap, na.rm = TRUE), 
                     nYears = sum(!is.na(annualPrecip))), .(site)]
  fourth$temporal<-"1980-2009"
  ##temporal 1980-2010
  fifth<- dfprecip1%>% filter(Year>=1981&Year<=2010)
  fifth<-fifth[, .(annualPrecip = mean(annual_gap, na.rm = TRUE),
                   jfmaprecip = mean(jfma_gap, na.rm = TRUE), 
                   nYears = sum(!is.na(annualPrecip))), .(site)]
  fifth$temporal<-"1981-2010"
  ##temporal 1980-2010
  sixth<- dfprecip1%>% filter(Year>=1970&Year<=2000)
  sixth<-sixth[, .(annualPrecip = mean(annual_gap, na.rm = TRUE),
                   jfmaprecip = mean(jfma_gap, na.rm = TRUE), 
                   nYears = sum(!is.na(annualPrecip))), .(site)]
  sixth$temporal<-"1970-2000"
  seventh<- dfprecip1%>% filter(Year>=1979&Year<=2016)
  seventh<-seventh[, .(annualPrecip = mean(annual_gap, na.rm = TRUE),
                   jfmaprecip = mean(jfma_gap, na.rm = TRUE), 
                   nYears = sum(!is.na(annualPrecip))), .(site)]
  seventh$temporal<-"1979-2016"
  dfprecip3<-rbind(first,second,third,fourth,fifth,sixth,seventh)
}
# add coordinates 
metStation_stri<- read.csv("data_ground/met_data/STRI_data/monthly_prec_stri.csv")%>% select(c("site", "lon_dd", "lat_dd"))%>% rename(stri_name_fixed=site, long_dd=lon_dd)
metStation_stri$ele_m<-NA
metStation_stri<- distinct(metStation_stri)
metStation<- metStation[,c(2,7,6,3)]
metStation<- rbind(metStation,metStation_stri)
dfprecip3 <-merge(dfprecip3,metStation[,c("stri_name_fixed","ele_m","long_dd","lat_dd")],all.x=T,by.x="site",by.y="stri_name_fixed")
dfprecip3<- dfprecip3 %>% rename(annualPrecip_acp= annualPrecip, jfmaPrecip_acp=jfmaprecip)
dfprecip3 <- dfprecip3[order(dfprecip3$annualPrecip_acp),]
# add elevation
for (i in 1:length(dfprecip3$ele_m)){
  dfprecip3$ele_m[i]<- ifelse(!is.na(dfprecip3$ele_m[i]),dfprecip3$ele_m[i],extract(panama_elevation, c(dfprecip3$long_dd[i],dfprecip3$lat_dd[i]))[2])
}
writexl::write_xlsx(dfprecip3,"tables/ground_data.xlsx")
write.csv(monthly_ground, "tables/monthly_ground.csv")