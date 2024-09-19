library(lintr)
lint(filename = "compare-reanalysis-v-ground.R")

# compare climate reanalysis products with observed rainfall data for ACP stations #### # nolint
# currently includes reanalysis products from CHELSA 1.2, CHELSA 2.1, and PBCOR CHELSA 1.2

library(readxl)
library(sp)
library(raster)
library(MASS)
library(dplyr)
library(data.table)
library(ggplot2)

rm(list=ls()) # nolint: infix_spaces_linter.

METLOCATIONFN <- "data_ground/met_data/ACP_data/cleanedHM/ACP_met_location_fixed_2022-08-15.xlsx" # nolint # nolint: object_name_linter.

PRECIPDATAFN <- "data_ground/met_data/ACP_data/cleanedHM/ACPrainDataVert_2022-08-11.xlsx" # nolint
MAPDIR <- "output/maps"
GRAPHDIR <- "output/graphs"

MINYEAR <- 1981 # only include ground data from 1980 on, to match CHELSA 2.1 dataset 



## open ACP met station data
metStation <- read_excel(METLOCATIONFN,sheet=1)
monthlyPrecipData1 <- read_excel(PRECIPDATAFN,sheet=1)

names(metStation) <- tolower(names(metStation))
metStation <- metStation[order(metStation$stri_name_fixed),]


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

# only keep years that have data for all 12 months
monthlyPrecipData[, nmonths := sum(!is.na(monthlyPrecip)), .(site, Year)]

## calculate annual and JFMA precipitation, per year (JFMA = January to April)
annualPrecipData <- monthlyPrecipData[nmonths == 12 & Year>=MINYEAR, .(annualPrecip = sum(monthlyPrecip), 
                                          jfmaPrecip = sum(monthlyPrecip[Month<5])), .(site, Year)]

# calculate mean over years for each site
dfprecip1 <- annualPrecipData[, .(annualPrecip_acp = mean(annualPrecip, na.rm = TRUE), 
                              jfmaPrecip_acp = mean(jfmaPrecip, na.rm = TRUE),
                              nYears = sum(!is.na(annualPrecip))
                              ), .(site)]


# which stations don't match?  
print("The following met station names appear in the met data file but not the location file:")
print(sort(dfprecip1$site[is.na(match(dfprecip1$site,metStation$stri_name_fixed))]))
print("The following met station names appear in the location file but not the met data file:")
print(sort(metStation$stri_name_fixed[is.na(match(metStation$stri_name_fixed,dfprecip1$site))]))


# add coordinates 
dfprecip2 <-merge(dfprecip1,metStation[,c("stri_name_fixed","ele_m","long_dd","lat_dd")],
                   all.x=T,by.x="site",by.y="stri_name_fixed")
dfprecip2 <- dfprecip2[order(dfprecip2$annualPrecip_acp),]

# take a look at where stations are 
win.graph(width=10,height=8)
plot(dfprecip2$long_dd,dfprecip2$lat_dd,cex=base::scale(dfprecip2$annualPrecip_acp,center=F))
text(dfprecip2$long_dd,dfprecip2$lat_dd,labels=dfprecip2$site,cex=0.7)

# get CHELSA 1 and 2 precipitation data
prec1 <- raster::raster(paste0(MAPDIR,"/precann1-maps.tif"))
precja1 <- raster::raster(paste0(MAPDIR,"/precjfma1-maps.tif"))
prec2 <- raster::raster(paste0(MAPDIR,"/precann2-maps.tif"))
precja2 <- raster::raster(paste0(MAPDIR,"/precjfma2-maps.tif"))

# extract values at site coordinates
dfprecip2$annualPrecip_chelsa1 <- raster::extract(prec1, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_chelsa1 <- raster::extract(precja1, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$annualPrecip_chelsa2 <- raster::extract(prec2, dfprecip2[, c("long_dd", "lat_dd")])
dfprecip2$jfmaPrecip_chelsa2 <- raster::extract(precja2, dfprecip2[, c("long_dd", "lat_dd")])

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


##################################
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
} # end this plotfn
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
pdf(paste0(GRAPHDIR,"/acp-vs-chelsa2-wmap.pdf"), width=7, height=7.5)
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


