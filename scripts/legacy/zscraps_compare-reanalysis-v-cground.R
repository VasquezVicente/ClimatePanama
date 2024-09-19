# code no longer used / needed in compare-reanalysis-v-ground.r


# comparing original and converted met station locations
#(in original file, UTM coordinats and decimal degree coordinates didn't always agree)

METLOCATIONFN <- "data_ground/met_data/ACP_data/cleanedHM/ACP_met_location_fixed_2022-08-15.xlsx"
metStation <- read_excel(METLOCATIONFN,sheet=1)
names(metStation) <- tolower(names(metStation))
metStation <- mutate(metStation,
                     ele_m=as.numeric(ele_m),
                     utm_n=as.numeric(utm_n),
                     utm_e=as.numeric(utm_e),
                     lat_n=as.numeric(lat_n),
                     long_w=as.numeric(long_w),
                     lat_dd=as.numeric(lat_dd),
                     long_dd=as.numeric(long_dd))

# check how decimal degree locations compare between original and Milton Solano's
hist(metStation$lat_dd-metStation$lat_n,xlab="Latitude difference new vs. old")
hist(metStation$long_dd-metStation$long_w,xlab="Latitude difference new vs. old")
# there are some substantial differences...



# fix met station locations where necessary
table(is.na(metStation$utm_n))
table(is.na(metStation$utm_e))
table(is.na(metStation$lat_dd))
table(is.na(metStation$long_dd))
table(is.na(metStation$utm_n)&is.na(metStation$lat_n))
# UTMs are available for more stations than decimal degrees, so need to convert

########################################################

dftc$DrySeasDeficit_chelsa <- raster::extract(cwdja, dftc[, c("Longitude", "Latitude")])
dftc$MaxDeficit_chelsa <- raster::extract(mcwd, dftc[, c("Longitude", "Latitude")])



origutmcoor<-SpatialPoints(cbind(metStation$utm_e,metStation$utm_n), proj4string=CRS("+proj=utm +zone=17"))
convlonglatcoor<-spTransform(origutmcoor,CRS("+proj=longlat"))
metStation <- cbind(metStation,convlonglatcoor)
names(metStation)[c(ncol(metStation)-1,ncol(metStation))] <- c("long_w_fromutm","lat_n_fromutm")

temp1 <- metStation[!is.na(metStation$lat_n)&!is.na(metStation$long_w),c("stri_name_fixed","lat_n","long_w")]
origlonglatcoord <- SpatialPoints(cbind(temp1$long_w,temp1$lat_n),proj4string=CRS("+proj=longlat"))
convutmcoord <- spTransform(origlonglatcoord,CRS("+proj=utm +zone=17 +datum=WGS84"))
temp1 <- cbind(temp1,convutmcoord)
names(temp1)[c(ncol(temp1)-1,ncol(temp1))] <- c("utm_e_fromdeg","utm_n_fromdeg")
metStation <- merge(metStation,temp1[,c("stri_name_fixed","utm_e_fromdeg","utm_n_fromdeg")])

hist(metStation$utm_e_fromdeg-metStation$utm_e)
hist(metStation$utm_n_fromdeg-metStation$utm_n)
table(abs(metStation$utm_e_fromdeg-metStation$utm_e)>20)
table(abs(metStation$utm_n_fromdeg-metStation$utm_n)>100)



# get data from chelsa
mcwd <- raster::raster("data/maps/mcwd-map.tif")
cwdja <- raster::raster("data/maps/cwd_ja-map.tif")

dftc$AnnualPrecip_chelsa <- raster::extract(prec, dftc[, c("Longitude", "Latitude")])
dftc$jfmaPrecip_chelsa <- raster::extract(precja, dftc[, c("Longitude", "Latitude")])


# reorganize data for figures
dfmcwd <- melt(dftc, id.vars = c("plot", "DrySeasonDeficit"), 
               measure.vars = c("DrySeasDeficit_chelsa", 
                                "MaxDeficit_chelsa", 
                                "jfmaPrecip_chelsa")
)

dfmcwd$variable <- factor(dfmcwd$variable)
levels(dfmcwd$variable) <- paste("CHELSA", 
                                 c("Dry Season Deficit (mm)", 
                                   "Maximum Deficit (mm)", 
                                   "Dry Season Precipitation (mm)"))


### compare MCWD data from CHELSA v2.1 with Condit 2013 MCWD

# url1 <- "https://repository.si.edu/bitstream/handle/10088/19529/TreeCommunityDrySeasonDailyRain.txt?sequence=3&isAllowed=y"
url1 <- "https://repository.si.edu/bitstream/handle/10088/19529/TreeCommunityDrySeasonDailyRain.txt"
url2 <- "https://repository.si.edu/bitstream/handle/10088/19529/TreeCommunityDrySeasonStationSite.txt"

if (!file.exists(paste0("data/", basename(url1)))) {
  download.file(url1, destfile = paste0("data/", basename(url1)))
}

if (!file.exists(paste0("data/", basename(url2)))) {
  download.file(url2, destfile = paste0("data/", basename(url2)))
}

acp_precip <- read.delim(paste0("data/", basename(url1)))
acp_sites <- read.delim(paste0("data/", basename(url2)))


### compare with Turner-Condit 2022data ####

url <- "https://datadryad.org/stash/dataset/doi:10.7291/D1B963"

dryad_data_path_soil <- rdryad::dryad_download("10.7291/D1B963")
path <- grep("\\.tsv", dryad_data_path_soil$`10.7291/D1B963`, value = TRUE)
dftc <- read.delim(path)

# get standardized plot names
dftc$plot <- tolower(dftc$Plot.code) %>%
  gsub(pattern = " | - ", replacement = "") %>%
  gsub(pattern = "gamboap1ha", replacement = "cihhutp") %>%
  gsub(pattern = "casa", replacement = "finca") %>%
  gsub(pattern = "plot", replacement = "p")  %>%
  gsub(pattern = "crane(finalhectares4?)", replacement = "")  %>%
  gsub(pattern = "metrop1ha", replacement = "metrop") 

dftc <- dftc[, c("plot", "Latitude", "Longitude", "AnnualPpt", "DrySeasonDeficit")]
setDT(dftc)

# get data from chelsa
mcwd <- raster::raster("data/maps/mcwd-map.tif")
cwdja <- raster::raster("data/maps/cwd_ja-map.tif")

dftc$AnnualPrecip_chelsa <- raster::extract(prec, dftc[, c("Longitude", "Latitude")])
dftc$jfmaPrecip_chelsa <- raster::extract(precja, dftc[, c("Longitude", "Latitude")])


# reorganize data for figures
dfmcwd <- melt(dftc, id.vars = c("plot", "DrySeasonDeficit"), 
               measure.vars = c("DrySeasDeficit_chelsa", 
                                "MaxDeficit_chelsa", 
                                "jfmaPrecip_chelsa")
)

dfmcwd$variable <- factor(dfmcwd$variable)
levels(dfmcwd$variable) <- paste("CHELSA", 
                                 c("Dry Season Deficit (mm)", 
                                   "Maximum Deficit (mm)", 
                                   "Dry Season Precipitation (mm)"))

ggplot(dfmcwd, aes(x = DrySeasonDeficit, y = value)) +
  geom_abline(slope = 1, intercept = 0, lty= 2) +
  geom_point() +
  facet_wrap(~variable, scales = "free", strip.position = "left") +
  theme_classic() +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  labs(x = "Turner-Condit Dry Season Deficit (mm)", y = "")

ggsave(paste0(GRAPHDIR,"/chelsa-vs-turner-condit-mcwd.pdf"), height = 4, width = 12)

# with annual precipitation
ggplot(dftc, aes(x = AnnualPpt, y = AnnualPrecip_chelsa)) +
  geom_abline(slope = 1, intercept = 0, lty= 2) +
  geom_point() +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0, 5)) +
  scale_y_continuous(expand = c(0, 5)) +
  theme_classic() +
  coord_equal() +
  labs(x = "Turner-Condit Annual Precipitation (mm)", y = "CHELSA Annual precipitation (mm)")
ggsave(paste0(GRAPHDIR,"/chelsa-vs-turner-condit-precip.pdf"), height = 4, width = 5)





raster::plot(useprec2,
             main = 'Annual Precip CHELSA 2.1 (mm/yr)')
points(lat_dd~long_dd, data=dfprecipa[!is.na(dfprecipa$cex2),],cex=cex2,col=col2)
legend("bottomright",legend=c("ground > reanalysis", "ground < reanalysis"),
       col=c("blue","red"),pch=1,bty="n")
g7 <- recordPlot()

raster::plot(useprecja2,
             main = 'JFMA Precip CHELSA 2.1 (mm/yr)')
points(lat_dd~long_dd, data=dfprecipb[!is.na(dfprecipb$cex2),],cex=cex2,col=col2)
legend("bottomright",legend=c("ground > reanalysis", "ground < reanalysis"),
       col=c("blue","red"),pch=1, bty="n")
g8 <- recordPlot()

win.graph(width=7.25,height=8.5)
plot_grid(g1,g2,g7,g8,nrow=2,ncol=2,
          labels="AUTO",
          axis="l", align="hv", label_x = 0,label_y = 1)


pdf(paste0(GRAPHDIR,"/acp-vs-chelsa2-wmap_alt.pdf"), width=8, height=8)
par(mfrow=c(2,2), oma=c(1,1,1,2),las=1)
plot(g1)

plot(g2)


par(las=1)
plot(chelsa2~acp, data=dfprecipa, pch=20,
     ylab="CHELSA v. 2.1)",
     xlab="ACP met station",
     main="Mean Annual Precipitation (mm)",
     xlim=rangeannprecsites,ylim=rangeannprecsites)
abline(a=0,b=1)

par(las=1)
plot(chelsa2~acp, data=dfprecipb, pch=20,
     ylab="CHELSA v. 2.1)",
     xlab="ACP met station",
     main="Mean Jan-Apr Precipitation (mm)",
     xlim=rangejfmaprecsites,ylim=rangejfmaprecsites)
abline(a=0,b=1)
g7 <- recordPlot()


# trying with layout # can't get this to work!
win.graph(width=9,height=7)
#par(mfrow=c(2,2), oma=c(1,1,1,2),las=1)
layout(matrix(c(1,2,2,3,4,4),nrow=2,byrow=T),widths=rep(1,3),heights=rep(1,2))
par(las=1)

baseplotfn(dfprecipa$acp,dfprecipa$chelsa2,xname="ACP meteorological stations", 
           yname = "CHELSA v 2.1",maintext="Mean Annual Precipitation (mm)")
raster::plot(useprec2,main = 'Annual Precip CHELSA 2.1 (mm/yr)',axes=F,box=F,bty="n")
points(lat_dd~long_dd, data=dfprecipa,cex=cex2,col=col2)
legend("bottomright",legend=c("ground > reanalysis", "ground < reanalysis"),
       col=c("blue","red"),
       #bty="n",
       pch=1)

baseplotfn(dfprecipb$acp,dfprecipb$chelsa2,xname="ACP meteorological stations", 
           yname = "CHELSA v 2.1",maintext="Mean Jan-April Precipitation (mm)")
raster::plot(useprecja2,main = 'JFMA Precip CHELSA 2.1 (mm/yr)',axes=F,box=F,bty="n")
points(lat_dd~long_dd, data=dfprecipb,cex=cex2,col=col2)
legend("bottomright",legend=c("ground > reanalysis", "ground < reanalysis"),
       col=c("blue","red"),pch=1, bty="n")


