# code written by KC Cushman to compare TERRA and CHELSA 1.2 climatologis
# against Panama ACP met station data. 

# Set working directory: change to  location of "TerraClimate folder on local computer
  # setwd("~/Desktop/NSF map files/TerraClimate")


#### TERRA DATA ####
data.files <- list.files("data/", pattern="ppt", full.names = T)


for(i in 1:length(data.files)){
  datai <- raster::brick(data.files[i])
  
  datai <- raster::crop(datai,
                             raster::extent(-83, -76, 7, 10))
  
  if(i==1){
         dataAll <- sum(datai)
         }
  if(i>1){
         dataAll <- sum(dataAll,sum(datai))
         }
}

dataAvg <- dataAll/length(data.files)

raster::plot(dataAvg)


#### CHELSA DATA ####
extentPanama <- raster::extent(-83, -76, 7, 10)

meanPrec <- raster::raster("CHELSA_bio10_12.tif")

meanPrecPanama <- raster::crop(meanPrec, extentPanama)

#### ACP DATA ####
  metLocs <- read.csv("metStationLocations.csv", encoding = "UTF-8")
  metDataLong <- read.csv("monthlyPrecipData.csv", header = T)

  # Only keep sites with precip data
  metLocs <- metLocs[metLocs$STRI.Name %in% names(metDataLong),]
  metLocs$STRI.Name <- as.character(metLocs$STRI.Name)

# Tidy STRI precip data
  yrs <- 2010:2018
  mos <- 1:12
  mosCh <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  mosDec <- round((mos-1)/12,2)
  dates <- paste(rep(yrs, each = length(mos)), rep(mos, length(yrs)), sep = "_")
  Yr.Mon <- c(rep(yrs, each = length(mos))+rep(mosDec, length(yrs)))
  datesCh <- paste0(rep(yrs, each = length(mos)), rep(mosCh, length(yrs)))
  
  metData <- data.frame(station = rep(unique(metLocs$STRI.Name), length(dates)),
                        date = rep(dates, each = length(unique(metLocs$STRI.Name))),
                        Yr.Mon = rep(Yr.Mon, each = length(unique(metLocs$STRI.Name))),
                        rainACP = NA,
                        rainSat = NA)
  metData$date <- as.character(metData$date)
  metData$station <- as.character(metData$station)

# Loop through each date to get ACP data
  for(i in 1:length(Yr.Mon)){
    for(j in 1:length(metLocs$STRI.Name)){
      metData[metData$Yr.Mon==Yr.Mon[i] & metData$station == metLocs$STRI.Name[j], "rainACP"] <- metDataLong[metDataLong$Yr.Mon==Yr.Mon[i],metLocs$STRI.Name[j]] 
    }
  }
  

  metData$mon <- round((metData$Yr.Mon-floor(metData$Yr.Mon))*12)+1
  
  siteAvg <- data.frame(station = unique(metData$station),
                        rainACP = NA,
                        rainSat = NA)
  
  for(i in 1:length(siteAvg$station)){
    siteData <- metData[metData$station==siteAvg$station[i],]
    
    rainACP_mo <- aggregate(siteData[!(siteData$rainACP==-9),"rainACP"], 
                            by = list(siteData[!(siteData$rainACP==-9),"mon"]),
                            FUN = "mean",
                            na.rm = T)
    
    siteAvg[i,"rainACP"] <- sum(rainACP_mo)
    
    # rainSat_mo <- aggregate(siteData[!(siteData$rainACP==-9),"rainSat"], 
    #                     by = list(siteData[!(siteData$rainACP==-9),"mon"]),
    #                     FUN = "mean",
    #                     na.rm = T)
    # 
    # siteAvg[i,"rainSat"] <- sum(rainSat_mo)
  }
  siteAvg <- merge(siteAvg, metLocs, by.x = "station", by.y = "STRI.Name", all.y = F)
  siteAvg$station <- as.character(siteAvg$station)
  
  # Manually add line for San Lorenza
  siteAvg <- rbind(siteAvg,
                   c("SANLORENZO","3300",NA,NA,NA,NA,9.28087,-79.9747))
  
  siteAvg$LAT_N <- as.numeric(siteAvg$LAT_N)
  siteAvg$LONG_W <- as.numeric(siteAvg$LONG_W)
  siteAvg$rainACP <- as.numeric(siteAvg$rainACP)
  
  siteAvg$rainTerra <- raster::extract(x=dataAvg, y=siteAvg[,c("LONG_W","LAT_N")])
  siteAvg$rainChlsa <- raster::extract(x=meanPrec, y=siteAvg[,c("LONG_W","LAT_N")])
  
  siteAvg$terraDev <- siteAvg$rainACP-siteAvg$rainTerra
  siteAvg$chlsaDev <- siteAvg$rainACP-siteAvg$rainChlsa
  
  siteAvg$terraCol <- NA
  siteAvg[siteAvg$terraDev>0 & !is.na(siteAvg$terraDev),"terraCol"] <- "blue"
  siteAvg[siteAvg$terraDev<0 & !is.na(siteAvg$terraDev),"terraCol"] <- "red"
  
  siteAvg$chlsaCol <- NA
  siteAvg[siteAvg$chlsaDev>0 & !is.na(siteAvg$chlsaDev),"chlsaCol"] <- "blue"
  siteAvg[siteAvg$chlsaDev<0 & !is.na(siteAvg$chlsaDev),"chlsaCol"] <- "red"
  
  summary(lm(rainACP~rainTerra, data=siteAvg))
  rmseTerra <- (mean(siteAvg$terraDev[!is.na(siteAvg$chlsaDev)]^2))^0.5
  rmseChlsa <- (mean(siteAvg$chlsaDev^2,na.rm=T))^0.5
  
  
#### PLOT ####  
  extentPanama <- raster::extent(c(-81,-79,8.5,10))
  meanPrecTerra <- raster::crop(dataAvg, extentPanama)
  meanPrecChlsa <- raster::crop(meanPrec, extentPanama)
  
  dataMin <- 1000
  dataMax <- 5000
  colBreaks <- seq(floor(dataMin),ceiling(dataMax),1)
  
  pdf("Precip figure_TerraVsChelsa.PDF", width=8, height=8)
  par(mfrow=c(2,2), oma=c(1,1,1,2))
  
  plot(rainACP~rainChlsa, data=siteAvg, pch=20,
       xlab="Satellite/model MAP (CHELSA)",
       ylab="ACP met station MAP",
       xlim=c(1500,5500),ylim=c(1500,5500))
  points(rainACP~rainChlsa, data=siteAvg[siteAvg$station=="SANLORENZO",],
         pch=20,col="lightblue",cex=1.5)
  abline(a=0,b=1)
  
  plot(rainACP~rainTerra, data=siteAvg, pch=20,
       xlab="Satellite/model MAP (TerraClimate)",
       ylab=NA,
       xlim=c(1500,5500),ylim=c(1500,5500))
  points(rainACP~rainTerra, data=siteAvg[siteAvg$station=="SANLORENZO",],
         pch=20,col="lightblue",cex=1.5)
  abline(a=0,b=1)

  raster::plot(meanPrecChlsa,
               main = 'CHELSA (mm/yr)',
               breaks = colBreaks,
               col=terrain.colors(length(colBreaks)-2,rev=T))
  points(LAT_N~LONG_W, data=siteAvg,
         cex=abs(siteAvg$chlsaDev/350)/2,
         col=siteAvg$chlsaCol)
  legend(c("ACP station > CHELSA",
           "ACP station < CHELSA"),
         x=-81,y=10,
         col=c("blue","red"),
         pch=1,
         bty="n")
  
  raster::plot(meanPrecTerra,
               main = 'Terra (mm/yr)',
                breaks = colBreaks,
               col=terrain.colors(length(colBreaks)-2,rev=T))
  points(LAT_N~LONG_W, data=siteAvg,
         cex=abs(siteAvg$terraDev/350)/2,
         col=siteAvg$terraCol)
  legend(c("ACP station > Terra",
           "ACP station < Terra"),
         x=-81,y=10,
         col=c("blue","red"),
         pch=1,
         bty="n")
  
  dev.off()
  
  