library(raster)
library(rgdal)
library(ncdf4)

CHELSADIR <- "data_reanalysis/CHELSA"
OUTMAPDIR <- "output/maps/pan_prec_maps"

panlatlon <-  c(-84,-76, 6, 10) # wide latitude and longitude limits of area to keep 
## Download CHELSA data ####

if (! dir.exists(CHELSADIR)) 
  dir.create(CHELSADIR)

# function to download and crop CHELSA data

dl_chelsa <- function(
    file,
    dir_chelsa,
    dir_local = CHELSADIR,
    rbox = NULL,
    version = 2
) {
  
  if (version==2) url <- paste(
    "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL",
    dir_chelsa, file,
    sep = "/"
  ) else url <- paste(  # Helene added
    "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1",
    dir_chelsa, file,
    sep = "/"
  )# version 1 
  

  destfile <- paste(dir_local, file, sep = "/")
  
  # download raster from url (if it doesn't already exist in the folder)
  if (!file.exists(destfile)) {
    utils::download.file(url, destfile, method = "curl")  
    
    ## crop to reduce memory usage
    
    if (!is.null(rbox)) {
      rst <- raster::raster(destfile)
      rst <- raster::crop(rst, raster::extent(rbox))
      raster::writeRaster(rst, file = destfile, overwrite = TRUE) 
    }
  }
}

## Potential evapotranspiration in kg/m2/month CHELSA 2

for (i in c(paste0("0", 1:9), 10:12)) {
  dl_chelsa(file = paste0("CHELSA_pet_penman_", i, "_1981-2010_V.2.1.tif"), 
            dir_chelsa = "climatologies/1981-2010/pet", 
            rbox = panlatlon)
}


## precipitation amount in kg/m2/month CHELSA 2 

for (i in c(paste0("0", 1:9), 10:12)) {
  dl_chelsa(file = paste0("CHELSA_pr_", i, "_1981-2010_V.2.1.tif"), 
            dir_chelsa = "climatologies/1981-2010/pr", 
            rbox = panlatlon)
}




## make climate water deficit map ####

monthly_cwd2 <- lapply(c(paste0("0", 1:9), 10:12), function(i) {
  
  # get rasters
  pr <- raster::raster(paste0(CHELSADIR, "/CHELSA_pr_", i, "_1981-2010_V.2.1.tif"))
  pet <- raster::raster(paste0(CHELSADIR, "/CHELSA_pet_penman_", i, "_1981-2010_V.2.1.tif"))
  
  # difference of precipitation and potential evapotranspiration
  diff <- pr - pet
  
  # only keep negative value (= deficit)
  diff[diff>0,] <- 0
  
  return(diff)
})


monthly_cwd2 <- raster::stack(monthly_cwd2)

# function for max cumulative water deficit = minimum value of total water
# deficit of months with consecutive prec - pet < 0

max_cum_sum <- function(x) {
  
  # if all values are negative: just the total sum: also deals with NAs (if any
  # NA: sum will return NA)
  if (all(x < 0) | any(is.na(x))) 
    return(sum(x))
  
  else {
    # repeat x (to have also consecutive months not in the same calendar year)
    # and split it every time there's a null or positive value (no water
    # deficit)
    rl <- rle(rep(x, 2) < 0)
    xcut <- split(rep(x, 2), rep(seq_len(length(rl$length)), rl$length))
    # then take the minimum sum(cwd) of all these consecutive months with cwd < 0
    return(min(sapply(xcut, sum)))
  }
}

cwd2 <- raster::calc(monthly_cwd2, sum)
mcwd2 <- raster::calc(monthly_cwd2, max_cum_sum)
rm(monthly_cwd2)

# crop to smaller Panama extent to make better maps 
panlatlon2 <- c(-83, -77, 7, 10) # Helene revised 
cwd2cut <- raster::crop(cwd2, raster::extent(panlatlon2))
mcwd2cut <- raster::crop(mcwd2, raster::extent(panlatlon2))

writeRaster(cwd2cut, file = paste0(OUTMAPDIR,"/pan-cwd2-maps.tif"),overwrite=T)  
writeRaster(mcwd2cut, file = paste0(OUTMAPDIR,"/pan-mcwd2-maps.tif"),overwrite=T)  

pdf(paste0(OUTMAPDIR,"/pan_map_cwd2.pdf"),width=10,height=6)
raster::plot(cwd2cut,main="CHELSA 2.1 CWD annual")
dev.off()

pdf(paste0(OUTMAPDIR,"/pan_map_mcwd2.pdf"),width=10,height=6)
raster::plot(mcwd2cut,main="CHELSA 2.1 MCWD")
dev.off()

monthly_pet2 <- lapply(c(paste0("0", 1:9), 10:12), function(i) {
  pet <- raster::raster(paste0(CHELSADIR,"/CHELSA_pet_penman_", i, "_1981-2010_V.2.1.tif"))
  return(pet)
})
monthly_pet2 <- raster::stack(monthly_pet2)
pet_ann2 <- raster::calc(monthly_pet2,sum)
rm(monthly_pet2)

writeRaster(pet_ann2, file = paste0(OUTMAPDIR,"/pet2-maps.tif"),overwrite=T)
pet_ann2cut <- raster::crop(pet_ann2, raster::extent(panlatlon2))
writeRaster(pet_ann2cut, file = paste0(OUTMAPDIR,"/pan-pet2-maps.tif"),overwrite=T)  

pdf(paste0(OUTMAPDIR,"/pan_map_petann2.pdf"),width=10,height=6)
raster::plot(pet_ann2cut,main="CHELSA 2.1 PET annual")
dev.off()


monthly_prec2 <- lapply(c(paste0("0", 1:9), 10:12), function(i) {
  pr <- raster::raster(paste0(CHELSADIR,"/CHELSA_pr_", i, "_1981-2010_V.2.1.tif"))
  return(pr)
})

monthly_prec2 <- raster::stack(monthly_prec2)
precip_ann2 <- raster::calc(monthly_prec2,sum)
precip_jfma2 <- raster::calc(monthly_prec2[[1:4]],sum)
rm(monthly_prec2)

# turn values over oceans into NA so map looks better
notwater<- pet_ann2>=0   # dimensions 480 x 960 x 1
notwatercut <- raster::crop(notwater, raster::extent(panlatlon2))

writeRaster(precip_ann2, file = paste0(OUTMAPDIR,"/precann2-maps.tif"),overwrite=T)
writeRaster(precip_jfma2, file = paste0(OUTMAPDIR,"/precjfma2-maps.tif"),overwrite=T)

precip_ann2cut <- raster::crop(precip_ann2, raster::extent(panlatlon2))
precip_jfma2cut <- raster::crop(precip_jfma2, raster::extent(panlatlon2))

writeRaster(precip_ann2cut, file = paste0(OUTMAPDIR,"/pan-precann2-maps.tif"),overwrite=T)
writeRaster(precip_jfma2cut, file = paste0(OUTMAPDIR,"/pan-precjfma2-maps.tif"),overwrite=T)

precip_ann2cut <- precip_ann2cut*notwatercut
precip_jfma2cut <- precip_jfma2cut*notwatercut

pdf(paste0(OUTMAPDIR,"/pan_map_precipann2.pdf"),width=10,height=6)
raster::plot(precip_ann2cut,main="CHELSA 2.1 precipitation annual")
dev.off()

pdf(paste0(OUTMAPDIR,"/pan_map_precipJFMA2.pdf"),width=10,height=6)
raster::plot(precip_jfma2cut, main="CHELSA 2.1 precipitation JFMA")
dev.off()



# do the same thing for CHELSA v1

## Potential evapotranspiration in kg/m2/month CHELSA 1
for (i in 1:12) {
  dl_chelsa(file = paste0("CHELSA_pet_", i, "_1979-2013.tif"), 
            dir_chelsa = "exchelsa/pet", 
            rbox = panlatlon,version=1)
}

## precipitation amount in kg/m2/month CHELSA 1 
for (i in c(paste0("0", 1:9), 10:12)) {
  dl_chelsa(file = paste0("CHELSA_prec_", i, "_V1.2_land.tif"), 
            dir_chelsa = "climatologies/prec", 
            rbox =panlatlon,version=1)
}

## make climate water deficit map ####

# ignore warning
monthly_cwd1 <- lapply(1:12, function(i) {
  
  # get rasters
  pr <- raster::raster(paste0(CHELSADIR,"/CHELSA_prec_", ifelse(i<10,"0",""),i, "_V1.2_land.tif"))
  pet <- raster::raster(paste0(CHELSADIR,"/CHELSA_pet_", i, "_1979-2013.tif"))
  
  # difference of precipitation and potential evapotranspiration
  diff <- pr - pet
  
  # only keep negative value (= deficit)
  diff[diff>0,] <- 0
  
  return(diff)
})


monthly_cwd1 <- raster::stack(monthly_cwd1)

cwd1 <- raster::calc(monthly_cwd1, sum)
mcwd1 <- raster::calc(monthly_cwd1, max_cum_sum)
rm(monthly_cwd1)

writeRaster(cwd1, file = paste0(OUTMAPDIR,"/cwd1-maps.tif"),overwrite=T)  
writeRaster(mcwd1, file = paste0(OUTMAPDIR,"/mcwd1-maps.tif"),overwrite=T)

cwd1cut <- raster::crop(cwd1, raster::extent(panlatlon2))
mcwd1cut <- raster::crop(mcwd1, raster::extent(panlatlon2))

writeRaster(cwd1cut, file = paste0(OUTMAPDIR,"/pan-cwd1-maps.tif"),overwrite=T)  
writeRaster(mcwd1cut, file = paste0(OUTMAPDIR,"/pan-mcwd1-maps.tif"),overwrite=T)  

cwd1acut <- cwd1cut*notwatercut
mcwd1acut <- mcwd1cut*notwatercut

pdf(paste0(OUTMAPDIR,"/pan_map_cwd1.pdf"),width=10,height=6)
raster::plot(cwd1acut, main="CHELSA 1.2 CWD annual")
dev.off()

pdf(paste0(OUTMAPDIR,"/pan_map_mcwd1.pdf"),width=10,height=6)
raster::plot(mcwd1acut, main="CHELSA 1.2 MCWD")
dev.off()


monthly_pet1 <- lapply(c(1:12), function(i) {
  pet <- raster::raster(paste0(CHELSADIR,"/CHELSA_pet_", i, "_1979-2013.tif"))
  return(pet)
})
monthly_pet1 <- raster::stack(monthly_pet1)
pet_ann1 <- raster::calc(monthly_pet1,sum)
rm(monthly_pet1)

writeRaster(pet_ann1, file = paste0(OUTMAPDIR,"/pet1-maps.tif"),overwrite=T)  
pet_ann1cut <- raster::crop(pet_ann1, raster::extent(panlatlon2))
writeRaster(pet_ann1cut, file = paste0(OUTMAPDIR,"/pan-pet1-maps.tif"),overwrite=T)  

pet_ann1acut <- pet_ann1cut*notwatercut
pdf(paste0(OUTMAPDIR,"/pan_map_petann1.pdf"),width=10,height=6)
raster::plot(pet_ann1acut,main="CHELSA 1.2 PET annual")
dev.off()



monthly_prec1 <- lapply(c(paste0("0", 1:9), 10:12), function(i) {
  pr <- raster::raster(paste0(CHELSADIR,"/CHELSA_prec_", i, "_V1.2_land.tif"))
  return(pr)
})

monthly_prec1 <- raster::stack(monthly_prec1)
precip_ann1 <- raster::calc(monthly_prec1,sum)
precip_jfma1 <- raster::calc(monthly_prec1[[1:4]],sum)
rm(monthly_prec1)


writeRaster(precip_ann1, file = paste0(OUTMAPDIR,"/precann1-maps.tif"),overwrite=T)  
writeRaster(precip_jfma1, file = paste0(OUTMAPDIR,"/precjfma1-maps.tif"),overwrite=T)  

precip_ann1cut <- raster::crop(precip_ann1, raster::extent(panlatlon2))
precip_jfma1cut <- raster::crop(precip_jfma1, raster::extent(panlatlon2))

writeRaster(precip_ann1cut, file = paste0(OUTMAPDIR,"/pan-precann1-maps.tif"),overwrite=T)  
writeRaster(precip_jfma1cut, file = paste0(OUTMAPDIR,"/pan-precjfma1-maps.tif"),overwrite=T)  

precip_ann1acut <- precip_ann1cut*notwatercut
precip_jfma1acut <- precip_jfma1cut*notwatercut

pdf(paste0(OUTMAPDIR,"/pan_map_precipann1.pdf"),width=10,height=6)
raster::plot(precip_ann1acut,main="CHELSA 1.2 precipitation annual")
dev.off()

pdf(paste0(OUTMAPDIR,"/pan_map_precipJFMA1.pdf"),width=10,height=6)
raster::plot(precip_jfma1acut, main="CHELSA 1.2 precipitation JFMA")
dev.off()


# Precipitation Bias Correction (PB COR) dataset for CHELSA V1.2
# this is only 0.05 degree resolution (not 0.01 degree resolution like the other datasets)
# these dataare in nc format; haven't figured out how to use these yet
fnpbcor <- "C:/Users/mullerh/Dropbox (Personal)/Carbon/Climate/SatelliteVsGround comparison/PBCOR_V1.0/CHELSA_V12.nc"
ncpbcor <- nc_open(fnpbcor)
pbcorann <- brick(fnpbcor,varname="corr_P_annual")
pbcoranncut <- raster::crop(pbcorann, raster::extent(panlatlon2))
pdf(paste0(OUTMAPDIR,"/pan_map_precipannpbcorch1.pdf"),width=10,height=6)
raster::plot(pbcoranncut,main="PBCOR CHELSA1.2 annual precipitation")
dev.off()

# still need to figure out how to sum JanFebMarApr precip - this is not working 
pbcormon <- brick(fnpbcor,varname="corr_P_monthly")
pbcormoncut <- raster::crop(pbcormon, raster::extent(panlatlon2))
pbcorjfmacut <- 0*pbcoranncut  # create a blank raster of the right dimensions
#pbcorjfmacut[1:60,1:120,1] <- pbcormoncut[1:60,1:120,1]
#pbcorjfmacut <- pbcormoncut[,,1] + pbcormoncut[,,2] + pbcormoncut[,,3]+ pbcormoncut[,,4]  # I AM HERE
#pbcorjfmacut <- raster::as.raster(pbcorjfmacut,nrow=80,ncol=160)
#pdf(paste0(OUTMAPDIR,"pan_map_precipjfmapbcorch1.pdf"),width=10,height=6)
#raster::plot(pbcorjfmacut,main="PBCOR CHELSA1.2 JFMA precipitation")
#dev.off()

writeRaster(pbcorann, file = paste0(OUTMAPDIR,"/precann3-maps.tif"),overwrite=T)  
writeRaster(pbcoranncut, file = paste0(OUTMAPDIR,"/pan=precann3-maps.tif"),overwrite=T)  


# compare the datasets over region of interest in central Panama
rbox2 <- c(-80,-79.5, 8.8,9.5)

# compare for MCWD 
submcwd2 <- raster::crop(mcwd2, raster::extent(rbox2))
submcwd1 <- raster::crop(mcwd1, raster::extent(rbox2))
subdef <- submcwd1<=0 # becomes NA where submcwd1 isn't defined (over water)
submcwd2a <- submcwd2*subdef
pdf(paste0(OUTMAPDIR,"/pancent_map_MCWD12.pdf"),width=13,height=5)
par(mfrow=c(1,4))
raster::plot(submcwd1, main = "CHELSA 1.2 MCWD")
raster::plot(submcwd2a, main="CHELSA 2.1 MCWD (same NA)")
raster::plot(submcwd2, main="CHELSA 2.1 MCWD (full)")

dev.off()


# compare for PET 
subpet1 <- raster::crop(pet_ann1, raster::extent(rbox2))
subpet2 <- raster::crop(pet_ann2, raster::extent(rbox2))
subpet2a <- subpet2*subdef
pdf(paste0(OUTMAPDIR,"/pancent_map_pet12.pdf"),width=10,height=5)
par(mfrow=c(1,3))
thiscol <- heat.colors(100)
raster::plot(subpet1, main = "CHELSA 1.2 PET",col=thiscol)
raster::plot(subpet2a, main="CHELSA 2.1 PET (same NA)",col=thiscol)
raster::plot(subpet2, main="CHELSA 2.1 PET (full)",col=thiscol)
dev.off()


# compare for annual precip 
subprecann1 <- raster::crop(precip_ann1, raster::extent(rbox2))
subprecann2 <- raster::crop(precip_ann2, raster::extent(rbox2))
subprecann2a <- subprecann2*subdef
subprecann3 <- raster::crop(pbcoranncut, raster::extent(rbox2))

pdf(paste0(OUTMAPDIR,"/pancent_map_precipann12.pdf"),width=13,height=5)
par(mfrow=c(1,4))
raster::plot(subprecann1, main = "CHELSA 1.2 Annual Precip")
raster::plot(subprecann2a, main="CHELSA 2.1 Annual Precip (same NA)")
raster::plot(subprecann2, main="CHELSA 2.1 Annual Precip (full)")
raster::plot(subprecann3,main="PBCOR CHELSA1.2 Annual Precip")
dev.off()

# compare for JFMA precip 
subprecjfma1 <- raster::crop(precip_jfma1, raster::extent(rbox2))
subprecjfma2 <- raster::crop(precip_jfma2, raster::extent(rbox2))
subprecjfma2a <- subprecjfma2*subdef
pdf(paste0(OUTMAPDIR,"/pancent_map_precipjfma12.pdf"),width=10,height=5)
par(mfrow=c(1,3))
raster::plot(subprecjfma1, main = "CHELSA 1.2 JFMA Precip")
raster::plot(subprecjfma2a, main="CHELSA 2.1 JFMA Precip (same NA)")
raster::plot(subprecjfma2, main="CHELSA 2.1 JFMA Precip (full)")
dev.off()




#world clim 
{
url<-"https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_prec.zip"
download.file(url = url,destfile = "data_reanalysis/WORLDCLIM/wc2.1_30s_prec.zip")
outDir<-"C:/Users/VasquezV/OneDrive - Smithsonian Institution/ClimatePanama/data_reanalysis/WORLDCLIM" 
unzip("data_reanalysis/WORLDCLIM/wc2.1_30s_prec.zip",exdir=outDir)
panlatlon <-  c(-84,-76, 6, 10)
worldclim<- list.files(path = "data_reanalysis/WORLDCLIM", pattern = '.tif',all.files=TRUE, full.names=FALSE)
worldclim_raster<- lapply(paste0("data_reanalysis/WORLDCLIM/",worldclim), raster)

worldclim_stack<- raster::stack(worldclim_raster)
worldclim_pan<-raster::crop(worldclim_stack, panlatlon)
worldclim_annual<-raster::calc(worldclim_pan,sum)

jfma_worldclim<-worldclim_raster[c(1,2,3,4)]
worldclim_stack_jfma<- raster::stack(jfma_worldclim)
worldclim_pan_jfma<-raster::crop(worldclim_stack_jfma, panlatlon)
worldclim_jfma<-raster::calc(worldclim_pan_jfma,sum)

writeRaster(worldclim_annual,paste0(MAPDIR,"/prec_worldclim_annual.tif"),overwrite=TRUE)
writeRaster(worldclim_jfma,paste0(MAPDIR,"/prec_worldclim_jfma.tif"),overwrite=TRUE)
}
##pbcor
{
MAPDIR <- "output/maps"
CHELSADIR <- "data_reanalysis/CHELSA"
OUTMAPDIR <- "output/maps"
panlatlon <-  c(-84,-76, 6, 10)
  url<-"http://www.gloh2o.org/data/PBCOR_V1.0.zip"
  download.file(url = url,destfile = "data_reanalysis/PBCOR/PBCOR_V1.0.zip")
  outDir<-"data_reanalysis/PBCOR" 
  unzip("data_reanalysis/PBCOR/PBCOR_V1.0.zip",exdir=outDir)
  
  monthcorr<- raster::raster("data_reanalysis/PBCOR/CHELSA_V12.nc", varname="corr_fac_monthly")
  monthcorr<-raster::crop(monthcorr, panlatlon)
  
  annualcorr<- raster::raster("data_reanalysis/PBCOR/CHELSA_V12.nc", varname="corr_fac_annual")
  annualcorr<-raster::crop(annualcorr, panlatlon)
  
  prec1 <- raster::raster(paste0(MAPDIR,"/precann1-maps.tif"))
  precja1 <- raster::raster(paste0(MAPDIR,"/precjfma1-maps.tif"))
  
  ag1<- raster::aggregate(prec1, fact=6)
  final<- ag1*annualcorr
  writeRaster(final, paste0(MAPDIR,"/prec_pbcor_chelsa1_annual.tif"), overwrite=TRUE)  
  
  ag2<-raster::aggregate(precja1, fact=6)
  final2<- ag2*monthcorr
  writeRaster(final2, paste0(MAPDIR,"/prec_pbcor_chelsa1_jfma.tif"),overwrite=TRUE)  

  ###
  monthcorr<- raster::raster("data_reanalysis/PBCOR/CHPclim_V1.nc", varname="corr_fac_monthly")
  monthcorr<-raster::crop(monthcorr, panlatlon)
  
  annualcorr<- raster::raster("data_reanalysis/PBCOR/CHPclim_V1.nc", varname="corr_fac_annual")
  annualcorr<-raster::crop(annualcorr, panlatlon)
  
  prec1 <- raster::raster(paste0(MAPDIR,"/chp_annual.tif"))
  precja1 <- raster::raster(paste0(MAPDIR,"/chp_jfma.tif"))
  
  final<- prec1*annualcorr
  writeRaster(final, paste0(MAPDIR,"/prec_pbcor_chp_annual.tif"), overwrite=TRUE)  

  final2<- precja1*monthcorr
  writeRaster(final2, paste0(MAPDIR,"/prec_pbcor_chp_jfma.tif"), overwrite=TRUE)  
  
  
  
  monthcorr<- raster::raster("data_reanalysis/PBCOR/WorldClim_V2.nc", varname="corr_fac_monthly")
  monthcorr<-raster::crop(monthcorr, panlatlon)
  
  annualcorr<- raster::raster("data_reanalysis/PBCOR/CHPclim_V1.nc", varname="corr_fac_annual")
  annualcorr<-raster::crop(annualcorr, panlatlon)
  
  prec1 <- raster::raster(paste0(MAPDIR,"/prec_worldclim_annual.tif"))
  precja1 <- raster::raster(paste0(MAPDIR,"/prec_worldclim_jfma.tif"))
  
  prec1<-raster::aggregate(prec1, fact=6)
  final<- prec1*annualcorr
  writeRaster(final, paste0(MAPDIR,"/prec_pbcor_worldclim_annual.tif"), overwrite=TRUE)  
  
  precja1<-raster::aggregate(precja1, fact=6)
  final2<- precja1*monthcorr
  writeRaster(final2, paste0(MAPDIR,"/prec_pbcor_worldclim_jfma.tif"), overwrite=TRUE)  

  }
##terra compile  1958–2015
{

  url<-"http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/summaries/TerraClimate19611990_ppt.nc"
  destfile<-"data_reanalysis/TERRA/TerraClimate19611990_ppt.nc"
  options(download.file.method="libcurl", url.method="libcurl", timeout = 8000)
 download.file(url= url, destfile = destfile, method="libcurl") 
  

 TERRA2<-raster::stack("data_reanalysis/TERRA/TerraClimate19611990_ppt.nc")

crs(TERRA2)<-"+proj=longlat +ellps=WGS84 +no_defs"
panlatlon <-  c(-84,-76, 6, 10)
terra2<-raster::crop(TERRA2, panlatlon)
terra<- raster::calc(terra2, sum)


terra3<- raster::subset(terra2, 1:4)
terra3<- raster::calc(terra3, sum)

writeRaster(terra, paste0(OUTMAPDIR,"/prec_terra_annual.tif"), overwrite=TRUE)
writeRaster(terra3, paste0(OUTMAPDIR,"/prec_terra_jfma.tif"), overwrite=TRUE)



T1<-raster::stack("data_reanalysis/TERRA/TerraClimate19812010_ppt.nc")
crs(T1)<-"+proj=longlat +ellps=WGS84 +no_defs"
t1<- raster::crop(T1, panlatlon)
t10<- raster::calc(t1,sum)
t3<-raster::subset(t1, 1:4)
t30<-raster::calc(t3, sum)
writeRaster(t10, paste0(OUTMAPDIR,"/prec_terra2_annual.tif"), overwrite=TRUE)
writeRaster(t30, paste0(OUTMAPDIR,"/prec_terra2_jfma.tif"), overwrite=TRUE)
}
#chp climatologies with ocean
{
link<-"http://data.chc.ucsb.edu/products/CHPclim/50N-50S.with_oceans/monthly/sub/chpclim." 
format<-".tif"
dest1<- "chpclim_"
dir<-"data_reanalysis/CHP/"
for (i in c(paste0("0", 1:9), 10:12)) {
  download.file(url = paste0(link, i,format),destfile = paste0(dir,dest1,i,format), mode="wb")
}

list1<- list.files(path="data_reanalysis/CHP")
list2<- paste0(dir,list1)
raslist<- lapply(list2, raster)
raslist<- lapply(list2, raster)
chpcl_jfma<- raster::stack(raslist[[1]],raslist[[2]],raslist[[3]],raslist[[4]],raslist[[5]],raslist[[6]],raslist[[7]],raslist[[8]],raslist[[9]],raslist[[10]],raslist[[11]],raslist[[12]])
chpcl<-raster::crop(chpcl, panlatlon)
chp_annual<- raster::calc(chpcl, sum)
chpcl_jfma<- raster::stack(raslist[[1]],raslist[[2]],raslist[[3]],raslist[[4]])
chpcl_jfma<-raster::crop(chpcl_jfma, panlatlon)
chp_jfma<- raster::calc(chpcl_jfma, sum)

writeRaster(chp_annual, paste0(OUTMAPDIR,"/chp_annual.tif"))
writeRaster(chp_jfma, paste0(OUTMAPDIR,"/chp_jfma.tif"))
}