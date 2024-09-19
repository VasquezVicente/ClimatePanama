#Data visualization of analysis of cliamte panama, reason to migrate to this code is for simplicity
library(tdr)
library(data.table)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(gridExtra)
library(raster)
#functions
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
#import main data set
dfprecip<- read.csv("dfprecip.csv")
dfprecip<- dfprecip[,-c(1)]
setDT(dfprecip)
#dcast the variable and the site, is it worth addint the number of years that the reanalyis derives its estimate?
dfprecip <- dcast(dfprecip, site + variable ~ source, value.var = "value")

annual<-dfprecip %>% subset(variable=="annualPrecip")
jfma<-dfprecip %>% subset(variable=="jfmaPrecip")


#error tables for annual and jfma of 79 sites (acp+stri met)
{
chelsa1<- as.data.frame(errors(c(annual$chelsa1,annual$chelsa2),c(annual$acp,annual$acp),functions = c("MAE", "MBE", "RMSE", "randError",
                                                                                                       "proprandError", "propRMSE", "propMBE","propMAE")))
chelsa1<-rename(chelsa1,chelsa1=ls(chelsa1))
chelsa1$stats<-rownames(chelsa1)
rownames(chelsa1)<- NULL
chelsa2<- as.data.frame(errors(annual$chelsa2, annual$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                        "proprandError", "propRMSE", "propMBE","propMAE")))
chelsa2<-rename(chelsa2,chelsa2=ls(chelsa2))
chelsa2$stats<-rownames(chelsa2)
rownames(chelsa2)<- NULL
pbcorChelsa1<- as.data.frame(errors(annual$pbcorChelsa1, annual$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                                  "proprandError", "propRMSE", "propMBE","propMAE")))
pbcorChelsa1<-rename(pbcorChelsa1,pbcorChelsa1=ls(pbcorChelsa1))
pbcorChelsa1$stats<-rownames(pbcorChelsa1)
rownames(pbcorChelsa1)<- NULL
pbcorChp<- as.data.frame(errors(annual$pbcorChp, annual$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                          "proprandError", "propRMSE", "propMBE","propMAE")))
pbcorChp<-rename(pbcorChp,pbcorChp=ls(pbcorChp))
pbcorChp$stats<-rownames(pbcorChp)
rownames(pbcorChp)<- NULL
pbcorWorldclim<- as.data.frame(errors(annual$pbcorWorldclim, annual$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                                      "proprandError", "propRMSE", "propMBE","propMAE")))
pbcorWorldclim<-rename(pbcorWorldclim,pbcorWorldclim=ls(pbcorWorldclim))
pbcorWorldclim$stats<-rownames(pbcorWorldclim)
rownames(pbcorWorldclim)<- NULL
terra<- as.data.frame(errors(annual$terra, annual$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                    "proprandError", "propRMSE", "propMBE","propMAE")))
terra<-rename(terra,terra=ls(terra))
terra$stats<-rownames(terra)
rownames(terra)<- NULL
terra2<- as.data.frame(errors(annual$terra2, annual$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                      "proprandError", "propRMSE", "propMBE","propMAE")))
terra2<-rename(terra2,terra2=ls(terra2))
terra2$stats<-rownames(terra2)
rownames(terra2)<- NULL
worldclim<- as.data.frame(errors(annual$worldclim, annual$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                            "proprandError", "propRMSE", "propMBE","propMAE")))
worldclim<-rename(worldclim,worldclim=ls(worldclim))
worldclim$stats<-rownames(worldclim)
rownames(worldclim)<- NULL
chp<- as.data.frame(errors(annual$chp, annual$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                "proprandError", "propRMSE", "propMBE","propMAE")))
chp<-rename(chp,chp=ls(chp))
chp$stats<-rownames(chp)
rownames(chp)<- NULL

stats <- list(worldclim,terra, terra2,chelsa1, chelsa2, pbcorChelsa1,pbcorChp,pbcorWorldclim,chp)
statsfinal<-stats %>% reduce(full_join, by='stats')%>% select("stats","worldclim","terra", "terra2","chelsa1", "chelsa2", "pbcorChelsa1","pbcorChp","pbcorWorldclim","chp")

statsfinal %>%
  kbl(caption = "Annual precipitation, predicted vs observed in ACP data (n=79)") %>%
  kable_styling()

chelsa1<- as.data.frame(errors(c(jfma$chelsa1,jfma$chelsa2),c(jfma$acp,jfma$acp),functions = c("MAE", "MBE", "RMSE", "randError",
                                                                     "proprandError", "propRMSE", "propMBE","propMAE")))
chelsa1<-rename(chelsa1,chelsa1=ls(chelsa1))
chelsa1$stats<-rownames(chelsa1)
rownames(chelsa1)<- NULL
chelsa2<- as.data.frame(errors(jfma$chelsa2, jfma$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                        "proprandError", "propRMSE", "propMBE","propMAE")))
chelsa2<-rename(chelsa2,chelsa2=ls(chelsa2))
chelsa2$stats<-rownames(chelsa2)
rownames(chelsa2)<- NULL
pbcorChelsa1<- as.data.frame(errors(jfma$pbcorChelsa1, jfma$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                        "proprandError", "propRMSE", "propMBE","propMAE")))
pbcorChelsa1<-rename(pbcorChelsa1,pbcorChelsa1=ls(pbcorChelsa1))
pbcorChelsa1$stats<-rownames(pbcorChelsa1)
rownames(pbcorChelsa1)<- NULL
pbcorChp<- as.data.frame(errors(jfma$pbcorChp, jfma$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                        "proprandError", "propRMSE", "propMBE","propMAE")))
pbcorChp<-rename(pbcorChp,pbcorChp=ls(pbcorChp))
pbcorChp$stats<-rownames(pbcorChp)
rownames(pbcorChp)<- NULL
pbcorWorldclim<- as.data.frame(errors(jfma$pbcorWorldclim, jfma$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                        "proprandError", "propRMSE", "propMBE","propMAE")))
pbcorWorldclim<-rename(pbcorWorldclim,pbcorWorldclim=ls(pbcorWorldclim))
pbcorWorldclim$stats<-rownames(pbcorWorldclim)
rownames(pbcorWorldclim)<- NULL
terra<- as.data.frame(errors(jfma$terra, jfma$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                        "proprandError", "propRMSE", "propMBE","propMAE")))
terra<-rename(terra,terra=ls(terra))
terra$stats<-rownames(terra)
rownames(terra)<- NULL
terra2<- as.data.frame(errors(jfma$terra2, jfma$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                        "proprandError", "propRMSE", "propMBE","propMAE")))
terra2<-rename(terra2,terra2=ls(terra2))
terra2$stats<-rownames(terra2)
rownames(terra2)<- NULL
worldclim<- as.data.frame(errors(jfma$worldclim, jfma$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                      "proprandError", "propRMSE", "propMBE","propMAE")))
worldclim<-rename(worldclim,worldclim=ls(worldclim))
worldclim$stats<-rownames(worldclim)
rownames(worldclim)<- NULL
chp<- as.data.frame(errors(jfma$chp, jfma$acp,functions = c("MAE", "MBE", "RMSE", "randError",
                                                                            "proprandError", "propRMSE", "propMBE","propMAE")))
chp<-rename(chp,chp=ls(chp))
chp$stats<-rownames(chp)
rownames(chp)<- NULL

stats <- list(worldclim,terra, terra2,chelsa1, chelsa2, pbcorChelsa1,pbcorChp,pbcorWorldclim,chp)
statsfinal<-stats %>% reduce(full_join, by='stats')%>% select("stats","worldclim","terra", "terra2","chelsa1", "chelsa2", "pbcorChelsa1","pbcorChp","pbcorWorldclim","chp")

statsfinal_jfma %>%
  kbl(caption = "JFMA precipitation, predicted vs observed in ACP data (n=79)") %>%
  kable_styling()
}
## correlation plots 18 plots total, doing images for power point
{
a<- thisplotfn(annual$acp, annual$terra, "ACP/STRI meteorological stations","Terra Climatology 1961-1990","Mean Annual Precipitation(mm)" )
b<- thisplotfn(jfma$acp, jfma$chp, "ACP/STRI meteorological stations","CHPclim v.1.0","Mean Jan-April Precipitation(mm)" )
c<- thisplotfn(annual$acp, annual$terra2, "ACP/STRI meteorological stations","Terra Climatology 1981-2010","Mean Annual Precipitation(mm)" )
d<- thisplotfn(jfma$acp, jfma$terra2, "ACP/STRI meteorological stations","Terra Climatology 1981-2010","Mean Jan-April Precipitation(mm)" )

f<- thisplotfn(annual$acp, annual$chp, "ACP/STRI meteorological stations","CHPclim v.1.0","Mean Annual Precipitation(mm)" )
g<- thisplotfn(jfma$acp, jfma$chp, "ACP/STRI meteorological stations","CHPclim v.1.0","Mean Jan-April Precipitation(mm)" )
h<- thisplotfn(annual$acp, annual$worldclim, "ACP/STRI meteorological stations","Worldclim v.2.1 1970-2000.","Mean Annual Precipitation(mm)" )
i<- thisplotfn(jfma$acp, jfma$worldclim, "ACP/STRI meteorological stations","Worldclim v.2.1 1970-2000.","Mean Jan-April Precipitation(mm)" )

j<- thisplotfn(annual$acp, annual$chelsa1, "ACP/STRI meteorological stations","CHELSA v.1.2","Mean Annual Precipitation(mm)" )
k<- thisplotfn(jfma$acp, jfma$chelsa1, "ACP/STRI meteorological stations","CHELSA v.1.2","Mean Jan-April Precipitation(mm)" )
l<- thisplotfn(annual$acp, annual$chelsa2, "ACP/STRI meteorological stations","CHELSA v.2.1 ","Mean Annual Precipitation(mm)" )
m<- thisplotfn(jfma$acp, jfma$chelsa2, "ACP/STRI meteorological stations","CHELSA v.2.1 ","Mean Jan-April Precipitation(mm)" )

n<- thisplotfn(annual$acp, annual$pbcorChelsa1, "ACP/STRI meteorological stations","Pbcor CHELSA v.1.2","Mean Annual Precipitation(mm)" )
o<- thisplotfn(jfma$acp, jfma$pbcorChelsa1, "ACP/STRI meteorological stations","Pbcor CHELSA v.1.2","Mean Jan-April Precipitation(mm)" )
p<- thisplotfn(annual$acp, annual$pbcorChp, "ACP/STRI meteorological stations","Pbcor CHPclim v.1.0","Mean Annual Precipitation(mm)" )
q<- thisplotfn(jfma$acp, jfma$pbcorChp, "ACP/STRI meteorological stations","Pbcor CHPclim v.1.0","Mean Jan-April Precipitation(mm)" )
r<- thisplotfn(annual$acp, annual$pbcorWorldclim, "ACP/STRI meteorological stations","PbcorWorldclim v.2.1","Mean Annual Precipitation(mm)" )
s<- thisplotfn(jfma$acp, jfma$pbcorWorldclim, "ACP/STRI meteorological stations","Pbcor Worldclim v.2.1","Mean Jan-April Precipitation(mm)" )

first<- grid.arrange(a,b,c,d, nrow=2)
ggsave( "output/graphs/scatter1.jpg",first)

second<- grid.arrange(f,g,h,i, nrow=2)
ggsave( "output/graphs/scatter2.jpg",second)

third<- grid.arrange(j,k,l,m, nrow=2)
ggsave( "output/graphs/scatter3.jpg",third)

fourth<- grid.arrange(n,o,p,q,r,s, nrow=3)
ggsave( "output/graphs/scatter4.jpg",fourth)
}
##maps with cex, must be modified
    ##import maps first
    MAPDIR <- "output/maps/pan_prec_maps/"
    models_maps<- list.files(MAPDIR)
    models_maps<- paste0(MAPDIR,models_maps)
    models_list<- lapply(models_maps, raster) 
  # extent and remove water
    extentMap  <- raster::extent(c(-80.7,-79.2, 8.2,9.7))
    models_crop<-lapply(X=models_list, FUN = crop, y= extentMap)
    
    notwater<- aggregate(models_crop[[17]], fact=6)
    notwater2<- !is.na(models_crop[[17]])
    models_crop[[1]]<- notwater*models_crop[[1]]
    models_crop[[2]]<- notwater*models_crop[[2]]
    models_crop[[16]]<- notwater2*models_crop[[16]]
    models_crop[[18]]<- notwater2*models_crop[[18]]
   
    
    
    rangeannprec1 <- cellStats(useprec1,range,na.rm=T)
    rangeannprec2 <- cellStats(useprec2,range,na.rm=T)
    rangeannprec <- range(c(rangeannprec1,rangeannprec2))
    colBreaksap <- seq(floor(rangeannprec[1]),ceiling(rangeannprec[2]),1)
    
