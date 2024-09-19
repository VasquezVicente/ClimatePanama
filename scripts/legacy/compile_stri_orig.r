#compile stri sites
library(dplyr)
library(readxl)
#load dir
STRIDIR<- "data_ground/met_data/STRI_data/"

stri_sites<-read_excel(paste0(STRIDIR,"stri_met.xlsx"))

#load all sites 

for (i in 1:nrow(stri_sites)){
  filename <- paste0(stri_sites$site[i])
  wd <- paste0(stri_sites$filename[i])
  assign(filename, read.csv(paste0(STRIDIR,wd)))
}

#celestino north
choke_good<- CELESTINON %>% filter(chk_note=="good")
choke_good$date<- as.Date(choke_good$date, format="%d/%m/%Y")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra), .groups = "rowwise")
daily_celestinon<- day_sum
daily_celestinon<- daily_celestinon %>% mutate(site="CELESTINON")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_celestinon<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra)`)" )
monthly_celestinon$site<- "CELESTINON"
monthly_celestinon$year<- substring(monthly_celestinon$yearmonth, 1, 4)
monthly_celestinon<- monthly_celestinon%>% filter(year!="2015"&year!="2022")
#celestino south 
choke_good<- CELESTINOS %>% filter(chk_note=="good")
choke_good$date<- as.Date(choke_good$date, format="%d/%m/%Y")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra), .groups = "rowwise")
daily_celestinos<- day_sum
daily_celestinos<- daily_celestinos %>% mutate(site="CELESTINOS")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_celestinos<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra)`)" )
monthly_celestinos$site<- "CELESTINOS"
monthly_celestinos$year<- substring(monthly_celestinos$yearmonth, 1, 4)
monthly_celestinos<- monthly_celestinos%>% filter(year!="2015"&year!="2022")
#BCIAVA
choke_good<- BCIAVA %>% filter(chk_note=="good")
choke_good$date<- as.Date(choke_good$date, format="%d/%m/%Y")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra), .groups = "rowwise")
daily_bciava<- day_sum
daily_bciava<- daily_bciava %>% mutate(site="BCIAVA")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_bciava<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra)`)" )
monthly_bciava$site<- "BCIAVA"
monthly_bciava$year<- substring(monthly_bciava$yearmonth, 1, 4)
monthly_bciava<- subset(monthly_bciava, year!="2018")
monthly_bciava<- monthly_bciava%>% filter(year!="2011"&year!="2022")
#bCI CLEAR ELECT
choke_good<- BCICLEAR %>% filter(chk_note=="good")
choke_good$date<- as.Date(choke_good$date, format="%d/%m/%Y")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra), .groups = "rowwise")
daily_bciclear<- day_sum
daily_bciclear<- daily_bciclear %>% mutate(site="BCICLEAR")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_bciclear<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra)`)" )
monthly_bciclear$site<- "BCICLEAR"
monthly_bciclear$year<- substring(monthly_bciclear$yearmonth, 1, 4)
monthly_bciclear<- monthly_bciclear%>% filter(year!="1929"&year!="2022")


#bocas
choke_good<- BOCAS %>% filter(chk_note=="good")
choke_good$date<- as.Date(choke_good$date, format="%d/%m/%Y")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra), .groups = "rowwise")
daily_bocas<- day_sum
daily_bocas<- daily_bocas %>% mutate(site="BOCAS")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_BOCAS<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra)`)" )
monthly_BOCAS$site<- "BOCAS"
monthly_BOCAS$year<- substring(monthly_BOCAS$yearmonth, 1, 4)
monthly_BOCAS<- filter(monthly_BOCAS,year!= "2020"&year!= "2005"&year!= "2002"&year!= "2022")


#pculebra
choke_good<- PCULEBRA %>% filter(chk_note=="good")
choke_good$date<- as.Date(choke_good$date, format="%d/%m/%Y")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra), .groups = "rowwise")
daily_pculebra<- day_sum
daily_pculebra<- daily_pculebra %>% mutate(site="PCULEBRA")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_PCULEBRA<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra)`)" )
monthly_PCULEBRA$site<- "PCULEBRA"
monthly_PCULEBRA$year<- substring(monthly_PCULEBRA$yearmonth, 1, 4)
monthly_PCULEBRA<- filter(monthly_PCULEBRA,year!= "2022"&year!= "2010")
#GALETALAB
choke_good<- GALETALAB
choke_good$date<- substring(choke_good$date.time..yyyy.mm.dd.hh.mm.ss., 1, 10)
choke_good$date<- as.Date(choke_good$date, format="%Y-%m-%d")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra.mm.), .groups = "rowwise")
daily_galetalab<- day_sum
daily_galetalab<- daily_galetalab %>% mutate(site="GALETALAB")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_galetalab<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra.mm.)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra.mm.)`)" )
monthly_galetalab$site<- "GALETALAB"
monthly_galetalab$year<- substring(monthly_galetalab$yearmonth, 1, 4)
monthly_galetalab<- filter(monthly_galetalab,year!= "2007")
#galetatowchoke_good<- choke_good%>% filter(year==c(1974:2006))
choke_good<- GALETATOW %>% filter(chk_note=="good")
choke_good$date<- as.Date(choke_good$date, format="%d/%m/%Y")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra), .groups = "rowwise")
daily_galetatow<- day_sum
daily_galetatow<- daily_galetalab %>% mutate(site="GALETATOW")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_GALETATOW<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra)`)" )
monthly_GALETATOW$site<- "GALETATOW"
monthly_GALETATOW$year<- substring(monthly_GALETATOW$yearmonth, 1, 4)
monthly_GALETATOW<- subset(monthly_GALETATOW,year!= "2017"&year!= "2003"&year!= "2022")

#sherman

choke_good<- SHERMAN %>% filter(chk_note=="good")
choke_good$date<- as.Date(choke_good$date, format="%d/%m/%Y")
choke_good$year<-format(choke_good$date, format="%Y")
day_sum<- choke_good%>% group_by(date)%>% summarise(sum(ra), .groups = "rowwise")
daily_sherman<- day_sum
daily_sherman<- daily_sherman %>% mutate(site="SHERMAN")
day_sum$yearmonth<- format(day_sum$date, format= "%Y/%m")
monthly_SHERMAN<-day_sum%>% group_by(yearmonth)%>% summarise(sum(`sum(ra)`), .groups="rowwise")%>%rename(ra= "sum(`sum(ra)`)" )
monthly_SHERMAN$site<- "SHERMAN"
monthly_SHERMAN$year<- substring(monthly_SHERMAN$yearmonth, 1, 4)
monthly_SHERMAN<- subset(monthly_SHERMAN,year!= "1997"&year!= "2022")
choke_good<- choke_good%>% filter(year==c(1998:2021))

##AVERAGE THE CELESTINOS 
CELESTINO<- merge(monthly_celestinon, monthly_celestinos, by.x = "yearmonth", by.y="yearmonth")
CELESTINO$ra<- (CELESTINO$ra.x+CELESTINO$ra.y)/2
monthly_celestino<- CELESTINO %>% rename(year=year.x)%>% mutate(site="CELESTINO")%>% select(-c("ra.x","site.y","site.x", "year.y","ra.y"))

daily_celestino<-merge(daily_celestinon,daily_celestinos, by.x = "date", by.y = "date")
daily_celestino$`sum(ra)`<- (daily_celestino$`sum(ra).x`+daily_celestino$`sum(ra).y`)/2
daily_celestino<- daily_celestino %>% mutate(site="CELESTINO")%>% select(-c("site.y","site.x","sum(ra).x","sum(ra).y"))

#average galeta
GALETASTRI<- merge(monthly_GALETATOW, monthly_galetalab, by.x = "yearmonth", by.y="yearmonth", all = TRUE)
GALETASTRI$year<- substring(GALETASTRI$yearmonth, 1, 4)
GALETASTRI<- subset(GALETA, year!="2007"&year!="2021")
GALETASTRI$ra.y<- ifelse(!is.na(GALETASTRI$ra.y), GALETASTRI$ra.y, 0)
GALETASTRI$ra.x<- ifelse(!is.na(GALETASTRI$ra.x), GALETASTRI$ra.x, 0)
GALETASTRI$ra<- (GALETASTRI$ra.x+GALETASTRI$ra.y)/2
monthly_GALETA<- GALETASTRI %>% mutate(site="GALETASTRI")%>% select(-c("ra.x","site.y","site.x", "year.y","ra.y","year.x"))

daily_galetastri<-merge(daily_galetalab,daily_galetatow, by.x = "date", by.y = "date")

daily_galetastri$`sum(ra)`<- (daily_galetastri$`sum(ra.mm.).x`+daily_galetastri$`sum(ra.mm.).y`)/2
daily_galetastri<- daily_galetastri %>% mutate(site="GALETASTRI")%>% select(-c("site.y","site.x","sum(ra.mm.).x","sum(ra.mm.).y"))
#bind rows: bciava, bciclear, celestino, galeta, bocas, culebra, and sherman monthly
monthly_stri<- bind_rows(monthly_bciava,monthly_bciclear,monthly_BOCAS,monthly_GALETA,monthly_PCULEBRA,monthly_SHERMAN,monthly_celestino)
sites_stri<-stri_sites[c(1,3,4,5,6,7,9,10),c(1,4,5)]
sites_stri[1,1]<-"CELESTINO"
sites_stri[6,1]<-"GALETASTRI"

daily_stri<- bind_rows(daily_galetastri,daily_celestino,daily_bciclear, daily_bciava,daily_sherman,daily_bocas,daily_pculebra)

daily_stri<- merge(daily_stri,sites_stri, by.x="site", by.y="site")
monthly_stri<- merge(monthly_stri, sites_stri, by.x = "site",by.y = "site", all = TRUE)

DIROUT<- "data_ground/met_data/STRI_data/"
write.csv(monthly_stri, paste0(DIROUT,"monthly_prec_stri.csv"),row.names=FALSE)
write.csv(daily_stri, paste0(DIROUT,"daily_prec_stri.csv"),row.names=FALSE)
