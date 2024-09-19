library(tidyverse)
library(ggplot2)
library(readxl)

#function for coordinates
convert_dms_to_dd <- function(dms) {
  degrees <- as.numeric(gsub(".*?(\\d+)°.*", "\\1", dms))
  minutes <- as.numeric(gsub(".*?(\\d+\\.?\\d*)'.*", "\\1", dms))
  seconds <- ifelse(grepl("\"", dms), as.numeric(gsub(".*?(\\d+\\.?\\d*)\".*", "\\1", dms)), 0)
  dd <- degrees + minutes/60 + seconds/3600
  return(dd)
}

DIR<-getwd()
DIRTOACP<- paste0(DIR,"/data_ground/met_data/ACP_data/cleanedVV")

#downloadlink
link<- 'https://smithsonian.figshare.com/ndownloader/files/41529324'
download.file(link,destfile =paste0(DIR,"/data_ground/met_data/ACP_data/cleanedVV/Monthly_Rain_ACP_Vertical.xlsx")  , mode = "wb")

#read data
ACP_data<-read_excel(paste0(DIR,"/data_ground/met_data/ACP_data/cleanedVV/Monthly_Rain_ACP_Vertical.xlsx"),sheet = 4)
ACP_met_stations<-read_excel(paste0(DIR,"/data_ground/met_data/ACP_data/cleanedVV/Monthly_Rain_ACP_Vertical.xlsx"),sheet = 2,skip = 1)

#clean data
##change the names, drop Gatuncentral
ACP_data <- ACP_data %>% 
  rename('PEDROMIGUEL' = 'PEDROMIGEL', 'SANPEDRO' = 'SAN PEDRO') %>% 
  select(-GATUNCENTRAL)%>% 
  pivot_longer(cols = 4:ncol(.), values_to = 'monthly_precip', names_to = 'site') %>% 
  mutate(monthly_precip = if_else(monthly_precip == '-9', NA, monthly_precip)) %>% 
  mutate(monthly_precip = as.numeric(monthly_precip)) %>% 
  mutate(month_year = as.Date(paste0("1","/",Month,"/", Year), format = "%d/%m/%Y")) %>%
  select(month_year, monthly_precip, site)

#clean met stations
ACP_met_stations <- ACP_met_stations %>% 
  select('ACP Name', 'STRI Name', 'ELE (m)', 'LAT (N)...6','LONG (W)...7','LAT (N)...8', 'LONG (W)...9') %>% 
  mutate('LAT (N)...8' = ifelse(is.na(`LAT (N)...8`),convert_dms_to_dd(`LAT (N)...6`), `LAT (N)...8`), 
        'LONG (W)...9' = ifelse(is.na(`LONG (W)...9`),convert_dms_to_dd(`LONG (W)...7`), `LONG (W)...9`)) %>%
  select('ACP Name', 'STRI Name', 'ELE (m)', 'LAT (N)...8', 'LONG (W)...9')%>%
  rename('ACP_name' = 'ACP Name', 'site' = 'STRI Name', 'Elevation' = 'ELE (m)', 'latitude' = 'LAT (N)...8', 'longitude' = 'LONG (W)...9') %>% 
  select(ACP_name,site, latitude, longitude, Elevation) %>%
  mutate(site = if_else(is.na(site) | site == "NA", ACP_name, site)) %>% 
  mutate(site = if_else(ACP_name == "Pedro Miguel", "PEDROMIGUEL", site)) %>% 
  mutate(site = if_else(ACP_name == "Pedro Miguel(LAKE)", "PEDROMIGUELLAKE", site)) %>%
  mutate(site = if_else(site == "SAN PEDRO", "SANPEDRO", site)) %>% 
  mutate(site = toupper(gsub(" ", "", site)))%>%
  mutate(site = if_else(site == "COCOLI", "CERROCOCOLI", site)) %>%
  mutate(site = if_else(site == "CHRISTOBAL", "CRISTOBAL", site)) %>%
  mutate(site=if_else(site=="VALLEGATUN","VALLECENTRALGATUN",site))

# add a row to acp_met_stations 
new_row <- data.frame(ACP_name = 'Palmarazo',
                      site = 'PALMARAZO',
                      latitude = 8.7336,
                      longitude = -80.6544,
                      Elevation = NA)
ACP_met_stations <- bind_rows(ACP_met_stations, new_row)

ACP_met_stations<- ACP_met_stations%>%filter(site!="DEACTIVATEDSTATIONS")%>%filter(ACP_name!=Pedro Miguel (LAKE))
View(ACP_met_stations)
if (length(setdiff(unique(ACP_data$site), unique(ACP_met_stations$site)))>0){
    print("There are sites in the data that are not in the met stations")
} else {
    print("There are no sites in the data that are not in the met stations")
}
write.csv(ACP_data, paste0(DIR, '/data_ground/met_data/ACP_data/cleanedVV/ACP_data.csv'))
write.csv(ACP_met_stations, paste0(DIR, '/data_ground/met_data/ACP_data/cleanedVV/ACP_met_stations.csv'))

