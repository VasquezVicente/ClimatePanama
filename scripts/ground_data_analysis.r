#Import packages#
library(tidyverse)
library(lme4)
library(ggplot2)


#Auxiliary objects#
WORKINGDIRECTORY<-getwd()
ACP_DATA<- "data_ground/met_data/ACP_data/cleanedVV/ACP_data.csv"
STRI_DATA<-"data_ground/met_data/STRI_data/STRI_monthlyPrecip.csv"

acpDATA<- read.csv(ACP_DATA)
striDATA<- read.csv(STRI_DATA)

#Ground monthly precip
monthlyPrecipData<- acpDATA%>%bind_rows(striDATA)

#make a year and month column
monthlyPrecipData$year<- as.numeric(format(as.Date(monthlyPrecipData$month_year, format="%Y-%m-%d"),"%Y"))
monthlyPrecipData$month<- as.numeric(format(as.Date(monthlyPrecipData$month_year, format="%Y-%m-%d"),"%m"))
nstations<- monthlyPrecipData %>% group_by(year)%>% filter(!is.na(monthly_precip))%>%summarize(nstations= n_distinct(site))

#number of stations per year
nstations<- monthlyPrecipData %>% group_by(year)%>%filter(!is.na(monthly_precip))%>%summarise(count= n_distinct(site))
ggplot(nstations, aes(x=year, y=count))+geom_point()+theme_bw()+labs(x="Year", y="Number of stations")

#annual precipitation and Jan-April precipitation for years with all months
precipitation_data<- monthlyPrecipData %>% filter(!is.na(monthly_precip))%>%
group_by(site, year)%>%filter(n() == 12)%>%
summarize(annualPrecip = sum(monthly_precip, na.rm = TRUE),
              jfmaPrecip = sum(monthly_precip[month %in% c(1,2,3,4)], na.rm = TRUE))

#keep only years with response variables 
long_term_averages<-precipitation_data%>%group_by(site)%>%
summarize(longterm_mean_annual= mean(annualPrecip),longterm_mean_jfma=mean(jfmaPrecip))

#how many sites should we include in the analysis?
nyears= precipitation_data %>% filter(year>=1970 & year<=2016)%>%
  group_by(site) %>% tally() %>% 
  rename(nyears= n)%>% full_join(long_term_averages,by='site')

ggplot(nyears, aes(x = longterm_mean_annual, y = nyears)) +
  geom_point() +
  geom_text(aes(label = site), vjust = -0.5, size = 2) +
  labs(x = "Mean annual precipitation", y = "Number of Years")
ggsave("plots/meanannual_v_nyears.jpg", width = 10, height = 6, dpi = 300)


#QAQC OF DATA,Are there repeated sites? 
#BCI has 3 sets of precipitation, Electronic record, ACP and Clearing manual
BCI_subset<- precipitation_data%>% 
filter(site=="BCI"|site=="BCICLEAR"|site=="BCIELECT")%>%
filter(year>=1970 & year<=2016)

#test the differences between the three sets of data
means_BCI<-BCI_subset%>%group_by(site)%>%summarize(mean_annual_precip=mean(annualPrecip),sd_annual_precip=sd(annualPrecip),n=n())
anova_result <- aov(annualPrecip ~ site, data = BCI_subset)
tukey_results <- TukeyHSD(anova_result)

#ANOVA results
print(means_BCI)
summary(anova_result)
print(tukey_results)

ggplot(BCI_subset, aes(x=year, y=annualPrecip, color=site))+
geom_line()+theme_bw()+labs(x="Year", y="Annual Precipitation (mm)")

#The station BCI is managed by the ACP and it is close to the heliport close to the labs.
#The station BCIELECT is mantained by STRI and it is located by the basketball court in the island
#The station BCICLEAR is mantained by STRI and it is located in the same spot than BCIELECT
#Results indicate that both BCIELECT and BCICLEAR are not significantly different from BCI.
#Results indicate that BCIELECT and BCICLEAR are significantly different from each other.
#BCICLEAR completes its missing days using a prorating method using the BCIELECT lecture.
#We will keep the three stations in the analysis until we decide what to do with this matter.

#GALETA AND GALETASTRI
GALETA_subset<- precipitation_data_subset %>% filter(site=="GALETA"|site=="GALETASTRI")%>%
filter(year>=1970 & year<=2016)
ggplot(GALETA_subset, aes(x=year, y=annualPrecip, color=site))+geom_line()+theme_bw()+labs(x="Year", y="Annual Precipitation (mm)")
#anova
means_GALETA<-GALETA_subset%>%group_by(site)%>%summarize(mean_annual_precip=mean(annualPrecip),sd_annual_precip=sd(annualPrecip),n=n())
anova_galeta<- aov(annualPrecip ~ site, data = GALETA_subset)
tukey_results_galeta <- TukeyHSD(anova_galeta)

#ANOVA results
print(means_GALETA)
summary(anova_galeta)
print(tukey_results_galeta)

#The difference between GALETA AND GALETASTRI is not significant.
#The figshare data does not contain ACP data exclusively, but also includes STRI data.
#We are sure that this two sets are the same but we do not know how was the monthly summaries ACP produced.
#We will keep GALETASTRI since this data comes from source and was calculated in stri_compile.r code. 

#Previously limiting to all sites with 32 or more years of data in the period 1970-2016
#relaxing the criteria to 30 years of data and removing GALETA
precipitation_data_subset<- precipitation_data %>% filter(site!="GALETA") 
precipitation_data_30<- merge(precipitation_data_subset, nyears, by.x= "site", by.y="site")
precipitation_data_subset<- precipitation_data_30%>% filter(nyears>=30)


#fit linear model with random effects for year and fixed effects for site 
model_annual<- lmer(annualPrecip~ site +(1|year), data=precipitation_data_subset)
model_jfma<- lmer(jfmaPrecip~ site +(1|year), data=precipitation_data_subset)

#create data frame with empty sequence of years from 1970 to 2016
year_sequence<- seq(1970,2016)
sites<- unique(precipitation_data_subset$site)
precipitation_predicted<- expand.grid(year_sequence,sites)%>%rename(year="Var1",site="Var2")

#subset the data to the time expand
precipitation_data_subset<- precipitation_data_subset%>%filter(year>=1970,year<=2016)

#predict on the new data
precipitation_predicted $predicted_annual<- predict(model_annual,newdata=precipitation_predicted)
precipitation_predicted$predicted_jfma<- predict(model_jfma,newdata=precipitation_predicted)

#merge and gap fill with actual data 
precipitation<- precipitation_predicted %>%
full_join(precipitation_data_subset, by=c("site","year"))%>%
mutate(gap_fill=if_else(is.na(annualPrecip),TRUE,FALSE),
       annual=if_else(is.na(annualPrecip),predicted_annual,annualPrecip),
       jfma=if_else(is.na(jfmaPrecip),predicted_jfma,jfmaPrecip))


#visualization of gap filled timeseries
ggplot(precipitation %>% filter(site %in% sites[1:8]), 
       aes(x = year, y = annual, group = site,color=site,shape=gap_fill)) + geom_line() + geom_point(size=3) +
  labs(x = "Year", y = "Annual Precipitation (mm)", color = "Site")

ggplot(precipitation %>% filter(site %in% sites[9:16]), 
       aes(x = year, y = annual, group = site,color=site,shape=gap_fill)) + geom_line() + geom_point(size=3) +
  labs(x = "Year", y = "Annual Precipitation (mm)", color = "Site")

ggplot(precipitation %>% filter(site %in% sites[17:24]), 
       aes(x = year, y = annual, group = site,color=site,shape=gap_fill)) + geom_line() + geom_point(size=3) +
  labs(x = "Year", y = "Annual Precipitation (mm)", color = "Site")

ggplot(precipitation %>% filter(site %in% sites[25:32]), 
       aes(x = year, y = annual, group = site,color=site,shape=gap_fill)) + geom_line() + geom_point(size=3) +
  labs(x = "Year", y = "Annual Precipitation (mm)", color = "Site")

#Save the data in tables
write.csv(precipitation,"tables/precipitation.csv", row.names = FALSE)
