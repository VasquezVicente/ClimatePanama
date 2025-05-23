# gap_fill_ground_data.r
# code for testing various approaches to gap-filling the ground data
# WORK IN PROGRESS

rm(list=ls())
library(tidyverse)
library(lme4)
library(ggplot2)
library(mice) # for imputation 


#Auxiliary objects#
WORKINGDIRECTORY<-getwd()
ACP_DATA<- "data_ground/met_data/ACP_data/cleanedVV/ACP_data.csv"
STRI_DATA<-"data_ground/met_data/STRI_data/STRI_monthlyPrecip.csv"

acpDATA<- read.csv(ACP_DATA)
striDATA<- read.csv(STRI_DATA)

#Ground monthly precip
monthlyPrecipAll<- acpDATA%>%bind_rows(striDATA) %>%
  select(-X) %>%
  mutate(year=as.numeric(format(as.Date(month_year, format="%Y-%m-%d"),"%Y")),
         month=as.numeric(format(as.Date(month_year, format="%Y-%m-%d"),"%m"))) %>%
  rename(precip=monthly_precip) %>%
  filter(!site %in% c("GALETA", "BCIELECT"))
# note removal of Galeta (duplicates GALETASTRI)
# also removed downwardly biased BCI electronic rain gauge

# combining the BCI records into one time series: 
bci1dat <- subset(monthlyPrecipAll,
                  !is.na(precip) & site=="BCICLEAR" )
bci2dat <- subset(monthlyPrecipAll,
                  !is.na(precip) & site=="BCI"& !month_year%in%bci1dat$month_year)
bcicombdat <- rbind(bci1dat,bci2dat) 
bci3dat <- subset(monthlyPrecipAll,
                  is.na(precip) & site=="BCICLEAR"& !month_year%in%bcicombdat$month_year)
bcicombdat <- rbind(bcicombdat,bci3dat) %>% mutate(site="BCICOMB")
monthlyPrecipAll <- monthlyPrecipAll %>%
  filter(!site %in% c("BCI","BCICLEAR")) %>%
  bind_rows(bcicombdat)

# SANBLAS station seems to have a lot of duplicate data
sanblas <- subset(monthlyPrecipAll,site=="SANBLAS")
# yup, all the san blas data supposedly for year 2000.  Drop it.

monthlyPrecipAll <- monthlyPrecipAll %>% 
  filter(site != "SANBLAS")

monthlyPrecipData <- monthlyPrecipAll %>%
  filter(!is.na(precip)) %>%
  select(-month_year)

nstations<- monthlyPrecipData %>% 
  group_by(year)%>% 
  filter(!is.na(precip))%>%
  summarize(nstations= n_distinct(site))

ggplot(nstations, aes(x=year, y=nstations))+
  geom_point()+
  theme_bw()+
  geom_vline(xintercept=1970) + 
  geom_vline(xintercept=2016) + 
  labs(x="Year", y="Number of stations")

monthlyprecipsubset <- subset(monthlyPrecipData,year>=1970&year<=2016)

# get months of data by station, and restrict to stations with more data
nmonths <- monthlyprecipsubset %>%
  group_by(site) %>%
  summarize(nmonth=n())

threshold_plot_data <- data.frame(threshold = sort(unique(nmonths$nmonth))) %>%
  rowwise() %>%
  mutate(n_above = sum(nmonths$nmonth > threshold)) %>%
  ungroup()

ggplot(threshold_plot_data, aes(x = threshold, y = n_above)) +
  geom_line() +
  labs(
    x = "Threshold (months)",
    y = "Number of stations with more than threshold months of data",
    title = "Stations with > Threshold Months of Data in 1970-2016"
  ) +
  theme_minimal()

# set a threshold of 36 months (30 years) of data 
usesites <- nmonths$site[nmonths$nmonth>=360]


monthlyprecipsubset <- subset(monthlyprecipsubset,site %in% usesites)

# now make a dataset with all combinations of site, year and month
monthlyprecipsubsetwna <- monthlyprecipsubset %>%
  complete(site,year,month) %>%
  arrange(site,year,month) %>%
  mutate(site=as.factor(site))  # note - it is very important that site is a factor!!!

# impute missing values using mice 
impmonthly <- mice(monthlyprecipsubsetwna,m=5,method='pmm',maxit=5)
summary(impmonthly)
impmonthly$loggedEvents

# the following figure is SLOW
stripplot(impmonthly, pch = 20, cex = 1.2)
densityplot(impmonthly, ~precip)

imp1 <- complete(impmonthly, 1)
table(imp1$precip==monthlyprecipsubsetwna$precip)
table(is.na(monthlyprecipsubsetwna$precip))
# so basically, imputed dataset has observed values when available

imp1$observed <- !is.na(monthlyprecipsubsetwna$precip)
table(imp1$observed)
table(imp1$precip[!imp1$observed]<0)  # good to see there is no negative precip!

imp2 <- complete(impmonthly, 2)
imp3 <- complete(impmonthly, 3)
imp4 <- complete(impmonthly, 3)
imp5 <- complete(impmonthly, 3)

# these are imputed datasets 



