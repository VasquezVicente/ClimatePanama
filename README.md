# ClimatePanama
Analyses of meteorological station data and climate reanalysis data for Panama

Central questions: 
What are the major patterns of climate variation across the country?
Which global gridded datasets best capture this variation?  

Met station data currently include ACP and STRI rainfall data compiled by Steve Paton and published in figshare 

Global gridded datasets currently include CHELSA 1.2, CHELSA 2.1, TERRA, and PBCOR CHELSA 1.2.  

# Code files here

## Download and summarize monthly precipitation from STRI met stations
The code to download and summarize the monthly precipitation for STRI met stations is avaliable at:[./scripts/stri_compile.r](https://github.com/hmullerlandau/ClimatePanama/blob/49bee2a8e6c130770f94487c75dcb0107225f685/scripts/stri_compile.r)

In the subdirectory ./tables there is a csv file with the stations and the links to figshare as well as the metadata for this sites.[./tables/stri_met_stations.csv](https://github.com/hmullerlandau/ClimatePanama/blob/49bee2a8e6c130770f94487c75dcb0107225f685/tables/stri_met_stations.csv)

Run the full code and it will output [./data_ground/met_data/STRI_data/STRI_monthlyPrecip.csv](https://github.com/hmullerlandau/ClimatePanama/blob/69a810c2bc953d42c3abb99e5f4a0d01935001d9/data_ground/met_data/STRI_data/STRI_monthlyPrecip.csv)

## Download and summarize monthly precipitation from ACP met stations
The code to download and summarize the monthly precipitation for ACP met stations is avaliable at:[./scripts/ACP_compile.r]()
The code will download the monthly summaries from the figshare repository and perform a series
of data cleaning steps to produce a single csv file with the monthly precipitation for each station and a csv file with the metadata for each station.

## Code to select the sites to be used in the analysis. 
The code in ./scripts/ground_data_analysis.r serves the purpose of calculating the response variables for the analysis. It filters the stations that are meant to be used in the analysisis and performs a 
series of QAQC steps to ensure that there is no repeated stations. I have relaxed the criteria to include stations with 30 or more years. 

## download-prepare-map-reanalysis-data.r
Download global gridded climatologies, extract data for Panama, save data for Panama, and produce maps for all of Panama and Central Panama.

## compare-reanalysis-v-ground.r
Load local meteorological data and met station locations, extract reanalysis data for the same points, and analyze how they relate.  

# Folders

## data_ground
Meteorological data from ground stations in Panama.

## data_locations
Location data for met stations in Panama

## data_reanalysis
Global climate reanalysis data for Panama, cached locally.

## modeled_climate
Local modeled climate values, based on modeling from ground stations (published model output, from work by Rick Condit).  

## output/maps
Map figures of climate variables

## output/graphs
Scatterplots and other graphs and figures of analysis output. 

