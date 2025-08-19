# ClimatePanama

The ClimatePanama repository was created with the purpose of version control of the code associated with the paper submission titled "Evaluation of high-resolution gridded climate products in reproducing spatial and temporal variation in precipitation in central Panama". The main objective was to compile available gridded products for precipitation in Panama and compare their values with in-situ measurments. 

## Climate Panama Book

The entire code and results are documented in the github page. You can follow the chapters and reproduce the results. It is important to highlight that this are very large downloads which are all in a desktop computer. [Climate Panama Book](https://vasquezvicente.github.io/ClimatePanama/).

## Code order
All code is available is sub directory ClimatePanamaBook.

 1. stri_compile.qmd : Downloads and wrangles with STRI MET station data
 2. acp_compile.qmd : Downloads and wrangles with ACP MET station data
 3. ground_data_wrangling.qmd : Consolidate ground station data, gap fills data for spatial analysis.
 4. Download_gridded_precipitation.qmd : Downloads and crops gridded products data. 
 5. results_precip.qmd : Does comparisons between ground data and gridded products. Returns tables with results.
 6. climate_panama_visualizations.qmd : generates all manuscript figures.
 7. references.qmd : code references.
