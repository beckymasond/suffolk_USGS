# suffolk_USGS

Authors: Rebecca Davies (CU Boulder), Joe Tuccillo (CU Boulder)

Contributors: Seth Spielman (CU Boulder), Nate Wood (USGS)

Project: Anaylsis of Suffolk County community hazard exposure by block group based on selected demographic characteristics.

Modules:
- acs_dataprep_usgs.py (Python script for downloading variable data from the 2010 - 2014 American Community Survey)
- clustering.R (R script to run Cluster Analysis and Diagnostics)
- data_prep.R (R script to prepare ACS data for cluster analysis)
- landcover_prep.R (R script that tabulates and broups NLCD 2011 data for Suffolk County block groups)
- support_functions.R (R script with helper functions for data prep, cluster analysis, and results visualization)

Languages: R and Python

Libraries: 
- Python - cenpy, pandas, NumPy, copy
- R - raster, rgdal, maptools, rgeos, RColorBrewer, cluster, ggplot2, reshape2, fpc

Data files:

- master_vars.csv (Master list of variables and their categories included in cluster analysis)
- rawACSdata (Raw data in csv format downloaded by variable (column) and block group GEOID (row) from ACS) 
- data/data_dict_ACS5Y2014.csv (Data dictionary for selected ACS demographic variables)

Contact Information:

Rebecca Davies,
rebecca.m.davies@colorado.edu

Joe Tuccillo,
joseph.tuccillo@colorado.edu
