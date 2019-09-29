#data preparation (to generate updated data basis)
library(cranlogs)
library(ctv)
library(dplyr)
library(scales)
library(cranly)
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(magrittr)
library(xml2)
library(lubridate)
library(readr)
#to start the data reproduction you have to load the externally saved functions into the R environment 
#(saved in the file /CRAN_vis_appl/R)

#generates a list with all packages in the ctvs (ctv::available views)
packages_per_ctv <- get_packages_per_ctv()

#psychometrics specific (creates a list of psy packages according to categories on CRAN)
psy_packages <- get_psy_packages()
psy_sub <- get_subcategory_of_psy_packages(psy_packages = psy_packages)

#get global data (collects data from cranlogs and additional api data for all pkgs mentioned in the ctvs)
global_edgelist <- data.frame()
global_api <- list()
global_download <- data.frame ()

for (view in ctv::available.views()[]) {
  packages <- view$packagelist$name
  download_statistics <- get_download_statistics(packages)
  api_data <- get_api_data(packages)
  global_api <- rbind(global_api, api_data)
  edgelist <- get_edgelist_of_packages(api_data)
  global_edgelist <- rbind(global_edgelist, edgelist)
  global_download <- rbind(global_download, download_statistics)
}

#get basic dataframe to create the VisNetwork
edgelist <- get_tutti_dependencies(global_edgelist = global_edgelist, packages_per_ctv = packages_per_ctv)

#get basic dataframe to generate tutti_time_monthly_ctv/pkg
tutti_timeseries <- get_tutti_timeseries(global_download = global_download,packages_per_ctv = packages_per_ctv)

#aggregate data (in terms of time and ctv/ pkg as basis for the plots)
tutti_time_monthly_ctv <- get_tutti_time_monthly_ctv(tutti_timeseries = tutti_timeseries)

#aggregate data (per package and time -> basis for package tab)
tutti_time_monthly_package <- get_tutti_time_monthly_pkg(tutti_timeseries = tutti_timeseries)

#get importance data (in terms of dependencies)
importance_data <- get_importance_data(edgelist = edgelist, packages_per_ctv = packages_per_ctv, tutti_time_monthly_package = tutti_time_monthly_package)

#csv files as basis for the application (overwrites existing csv if not adjusted)
export_data_to_csv()
