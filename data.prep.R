#data preparation (to generate updated data basis)
packages_per_ctv <- get_packages_per_ctv()

#psychometrics specific
psy_packages <- get_psy_packages()
psy_sub <- get_subcategory_of_psy_packages(psy_packages = psy_packages)

#get global data
global_edgelist <- data.frame()
global_api <- list()
global_download <- data.frame ()

for (view in ctv::available.views()) {
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

#distinct() is important to get each package just once
#because some packages are hosted in more CTVS
tutti_time_monthly_package <- get_tutti_time_monthly_pkg(tutti_timeseries = tutti_timeseries)

export_data_to_csv()
