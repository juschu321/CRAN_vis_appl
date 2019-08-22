#data prep (to generate the data basis)
packages_per_ctv <- get_packages_per_ctv()

psy_packages <- get_psy_packages()
psy_sub <-
  get_subcategory_of_psy_packages(psy_packages = psy_packages)

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

psy_subcategories <- get_subcategory_of_psy_packages(psy_packages)


tutti_time_monthly_ctv <- tutti_timeseries %>%
  mutate(month = as.Date(cut(date,breaks = "month"))) %>%
  group_by(ctv, month) %>% 
  summarise(total = sum(count))

#distinct() is important to get each package just once
#because some packages are hosted in more CTVS
tutti_time_monthly_package <- tutti_timeseries %>%
  mutate(month = as.Date(cut(date,breaks = "month"))) %>%
  distinct(package, month, count) %>%
  group_by(package, month) %>% 
  summarise(total = sum(count))