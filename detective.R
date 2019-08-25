#the following script serves to check the data (and served to find an inconsistency in the data)
merged_data <- inner_join(tutti_time_monthly_package,
             packages_per_psy_sub,
             by = c("package" = "package")) %>%
  filter (category == "Item Response Theory (IRT)") %>%
  group_by(category, month) %>%
  summarise(total = sum(total))

#which packages are in Item Response Theory?? 61 packages
pp <- packages_per_psy_sub %>%
  filter (category == "Item Response Theory (IRT)") %>%
  distinct(package)

psychometrics <- tutti_time_monthly_ctv %>%
  filter (ctv == "Psychometrics")

#ctv plot mit tutti_time_monthly_package

c <-inner_join (tutti_time_monthly_package,
              packages_per_ctv,
              by = c("package" = "package"))

#all Psychometrics packages
ctv_filter <- c %>%
  filter (ctv == "Psychometrics")

#220 packages of Psychometrics April 2019
ctv_date <- ctv_filter %>%
  filter (month == "2019-04-01")

#number of downloads of Psychometrics based on tutti_time_monthly_package
count_psych_ctv <- ctv_date %>%
  summarise(total = sum(total))

count_pkg <- packages_per_ctv %>%
  select(ctv, package) %>%
  filter(ctv == "Psychometrics") %>%
  group_by(ctv) %>%
  summarise(count = n_distinct(package))

#how many packages do we get from download_data_from_CRAN? (workspace from 2019-08-11)
#result: 3314 entries with 2505 counts (daily over the last years)
#packages from CTVs because "packages" has been created by packages_per_ctv
amount_of_packages <- download_statistics%>%
  count(package)

packages_CRAN_1month <- cran_downloads(packages = NULL, from = "2019-07-01", to = "2019-08-01")

top_CRAN_1month <- cran_top_downloads(when = c("last-month"), count = 10)

tutti <- available.packages(filters = "CRAN")
# 14,782 entries, where I take the date from?

#to check if in tutti_timeseries are some packages twice
tutti_timeseries%>%
  count(package)
#3314 entries 

#in more than one ctv
more_than_one <- packages_per_ctv %>%
  filter(core == FALSE) %>%
  select(package, ctv) %>%
  group_by(package)%>%
  count()%>%
  filter(n >= 2)