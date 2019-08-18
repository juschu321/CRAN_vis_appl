merged_data <-
  inner_join(tutti_time_monthly_package,
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

c <-
  inner_join (tutti_time_monthly_package,
              packages_per_ctv,
              by = c("package" = "package"))

#all Psychometrics packages
cf <- c %>%
  filter (ctv == "Psychometrics")

#220 packages of Psychometrics April 2019
cd <- cf %>%
  filter (month == "2019-04-01")

#number of downloads of Psychometrics based on tutti_time_monthly_package
count_pctv <- cd %>%
  summarise(total = sum(total))



count_pkg <- packages_per_ctv %>%
  select(ctv, package) %>%
  filter(ctv == "Psychometrics") %>%
  group_by(ctv) %>%
  summarise(count = n_distinct(package))


#