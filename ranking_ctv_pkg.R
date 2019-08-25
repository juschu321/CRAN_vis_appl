overview <- tutti_time_monthly_ctv%>%
  select(ctv, total)%>%
  group_by(ctv)%>%
  summarise(count = sum(total))



overview_2018_19 <- tutti_time_monthly_ctv%>%
  select(ctv, total, month)%>%
  group_by(ctv, month)%>%
  filter(month >= as.Date("2018-07-01"))%>%
  group_by(ctv)%>%
  summarise(count = sum(total))


psy_pkg_monthly <- inner_join (tutti_time_monthly_package, packages_per_psy_sub, by = c("package" = "package"))

total_psy_pkg_2018_19 <- psy_pkg_monthly%>%
  select(package, total, month)%>%
  filter(month >= as.Date("2018-07-01"))%>%
  group_by(package)%>%
  summarise(count = sum(total))

psy_ov_sub <- inner_join(total_psy_pkg_2018_19, psy_sub, by = c("package" = "package"))
  
#to figure out which categories of Psychometrics played an important role
psycho_sub <- inner_join(tutti_time_monthly_package, psy_sub, by = c("package" = "package"))%>%
  group_by(category)%>%
  select(category, month, total)%>%
  filter(month >= as.Date("2013-07-01"), month <= as.Date("2014-07-01"))%>%
  summarise(count = sum(total))




#figure out popular packages of Environmetrics
merge <- inner_join(tutti_time_monthly_package, packages_per_ctv, by = c("package" = "package"))%>%
  select(package, month, ctv, total)%>%
  group_by(ctv)%>%
  filter(month >= as.Date("2018-07-01"))%>%
  filter(ctv == "Environmetrics")%>%
  group_by(package)%>%
  summarise(count = sum (total))

    