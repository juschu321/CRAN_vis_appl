overview <- tutti_time_monthly_ctv%>%
  select(ctv, total)%>%
  group_by(ctv)%>%
  summarise(count = sum(total))



overview_2018_19 <- tutti_time_monthly_ctv%>%
  select(ctv, total, month)%>%
  group_by(ctv, month)%>%
  filter(month >= as.Date("2018-01-01"))%>%
  group_by(ctv)%>%
  summarise(count = sum(total))


psy_pkg_monthly <- inner_join (tutti_time_monthly_package, packages_per_psy_sub, by = c("package" = "package"))

total_psy_pkg_2018_19 <- psy_pkg_monthly%>%
  select(package, total, month)%>%
  filter(month >= as.Date("2018-01-01"))%>%
  group_by(package)%>%
  summarise(count = sum(total))
  


