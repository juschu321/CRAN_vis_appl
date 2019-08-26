dep <- edgelist%>%
  filter(type %in% c("imports", "depends"))%>%
  group_by(to)%>%
  summarise(count = n()) 


#ranking over time 
overview <- tutti_time_monthly_ctv%>%
  filter(month >= as.Date("2012-10-01"), month <= as.Date("2019-07-01"))%>%
  group_by(ctv,month)%>%
  summarise(count = sum(total))%>%
  group_by(month)%>%
  mutate(rank = 41-rank(interaction(ctv, count)))

overview%>%
  group_by(ctv)%>%
  summarise(mean(rank), sd(rank))

#plot ranking = desaster
p <- ggplot(overview)+
  geom_line(aes(month, rank, color = overview$ctv))



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

#delete packages which are in more than one CTV 
total_psy_pkg_2018_19 <- psy_pkg_monthly%>%
select(package, total, month)%>%
  filter(month >= as.Date("2018-07-01"), month <= as.Date("2019-07-01"))%>%
  group_by(package)%>%
  summarise(count = sum(total))

#psy packages which are in more than one CTV 
psy_more <- inner_join(more_than_one, packages_per_ctv, by = c("package" = "package") )%>%
  filter(core == FALSE)%>%
  filter(ctv == "Psychometrics")

#these psycho packages are in more than one CTV
psycho_more <- psy_more %>%
  select(package)

#option to check for packages which are not included in more ctvs 
n_ctv_p_pkg <- packages_per_ctv %>%
  filter(core == FALSE) %>%
  select(package, ctv) %>%
  group_by(package)%>%
  count()

tutti_n_p_ctv <- inner_join(tutti_time_monthly_package, n_ctv_p_pkg, by = c("package" = "package"))

not_incl_more_ctvs_psy <- full_join(tutti_n_p_ctv, packages_per_ctv, by = c("package" = "package"))%>%
  select(package, month, total, n, ctv)%>%
  filter(ctv == "Psychometrics")%>%
  filter(n == 1)%>%
  group_by(package)%>%
  filter(month >= as.Date("2018-07-01"), month <= as.Date("2019-07-01"))%>%
  summarise(count = sum(total))

#to get an overview of the role of the categories 
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

    
#check the downloads per package during the last year
invest <- tutti_time_monthly_package%>%
  filter(month >= as.Date("2018-07-01"), month <= as.Date("2019-07-01"))%>%
  group_by(package)%>%
  summarise(count = sum(total))

#compute the variation in the download statistics across the last year (tutti with more CTVs)
#mean = 105215
#var = 297339128869
#sd = 545288
#min = 182
#max = 10869025
invest%>%  
  summarise(max(count))

#compute the variation in the download statistics across the last year (psycho with more CTVs)
#mean = 76810
#var = 81019579721
#sd = 284639
#min = 401
#max = 2712386
total_psy_pkg_2018_19%>%
  summarise(max(count))

##compute the variation in the download statisstics across the last year (psycho no more CTVs)
#mean = 19642
#var =  6059379666
#sd =  77842
#min = 966
#max = 937852
not_incl_more_ctvs_psy%>%
  summarise(mean(count))

#descriptive statistics psycho packages variation (FALSCH)
Psychometrics <- c(105215,297339128869, 545288, 182,10869025  )
Psychometrics_without_overlapping_pkg <- c(47435,25349372127, 159215, 1037, 1860598 )
Z <- rbind(Psychometrics, Psychometrics_without_overlapping_pkg)
colnames(Z) <- c("mean", "var", "sd", "min", "max")