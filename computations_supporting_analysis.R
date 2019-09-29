#to calculate the results you have to run the application once (generates basic data)
#basis for the computation of the rankings of the ctvs

#####general computations####

tutti <- available.packages(filters = "CRAN")
#  14,978 entries

#returns a list of packages and to how many ctvs they belong to
n_ctv_p_pkg <- packages_per_ctv %>%
  filter(core == FALSE) %>%
  select(package, ctv) %>%
  group_by(package)%>%
  count()

#generates a list of how many imports and suggests a dependent has 
#(how many "arrows" point on this package)
dep <- edgelist%>%
  filter(type %in% c("imports", "depends"))%>%
  group_by(to)%>%
  summarise(count = n()) 

#combination of number of ctvs a package belongs to and number of dependencies they have
#(does the membership to more ctvs influence the dependencies?)
dep_nr <- inner_join(dep, n_ctv_p_pkg, by = c("to" = "package"))

#check the download statistics for specific packages
x <- tutti_time_monthly_package%>%
  filter(package %in% c("MASS", "ggplot2", "Rcpp"))%>%
  group_by(package)%>%
  summarise(count = sum(total))

#ranking ctv tutti timespan (reveals list of ctvs and according download counts)
overview <- tutti_time_monthly_ctv%>%
  select(ctv, total, month)%>%
  group_by(ctv, month)%>%
  filter(month >= as.Date("2012-10-01"), month <= as.Date("2019-08-01"))%>%
  group_by(ctv)%>%
  summarise(count = sum(total))

#second step: adds ranking numbers over time 
overview <- tutti_time_monthly_ctv%>%
  select(ctv, total, month)%>%
  filter(month >= as.Date("2012-10-01"), month <= as.Date("2019-08-01"))%>%
  group_by(ctv,month)%>%
  summarise(count = sum(total))%>%
  group_by(month)%>%
  mutate(rank = 41-rank(interaction(ctv, count)))

#calculates the mean rank and sd to see how the ctvs developed over time
rank_tutti <- overview%>%
  group_by(ctv)%>%
  summarise(mean(rank), sd(rank))

#ctv download counts over the last year
overview_2018_19 <- tutti_time_monthly_ctv%>%
  select(ctv, total, month)%>%
  group_by(ctv, month)%>%
  filter(month >= as.Date("2018-08-01"))%>%
  group_by(ctv)%>%
  summarise(count = sum(total))

#which packages are in more than one ctv?
more_than_one <- packages_per_ctv %>%
  filter(core == FALSE) %>%
  select(package, ctv) %>%
  group_by(package)%>%
  count()%>%
  filter(n >= 2)

#overview of packages with monthly download numbers + nr of memberships to ctvs
tutti_n_p_ctv <- inner_join(tutti_time_monthly_package, n_ctv_p_pkg, by = c("package" = "package"))

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

####Psychometrics specific####

#basis to check which packages of the ctv psychometrics have been downloaded the most 
psy_pkg_monthly <- inner_join (tutti_time_monthly_package, packages_per_psy_sub, by = c("package" = "package"))

#computation of the download counts of the psychometrics packages
total_psy_pkg_2018_19 <- psy_pkg_monthly%>%
  select(package, total, month)%>%
  filter(month >= as.Date("2018-08-01"), month <= as.Date("2019-08-01"))%>%
  group_by(package)%>%
  summarise(count = sum(total))

#psy packages which are in more than one CTV  
psy_more <- inner_join(more_than_one, packages_per_ctv, by = c("package" = "package") )%>%
  filter(core == FALSE)%>%
  filter(ctv == "Psychometrics")

#these psycho packages are in more than one CTV = 44 packages
psycho_more <- psy_more %>%
  select(package)

#shows psy pkgs which are not included in more than one ctv
not_incl_more_ctvs_psy <- full_join(tutti_n_p_ctv, packages_per_ctv, by = c("package" = "package"))%>%
  select(package, month, total, count, ctv)%>%
  filter(ctv == "Psychometrics")%>%
  filter(count == 1) %>%
  group_by(package)%>%
  filter(month >= as.Date("2018-08-01"), month <= as.Date("2019-08-01"))%>%
  summarise(count = sum(total))

#to get an overview of the role of the categories (psychometrics)
psy_ov_sub <- inner_join(total_psy_pkg_2018_19, packages_per_psy_sub,by = c("package" = "package"))
  
#to figure out which categories of Psychometrics played an important role
psycho_sub <- inner_join(tutti_time_monthly_package, packages_per_psy_sub,by = c("package" = "package"))%>%
  group_by(category)%>%
  select(category, month, total)%>%
  filter(month >= as.Date("2013-07-01"), month <= as.Date("2014-07-01"))%>%
  summarise(count = sum(total))

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

####additional computations####

#figure out popular packages of Environmetrics
merge <- inner_join(tutti_time_monthly_package, packages_per_ctv, by = c("package" = "package"))%>%
  select(package, month, ctv, total)%>%
  group_by(ctv)%>%
  filter(month >= as.Date("2018-07-01"))%>%
  filter(ctv == "Environmetrics")%>%
  group_by(package)%>%
  summarise(count = sum (total))