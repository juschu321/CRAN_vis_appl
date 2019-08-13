#' aggregation_time
#' 
#' time aggregation online (not necessary now, because already aggregated beforehand)
#' @param filtered_data dataframe contains data according to parameter in filter function
#' @param tutti_timeseries dataframe contains downloadstatistics per package (+ corresponding ctv)
#' @export
aggregate_timeseries_data <-
  function (timelevel = "monthly", filtered_data) {
    download_monthly <- filtered_data %>%
      mutate(month = as.Date(cut(date, breaks = "month"))) %>%
      group_by_at(vars(-c(date, count))) %>% # group by everything but date, day, count
      summarise(total = sum(count))
  }

#further preparation/ aggregation as basis for the application
#data basis for timeline ctv 
tutti_time_monthly_ctv <-tutti_timeseries %>%
  select(date,count,ctv)%>%
  mutate(month = as.Date(cut(date, breaks = "month"))) %>%
  group_by_at(vars(-c(date, count))) %>% # group by everything but date, day, count
  summarise(total = sum(count))

#data basis for timeline package
tutti_time_monthly_package <-tutti_timeseries %>%
  select(date,count,package)%>%
  mutate(month = as.Date(cut(date, breaks = "month"))) %>%
  group_by_at(vars(-c(date, count))) %>% # group by everything but date, day, count
  summarise(total = sum(count))