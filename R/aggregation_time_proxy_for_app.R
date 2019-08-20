#' aggregation_time
#' 
#' time aggregation online (not necessary now, because already aggregated beforehand)
#' @param filtered_data dataframe contains data according to parameter in filter function
#' @export
aggregate_timeseries_data <-
  function (timelevel = "monthly", filtered_data) {
    download_monthly <- filtered_data %>%
      mutate(month = as.Date(cut(date, breaks = "month"))) %>%
      group_by_at(vars(-c(date, count))) %>% # group by everything but date, day, count
      summarise(total = sum(count))
  }