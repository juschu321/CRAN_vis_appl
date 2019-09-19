#' aggregate by month and package
#'
#' 
#' @param tutti_timeseries
#' @export
get_tutti_time_monthly_pkg <- function(tutti_timeseries) {
  tutti_time_monthly_package <- tutti_timeseries %>%
    mutate(month = as.Date(cut(date, breaks = "month"))) %>%
    distinct(package, month, count) %>%
    group_by(package, month) %>%
    summarise(total = sum(count))
  
  
}