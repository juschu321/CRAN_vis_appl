#' aggregate by month and ctv
#'
#'
#' @param tutti_timeseries
#' @export
get_tutti_time_monthly_ctv <- function(tutti_timeseries) {
  tutti_timeseries %>%
    mutate(month = as.Date(cut(date, breaks = "month"))) %>%
    group_by(ctv, month) %>%
    summarise(total = sum(count))
}