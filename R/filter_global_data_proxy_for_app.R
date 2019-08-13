#' filter_global_data 
#' 
#' used as basis for the filter options in the application
#' just works in the application with the input from the filter
#' @param selected_from date 
#' @param selected_to date
#' @param selected_packages chr names of packages
#' @param selected_min_count int
#' @param selected_ctv chr 
#' @param level chr
#' @export
filter_timeseries_data <-
  function (selected_from = as.Date("2012-10-01"),
            selected_to = Sys.Date() - 1,
            selected_packages = list(),
            selected_min_count = 0,
            selected_ctv = list(),
            level = "ctv") {
    if (level == "package") {
      filtered_data <- tutti_timeseries %>%
        filter (package == selected_packages) %>%
        filter (date >= selected_from, date <= selected_to) %>%
        filter (count >= selected_min_count) %>%
        select (date, count, package)
    }
    else{
      filtered_data <- tutti_timeseries %>%
        filter (ctv == selected_ctv) %>%
        filter (date >= selected_from, date <= selected_to) %>%
        filter (count >= selected_min_count) %>%
        select (date, count, ctv)
    }
    
    return (filtered_data)
  }
