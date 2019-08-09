#' get_merged_data_tutti_timeseries
#' 
#' merge data as basis for timeline structure
#' with ctv classification
#' to be able to use the ctv category as well (additional information)
#' without core-packages
#' @param global_download list of all downloads
#' @param packages_per_ctv list of corresponding packages + ctv
#' @export 
tutti_timeseries <-
  inner_join(global_download, packages_per_ctv, by = c("package" = "package"))%>%
  filter(core == FALSE)