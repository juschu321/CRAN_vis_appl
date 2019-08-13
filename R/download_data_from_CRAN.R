#' download_data_from_CRAN
#' 
#' by applying this function you can easily download the 
#' download statistics from the CRAN website from october 2012 until now
#' @param packages chr
#' @export
get_download_statistics <- function(packages) {
  download_statistics <- data.frame()
  for (package in packages) {
    current_package_stats <- cran_downloads(
      package = package,
      from    = as.Date("2012-10-01"),
      to      = Sys.Date() - 1
    )
    download_statistics <-
      rbind(download_statistics, current_package_stats)
  }
  return(download_statistics)
}

download_statistics <- get_download_statistics(packages = packages_per_ctv$package )
