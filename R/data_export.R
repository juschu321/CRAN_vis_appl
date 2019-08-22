#' data export 
#' 
#' 
#' @param 
#' @export
export_data_to_csv <- function() {
  write.csv(tutti_time_monthly_ctv, file = "inst.app/data/monthly_ctv.csv")
  write.csv(tutti_time_monthly_package, file = "inst.app/data/monthly_package.csv")
  write.csv(packages_per_ctv, file = "data/packages_per_ctv.csv")
  write.csv(psy_sub, file = "data/psy_sub.csv")
  write.csv(global_edgelist, file = "data/edgelist.csv")
}