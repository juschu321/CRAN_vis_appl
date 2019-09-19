#' data export 
#' 
#' 
#' @param 
#' @export
export_data_to_csv <- function() {
  write.csv(tutti_time_monthly_ctv, file = "inst.app/refreshed_data/monthly_ctv.csv")
  write.csv(tutti_time_monthly_package, file = "inst.app/refreshed_data/monthly_package.csv")
  write.csv(packages_per_ctv, file = "inst.app/refreshed_data/packages_per_ctv.csv")
  write.csv(psy_sub, file = "inst.app/refreshed_data/psy_sub.csv")
  write.csv(global_edgelist, file = "inst.app/refreshed_data/edgelist.csv")
}