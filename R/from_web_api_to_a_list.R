#' from_web_api_to_a_list
#' 
#' this function returns the api data as a list 
#' @param packages chr (list of package names)
#' @export 
get_api_data <- function (packages) {
  api_data <- list()
  for (package in packages) {
    package.api.data <- r_api(package)
    api_data <- c(api_data, list(package.api.data))
  }
  return(api_data)
}