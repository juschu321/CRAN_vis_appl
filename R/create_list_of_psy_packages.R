#' create_list_of_psy_packages
#'
#' returns a list of packages of the ctv psychometrics
#' @param
#' @export
get_psy_packages <- function() {
  ctv::available.views()[[30]]$packagelist$name
}
