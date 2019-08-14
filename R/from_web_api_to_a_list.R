#' from_web_api_to_a_list
#'
#' #' receive the api data for each package
#' basis for dependency structure
#' returns a list of the corresponding api data
#' this function returns the api data as a list
#' @param packages chr (list of package names)
#' @export

get_api_data <- function (packages) {
  api_data <- list()
  for (package in packages) {
    url <- modify_url("http://crandb.r-pkg.org/", path = package)
    
    resp <- GET(url)
    if (http_type(resp) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    package.api.data <-
      jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    api_data <- c(api_data, list(package.api.data))
  }
  return(api_data)
}
