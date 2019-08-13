#' from_web_api_to_a_list
#' 
#' #' receive the api data for each package
#' basis for dependency structure
#' returns a list of the corresponding api data
#' this function returns the api data as a list 
#' @param packages chr (list of package names)
#' @export 
r_api <- function(package) {
  url <- modify_url("http://crandb.r-pkg.org/", path = package)
  
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  parsed <-
    jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

get_api_data <- function (packages) {
  api_data <- list()
  for (package in packages) {
    package.api.data <- r_api(package)
    api_data <- c(api_data, list(package.api.data))
  }
  return(api_data)
}

api_data <- get_api_data(packages = packages_per_ctv$package)
