#' get_api_data_from_the_web
#' 
#' receive the api data for each 
#' basis for dependency structure
#' returns a list of the corresponding api data
#' @param package chr 
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


