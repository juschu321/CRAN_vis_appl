#' get_edgelist_of_packages
#' 
#' takes the api data as list and returns
#' a edgelist of corresponding "depends", 
#' "imports" and "suggests"
#' @param api_data_of_packages list of packages (data from api web) 
#' and corresponding information/ characteristics 
#' @export 
get_edgelist_of_packages <- function (api_data_of_packages) {
  edgelist <- data.frame()
  for (package in api_data_of_packages) {
    for (dependency in names(package$Depends)) {
      edgelist <-
        rbind(
          edgelist,
          data.frame(
            this = package$Package,
            needs_this = dependency,
            type = "depends"
          )
        )
    }
    for (import_dependency in names(package$Imports)) {
      edgelist <-
        rbind(
          edgelist,
          data.frame(
            this = package$Package,
            needs_this = import_dependency,
            type = "imports"
          )
        )
    }
    
    for (import_dependency in names(package$Suggests)) {
      edgelist <-
        rbind(
          edgelist,
          data.frame(
            this = package$Package,
            needs_this = import_dependency,
            type = "suggests"
          )
        )
    }
  }
  
  return(edgelist)
}

edgelist <- get_edgelist_of_packages(api_data)
