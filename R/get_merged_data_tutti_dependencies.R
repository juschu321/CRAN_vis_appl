#' get_merged_data_tutti_dependencies
#' 
#' merge data as basis for dependency structure
#' with ctv classification
#' to be able to use the ctv category as well (additional information)
#' @param global_edgelist (created in in data.prep)
#' @param packages_per_ctv (created by get_packages_per_ctv())
#' @export 
get_tutti_dependencies <- function(global_edgelist, packages_per_ctv){tutti_dependencies <-
  inner_join(global_edgelist, packages_per_ctv, by = c ("this" = "package"))}

