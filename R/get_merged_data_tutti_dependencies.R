#' get_merged_data_tutti_dependencies
#' 
#' merge data as basis for dependency structure
#' with ctv classification
#' to be able to use the ctv category as well (additional information)
#' @param global_edgelist
#' @param packages_per_ctv
#' @export 
get_tutti_dependencies <- function(){tutti_dependencies <-
  inner_join(global_edgelist, packages_per_ctv, by = c ("this" = "package"))}

