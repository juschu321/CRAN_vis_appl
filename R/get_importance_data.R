#' get_importance_data
#'
#' basis for importance histogram plot
#' returns a dataframe containing all available packages and the number of dependencies for each dependency type
#' @export

get_importance_data <- function(edgelist, packages_per_ctv, tutti_time_monthly_package) {
  
  # Counting the references per package and type
  imp_data <-
    edgelist %>%
    group_by(to, type) %>%
    summarise(count = n())
  
  # Aggregating all available packages
  unique_packages_1 <- data.frame(package = unique(edgelist$from))
  unique_packages_2 <- data.frame(package = unique(edgelist$to))
  unique_packages_3 <- data.frame(package = unique(packages_per_ctv$packages))
  unique_packages <- unique(bind_rows(unique_packages_1, unique_packages_2, unique_packages_3))
  
  # Generate all combinations of dependency types and packages (Cartesian Product)
  packages_dependency_types <- crossing(unique_packages, data.frame(type=as.factor(c("depends", "imports", "suggests"))))
  
  # Merging the Cartesian Product with the calculated importance
  imp_data <- full_join(imp_data, packages_dependency_types, by=c("to" = "package", "type" = "type"))
  
  # Replace NA with 0
  imp_data$count <- ifelse(is.na(imp_data$count), 0, imp_data$count)
  
  # Add flags to indicate whether package is available in download statistics and/or dependency edgelist
  imp_data$inDownload <- ifelse(imp_data$to %in% tutti_time_monthly_package$package, TRUE, FALSE)
  imp_data$inEdgelist <- ifelse(imp_data$to %in% edgelist$from, TRUE, FALSE)
  
  return (imp_data)
}

