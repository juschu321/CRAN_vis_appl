#' get_subcategory_of_psy
#' 
#' returns a list of psy_packages with each subcategory
#' subcategories according to implemented clusters on the CRAN repository psychometrics
#' @param psy_packages list of packages of the ctv psychometrics
#' @export 
get_subcategory_of_psy_packages <- function(psy_packages) {
  html <-
    read_html("https://cran.r-project.org/web/views/Psychometrics.html")
  
  # target data frame
  psy_package_categories <- data.frame()
  
  
  # looping over all packages
  # "psy_packages" may need to be replaced
  for (package in psy_packages) {
    # Get all a (links) with text equal to the current package name
    # then select the next higher ul (unordered list)
    # then select the p (paragraph) before that ul
    # that p should contain the name of the categoriy
    # XPATH
    query_string <-
      paste0("//a[text() = '",
             package,
             "'][1]/ancestor::ul/preceding::p[1]")
    category <-
      html %>% html_node(xpath = query_string) %>% html_text(trim = TRUE)
    
    # remove colon of category name
    category <- substr(category, 1, nchar(category) - 1)
    
    psy_package_categories <-
      rbind(psy_package_categories,
            data.frame(package = package, category = category))
  }
  
  return (psy_package_categories)
}

psy_subcategories <- get_subcategory_of_psy_packages(psy_packages)