#' get_packages_per_ctv
#' 
#' by this function you sort the packages to the corresponding ctv 
#' it returns a list with the packages and ctv
#' @param 
#' @export
get_packages_per_ctv <- function () {
  package_ctv <- data.frame()
  for (view in ctv::available.views()) {
    packagelist <- view[["packagelist"]]
    for (i in 1:dim(packagelist)[1]) {
      core <- (packagelist[["core"]][i])
      name <- (packagelist[["name"]][i])
      package_ctv <-
        rbind(package_ctv,
              data.frame(
                package = name,
                ctv = view$name,
                core = core
              ))
    }
  }
  return(package_ctv)
}