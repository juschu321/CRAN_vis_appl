#required parameters for the application (aggregation steps for the basis)

library(readr)

packages_per_ctv <- read_csv("data/packages_per_ctv.csv", 
                             col_types = cols(X1 = col_skip()))


ctvs <- packages_per_ctv %>%
  distinct(ctv)

packages <- packages_per_ctv %>%
  distinct(package)

tutti_time_monthly_package <- read_csv("data/monthly_package.csv", 
                            col_types = cols(X1 = col_skip(), month = col_date(format = "%Y-%m-%d"), 
                                             total = col_integer()))

tutti_time_monthly_ctv <- read_csv("data/monthly_ctv.csv", 
                        col_types = cols(X1 = col_skip(), month = col_date(format = "%Y- %m- %d"), 
                                         total = col_integer()))