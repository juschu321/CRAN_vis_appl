#required parameters for the application (aggregation steps for the basis)

library(cranlogs)
library(ctv)
library(dplyr)
library(ggplot2)
library(scales)
library(cranly)
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(magrittr)
library(xml2)
library(lubridate)
library(plotly)
library(readr)
library(visNetwork)

packages_per_ctv <- read_csv("data/packages_per_ctv.csv",
                             col_types = cols(X1 = col_skip()))

tutti_time_monthly_package <- read_csv(
  "data/monthly_package.csv",
  col_types = cols(
    X1 = col_skip(),
    month = col_date(format = "%Y-%m-%d"),
    total = col_integer()
  )
)

tutti_time_monthly_ctv <- read_csv(
  "data/monthly_ctv.csv",
  col_types = cols(
    X1 = col_skip(),
    month = col_date(format = "%Y- %m- %d"),
    total = col_integer()
  )
)

packages_per_psy_sub <- read_csv("data/psy_sub.csv",
                                 col_types = cols(X1 = col_skip()))

ctvs <- packages_per_ctv %>%
  distinct(ctv)

packages <- packages_per_ctv %>%
  filter(core == FALSE) %>%
  distinct(package)

psy_subs <- packages_per_psy_sub %>%
  distinct(category)

edgelist <- read_csv("data/edgelist.csv",
                     col_types = cols(X1 = col_skip()))
names(edgelist) <- c("from", "to", "type")
edgelist$type <- as.factor(edgelist$type)

n_ctv_p_pkg <- packages_per_ctv %>%
  filter(core == FALSE) %>%
  select(package, ctv) %>%
  group_by(package)%>%
summarise(count = n())