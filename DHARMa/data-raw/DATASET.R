#' hurricanes
#' #' Hurrican dataset published by "Jung et al., PNAS, 2014"

library(tidyverse)
library(readxl)
library(usethis)
hurricanes <- readxl::read_excel('data-raw/Hurricane.xlsx',
                          range = "A1:N93",
                          col_names = TRUE,
                          na = "(NA)")

hurricanes$Minpressure_Updated.2014 <- hurricanes$`Minpressure_Updated 2014`
usethis::use_data(hurricanes, overwrite = TRUE, compress = 'xz')
