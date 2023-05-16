## code to prepare `DATASET` dataset goes here

library(tidyverse)

hdtypes <- read_csv("data-raw/hdtypes.csv", show_col_types = FALSE)

hdtypes <- hdtypes |>
  filter(implemented == 1)

usethis::use_data(hdtypes, overwrite = TRUE, internal = TRUE)
