## code to prepare `DATASET` dataset goes here

library(tidyverse)

hdTypes <- read_csv("data-raw/hdTypes.csv", show_col_types = FALSE)

hdTypes <- hdTypes |>
  filter(implemented == 1)

usethis::use_data(hdTypes, overwrite = TRUE, internal = TRUE)
