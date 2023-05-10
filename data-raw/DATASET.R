## code to prepare `DATASET` dataset goes here

library(tidyverse)

hdTypes <- read_csv("data-raw/hdTypes.csv")

usethis::use_data(hdTypes, overwrite = TRUE, internal = TRUE)
