library(readr)
library(omopgenerics)
library(here)
library(dplyr)
library(tidyr)
library(visOmopResults)
library(stringr)
library(zip)

# Get data
folderName <- "Results_SIDIAP_20250114 (1)/Results_SIDIAP"
newCDMName <- "SIDIAP_20250114"

files <- list.files(here::here("dataOld", folderName), full.names = TRUE)
for (file in files) {
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) |>
    mutate(cdm_name = newCDMName) |>
    readr::write_csv(file)
}



