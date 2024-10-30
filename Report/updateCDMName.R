library(readr)
library(omopgenerics)
library(here)
library(dplyr)
library(tidyr)
library(visOmopResults)
library(stringr)
library(zip)

# Get data
folderName <- "Results_MBRN_UiO_MBRN_20241028/Results_MBRN_UiO_MBRN"
newCDMName <- "UiO_MBRN"

files <- list.files(here::here("data", folderName), full.names = TRUE)
for (file in files) {
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) |>
    mutate(cdm_name = newCDMName) |>
    readr::write_csv(file)
}



