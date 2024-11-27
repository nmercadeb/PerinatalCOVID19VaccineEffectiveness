library(readr)
library(omopgenerics)
library(here)
library(dplyr)
library(tidyr)
library(visOmopResults)
library(stringr)
library(zip)

# Get data
folderName <- "Results_scifi-pearl_20241119/Results_scifi-pearl"
newCDMName <- "SCIFI-PEARL"

files <- list.files(here::here("data", folderName), full.names = TRUE)
for (file in files) {
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) |>
    mutate(cdm_name = newCDMName) |>
    readr::write_csv(file)
}



