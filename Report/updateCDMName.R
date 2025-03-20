library(readr)
library(omopgenerics)
library(here)
library(dplyr)
library(tidyr)
library(visOmopResults)
library(stringr)
library(zip)

# Get data
folderName <- "Results_CPRD Gold_20250217/Results_CPRD Gold"
newCDMName <- "CPRD GOLD"

files <- list.files(here::here("data", folderName), full.names = TRUE)
for (file in files) {
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) |>
    mutate(cdm_name = newCDMName) |>
    readr::write_csv(file)
}



# From R file

for (nm in names(data)) {
  data[[nm]] <- data[[nm]] |>
    mutate(
      across(
        .cols = any_of(c("exposed", "outcome")),
        .fns = ~ str_to_sentence(gsub("_", " ", .x))
      ),
      across(
        .cols = where(is.character),
        .fns = ~ case_when(
          as.character(.x) == "complete_booster" ~ "Booster vs. Primary Vaccination",
          as.character(.x) == "none_first" ~ "Primary Vaccination vs. Unvaccination",
          as.character(.x) == "diagnostic_test" ~ "Diangostics and Positive Tests",
          as.character(.x) == "test" ~ "Positive Tests",
          as.character(.x) == "pfizer" ~ "BNT162b2",
          as.character(.x) == "moderna" ~ "mRNA-1273",
          as.character(.x) == "T1" ~ "Trimester 1",
          as.character(.x) == "T2" ~ "Trimester 2",
          as.character(.x) == "T3" ~ "Trimester 3",
          as.character(.x) == "vaccine_brand" ~ "Vaccine brand",
          as.character(.x) == "trimester" ~ "Trimester",
          as.character(.x) == "pregnancy_end" ~ "End of pregnancy",
          as.character(.x) == "cohort_end_date" ~ "End of data",
          as.character(.x) == "cohort_end_date_pregnancy" ~ "End of pregnancy",
          as.character(.x) == "Icu covid" ~ "COVID-19 related ICU",
          as.character(.x) == "Inpatient covid" ~ "COVID-19 related Hospitalisation",
          as.character(.x) == "Covid" ~ "COVID-19",
          .default = .x
        )
      )
    )
}

save(data, file = here("shinyData-mod.Rdata"))
