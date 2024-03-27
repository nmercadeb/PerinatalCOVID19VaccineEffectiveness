info(logger, "Instantiate codelists cohorts: ")
# info(logger, "  - Generate NCO codelists")
# source(here("Data", "codelistNCO.R"))

info(logger, "  - Load codelists")
# load(here("Data", "atc.RData"))
# load(here("Data", "icd.RData"))
load(here("Data", "nco.RData"))
#
# names(atc_codes) <- paste0("atc3rd_index_", 1:length(atc_codes))
# names(icd10_codes) <- paste0("icd10_index_", 1:length(icd10_codes))
#
# # atc
# info(logger, "  - ATC")
# cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
#   cdm = cdm,
#   conceptSet = atc_codes,
#   name = "atc_cohort"
# )
#
# # icd10
# cdm <- CDMConnector::generateConceptCohortSet(
#   cdm = cdm,
#   conceptSet = icd10_codes,
#   name = "icd_cohort",
#   limit = "all",
#   requiredObservation = c(0, 0),
#   end = "event_end_date",
#   overwrite = TRUE
# )
#
# nco
info(logger, "  - NCO")
cdm <- CDMConnector::generateConceptCohortSet(
  cdm = cdm,
  conceptSet = nco_codelists,
  name = nco_table_name,
  limit = "all",
  requiredObservation = c(0, 0),
  end = "event_end_date",
  overwrite = TRUE
)



