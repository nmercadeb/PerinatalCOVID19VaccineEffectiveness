output_folder <- here(results)
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# create logger ----
log_file <- here(results, "log.txt")
if (file.exists(log_file)) {
  unlink(log_file)
}

logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"
info(logger, "CREATE LOGGER")

# STEP 0 Load study parameters and functions ----
info(logger, "STEP 0 INITIAL SETTINGS ----")

info(logger, "Load study parameters")

# Dates:
enrollment.end <- study.end - days(90)

# Instantiate cohorts:
table_stem <- tolower(table_stem)
vaccine_json_table_name    <- "vaccine_json"
medications_table_name     <- "medications"
conditions_table_name      <- "conditions"
covid_table_name           <- "covid"
other_vaccines_table_name  <- "other_vax"
ps_covariates_table_name   <- "ps_covariates"
nco_table_name             <- "nco_cohort"
source_pregnant_table_name <- "source_pregnant"
mother_table_name          <- "mother_table"
outcomes_table_name        <- "outcomes"
matched_cohort_table_name  <- "matched"


# Load study functions
info(logger, "Load study functions ")
source(here("0_SetUp", "functions.R"))

# Database snapshot:
readr::write_csv(CDMConnector::snapshot(cdm), here(output_folder, paste0("cdm_snapshot_", cdmName(cdm), ".csv")))

info(logger, "STEP 1 INSTANTIATE COHORTS ----")
if (runInstantiateCohorts) {
  source(here("1_InstantiateCohorts", "instantiate_json.R"))
  source(here("1_InstantiateCohorts", "instantiate_nco.R"))
  source(here("1_InstantiateCohorts", "instantiate_vaccination_table.R"))
  source(here("1_InstantiateCohorts", "instantiate_source_pregnant.R"))
  source(here("1_InstantiateCohorts", "instantiate_outcomes.R"))
} else  {
  cdm <- cdmFromCon(
    con = db,
    cdmSchema = cdm_database_schema,
    writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
    cdmName = database_name,
    cohortTables = c(vaccine_json_table_name, medications_table_name, conditions_table_name,
                     covid_table_name, other_vaccines_table_name, ps_covariates_table_name,
                     nco_table_name, source_pregnant_table_name, mother_table_name, outcomes_table_name)
  )
}

info(logger, "STEP 2 MATCHING ----")
if (runPSMathcing) {
  source(here("2_PSMatching", "matching.R"))
}
# if (runPSMathcing) {
#   if (! runInstantiateCohorts) {
#     info(logger, "Load cohorts")
#
    # cdm <- cdmFromCon(
    #   con = db,
    #   cdmSchema = cdm_database_schema,
    #   writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
    #   cdmName = database_name,
    #   cohortTables = c(vaccine_json_table_name, medications_table_name,
    #                    conditions_table_name, other_vaccines_table_name,
    #                    covid_table_name, source_pregnant_table_name,
    #                    atc_table_name, icd_table_name, ps_covariates_table_name,
    #                    nco_table_name, outcomes_table_name)
    # )
#     mother_table <- tbl(db, sql(sql_mother))
#     if (motherChildLinkage) {
#       child_table <- tbl(db, sql(child_table_name))
#     }
#     vaccine_schema <- tbl(db, sql(paste0("SELECT * FROM ", results_database_schema, ".", table_stem, "vaccine_schema")))
#     cdm <- CDMConnector::cdm_subset(cdm, cdm$source_pregnant %>% distinct(subject_id) %>% pull())
#   }
#
#   source(here("2_PSMatching", "feature_extraction_pregnant.R"))
#   source(here("2_PSMatching", "matching_pregnant_obj1.R"))
#   source(here("2_PSMatching", "survival_cohorts.R"))
#
# }
#
# info(logger, "STEP 3 EVALUATE COHORTS ----")
# if (runEvaluateCohorts) {
#   if (! runPSMathcing) {
#     info(logger, "Load cohorts")
#
#     cdm <- cdmFromCon(
#       con = db,
#       cdmSchema = cdm_database_schema,
#       writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
#       cdmName = database_name,
#       cohortTables = c(vaccine_json_table_name, medications_table_name,
#                        conditions_table_name, other_vaccines_table_name,
#                        covid_table_name, source_pregnant_table_name,
#                        atc_table_name, icd_table_name, nco_table_name,
#                        matched_cohort_table_name, ps_covariates_table_name,
#                        outcomes_table_name)
#     )
#     cdm$mother_table <- tbl(db, sql(sql_mother))
#     if (motherChildLinkage) {
#       cdm$child_table <- tbl(db, sql(child_table_name))
#     }
#     cdm$vaccine_schema <- tbl(db, sql(paste0("SELECT * FROM ", results_database_schema, ".", table_stem, "vaccine_schema")))
#     cdm <- CDMConnector::cdm_subset(cdm, cdm$source_pregnant %>% distinct(subject_id) %>% pull())
#   }
#
#   source(here("3_EvaluateCohorts", "evaluate_observed_confounding.R"))
#   source(here("3_EvaluateCohorts", "characterise_cohorts.R"))
# }
#
# info(logger, "STEP 4 OUTCOME MODEL ----")
# if (runOutcomeModel) {
#   if (!runPSMathcing & !runEvaluateCohorts) {
#     info(logger, "Load cohorts")
#
#     cdm <- cdmFromCon(
#       con = db,
#       cdmSchema = cdm_database_schema,
#       writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
#       cdmName = database_name,
#       cohortTables = c(vaccine_json_table_name, medications_table_name,
#                        conditions_table_name, other_vaccines_table_name,
#                        covid_table_name, source_pregnant_table_name,
#                        atc_table_name, icd_table_name, nco_table_name,
#                        matched_cohort_table_name, ps_covariates_table_name,
#                        outcomes_table_name)
#     )
#     cdm$mother_table <- tbl(db, sql(sql_mother))
#     if (motherChildLinkage) {
#       cdm$child_table <- tbl(db, sql(child_table_name))
#     }
#     cdm$vaccine_schema <- tbl(db, sql(paste0("SELECT * FROM ", results_database_schema, ".", table_stem, "vaccine_schema")))
#     cdm <- CDMConnector::cdm_subset(cdm, cdm$source_pregnant %>% distinct(subject_id) %>% pull())
#   }
#
#   source(here("4_OutcomeModel", "survival.R"))
#
# }
#
# info(logger, "STEP 4 ZIP RESULTS ----")
# output_folder <- basename(output_folder)
# zip(
#   zipfile = paste0(output_folder, ".zip"),
#   files = list.files(output_folder, full.names = TRUE)
# )
#
# info(logger, " -- DONE! --")
