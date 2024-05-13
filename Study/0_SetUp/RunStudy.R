output_folder <- here(results)
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# create logger ----
log_file <- here(results, paste0("log", "_", gsub("-", "", today()), ".txt"))
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
outcomes_table_name        <- "outcomes"
matched_cohort_table_name  <- "matched"
clean_mother_table_name    <- "mother_table"


# Load study functions
info(logger, "Load study functions ")
source(here("0_SetUp", "functions.R"))

# Database snapshot:
readr::write_csv(CDMConnector::snapshot(cdm), here(output_folder, paste0("cdm_snapshot_", cdmName(cdm), ".csv")))

if (runInstantiateCohorts) {
  cdm$condition_occurrence <- cdm$condition_occurrence %>%
    filter(condition_start_date <= condition_end_date)
  info(logger, "STEP 1 INSTANTIATE COHORTS ----")
  source(here("1_InstantiateCohorts", "instantiate_json.R"))
  source(here("1_InstantiateCohorts", "instantiate_nco.R"))
  source(here("1_InstantiateCohorts", "instantiate_vaccination_table.R"))
  source(here("1_InstantiateCohorts", "instantiate_source_pregnant.R"))
  source(here("1_InstantiateCohorts", "instantiate_outcomes.R"))
}

if (runPSMathcing) {
  if (! runInstantiateCohorts) {
    info(logger, "Load cohorts")
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
      cdmName = database_name,
      cohortTables = c(vaccine_json_table_name, medications_table_name, conditions_table_name,
                       covid_table_name, other_vaccines_table_name, ps_covariates_table_name,
                       nco_table_name, source_pregnant_table_name, clean_mother_table_name,
                       outcomes_table_name),
      .softValidation = TRUE
    )
    cdm$vaccine_schema <- tbl(db, inSchema(schema = results_database_schema, table = paste0(table_stem, "vaccine_schema"))) %>%
      compute()
  }
  info(logger, "STEP 2 MATCHING ----")
  source(here("2_Matching", "matching.R"))
}

if (runCharacterisation) {
  if (! runPSMathcing) {
    info(logger, "Load cohorts")
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
      cdmName = database_name,
      cohortTables = c(
        vaccine_json_table_name, medications_table_name, conditions_table_name,
        covid_table_name, other_vaccines_table_name, nco_table_name,
        source_pregnant_table_name, outcomes_table_name,
        matched_cohort_table_name, ps_covariates_table_name,
        clean_mother_table_name),
      .softValidation = TRUE
    )
    cdm$vaccine_schema <- tbl(db, inSchema(schema = results_database_schema, table = paste0(table_stem, "vaccine_schema"))) %>%
      compute()
  }
  info(logger, "STEP 3 EVALUATE COHORTS ----")
  source(here("3_Characterisation", "characteristics.R"))
  source(here("3_Characterisation", "cohort_stats.R"))
}

info(logger, "STEP 4 OUTCOME MODEL ----")
if (runOutcomeModel) {
  if (!runPSMathcing & !runCharacterisation) {
    info(logger, "Load cohorts")
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
      cdmName = database_name,
      cohortTables = c(nco_table_name, outcomes_table_name, matched_cohort_table_name)
    )
    cdm$vaccine_schema <- tbl(db, inSchema(schema = results_database_schema, table = paste0(table_stem, "vaccine_schema"))) %>%
      compute()
  }
  source(here("4_OutcomeModel", "get_survival_data.R"))
  source(here("4_OutcomeModel", "estimate_survival.R"))
}

info(logger, "STEP 4 ZIP RESULTS ----")
output_folder <- basename(output_folder)
zip(
  zipfile = paste0(output_folder, "_", gsub("-", "", today()), ".zip"),
  files = list.files(output_folder, full.names = TRUE)
)

dbDisconnect(db)

info(logger, " -- DONE! --")
