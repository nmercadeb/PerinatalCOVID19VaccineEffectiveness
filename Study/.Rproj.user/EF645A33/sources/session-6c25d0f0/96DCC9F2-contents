# Read medication concept sets
info(logger, "Instantiate characterisation cohorts: ")

# Medications
info(logger, "  - Medications")
medications_cohort_set <- readCohortSet(
  path = here("1_InstantiateCohorts", "Cohorts", "Comedications")
)
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = medications_cohort_set,
  name = medications_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# Read condition cohorts
info(logger, "  - Conditions")
conditions_concept_list <- readConceptList(
  cdm = cdm,
  path = here("1_InstantiateCohorts", "Cohorts", "Comorbidities")
)

cdm <-  DrugUtilisation::generateConceptCohortSet(
  cdm = cdm,
  name = conditions_table_name,
  conceptSet = conditions_concept_list,
  limit = "all",
  end = "event_end_date",
  overwrite = TRUE
)

# COVID
info(logger, "  - COVID-19")
covid_json_cohort_set <- readCohortSet(
  here("1_InstantiateCohorts", "Cohorts", "Covid")
)

cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = covid_json_cohort_set,
  name = covid_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# COVID-19 vaccines
info(logger, "  - COVID-19 vaccines")
vaccine_json_cohort_set <- readCohortSet(here("1_InstantiateCohorts", "Cohorts", "CovidVaccines"))

cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = vaccine_json_cohort_set,
  name = vaccine_json_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)


# Ohter vax
info(logger, "  - Other vaccines")
other_vaccines_cohort_set <- readCohortSet(
  here("1_InstantiateCohorts", "Cohorts", "OtherVaccines")
)

cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = other_vaccines_cohort_set,
  name = other_vaccines_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# Conditions to prioritize vaccination CAT
info(logger, "  - Conditions for prior vaccination")
ps_covariates_cohort_set <- readCohortSet(
  here("1_InstantiateCohorts", "Cohorts", "Matching")
)

cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = ps_covariates_cohort_set,
  name = ps_covariates_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)


# export counts
json_cohort_counts <- cdm[[medications_table_name]] %>%
  cohort_set() %>%
  select(cohort_definition_id, cohort_name) %>%
  inner_join(cdm[[medications_table_name]] %>%
               cohort_count() %>%
               mutate(
                 number_records = as.integer64(number_records),
                 number_subjects = as.integer64(number_subjects),
                 cohort_group = "medications"
               )
  ) %>%
  union_all(cdm[[conditions_table_name]] %>%
              cohort_set() %>%
              select(cohort_definition_id, cohort_name) %>%
              inner_join(cdm[[conditions_table_name]] %>%
                           cohort_count() %>%
                           mutate(
                             number_records = as.integer64(number_records),
                             number_subjects = as.integer64(number_subjects),
                             cohort_group = "conditions"))) %>%
  union_all(cdm[[covid_table_name]] %>%
              cohort_set() %>%
              inner_join(cdm[[covid_table_name]] %>%
                           cohort_count() %>%
                           mutate(cohort_group = "covid"))) %>%
  union_all(cdm[[vaccine_json_table_name]] %>%
              cohort_set() %>%
              inner_join(cdm[[vaccine_json_table_name]] %>%
                           cohort_count() %>%
                           mutate(cohort_group = "covid_vaccines"))) %>%
  union_all(cdm[[other_vaccines_table_name]] %>%
              cohort_set() %>%
              inner_join(cdm[[other_vaccines_table_name]] %>%
                           cohort_count() %>%
                           mutate(cohort_group = "other_vaccines")))  %>%
  union_all(cdm[[ps_covariates_table_name]] %>%
              cohort_set() %>%
              inner_join(cdm[[ps_covariates_table_name]] %>%
                           cohort_count() %>%
                           mutate(cohort_group = "ps_covariates")))

write_csv(json_cohort_counts,
          here(output_folder, paste0("json_cohort_counts_", database_name, ".csv")))
