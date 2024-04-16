table_one <- summariseCharacteristics(
  cdm[[matched_cohort_table_name]],
  strata = list(c("vaccine_brand", "exposed"), c("trimester", "exposed"), "exposed"),
  demographics = TRUE,
  ageGroup = list(c(12,24), c(25,39), c(40,55)),
  tableIntersect = list(
    "Number visits prior year" = list(
      tableName = "visit_occurrence", value = "count", window = c(-365, -1)
    )
  ),
  cohortIntersect = list(
    "Drugs prior year" = list(
      targetCohortTable = "medications", value = "flag", window = c(-365, -1)
    ),
    "Conditions any time prior" = list(
      targetCohortTable = "conditions", value = "flag", window = c(-Inf, -1)
    ),
    "PS conditions any time prior" = list(
      targetCohortTable = "ps_covariates", value = "flag", window = c(-Inf, -1)
    ),
    "COVID-19 any time prior" = list(
      targetCohortTable = "covid", value = "count", window = c(-Inf, -1)
    ),
    "Ohter vaccines any time prior" = list(
      targetCohortTable = "other_vax", value = "flag", window = c(-Inf, -1)
    )
  ),
  otherVariables = c("vaccine_brand", "trimester")
)

table_one |>
  write_csv(file = here(output_folder, paste0("characteristics_", cdmName(cdm), ".csv")))
