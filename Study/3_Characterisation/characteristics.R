# Table One ----
info(logger, "Characteristics")
table_one <- summariseCharacteristics(
  cdm[[matched_cohort_table_name]] |> addRegion(database_name = cdmName(cdm)),
  strata = list(c("vaccine_brand", "exposed"), c("trimester", "exposed"), "exposed"),
  demographics = TRUE,
  ageGroup = list(c(12,24), c(25,39), c(40,55)),
  tableIntersect = list(
    "Number visits prior year" = list(
      tableName = "visit_occurrence", value = "count", window = c(-365, -1)
    )
  ),
  cohortIntersect = list(
    "Drugs prior 180 days" = list(
      targetCohortTable = "medications", value = "flag", window = c(-180, -1)
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
    ),
    "Number prior pregnancies" = list(
      targetCohortTable = "mother_table", value = "count", window = c(-Inf, -1)
    )
  ),
  otherVariables = c("vaccine_brand", "trimester")
)

table_one |>
  write_csv(file = here(output_folder, paste0("characteristics_", cdmName(cdm), ".csv")))

# Large scale characteristics ----
info(logger, "Summarise large scale characteristics")
summarised_lsc <- summariseLargeScaleCharacteristics(
  cdm[[matched_cohort_table_name]],
  strata = list(c("vaccine_brand", "exposed"), c("trimester", "exposed"), "exposed"),
  window = list(c(-365, -181), c(-180, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 180), c(181, 365)),
  eventInWindow = c("condition_occurrence", "visit_occurrence"),
  episodeInWindow = "drug_exposure",
  indexDate = "cohort_start_date",
  minimumFrequency = 0.005
)
summarised_lsc <- summarised_lsc |>
  filter(group_name != "overall") |>
  splitStrata() |>
  uniteStrata(cols = c("vaccine_brand", "trimester"))

info(logger, "Standardised Mean Differences")
smd <- summarised_lsc %>%
  filter(exposed == "0") %>%
  select(-"exposed", -"estimate_type") %>%
  mutate(estimate_name = paste0(estimate_name, "_reference"),
         estimate_value = as.numeric(estimate_value)) %>%
  pivot_wider(names_from = estimate_name, values_from = estimate_value) %>%
  left_join(
    summarised_lsc %>%
      filter(exposed == "1") %>%
      select(-"exposed", -"estimate_type") %>%
      mutate(estimate_name = paste0(estimate_name, "_comparator"),
             estimate_value = as.numeric(estimate_value)) %>%
      pivot_wider(names_from = estimate_name, values_from = estimate_value),
    by = join_by(
      result_id, cdm_name, result_type, package_name, package_version,
      group_name, group_level, strata_name, strata_level, variable_name,
      variable_level, additional_name, additional_level
    )
  ) %>%
  mutate(
    across(.cols = starts_with("percentage"), .fn = ~if_else(is.na(.x), 0, .x)),
    across(.cols = starts_with("count"), .fn = ~if_else(is.na(.x), 0, .x)),
    smd = (percentage_comparator/100 - percentage_reference/100) / sqrt((percentage_comparator/100*(1-percentage_comparator/100) + percentage_reference/100*(1-percentage_reference/100))/2),
    across(.cols = contains("count"), .fn = ~if_else(.x < 5 & .x > 0, NA, .x)),
    estimate_value = if_else(is.na(count_reference) | is.na(count_comparator), NA_character_, as.character(smd)),
    estimate_type = "numeric",
    estimate_name = "smd",
    result_type = "large_scale_differences"
  ) |>
  select(all_of(resultColumns()))


## PS vars
psVars <- cdm$matched |>
  mutate(week_start = cohort_start_date) |>
  inner_join(
    union_all(
      cdm$source_pregnant |>
        mutate(cohort_definition_id = if_else(cohort_definition_id == 2, 3, cohort_definition_id)) |>
        select(
          "cohort_definition_id", "subject_id", "pregnancy_id", "previous_vaccine_date",
          "previous_vaccine_brand", "observation_period_start_date"
        ) |>
        distinct(),
      cdm$source_pregnant |>
        mutate(cohort_definition_id = if_else(cohort_definition_id == 1, 2, 4)) |>
        select(
          "cohort_definition_id", "subject_id", "pregnancy_id", "previous_vaccine_date",
          "previous_vaccine_brand", "observation_period_start_date"
        ) |>
        distinct()
    ) |>
      compute()
  ) |>
  matchItDataset(2)

continuous <- c("visits_m365_m181", "visits_m180_m31", "visits_m30_m1", "covid_test", "influenza",
                "tdap", "previous_pregnancies", "previous_observation", "days_previous_vaccine")
binary <- c("liver_disease_chronic_severe", "diabetes", "bloodcancer_within_past_5yr", "organ_transplant_receipient", "solid_cancer_within_past_5yr",
            "obesity", "immunodeficiency", "asthma_copd_bronchiectasis_bronchitis", "cardiologicaldisease_excl_hypertension")
categorical <- c("gestational_age", "previous_vaccine_brand", "days_previous_vaccine_band_month")
asmd <- list()
for (id in settings(cdm$matched)$cohort_definition_id) {
  asmd[[id]] <- bind_rows(
    psVars |>
      filter(cohort_definition_id == id) |>
      asmdCategorical(variables = categorical, groupName = "exposed") |>
      mutate(cohort_name = settings(cdm$matched)$cohort_name[id]),
    psVars |>
      filter(cohort_definition_id == id) |>
      asmdBinary(variables = binary, groupName = "exposed") |>
      mutate(cohort_name = settings(cdm$matched)$cohort_name[id]),
    psVars |>
      filter(cohort_definition_id == id) |>
      asmdContinuous(variables = continuous, groupName = "exposed") |>
      mutate(cohort_name = settings(cdm$matched)$cohort_name[id])
  )
}

asmd <- asmd |>
  bind_rows() |>
  filter(!is.na(asmd)) |>
  visOmopResults::uniteGroup("cohort_name") |>
  visOmopResults::uniteStrata() |>
  mutate(result_id = 1,
         cdm_name = cdmName(cdm),
         result_type = "large_scale_differences",
         package_name = "study_code",
         package_version = NA_character_,
         variable_level = "propensity_score",
         estimate_type = "numeric",
         estimate_name = "asmd",
         exposed = "overall", ) |>
  rename("variable_name" = "variable", "estimate_value" = "asmd") |>
  select(!c("asmd_type")) |>
  visOmopResults::uniteAdditional("exposed") |>
  newSummarisedResult()

summarised_lsc |>
  mutate(
    additional_name = paste0(additional_name, " &&& exposed"),
    additional_level = paste0(additional_level, " &&& ", exposed)
  ) |>
  select(-exposed) |>
  bind_rows(smd, asmd) |>
  write_csv(file = here(output_folder, paste0("large_scale_characteristics_", cdmName(cdm), ".csv")))
