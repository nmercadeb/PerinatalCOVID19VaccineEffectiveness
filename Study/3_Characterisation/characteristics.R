# Table One ----
info(logger, "Summarise large scale characteristics")
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
      tableName = "mother_table", value = "count", window = c(-Inf, -1)
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
  eventInWindow = "condition_occurrence",
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

summarised_lsc |>
  mutate(
    additional_name = paste0(additional_name, " &&& exposed"),
    additional_level = paste0(additional_level, " &&& ", exposed)
  ) |>
  select(-exposed) |>
  bind_rows(smd) |>
  write_csv(file = here(output_folder, paste0("large_scale_characteristics_", cdmName(cdm), ".csv")))
