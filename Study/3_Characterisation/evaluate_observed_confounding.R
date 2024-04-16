info(logger, "Summarise large scale characteristics")
summarised_lsc <- summariseLargeScaleCharacteristics(
  cdm[[matched_cohort_table_name]],
  strata = list(c("vaccine_brand", "exposed"), c("trimester", "exposed"), "exposed"),
  window = list(c(-Inf, -366), c(-365, -31), c(-30, -1)),
  eventInWindow = c("condition_occurrence"),
  # episodeInWindow = c("drug_exposure"),
  indexDate = "cohort_start_date",
  minimumFrequency = 0.005
)
summarised_lsc <- summarised_lsc |>
  filter(strata_name != "overall", group_name != "overall") |>
  splitStrata() |>
  uniteStrata(cols = c("vaccine_brand", "trimester"))

info(logger, "Standardised Mean Differences")
asmd <- summarised_lsc %>%
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
    by = join_by(result_id, cdm_name, result_type, package_name, package_version, group_name, group_level, strata_name,
                 strata_level, variable_name, variable_level, additional_name, additional_level)
  ) %>%
  mutate(
    across(.cols = starts_with("percentage"), .fn = ~if_else(is.na(.x), 0, .x)),
    across(.cols = starts_with("count"), .fn = ~if_else(is.na(.x), 0, .x)),
    smd = (percentage_comparator/100 - percentage_reference/100) / sqrt((percentage_comparator/100*(1-percentage_comparator/100) + percentage_reference/100*(1-percentage_reference/100))/2),
    across(.cols = contains("count"), .fn = ~if_else(.x < 5 & .x > 0, NA, .x)),
    estimate_value = if_else(is.na(count_reference) | is.na(count_comparator), NA_character_, as.character(smd)),
    estimate_type = "numeric",
    estimate_name = "smd"
  ) |>
  select(all_of(resultColumns()))

summarised_lsc |>
  bind_rows(asmd) |>
  write_csv(file = here(output_folder, paste0("large_scale_characteristics_", cdmName(cdm), ".csv")))
