info(logger, "Getting cohort stats: ")
cohort <- cdm$matched %>%
  addCohortName() %>%
  mutate(overall = "overall") %>%
  compute()

# sample independence
info(logger, " - Control reenrollment as exposed")
recontributions <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort %>%
      filter(exposed == 1) %>%
      select(all_of(c("cohort_name", "subject_id", x))) %>%
      inner_join(
        cohort %>%
          filter(exposed == 0) %>%
          select(all_of(c("cohort_name", "subject_id", x))),
        by = c("cohort_name", "subject_id", x)
      ) %>%
      group_by(.data$cohort_name, .data[[x]])%>%
      tally(name = "estimate_value") %>%
      collect()
  }) %>% bind_rows() |>
  rename("group_level" = "cohort_name") |>
  mutate(
    result_type = "recontributions",
    estimate_name = "count",
    estimate_type = "integer",
    variable_name = "number subjects",
    variable_level = NA,
    cdm_name = cdmName(cdm),
    estimate_value = as.character(estimate_value),
    group_name = "cohort_name",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level))

# index date distribution
info(logger, " - Index date distribution")
index_date_bins <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort %>%
      group_by(cohort_name, cohort_start_date, .data[[x]])%>%
      tally(name = "estimate_value") %>%
      collect()
  }) %>% bind_rows() |>
  mutate(
    result_type = "index_date",
    cdm_name = cdmName(cdm),
    estimate_name = "count",
    estimate_type = "integer",
    variable_name = "number subjects",
    variable_level = NA,
    estimate_value = as.character(estimate_value),
    group_name = "cohort_name",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level))

# available follow-up
info(logger, " - Available follow-up")
followup_cohort_end <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort %>%
      group_by(cohort_name, .data[[x]], exposed)%>%
      mutate(time = !!datediff("cohort_start_date", "cohort_end_date")) %>%
      summarise(
        mean = mean(time),
        median = median(time),
        q25 = quantile(time, 0.25),
        q75 = quantile(time, 0.75),
        min = min(time),
        max = max(time)
      ) |>
      pivot_longer(
        cols = c("mean", "median", "q25", "q75", "min", "max"),
        names_to = "estimate_name", values_to = "estimate_value"
      ) |>
      collect()
  }) %>% bind_rows() |>
  rename("additional_level" = "exposed", "group_level" = "cohort_name") |>
  mutate(
    result_type = "followup_cohort_end",
    cdm_name = cdmName(cdm),
    estimate_type = "integer",
    variable_name = "followup time",
    variable_level = NA,
    estimate_value = as.character(estimate_value),
    additional_name = "exposed",
    additional_level = as.character(additional_level),
    group_name = "cohort_name"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level))

followup_pregnancy_end <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort %>%
      group_by(cohort_name, .data[[x]], exposed)%>%
      mutate(time = !!datediff("cohort_start_date", "pregnancy_end_date")) %>%
      summarise(
        mean = mean(time),
        median = median(time),
        q25 = quantile(time, 0.25),
        q75 = quantile(time, 0.75),
        min = min(time),
        max = max(time)
      ) |>
      pivot_longer(
        cols = c("mean", "median", "q25", "q75", "min", "max"),
        names_to = "estimate_name", values_to = "estimate_value"
      ) |>
      collect()
  }) %>% bind_rows() |>
  rename("additional_level" = "exposed", "group_level" = "cohort_name") |>
  mutate(
    result_type = "followup_pregnancy_end",
    cdm_name = cdmName(cdm),
    estimate_type = "integer",
    variable_name = "followup time",
    variable_level = NA,
    estimate_value = as.character(estimate_value),
    additional_name = "exposed",
    additional_level = as.character(additional_level),
    group_name = "cohort_name"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level))

write_csv(
  bind_rows(index_date_bins, recontributions, followup_cohort_end, followup_pregnancy_end) %>%
    mutate(
      package_name = "StudyCode",
      package_version = "today()"
    ),
  here(output_folder, paste0("cohort_stats", cdmName(cdm), ".csv"))
)
