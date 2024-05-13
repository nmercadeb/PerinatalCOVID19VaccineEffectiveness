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
    variable_level = cohort_start_date,
    estimate_value = as.character(estimate_value),
    group_name = "cohort_name",
    group_level = cohort_name,
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level)) |>
  select(!c("cohort_name", "cohort_start_date"))

# 2nd dose distribution in 1st vs. none
info(logger, " - 2nd dose distribution")
cohort_obj1 <- cohort |>
  filter(cohort_definition_id %in% 1:2, exposed == 1) |>
  addCohortIntersectDate(
    targetCohortTable = "vaccine_json",
    targetCohortId = 1,
    indexDate = "cohort_start_date",
    censorDate = "pregnancy_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(1, Inf),
    nameStyle = "second_dose_date"
  ) |>
  filter(!is.na(second_dose_date))
second_dose_bins <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort_obj1 %>%
      group_by(cohort_name, second_dose_date, .data[[x]])%>%
      tally(name = "estimate_value") %>%
      collect()
  }) %>% bind_rows() |>
  mutate(
    result_type = "second_dose_date",
    cdm_name = cdmName(cdm),
    estimate_name = "count",
    estimate_type = "integer",
    variable_name = "number subjects",
    variable_level = second_dose_date,
    estimate_value = as.character(estimate_value),
    group_name = "cohort_name",
    group_level = cohort_name,
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level)) |>
  select(!c("second_dose_date", "cohort_name"))

# Primary schema distribution
info(logger, " - Primary schema distribution")
cohort_obj2 <- cohort |>
  filter(cohort_definition_id %in% 3:4) %>%
  inner_join(
    cdm$vaccine_schema |>
      filter(dose_id %in% 1:2) |>
      select(subject_id, vaccine_date, dose_id) %>%
      pivot_wider(names_from = dose_id, names_prefix = "vaccine_", values_from = "vaccine_date"),
    by = "subject_id"
  )

first_prior_dose_bins <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort_obj2 %>%
      group_by(cohort_name, vaccine_1, .data[[x]])%>%
      tally(name = "estimate_value") %>%
      collect()
  }) %>% bind_rows() |>
  mutate(
    result_type = "first_prior_dose",
    cdm_name = cdmName(cdm),
    estimate_name = "count",
    estimate_type = "integer",
    variable_name = "number subjects",
    variable_level = vaccine_1,
    estimate_value = as.character(estimate_value),
    group_name = "cohort_name",
    group_level = cohort_name,
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level)) |>
  select(!c("vaccine_1", "cohort_name"))

second_prior_dose_bins <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort_obj2 %>%
      group_by(cohort_name, vaccine_2, .data[[x]])%>%
      tally(name = "estimate_value") %>%
      collect()
  }) %>% bind_rows() |>
  mutate(
    result_type = "second_prior_dose",
    cdm_name = cdmName(cdm),
    estimate_name = "count",
    estimate_type = "integer",
    variable_name = "number subjects",
    variable_level = vaccine_2,
    estimate_value = as.character(estimate_value),
    group_name = "cohort_name",
    group_level = cohort_name,
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level)) |>
  select(!c("vaccine_2", "cohort_name"))

write_csv(
  bind_rows(index_date_bins, recontributions, second_dose_bins, first_prior_dose_bins, second_prior_dose_bins) %>%
    mutate(
      package_name = "StudyCode",
      package_version = today()
    ),
  here(output_folder, paste0("cohort_stats_", cdmName(cdm), ".csv"))
)
