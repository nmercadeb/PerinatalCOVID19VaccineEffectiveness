info(logger, "Prepare data for survival")
cdm$survival_raw <- cdm$matched |>
  addCohortName() |>
  # mutate(id_censor = if_else(grepl("none", cohort_name), 1, 2)) |>
  # left_join(
  #   cdm$vaccine_schema |>
  #     filter(schema_id %in% c("booster_1", "booster_2")) |>
  #     mutate(id_censor = if_else(grepl("booster_1", schema_id), 1, 2)) |>
  #     select("subject_id", "censor_date" = "vaccine_date", "id_censor"),
  #   by = c("subject_id", "id_censor")
  # ) |>
  # select(-"id_censor") |>
  addCohortIntersectDate(
    targetCohortTable = nco_table_name,
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "nco_{cohort_name}"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = outcomes_table_name,
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "study_{cohort_name}"
  ) |>
  compute(name = "survival_raw", temporary = FALSE)
