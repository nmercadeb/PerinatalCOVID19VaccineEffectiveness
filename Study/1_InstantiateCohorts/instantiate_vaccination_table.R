info(logger, "Create vaccine schema table: ")
cdm$vaccine_schema <- cdm$vaccine_json %>%
  filter(cohort_definition_id == !!getId(cdm$vaccine_json, "any_covid_vaccine")) %>%
  select(-cohort_definition_id) %>%
  left_join(cdm$vaccine_json %>%
              filter(cohort_definition_id != !!getId(cdm$vaccine_json, "any_covid_vaccine")) %>%
              addCohortName() %>%
              select(-cohort_definition_id),
            by = c("subject_id", "cohort_start_date", "cohort_end_date")
  ) %>%
  mutate(cohort_name = if_else(is.na(cohort_name), "unkown", cohort_name)) %>%
  select(subject_id, vaccine_date = cohort_start_date, vaccine_brand = cohort_name) %>%
  group_by(subject_id) %>%
  window_order(vaccine_date) %>%
  mutate(dose_id = row_number()) %>%
  mutate(first_janssen = if_else(any(vaccine_brand == "janssen" & dose_id == 1), TRUE, FALSE)) %>%
  ungroup() %>%
  mutate(
    schema_id =
      if_else(first_janssen,
              case_when(
                dose_id == 1 ~ "complete",
                dose_id == 2 ~ "booster_1",
                dose_id == 3 ~ "booster_2",
                dose_id == 4 ~ "booster_3",
                dose_id == 5 ~ "booster_4",
                dose_id == 6 ~ "booster_5"),
              case_when(
                dose_id == 1 ~ "partial",
                dose_id == 2 ~ "complete",
                dose_id == 3 ~ "booster_1",
                dose_id == 4 ~ "booster_2",
                dose_id == 5 ~ "booster_3",
                dose_id == 6 ~ "booster_4",
                dose_id == 7 ~ "booster_5")
      )
  ) %>%
  compute(name = "vaccine_schema", temporary = FALSE) %>%
  newCdmTable(src = cdmSource(cdm), name = "vaccine_schema")


# censoring times:
deviaton.days <- 5
censor_vaccination <- cdm$vaccine_schema %>%
  addCohortIntersectDays(
    targetCohortTable = "vaccine_json",
    targetCohortId = getId(cdm$vaccine_json, "any_covid_vaccine"),
    indexDate = "vaccine_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(-Inf, -1),
    nameStyle = "days_prior_vaccine"
  ) %>%
  mutate(
    censor =
      case_when(
        schema_id == "complete" & vaccine_brand == "pfizer" &
          (abs(days_prior_vaccine) < days.pfizer-deviaton.days) ~ "Second dose of primary vaccine schema adiministered before recommended time",
        schema_id == "complete" & vaccine_brand == "astrazeneca" &
          (abs(days_prior_vaccine) < days.astrazeneca-deviaton.days) ~ "Second dose of primary vaccine schema adiministered before recommended time",
        schema_id == "complete" & vaccine_brand == "moderna" &
          (abs(days_prior_vaccine) < days.moderna-deviaton.days) ~ "Second dose of primary vaccine schema adiministered before recommended time",
        schema_id == "complete" & vaccine_brand == "janssen" &
          (abs(days_prior_vaccine) < booster.janssen-deviaton.days) ~ "Booster dose adiministered before recommended time",
        grepl("booster", schema_id) &
          (abs(days_prior_vaccine) < days.booster-deviaton.days) ~ "Booster dose adiministered before recommended time",
        .default = NA
      )
  ) %>%
  group_by(subject_id, vaccine_date) %>%
  mutate(
    censor = if_else(n() > 1, "Two vaccine records in the same day", censor)
  ) %>%
  filter(!is.na(censor)) %>%
  ungroup() %>%
  group_by(subject_id) %>%
  filter(vaccine_date == min(vaccine_date)) %>%
  ungroup() %>%
  select(subject_id, vaccine_censor_date = vaccine_date, censor) %>%
  distinct() %>%
  compute()

# clean vaccine table
