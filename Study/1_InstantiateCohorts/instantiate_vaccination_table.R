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
