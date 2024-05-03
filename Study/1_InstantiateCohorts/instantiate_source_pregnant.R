# Mother table:
cdm$mother_table <- tbl(db, inSchema(schema = mother_table_schema, table = mother_table_name)) %>%
  compute()

# CLEAN MOTHER TABLE----
info(logger, "Clean mother table")
## Start with all population
cdm$mother_table <- cdm$mother_table %>%
  mutate(cohort_definition_id = 1,
         cohort_start_date = pregnancy_start_date,
         cohort_end_date = pregnancy_end_date) %>%
  rename("subject_id" = "person_id") %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  newCohortTable(.softValidation = TRUE)

## In observation at pregnancy start date
cdm$mother_table <- cdm$mother_table %>%
  addInObservation() %>%
  filter(in_observation == 1) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "In observation at pregnancy start date")

## In observation at pregnancy end date
cdm$mother_table <- cdm$mother_table %>%
  addInObservation(indexDate = "cohort_end_date") %>%
  filter(in_observation == 1) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "In observation at pregnancy end date")

## end date > start_date
cdm$mother_table <- cdm$mother_table %>%
  filter(pregnancy_start_date < pregnancy_end_date) %>%
  mutate(cohort_definition_id = 1) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Pregnancy end date > pregnancy start_date")

## gestational length < 308 days
cdm$mother_table <- cdm$mother_table %>%
  filter(!!datediff("pregnancy_start_date", "pregnancy_end_date") < 308) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Gestational length < 308 days")

## gestational length  days != 0
cdm$mother_table <- cdm$mother_table %>%
  filter(gestational_length_in_day != 0) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Gestational length days != 0")

## 2 pregnancies starting in the same day
cdm$mother_table <- cdm$mother_table %>%
  addCohortIntersectCount(
    targetCohortTable = "mother_table",
    window = list(c(0, Inf)),
    indexDate = "pregnancy_start_date",
    censorDate = "pregnancy_end_date",
    targetStartDate = "pregnancy_start_date",
    targetEndDate = "pregnancy_end_date",
    nameStyle = "overlap"
  ) %>%
  filter(overlap <= 1) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "No overlapping pregnancy records")

## enrollment period: start
cdm$mother_table <- cdm$mother_table %>%
  filter(pregnancy_end_date > study.start) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Pregnancy end date > study start date")

## enrollment period: end
cdm$mother_table <- cdm$mother_table %>%
  filter(pregnancy_start_date < enrollment.end) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Pregnancy start date < enrollment end date") %>%
  newCohortTable()

# SOURCE POPULATION ----
info(logger, "Inclusion pregnant cirteria")
## 1 year of prior observation
cdm$source_pregnant <- cdm$mother_table  %>%
  addDemographics() %>%
  filter(prior_observation >= 365) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "1 year of prior observation at pregnancy start date")

## age
cdm$source_pregnant <- cdm$source_pregnant %>%
  filter(age >=12 & age <= 55) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Age at pregnancy start date in [12, 55]")

## sex
cdm$source_pregnant <- cdm$source_pregnant %>%
  filter(sex == "Female") %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Sex: female")

## stop follow-up at vaccine irregularities
# censoring times:
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
        schema_id == "complete" & vaccine_brand == "pfizer" & (abs(days_prior_vaccine) < days.pfizer) ~ "Second dose of primary vaccine schema adiministered before recommended time",
        schema_id == "complete" & vaccine_brand == "astrazeneca" & (abs(days_prior_vaccine) < days.astrazeneca) ~ "Second dose of primary vaccine schema adiministered before recommended time",
        schema_id == "complete" & vaccine_brand == "moderna" & (abs(days_prior_vaccine) < days.moderna) ~ "Second dose of primary vaccine schema adiministered before recommended time",
        schema_id == "complete" & vaccine_brand == "janssen" & (abs(days_prior_vaccine) < booster.janssen) ~ "Booster dose adiministered before recommended time",
        grepl("booster", schema_id) & (abs(days_prior_vaccine) < days.booster) ~ "Booster dose adiministered before recommended time",
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
# save counts:
cdm$source_pregnant <- cdm$source_pregnant %>%
  left_join(censor_vaccination, by = "subject_id") %>%
  compute()
cdm$source_pregnant %>%
  filter(!is.na(censor)) %>%
  group_by(censor) %>%
  tally() %>%
  mutate(population = "pregnant") %>%
  ungroup() %>%
  union_all(censor_vaccination %>%
              group_by(censor) %>%
              tally() %>%
              mutate(population = "general") %>%
              ungroup()) %>%
  collect() %>%
  write_csv(file = here(output_folder, paste0("vaccine_records_censor_", database_name, ".csv")))
# exclude/censor at vaccine irregularities:
cdm$source_pregnant <- cdm$source_pregnant %>%
  select(-censor) %>%
  filter(is.na(vaccine_censor_date) | vaccine_censor_date - days(1) >= pregnancy_start_date) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "No irregular vaccine records before pregnancy start date")

# BY OBJECTIVE: 0-1, 2-3 ----
info(logger, "By objective inclusion criteria")
## 0-1, no vaccine before:
cdm$temp_none_first <- cdm$source_pregnant %>%
  compute(name = "temp_none_first", temporary = FALSE) %>%
  newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1, cohort_name = "none_first"))
cdm$temp_second_third <- cdm$source_pregnant %>%
  compute(name = "temp_second_third", temporary = FALSE) %>%
  newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1, cohort_name = "complete_booster"))

cdm <- omopgenerics::bind(cdm$temp_none_first, cdm$temp_second_third, name = "source_pregnant")
cdm <- dropTable(cdm, starts_with("temp"))

cdm$source_pregnant <- cdm$source_pregnant %>%
  mutate(cohort_start_date = if_else(.data$cohort_start_date < .env$study.start, .env$study.start, .data$cohort_start_date)) %>%
  # NO index vaccine before pregnancy start
  left_join(cdm$vaccine_schema %>%
              filter(dose_id == 1 | schema_id == "booster_1") %>%
              mutate(cohort_definition_id = if_else(dose_id == 1, 1, 2)) %>%
              select(cohort_definition_id, subject_id, index_vaccine_date = vaccine_date, index_vaccine_brand = vaccine_brand),
            by = c("cohort_definition_id", "subject_id")) %>%
  filter(is.na(index_vaccine_date) | index_vaccine_date > cohort_start_date) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "No index vaccine before pregnancy start") %>%
  # second (for 2-3) vaccine before pregnancy start
  left_join(cdm$vaccine_schema %>%
              filter(schema_id == "complete") %>%
              mutate(cohort_definition_id = 2) %>%
              select(cohort_definition_id, subject_id, previous_vaccine_date = vaccine_date, previous_vaccine_brand = vaccine_brand),
            by = c("cohort_definition_id", "subject_id")) %>%
  filter((previous_vaccine_date + days(90) <= pregnancy_end_date & !is.na(previous_vaccine_date)) | cohort_definition_id == 1) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Elegible for booster before pregnancy end date", cohortId = 2)

# Set cohort dates and columns to keep:
cdm$source_pregnant <- cdm$source_pregnant %>%
  select(-cohort_end_date) %>%
  left_join(cdm$observation_period %>%
              select(subject_id = person_id, cohort_end_date = observation_period_end_date, observation_period_start_date),
            by = "subject_id") %>%
  mutate(
    cohort_end_date = if_else(!is.na(vaccine_censor_date) & vaccine_censor_date <= cohort_end_date,
                              !!dateadd("vaccine_censor_date", -1), cohort_end_date)
    ) %>%
  addCohortIntersectDate(
    targetCohortTable = "mother_table",
    indexDate = "pregnancy_end_date",
    window = c(0, Inf),
    nameStyle = "next_pregnancy_date"
  ) %>%
  mutate(
    cohort_end_date = if_else(!is.na(next_pregnancy_date) & next_pregnancy_date <= cohort_end_date,
                              !!dateadd("next_pregnancy_date", -1), cohort_end_date),
    cohort_end_date = as.Date(cohort_end_date)
  ) %>%
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, pregnancy_id,
         pregnancy_start_date, pregnancy_end_date, age, index_vaccine_date, index_vaccine_brand,
         previous_vaccine_date, previous_vaccine_brand, observation_period_start_date) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  newCohortTable()

