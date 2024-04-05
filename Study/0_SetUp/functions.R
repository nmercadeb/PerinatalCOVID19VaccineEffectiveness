getId <- function(x, name) {
  settings(x) %>%
    filter(.data$cohort_name == name) %>%
    pull("cohort_definition_id")
}


generateVisitRelatedOutcomes <- function(codes, window, name, attritionReason) {
  # covid visit cohort
  covid_visit <- cdm$temp_covid %>%
    inner_join(
      cdm$visit_occurrence %>%
        filter(visit_concept_id %in% c(codes)) %>%
        filter(visit_start_date >= study.start)  %>%
        select(subject_id = person_id, visit_start_date) %>%
        distinct(),
      by = "subject_id"
    ) %>%
    mutate(diff_days = visit_start_date - cohort_start_date) %>%
    filter(diff_days >= !!window[1] & diff_days <= !!window[2]) %>%
    compute()
  cdm[[paste0("temp_", name)]] <- covid_visit %>%
    select(-visit_start_date, -diff_days) %>%
    compute(name = paste0("temp_", name), temporary = FALSE) %>%
    recordCohortAttrition(reason = attritionReason) %>%
    newCohortTable(cohortSetRef = settings(cdm$temp_covid) %>%
                     mutate(cohort_name = paste0(name, "_", cohort_name)))
  # covid visit cohort - delivery date
  cdm[[paste0("temp_", name, "_delivery")]] <- covid_visit %>%
    addCohortIntersectFlag(
      targetCohortTable = "mother_table",
      window = c(0,0),
      indexDate = "visit_start_date",
      targetStartDate = "cohort_end_date",
      nameStyle = "is_delivery_date") %>%
    filter(is_delivery_date == 0) %>%
    select(-visit_start_date, -diff_days) %>%
    compute(name = paste0("temp_", name, "_delivery"), temporary = FALSE) %>%
    recordCohortAttrition(reason = paste0(attritionReason, " (not delivery)")) %>%
    newCohortTable(cohortSetRef = settings(cdm$temp_covid) %>%
                     mutate(cohort_name = paste0(name, "_no_delivery_", cohort_name)))
  return(cdm)
}

addMatchingAgeGroup <- function(x) {
  return(x %>%
           mutate(
             maternal_age = case_when(
               .data$age >= 12 & .data$age < 14 ~ 12,
               .data$age >= 14 & .data$age < 16 ~ 14,
               .data$age >= 16 & .data$age < 18 ~ 16,
               .data$age >= 18 & .data$age < 20 ~ 18,
               .data$age >= 20 & .data$age < 22 ~ 20,
               .data$age >= 22 & .data$age < 24 ~ 22,
               .data$age >= 24 & .data$age < 26 ~ 24,
               .data$age >= 26 & .data$age < 28 ~ 26,
               .data$age >= 28 & .data$age < 30 ~ 28,
               .data$age >= 30 & .data$age < 32 ~ 30,
               .data$age >= 32 & .data$age < 34 ~ 32,
               .data$age >= 34 & .data$age < 36 ~ 34,
               .data$age >= 36 & .data$age < 38 ~ 36,
               .data$age >= 38 & .data$age < 40 ~ 38,
               .data$age >= 40 & .data$age < 42 ~ 40,
               .data$age >= 42 & .data$age < 44 ~ 42,
               .data$age >= 44 & .data$age < 46 ~ 44,
               .data$age >= 46 & .data$age < 48 ~ 46,
               .data$age >= 48 & .data$age < 50 ~ 48,
               .data$age >= 50 & .data$age < 52 ~ 50,
               .data$age >= 52 & .data$age < 54 ~ 52,
               .data$age >= 54 & .data$age < 56 ~ 54
             )
           )
  )
}

addRegion <- function(x, database_name) {
  if(database_name != "CPRD") {
    x %>%
      left_join(
        cdm$person %>%
          left_join(cdm$location, by = "location_id") %>%
          select(subject_id = person_id, region = location_source_value),
        by = "subject_id"
      )
  } else {
    x %>%
      left_join(
        cdm$person %>%
          left_join(cdm$care_site, by = "care_site_id") %>%
          select(person_id, location_id = location_id.y) %>%
          left_join(cdm$location, by = "location_id") %>%
          select(subject_id = person_id, region = location_source_value),
        by = "subject_id"
      )
  }

}

pregnantMatchingTable <- function(sourceTable, covidId, weekStart, weekEnd, excludeControls, objective_id, days.booster) {
  temp <- sourceTable %>%
    mutate(week_start = as.Date(weekStart), week_end = as.Date(weekEnd)) %>%
    # pregnant at week.k
    filter(
      pregnancy_start_date <= week_start &  # start before the week
        pregnancy_end_date >= week_end      # end after the week
    ) %>%
    # in observation at week.k
    filter(
      cohort_start_date <= week_start &  # start before the week
        cohort_end_date >= week_end      # end after the week
    ) %>%
    # no covid-19 in the last three months from week start date
    addCohortIntersectDate(
      targetCohortTable = "covid",
      targetCohortId = covidId,
      window = c(-Inf, -1),
      order = "last",
      indexDate = "week_start",
      nameStyle = "covid_date"
    ) %>%
    filter(week_start - covid_date >= 90 | is.na(covid_date)) %>%
    select(-covid_date) %>%
    # classify exposed - unexposed
    mutate(exposed = if_else(index_vaccine_date >= week_start & index_vaccine_date <= week_end, 1, 0),
           exposed = if_else(is.na(exposed), 0, exposed)) %>%
    # exclude if not pfizer or moderna
    filter(!(exposed == 1 & index_vaccine_brand %in% c("janssen", "astrazeneca"))) %>%
    # in observation at week end date
    filter(cohort_end_date >= week_end) %>%
    # covid during the week
    addCohortIntersectDate(
      targetCohortTable = "covid",
      targetCohortId = covidId,
      window = c(0, Inf),
      order = "first",
      indexDate = "week_start",
      censorDate = "week_end",
      nameStyle = "covid_date_week"
    ) %>%
    # Exclude previous controls if their exposure did not change
    {if (!is.null(excludeControls))
      anti_join(., excludeControls, by = c("subject_id", "exposed"), copy = TRUE)
    else . } %>%
    compute()
  # exclude covid before exposure
  temp <- temp %>%
    anti_join(temp %>% filter(covid_date_week < index_vaccine_date), by = colnames(temp)) %>%
    compute()
  if (objective_id == 2) {
    # check booster elegibility
    temp <- temp %>%
      filter(week_start - previous_vaccine_date >= days.booster)
  }
  return(temp)
}

matchItDataset <- function(x, objective_id) {
  x <- x %>%
    mutate(gestational_age = cut(as.numeric(week_start - pregnancy_start_date), c(0, 90, 180, 330), include.lowest = TRUE)) %>%
    addTableIntersectCount(
      tableName = "visit_occurrence",
      indexDate = "week_start",
      window = list(c(-Inf, -31), c(-30, -1)),
      targetStartDate = "visit_start_date",
      targetEndDate = NULL,
      nameStyle = "visits_{window_name}"
    ) %>%
    addCohortIntersectCount(
      targetCohortTable = "covid",
      targetCohortId = covid_id,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-Inf, -1)),
      nameStyle = "{cohort_name}"
    ) %>%
    addCohortIntersectCount(
      targetCohortTable = other_vaccines_table_name,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-Inf, -1)),
      nameStyle = "{cohort_name}"
    ) %>%
    addCohortIntersectFlag(
      targetCohortTable = ps_covariates_table_name,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-Inf, -1)),
      nameStyle = "{cohort_name}"
    ) %>%
    addCohortIntersectCount(
      targetCohortTable = "mother_table",
      indexDate = "week_start",
      window = list(c(-Inf, -1)),
      nameStyle = "previous_pregnancies"
    ) %>%
    collect() %>%
    mutate(previous_observation = as.numeric(week_start - observation_period_start_date))

  if (objective_id == 1) {
    x <- x %>%
      select(-c(cohort_start_date, cohort_end_date, observation_period_start_date, covid_date_week,
              week_start, week_end, previous_vaccine_date, previous_vaccine_brand,
              index_vaccine_date, index_vaccine_brand, pregnancy_start_date, pregnancy_end_date))
  } else if (objective_id == 2) {
    x <- x %>%
      mutate(days_previous_vaccine = as.numeric(week_start - previous_vaccine_date)) %>%
      select(-c(cohort_start_date, cohort_end_date, observation_period_start_date, covid_date_week,
                week_start, week_end, previous_vaccine_date, index_vaccine_date, index_vaccine_brand,
                pregnancy_start_date, pregnancy_end_date))
  }

}

addAttritionReason <- function(attrition_table, reason, x) {
  n_records <- nrow(x)
  n_subjects <-  nrow(x %>% distinct(subject_id))
  new_attrition <- attrition_table %>%
    bind_rows(
      tibble(
        cohort_definition_id = unique(attrition_table$cohort_definition_id),
        number_records = n_records,
        number_subjects = n_subjects,
        reason_id = max(attrition_table$reason_id) + 1,
        reason = reason,
        excluded_records = attrition_table$number_records[attrition_table$reason_id == max(attrition_table$reason_id)] - n_records,
        excluded_subjects = attrition_table$number_subjects[attrition_table$reason_id == max(attrition_table$reason_id)] - n_subjects
      )
    )
  return(new_attrition)
}


censorExposedPair <- function(x) {
  x %>%
    left_join(
      x %>%
        filter(control_censored) %>%
        select(match_id, new_cohort_end_date = cohort_end_date)
    ) %>%
    mutate(cohort_end_date = if_else(is.na(new_cohort_end_date), cohort_end_date, new_cohort_end_date)) %>%
    select(-new_cohort_end_date)
}
