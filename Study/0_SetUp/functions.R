getId <- function(x, name) {
  settings(x) |>
    filter(.data$cohort_name == name) |>
    pull("cohort_definition_id")
}


generateVisitRelatedOutcomes <- function(codes, window, name, attritionReason) {
  # covid visit cohort
  covid_visit <- cdm$temp_covid |>
    inner_join(
      cdm$visit_occurrence |>
        filter(visit_concept_id %in% c(codes)) |>
        filter(visit_start_date >= study.start)  |>
        select(subject_id = person_id, visit_start_date) |>
        distinct(),
      by = "subject_id"
    ) %>%
    mutate(diff_days = !!datediff("cohort_start_date", "visit_start_date")) |>
    filter(diff_days >= !!window[1] & diff_days <= !!window[2]) |>
    compute()
  cdm[[paste0("temp_", name)]] <- covid_visit |>
    select(-visit_start_date, -diff_days) |>
    distinct() |>
    compute(name = paste0("temp_", name), temporary = FALSE) |>
    recordCohortAttrition(reason = attritionReason) |>
    newCohortTable(cohortSetRef = settings(cdm$temp_covid) |>
                     mutate(cohort_name = paste0(name, "_", cohort_name)))
  # covid visit cohort - delivery date
  cdm[[paste0("temp_", name, "_delivery")]] <- covid_visit |>
    addCohortIntersectFlag(
      targetCohortTable = "mother_table",
      window = c(-2,2),
      indexDate = "visit_start_date",
      targetStartDate = "cohort_end_date",
      nameStyle = "is_delivery_date") |>
    filter(is_delivery_date == 0) |>
    select(-visit_start_date, -diff_days) |>
    distinct() |>
    compute(name = paste0("temp_", name, "_delivery"), temporary = FALSE) |>
    recordCohortAttrition(reason = paste0(attritionReason, " (not delivery)")) |>
    newCohortTable(cohortSetRef = settings(cdm$temp_covid) |>
                     mutate(cohort_name = paste0(name, "_no_delivery_", cohort_name)))
  return(cdm)
}

addMatchingAgeGroup <- function(x) {
  return(x |>
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
    x |>
      left_join(
        cdm$person |>
          left_join(cdm$location, by = "location_id") |>
          select(subject_id = person_id, region = location_source_value),
        by = "subject_id"
      )
  } else {
    x |>
      left_join(
        cdm$person |>
          left_join(cdm$care_site, by = "care_site_id") |>
          select(person_id, location_id = location_id.y) |>
          left_join(cdm$location, by = "location_id") |>
          select(subject_id = person_id, region = location_source_value),
        by = "subject_id"
      )
  }

}

pregnantMatchingTable <- function(sourceTable, covidId, weekStart, weekEnd, excludeControls, objective_id, days.booster) {
  temp <- sourceTable |>
    mutate(week_start = as.Date(weekStart), week_end = as.Date(weekEnd)) |>
    # pregnant at week.k
    filter(
      pregnancy_start_date <= week_start &  # start before the week
        pregnancy_end_date >= week_end      # end after the week
    ) |>
    # in observation at week.k
    filter(
      cohort_start_date <= week_start &  # start before the week
        cohort_end_date >= week_end      # end after the week
    ) |>
    # no covid-19 in the last three months from week start date
    addCohortIntersectFlag(
      targetCohortTable = "covid",
      targetCohortId = covidId,
      window = c(-90, -1),
      indexDate = "week_start",
      nameStyle = "covid"
    ) |>
    filter(covid == 0) |>
    select(-covid) |>
    # classify exposed - unexposed
    mutate(exposed = if_else(index_vaccine_date >= week_start & index_vaccine_date <= week_end, 1, 0),
           exposed = if_else(is.na(exposed), 0, exposed)) |>
    # exclude if not pfizer or moderna
    filter(!(exposed == 1 & index_vaccine_brand %in% c("janssen", "astrazeneca"))) |>
    # exclude if exposed before week start
    filter(index_vaccine_date >= week_start | is.na(index_vaccine_date)) |>
    # in observation at week end date
    filter(cohort_end_date >= week_end) |>
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
  temp <- temp |>
    anti_join(temp |> filter(exposed == 1, covid_date_week < index_vaccine_date), by = colnames(temp)) |>
    compute()
  # if (objective_id == 2) {
  #   # check booster elegibility
  #   temp <- temp %>%
  #     filter(!!datediff("previous_vaccine_date", "week_start") >= days.booster)
  # }
  return(temp)
}

matchItDataset <- function(x, objective_id) {
  x <- x %>%
    mutate(
      trimester = cut(
        as.numeric(!!datediff("pregnancy_start_date", "week_start")),
        c(0, 90, 180, 330),
        include.lowest = TRUE
        ),
      gestational_age = cut(
        as.numeric(!!datediff("pregnancy_start_date", "week_start")),
        !!c(seq(0, 293, 7*6), 303),
        include.lowest = TRUE)
    ) |>
    addTableIntersectCount(
      tableName = "visit_occurrence",
      indexDate = "week_start",
      window = list(c(-365, -181), c(-180, -31), c(-30, -1)),
      targetStartDate = "visit_start_date",
      targetEndDate = NULL,
      nameStyle = "visits_{window_name}"
    ) |>
    addTableIntersectCount(
      tableName = "visit_occurrence",
      indexDate = "week_start",
      window = list(c(-Inf, -1)),
      targetStartDate = "visit_start_date",
      targetEndDate = NULL,
      censorDate = "pregnancy_start_date",
      nameStyle = "pregnant_visits"
    ) |>
    addCohortIntersectCount(
      targetCohortTable = "covid",
      targetCohortId = covid_id,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-365, -1)),
      nameStyle = "{cohort_name}"
    ) |>
    addCohortIntersectCount(
      targetCohortTable = other_vaccines_table_name,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-365, -1)),
      nameStyle = "{cohort_name}"
    ) |>
    addCohortIntersectFlag(
      targetCohortTable = ps_covariates_table_name,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-Inf, -1)),
      nameStyle = "{cohort_name}"
    ) |>
    addCohortIntersectCount(
      targetCohortTable = "mother_table",
      indexDate = "pregnancy_start_date",
      window = list(c(-Inf, -1)),
      nameStyle = "previous_pregnancies"
    ) %>%
    mutate(previous_observation = as.numeric(!!datediff("observation_period_start_date", "week_start"))) |>
    collect()

  if (objective_id == 1) {
    x <- x |>
      select(-c(cohort_start_date, cohort_end_date, observation_period_start_date, covid_date_week,
                week_start, week_end, previous_vaccine_date, previous_vaccine_brand,
                index_vaccine_date, index_vaccine_brand, pregnancy_start_date, pregnancy_end_date))
  } else if (objective_id == 2) {
    x <- x |>
      mutate(days_previous_vaccine = as.numeric(week_start - previous_vaccine_date)) |>
      select(-c(cohort_start_date, cohort_end_date, observation_period_start_date, covid_date_week,
                week_start, week_end, previous_vaccine_date, index_vaccine_date, index_vaccine_brand,
                pregnancy_start_date, pregnancy_end_date))
  }

}

addAttritionReason <- function(attrition_table, reason, x) {
  if (length(x) == 0) {
    n_records <- 0
    n_subjects <- 0
  } else {
    n_records <- nrow(x)
    n_subjects <-  nrow(x |> distinct(subject_id))
  }

  new_attrition <- attrition_table |>
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
  x |>
    left_join(
      x |>
        filter(control_censored) |>
        select(cohort_definition_id, match_id, new_cohort_end_date = cohort_end_date),
      by = c("cohort_definition_id", "match_id")
    ) |>
    mutate(cohort_end_date = if_else(is.na(new_cohort_end_date) | new_cohort_end_date > cohort_end_date,
                                     cohort_end_date, new_cohort_end_date)) |>
    select(-new_cohort_end_date)
}

trimDates <- function(x, interval, outcomes, endData, analysis) {
  if (analysis == "sensitivity") {
    # end data adjusted to sensitivity
    x <- x |>
      mutate(!!endData := if_else(censor_date < .data[[endData]] & !is.na(censor_date), censor_date, .data[[endData]]))
  }
  # set end date
  if (is.infinite(interval[2])) {
    x <- x |>
      mutate("end_date" = .data[[endData]])
  } else {
    x <- x %>%
      mutate("end_date" = !!dateadd(date = "cohort_start_date", number = interval[2]))
  }

  # set start date
  x <- x |>
    rename("end_data" := !!endData) %>%
    mutate(
      "start_date" = !!dateadd(date = "cohort_start_date", number = interval[1]),
      "start_date" = as.Date(start_date)
    ) |>
    select(all_of(c(
      "cohort_name", "subject_id", "match_id", "exposed", "trimester",
      "vaccine_brand", "maternal_age","end_date", "start_date", "end_data",
      outcomes
    )))

  # match to exclude (not in observation)
  x <- x |>
    anti_join(
      x |>
        filter(start_date > end_data) |> # window starts out of observation
        distinct(cohort_name, match_id),
      by = c("cohort_name", "match_id")
    ) |>
    # reset end date
    mutate(end_date = if_else(end_date > end_data, end_data, end_date))
  return(x)
}

survivalFormat <- function(x, out) {
  # exclusion if match outcome before
  x <- x |>
    anti_join(
      x |>
        filter(start_date > .data[[out]]) |> # outcome before
        distinct(cohort_name, match_id),
      by = c("cohort_name", "match_id")
    )
  x <- x %>%
    mutate(
      status = if_else(.data[[out]] > start_date & .data[[out]] < end_date, 1, 0),
      status = if_else(is.na(status), 0, status),
      time = if_else(status == 1, !!datediff("start_date", out), !!datediff("start_date", "end_date"))
    ) |>
    select(all_of(c(
      "cohort_name", "subject_id", "match_id", "exposed", "trimester", "vaccine_brand",
      "status", "time"
    ))) |>
    compute()
  return(x)
}

estimateSurvival <- function(data, group, strata,
                             cox = TRUE, binomial = TRUE) {
  results <- list()
  groupLevel = unique(data |> pull(.data[[group]]))
  k <- 1
  for (group.k in groupLevel) { # group level
    for (strata.k in strata) { # strata name
      strataLevels <- unique(data |> pull(strata.k))
      for(strataLevel.k in strataLevels) { # strata level
        # # for double robust estimation
        # covariates <-  smd |>
        #   filter(estimate_name == "smd", as.numeric(estimate_value) > 0.1 | as.numeric(estimate_value) < -0.1) |>
        #   rename("concept_id" = "additional_level") |>
        #   inner_join(cdm$concept, by = "concept_id", copy = TRUE) |>
        #   filter(domain_id %in% c("Drug", "Condition")) |>
        #   pull(concept_id) |>
        #   unique()
        # names(covariates) <- covariates

        # data
        data.k <- data |>
          filter(cohort_name == group.k, .data[[strata.k]] == strataLevel.k) |>
          collect()
        # formula
        # if (length(covariates) == 0) {
        covariates_formula <- "exposed"
        # # } else {
        #
        #   windowFlag <- unique(smd |> pull("variable_level"))
        #   cdm$matched_asmd <- cdm$matched |>
        #     addConceptIntersectFlag(
        #       conceptSet = as.list(covariates),
        #       window = gsub("i", "I", windowFlag) |> strsplit(" to ") |> lapply(as.numeric)
        #     ) |>
        #     compute(name = "matched_asmd", temporary = FALSE)
        #   covariates_formula <- c("exposed", covariates)
        #   covariates_formula <- paste0(covariates_formula, collapse = " + ")
        # }

        # cox ----
        if (cox) {
          tryCatch({
            formula <- as.formula(paste0("Surv(time, status) ~ ", covariates_formula))
            coxRegression <- coxph(formula,  data = data.k)
            results[[k]] <-  summary(coxRegression)$coefficients |>
              as_tibble(rownames = "variable") |>
              select(
                "variable", "coef", "exp_coef" = "exp(coef)",
                "se_coef" = "se(coef)", "z", "p" = "Pr(>|z|)"
              ) |>
              filter(variable == "exposed") |>
              left_join(
                summary(coxRegression)$conf.int |>
                  as_tibble(rownames = "variable") |>
                  select("variable", "lower_ci" = "lower .95", "upper_ci" = "upper .95"),
                by = "variable"
              ) |>
              regressionToSummarised(
                type = "cox", groupLevel = group.k,
                strataName = strata.k, strataLevel = strataLevel.k
              ) |>
              mutate(exposed = NA) # for KM and followup
            k <- k + 1
          },
          error = function(e) {
          })
        }

        # binomial ----
        if (binomial) {
          # regression
          tryCatch({
            log <- glm(as.formula(paste0("status ~ ", covariates_formula)),
                       data = data.k, family = binomial(link = log))
            results[[k]] <- summary(log)$coefficients |>
              as_tibble(rownames = "variable") |>
              filter(variable == "exposed") |>
              rename("coef" = "Estimate", "se_coef" = "Std. Error",
                     "z" = "z value", "p" = "Pr(>|z|)") |>
              mutate("exp_coef" = exp(coef)) |>
              inner_join(
                confint(log) |>
                  as_tibble(rownames = "variable") |>
                  mutate(lower_ci = exp(`2.5 %`), upper_ci = exp(`97.5 %`)) |>
                  select(variable, lower_ci, upper_ci),
                by = "variable") |>
              regressionToSummarised(
                type = "binomial", groupLevel = group.k,
                strataName = strata.k, strataLevel = strataLevel.k
              ) |>
              mutate(exposed = NA) # for KM and followup
            k <- k + 1
          },
          error = function(e) {
          })
        }

        # follow-up stats ----
        results[[k]] <- data.k |>
          group_by(exposed) |>
          summarise(
            mean = mean(time),
            median = median(time),
            q25 = quantile(time, 0.25),
            q75 = quantile(time, 0.75),
            min = min(time),
            max = max(time)
          ) |>
          mutate(
            group_name = "cohort_name",
            group_level = group.k,
            strata_name = strata.k,
            strata_level = strataLevel.k,
            result_type = "followup",
            estimate_type = "numeric",
            num_control = sum(data.k$exposed == 0),
            num_exposed = sum(data.k$exposed == 1),
            num_events_control = sum(data.k$exposed == 0 & data.k$status == 1),
            num_events_exposed = sum(data.k$exposed == 1 & data.k$status == 1)
          ) |>
          pivot_longer(
            cols = c("mean", "median", "q25", "q75", "min", "max", "num_control",
                     "num_exposed", "num_events_control", "num_events_exposed"),
            names_to = "estimate_name", values_to = "estimate_value"
          ) |>
          mutate(estimate_value = as.character(estimate_value))
        k <- k + 1
      }
    }
  }
  return(results |> bind_rows())
}

regressionToSummarised <- function(
    x, type, groupLevel, strataName, strataLevel,
    cols = c("coef", "se_coef", "exp_coef", "z", "p", "lower_ci", "upper_ci")) {
  x |>
    mutate(
      estimate_type = "numeric",
      group_name = "cohort_name",
      group_level = groupLevel,
      strata_name = strataName,
      strata_level = strataLevel,
      result_type = type
    ) |>
    select(-variable) |>
    pivot_longer(
      cols = all_of(cols),
      names_to = "estimate_name", values_to = "estimate_value"
    ) |>
    mutate(estimate_value = as.character(estimate_value))
}
