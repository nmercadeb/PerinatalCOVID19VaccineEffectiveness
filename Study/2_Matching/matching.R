# prepare data:
cdm$source_pregnant <- cdm$source_pregnant %>%
  addMatchingAgeGroup() %>%
  addRegion(database_name = database_name) %>%
  compute()

summary <- list()
matched_cohorts <- list()
cohort_set <- NULL
cohort_attrition <- NULL

settings_source_pregnant <- settings(cdm$source_pregnant)
settings_covid <- settings(cdm$covid)
jj <- 0

for (source_id in settings_source_pregnant$cohort_definition_id) {
  # source cohort name
  source_name <- settings_source_pregnant$cohort_name[settings_source_pregnant$cohort_definition_id == source_id]
  for (covid_id in settings(cdm$covid)$cohort_definition_id) {
    # covid cohort name
    covid_name <- settings_covid$cohort_name[settings_covid$cohort_definition_id == covid_id]
    # source cohort
    matching_source <- cdm$source_pregnant %>% # source population to start matching at each iteration
      filter(cohort_definition_id == source_id) %>%
      select(-cohort_definition_id) %>%
      compute()
    # trial weeks
    first_day <- min(unique(matching_source %>% pull(index_vaccine_date)), na.rm = TRUE)
    last_day  <- min(max(unique(matching_source %>% pull(index_vaccine_date)), na.rm = TRUE), enrollment.end, na.rm = TRUE)
    trialWeeks <- seq.Date(first_day, last_day, "week")
    # to save macthing results
    matched.population <- list()
    # save controls to exclude
    working.excludeControls <- NULL
    # matching stats
    summary.matching <- NULL
    # analysis id
    jj <- jj + 1
    info(logger, paste0("Matching population: ", source_name, ". Covid definition: ", covid_name))
    paste0("Matching population: ", source_name, ". Covid definition: ", covid_name)
    for(kk in 1:length(trialWeeks)) {
      week.k <- trialWeeks[kk]
      week.k.end <- week.k + weeks(1) - days(1)
      print(paste0("Processing week: ", week.k, ". Progress: ", as.character(round(kk/length(trialWeeks) *100, 2)), " %."))
      # Get working table ----
      working.table <- matching_source %>%
        pregnantMatchingTable(
          covidId = covid_id, weekStart = week.k, weekEnd = week.k.end,
          excludeControls = working.excludeControls, objective = source_id,
          days.booster = days.booster
        ) %>%
        compute()
      # If less than 5 vaccinated: no matching
      if (working.table %>% filter(exposed == 1) %>% tally() %>% pull() <= 5) {
        summary.matching <- summary.matching %>%
          union_all(
            tibble(
              matching_day = week.k,
              exposed_pre = working.table %>% filter(exposed == 1) %>% tally() %>% pull(),
              unexposed_pre = working.table %>% filter(exposed == 0) %>% tally() %>% pull(),
              exposed_post =  0,
              unexposed_post =  0
            )
          )
      } else {
        # Matching dataframe
        working.match_data <- matchItDataset(working.table, source_id)
        columns <- sapply(lapply(working.match_data, unique), length)
        columns <- names(columns)[columns > 1]
        working.match_data <- working.match_data %>% select(all_of(columns))
        doMatching <- TRUE
        matchSucceed <- FALSE
        tryCatch({
          while (doMatching) {
            exactMatch = c("maternal_age", "gestational_age")
            exactMatch = exactMatch[exactMatch %in% colnames(working.match_data)]
            working.match <- matchit(exposed ~ .- subject_id - pregnancy_id - trimester, data = working.match_data,
                                     method = "nearest", distance = "glm", caliper = 0.2,
                                     ratio = 1, std.caliper = FALSE,
                                     exact = formula(paste0(". ~", paste0(exactMatch, collapse = " + "))))
            # Save matched pairs
            working.matched.population <- match.data(working.match) %>%
              inner_join(working.table,
                         by = c("subject_id", "pregnancy_id", "age", "maternal_age", "exposed"),
                         copy = TRUE) %>%
              mutate(match_id = paste0(gsub("-", "", week.k), subclass))
            # Assign index dates and vaccine brand
            working.matched.population <- working.matched.population %>%
              select(-cohort_start_date, -index_vaccine_brand) %>%
              inner_join(working.matched.population %>%
                           filter(exposed == 1) %>%
                           select(match_id, cohort_start_date = index_vaccine_date, vaccine_brand = index_vaccine_brand),
                         by = "match_id",
                         copy = TRUE) %>%
              compute()
            covidBefore <- working.matched.population %>% filter(covid_date_week < cohort_start_date)
            if (nrow(covidBefore) == 0) {
              doMatching <- FALSE
            } else {
              working.match_data <- working.match_data %>%
                anti_join(covidBefore %>% select(subject_id, exposed), by = c("subject_id", "exposed"))
            }
          }
          matchSucceed <- TRUE},
          error = function(cond) {
            message("No units could be matched.")
          }
        )

        if (matchSucceed) {
          # save matched population
          matched.population[[kk]] <- working.matched.population
          # Keep track of controls (to exclude or include if exposed)
          working.excludeControls <- working.excludeControls %>%
            union_all(working.match_data[as.numeric(working.match$match.matrix), c("subject_id", "exposed")])
          # update stats
          vax_post <- matched.population[[kk]] %>% filter(exposed == 1) %>% tally() %>% pull()
          unvax_post <- matched.population[[kk]] %>% filter(exposed == 0) %>% tally() %>% pull()
        } else {
          vax_post <- 0
          unvax_post <- 0
        }

        summary.matching <- summary.matching %>%
          union_all(
            tibble(
              matching_day = week.k,
              exposed_pre = working.table %>% filter(exposed == 1) %>% tally() %>% pull(),
              unexposed_pre = working.table %>% filter(exposed == 0) %>% tally() %>% pull(),
              exposed_post =  vax_post,
              unexposed_post =  unvax_post
            )
          )
      }
      # Update working source
      matching_source <- matching_source %>%
        anti_join(
          working.table %>% filter(exposed == 1) %>% select(subject_id), # exclude exposed
          copy = TRUE,
          by = "subject_id"
        )
    }
    # save summary
    summary[[jj]] <- summary.matching %>%
      mutate(covid_cohort = covid_name, population = source_name)
    # save cohort
    if (length(matched.population %>% bind_rows()) > 0) {
      matched_cohorts[[jj]] <- matched.population %>%
        bind_rows() %>%
        mutate(
          cohort_definition_id = jj,
          trimester = case_when(
            "[0,90]" == trimester ~ "T1",
            "(90,180]" == trimester ~ "T2",
            "(180,330]" == trimester ~ "T3")
        ) %>%
        select(cohort_definition_id,  subject_id, cohort_start_date, cohort_end_date,
               match_id, exposed, pregnancy_id, pregnancy_start_date, pregnancy_end_date,
               trimester, index_vaccine_date, vaccine_brand, age, maternal_age)
    } else {
      matched_cohorts[[jj]] <- matched.population %>%
        bind_rows()
    }

    # cohort settings
    cohort_set <- cohort_set %>%
      union_all(
        tibble(
          cohort_definition_id = jj,
          cohort_name = paste0(source_name, "_", covid_name)
        )
      )
    # cohort_attrition
    cohort_attrition <- cohort_attrition %>%
      union_all(
        addAttritionReason(attrition(cdm$source_pregnant) %>% filter(cohort_definition_id == source_id), "Matching", matched.population %>% bind_rows()) %>%
          mutate(cohort_definition_id = jj)
      )
  }
}

# export summary
summary %>% bind_rows() %>% mutate(cdm_name = cdmName(cdm)) %>%
  write_csv(file = here(output_folder, paste0("matching_summary_", database_name, ".csv")))

# instantiate matching cohort
matched_cohorts <- matched_cohorts %>% bind_rows()
class(matched_cohorts) <- class(matched_cohorts)[!class(matched_cohorts) %in% "matchdata"]
cdm <- insertTable(cdm, name = "matched_raw", table = matched_cohorts, overwrite = TRUE)
cdm$matched_raw <- tbl(db, inSchema(schema = results_database_schema, table = paste0(table_stem, "matched_raw"))) %>%
  compute()

# "clean" cohort ----
# "clean" cohort
cdm$matched <- cdm$matched_raw %>%
  mutate(reason = "end_observation") %>%
  left_join(
    cdm$vaccine_schema %>%
      filter(!is.na(schema_id)) %>%
      select(subject_id, schema_id, vaccine_date) %>%
      pivot_wider(names_from = schema_id, values_from = vaccine_date) %>%
      select(subject_id, partial, complete, booster_1, booster_2),
    by = "subject_id"
  ) %>%
  ##### Subject level censoring
  mutate(
    # do not follow primary schema treatment
    # days_primary = !!datediff("cohort_start_date", "complete"),
    recommended_lower = if_else(vaccine_brand == "pfizer", as.Date(!!dateadd("cohort_start_date", pfizer[1])), as.Date(!!dateadd("cohort_start_date", moderna[1]))),
    recommended_upper = if_else(vaccine_brand == "pfizer", as.Date(!!dateadd("cohort_start_date", pfizer[2])), as.Date(!!dateadd("cohort_start_date", moderna[2]))),
    reason = case_when(
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 1 & is.na(.data$complete) & .data$cohort_end_date > .data$recommended_upper ~ "no second dose", # censor at window recommended days upper
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 1 & .data$complete < .data$recommended_lower & .data$cohort_end_date >= .data$complete ~ "second dose before recommended time", # censor at 2nd dose - 1 day
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 1 & .data$complete > .data$recommended_upper & .data$cohort_end_date > .data$recommended_upper ~ "second dose after recommended time", # censor at window recommended days upper
      .default = .data$reason
    ),
    cohort_end_date =  case_when(
      .data$reason == "no second dose" ~ recommended_upper, # censor at window recommended days upper
      .data$reason == "second dose before recommended time" ~ as.Date(!!dateadd("complete", -1)), # censor at 2nd dose
      .data$reason == "second dose after recommended time" ~ recommended_upper,
      .default = .data$cohort_end_date
    ),
    # deviate from treatment strategy
    reason = case_when(
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 1 & .data$cohort_end_date >= .data$booster_1 ~ "Exposed 3rd dose",
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 0 & .data$cohort_end_date >= .data$partial ~ "Unexposed 1st dose",
      .data$cohort_definition_id %in% 3:4 & .data$exposed == 1 & .data$cohort_end_date >= .data$booster_2 ~ "Exposed 4th dose",
      .data$cohort_definition_id %in% 3:4 & .data$exposed == 0 & .data$cohort_end_date >= .data$booster_1 ~ "Unexposed 3rd dose",
      .default = .data$reason
    ),
    cohort_end_date = case_when(
      grepl("3rd dose", .data$reason) ~ as.Date(add_days(.data$booster_1, -1)),
      .data$reason == "Unexposed 1st dose" ~ as.Date(add_days(.data$partial, -1)),
      .data$reason == "Exposed 4th dose" ~ as.Date(add_days(.data$booster_2, -1)),
      .default = cohort_end_date
    )
  ) %>%
  compute(name = "matched", temporary = FALSE)

##### Match level censoring
cdm$matched  <- cdm$matched  %>%
  select(-reason, -cohort_end_date) %>%
  inner_join(
    cdm$matched  %>%
      group_by(cohort_definition_id, match_id) %>%
      filter(cohort_end_date == min(cohort_end_date)) %>%
      ungroup() %>%
      select(cohort_definition_id, match_id, cohort_end_date, reason) %>%
      group_by(cohort_definition_id, match_id) %>%
      mutate(reason = str_flatten(reason, collapse = "; ")) %>%
      ungroup() %>%
      distinct(),
    by = c("cohort_definition_id", "match_id")
  ) %>%
  compute(name = "matched", temporary = FALSE)

# Report censoring
censoring <- cdm$matched  %>%
  mutate(time = !!datediff("cohort_start_date", "cohort_end_date")) %>%
  group_by(cohort_definition_id, reason) %>%
  summarise(n = n(), mean = mean(time), sd = sd(time), median = median(time), q25 = quantile(time, 0.25), q75 = quantile(time, 0.75)) %>%
  collect()

write_csv(
  censoring |> mutate(cdm_name = cdmName(cdm)),
  file = here(output_folder, paste0("censoring_", cdmName(cdm), ".csv"))
)

cdm$matched <- cdm$matched %>%
  group_by(cohort_definition_id, match_id) %>%
  mutate(cohort_end_date = min(cohort_end_date)) %>%
  select(-c(age, maternal_age, recommended_lower, recommended_upper, reason)) %>%
  compute(name = "matched", temporary = FALSE) %>%
  omopgenerics::newCohortTable(cohortSetRef = cohort_set, cohortAttritionRef = cohort_attrition)

write_csv(
  attrition(cdm$matched) |> inner_join(settings(cdm$matched)) |> mutate(cdm_name = cdmName(cdm)),
  file = here(output_folder, paste0("population_attrition_", cdmName(cdm), ".csv"))
)
