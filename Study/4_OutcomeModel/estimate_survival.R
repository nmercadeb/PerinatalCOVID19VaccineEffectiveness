info(logger, "Start survival analyses")
info(logger, "1) Relative risk estimates")
# For loop settings
outcomes <- colnames(cdm$survival_raw)
outcomes <- outcomes[grepl("nco_|study_", outcomes)]
study_ends <- c("cohort_end_date", "pregnancy_end_date")
windows <- list(c(0, Inf), c(0, 10), c(11, 27), c(28, 88), c(89, 147), c(148, Inf))
analyses <- c("main", "sensitivity")

results <- list()
k <- 1
for (analysis in analyses) {
  info(logger, paste0(" - analysis: ", analysis))
  for (studyEnd in study_ends) {
    info(logger, paste0("  - study end: ", studyEnd))
    for (window.k in 1:length(windows)) {
      # data for window
      window <- windows[[window.k]]
      info(logger, paste0("   - window: ", paste0(as.character(window), collapse = "_")))
      survival_window <- cdm$survival_raw |>
        trimDates(interval = window, outcomes = outcomes, endData = studyEnd, analysis = analysis) |>
        compute()
      # set outcomes to evaluate depending on window
      if (window[1] == 0 & is.infinite(window[2])) {
        outcomes.k <- outcomes
      } else {
        outcomes.k <- outcomes[grepl("study_", outcomes)]
      }
      for (outcome in outcomes.k) {
        # survival format for outcome
        survival_data <- survivalFormat(survival_window, outcome)|>
          mutate(overall = "overall")
        # get estimates
        if(grepl("nco", outcome)) {
          results[[k]] <- estimateSurvival(
            data = survival_data,
            group = "cohort_name", c("overall", "vaccine_brand", "trimester"),
            cox = TRUE, binomial = FALSE
          ) |>
          mutate(
            cdm_name = cdmName(cdm),
            variable_name = "nco",
            variable_level = gsub("nco_", "", outcome),
            window = paste0(as.character(window), collapse = "_"),
            analysis = analysis,
            study_end = studyEnd
          )
        } else {
          results[[k]] <- estimateSurvival(
            data = survival_data,
            group = "cohort_name", strata = c("overall", "vaccine_brand", "trimester")
          ) |>
            mutate(
              cdm_name = cdmName(cdm),
              variable_name = "study",
              variable_level = gsub("study_", "", outcome),
              window = paste0(as.character(window), collapse = "_"),
              analysis = analysis,
              study_end = studyEnd
            )
        }
        k <- k + 1
      }
    }
  }
}

info(logger, "2) Survival estimates")
# results survival
km_results <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "matched",
  outcomeCohortTable = "outcomes",
  outcomeDateVariable = "cohort_start_date",
  outcomeWashout = 1,
  censorOnCohortExit = TRUE,
  strata = list(c("vaccine_brand", "exposed"), c("trimester", "exposed"), "exposed"),
)

# export results ----
survival_results <- results |> bind_rows() |>
  uniteAdditional(c("exposed", "window", "analysis", "study_end")) |>
  mutate(
    result_id = 1,
    package_name = "StudyCode",
    package_version = "today()"
  )

# write
write_csv(
  survival_results |> bind_rows(km_results),
  file = here(output_folder, paste0("survival_", cdmName(cdm), ".csv"))
)

