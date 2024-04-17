
# Risk estimates
comparison_names <- settings(cdm$matched) |> pull("cohort_name")
outcomes <- colnames(survival_raw)
outcomes <- outcomes[grepl("nco_|study_", outcomes)]
study_ends <- c("cohort_end_date", "pregnancy_end_date")
windows <- list(c(0, Inf), c(0, 10), c(11, 27), c(28, 88), c(89, 147), c(148, Inf))
analyses <- c("main", "sensitivity")

### for compariosn_names
for (analysis in analyses) {
  for (studyEnd in study_end) {
    for (window.k in 1:length(windows)) {
      # data for window
      window <- windows[[window.k]]
      survival_window <- survival_raw |>
        trimDates(interval = window, outcomes = outcomes, endData = studyEnd, analysis = analysis)
      # set outcomes to evaluate depending on window
      if (window[1] == 0 & is.infinite(window[2])) {
        outcomes.k <- outcomes
      } else {
        outcomes.k <- outcomes[grepl("study_", outcomes)]
      }
      for (outcome in outcomes.k) {
        # survival format for outcome
        survival_data <- survivalFormat(survival_window, outcome)
        # get estimates
        if(grepl("nco", outcome)) {
          results[[k]] <- estimateSurvival(
            data = survival_data, asmd = asmd,
            group = "cohort_name", strata = c("vaccine_brand", "trimester"),
            cox = TRUE, binomial = FALSE, kaplanMeier = FALSE
          ) |>
          mutate(
            cdm_name = cdmName(cdm),
            result_type = "survival",
            variable = "nco",
            variable_level = gsub("nco_", "", outcome),
            window = window,
            analysis = analysis,
            studyEnd = studyEnd
          )
        } else {
          results[[k]] <- estimateSurvival(
            data = survival_data, asmd = asmd,
            group = "cohort_name", strata = c("vaccine_brand", "trimester")
          )
        }
      }
    }
  }
}

# sample independence stats

