# Load functions ----
source(here("functions.R"))

# Read results ----
result_patterns <- c(
  "cdm_snapshot", "characteristics", "cohort_stats", "cohort_counts",
  "attrition", "matching_summary", "relative_risk", "vaccine_records_censor",
  "kaplan_meier"
)
pre_data <- readData(here("data")) |> mergeData(result_patterns)

# Shiny format ----
population_attrition <- pre_data$attrition |>
  select(!"cohort_definition_id") |>
  relocate(c("cdm_name", "cohort_name")) |>
  mutate(
    reason_id = as.numeric(reason_id),
    across(starts_with("number"), ~niceNum(.x)),
    across(starts_with("excluded"), ~niceNum(.x))
  ) |>
  niceCohortName()
population_count <- pre_data$relative_risk |>
  filter(
    result_type == "followup",
    estimate_name %in% c("num_control", "num_exposed")
  ) |>
  splitAll() |>
  filter(window == "0_Inf") |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester", "estimate_name",
    "estimate_value"
  ) |>
  distinct() |>
  mutate(estimate_value = as.numeric(estimate_value)) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  uniteStrata(c = c("vaccine_brand", "trimester")) |>
  niceCohortName()
index_date <- pre_data$cohort_stats |>
  filter(result_type == "index_date") |>
  mutate("index_date" = as.Date(variable_level)) |>
  splitAll() |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester", "index_date",
    "counts" = "estimate_value"
  ) |>
  mutate(counts = as.numeric(counts)) |>
  uniteStrata(c = c("vaccine_brand", "trimester")) |>
  niceCohortName() |>
  select(cdm_name, comparison, covid_definition, strata_name, strata_level, index_date, counts) |>
  mutate(
    strata_level = factor(strata_level,
                          levels = c("overall", "pfizer", "moderna", "T1", "T2", "T3"),
                          label = c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3")),
    strata_name = factor(strata_name,
                         levels = c("overall", "vaccine_brand", "trimester"),
                         labels = c("Overall", "Vaccine brand", "Trimester")),
    date = lubridate::floor_date(index_date, unit = "weeks")
  ) %>%
  group_by(date, strata_level) %>%
  mutate(nn = sum(counts)) %>%
  ungroup()
baseline <- pre_data$characteristics |>
  filter(!result_type %in% c("summarised_large_scale_characteristics", "large_scale_differences")) |>
  splitStrata() |>
  filter(exposed != "overall") |>
  select(-starts_with("additional")) |>
  uniteAdditional(cols = c("exposed")) |>
  uniteStrata(cols = c("vaccine_brand", "trimester")) |>
  niceCohortName(col = "group_level", removeCol = FALSE)
smd <- pre_data$characteristics |>
  filter(result_type %in% c("large_scale_differences")) |>
  splitGroup() |>
  splitAdditional() |>
  mutate(smd = as.numeric(estimate_value), asmd = abs(smd)) |>
  rename("window" = "variable_level", "concept_name" = "variable_name") |>
  select(!c("result_id", "result_type", "package_name", "package_version",
            "estimate_type", "estimate_name", "estimate_value")) |>
  niceCohortName() |>
  relocate(c("comparison", "covid_definition"), .after = "cdm_name") |>
  mutate(
    strata_level = factor(strata_level,
                          levels = c("overall", "pfizer", "moderna", "T1", "T2", "T3"),
                          label = c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3")),
    strata_name = factor(strata_name,
                         levels = c("overall", "vaccine_brand", "trimester"),
                         labels = c("Overall", "Vaccine brand", "Trimester")),
    balance = case_when(
      asmd <= 0.1 ~ "ASMD $\\leq$ 0.1",
      asmd > 0.2 ~ "ASMD > 0.2",
      .default = "0.1 < ASMD $\\leq$ 0.2"
    )
  )
survival_summary <- pre_data$relative_risk |>
  filter(result_type == "followup", variable_name == "study") |>
  newSummarisedResult() |>
  mutate(estimate_name = gsub("num", "count", estimate_name)) |>
  splitGroup() |>
  splitAdditional() |>
  filter(!(grepl("control", estimate_name) & exposed == 1)) |>
  filter(!(grepl("exposed", estimate_name) & exposed == 0)) |>
  filter(estimate_name != "mean", grepl("count", estimate_name), window == "0_Inf") |>
  mutate(estimate_type = if_else(grepl("count", estimate_name), "integer", estimate_type)) |>
  mutate(
    estimate_name = gsub("_control|_exposed", "", estimate_name),
    exposed = if_else(exposed == "1", "Exposed", "Unexposed")
  ) |>
  select(!c("result_id", "estimate_type")) |>
  rename("outcome" = "variable_level") |>
  mutate(
    # estimate_value = if_else(suppress, NA, estimate_value),
    exposed_censoring = if_else(analysis == "main", "none", "3rd/4rt dose"),
    estimate_value = as.numeric(estimate_value)
  ) |>
  rename("followup_end" = "study_end") |>
  select(!c("result_type", "package_name", "package_version", "analysis")) |>
  niceCohortName() |>
  niceOutcomeName() |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  mutate(
    percentage_events = count_events/count * 100,
    count_events = if_else(count_events < 5 & count_events > 0, "<5",
                           if_else(count_events == 0, niceNum(count_events),
                                   paste0(niceNum(count_events), " (", niceNum(percentage_events, dec = 2), "%)"))),
    count = if_else(count < 5 & count > 0, "<5", niceNum(count))
  ) |>
  select(cdm_name, strata_name, strata_level, outcome, delivery_excluded, followup_end, exposed_censoring, comparison, covid_definition, exposed, count, count_events)
risk <- pre_data$relative_risk |>
  filter(result_type %in% c("binomial", "cox")) |>
  splitGroup() |>
  splitAdditional() |>
  rename("outcome" = "variable_level", "regression" = "result_type") |>
  select(!c("package_name", "package_version", "result_id")) |>
  mutate(
    estimate_value = as.numeric(estimate_value),
    exposed_censoring = if_else(analysis == "main", "none", "3rd/4rt dose")
  ) |>
  select(
    c("cdm_name", "cohort_name", "strata_name", "strata_level", "regression",
      "exposed_censoring","followup_end" = "study_end", "window", "outcome", "variable_name",
      "estimate_name", "estimate_value")
  ) |>
  niceCohortName() |>
  niceOutcomeName() |>
  relocate(c("comparison", "covid_definition"), .after = "cdm_name") |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")
population_count <- population_count |>
  mutate(total = num_control + num_exposed) |>
  select(cdm_name, comparison, covid_definition, strata_name, strata_level, total, num_control, num_exposed)
kaplan_meier <- pre_data$kaplan_meier |>
  filter(strata_name != "overall") |>
  niceCohortName(col = "group_level", removeCol = FALSE) |>
  filter(result_type == "survival_estimate") |>
  splitAdditional() |>
  splitStrata() |>
  uniteStrata(cols = c("vaccine_brand", "trimester")) |>
  niceOutcomeName(col = "variable_level") |>
  mutate(
    estimate_value = as.numeric(estimate_value),
    followup_end = if_else(result_id == 1, "cohort_end_date", "pregnancy_end_date"),
    time = as.numeric(time),
    Cohort = if_else(exposed == "0", "Unexposed", "Exposed"),
    strata_level = factor(strata_level,
                          levels = c("overall", "pfizer", "moderna", "T1", "T2", "T3"),
                          label = c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3"))
  ) |>
  select(-c("result_id", "result_type", "package_name", "package_version", "group_name", "group_level", "analysis_type", "variable_name", "variable_level", "estimate_type", "exposed")) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

