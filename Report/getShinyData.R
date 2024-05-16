# libraries ----
library(dplyr)
library(readr)
library(here)
library(tidyr)
library(stringr)
library(visOmopResults)
library(omopgenerics)

# Load functions ----
source(here("functions.R"))

# Read results ----
result_patterns <- c(
  "cdm_snapshot", "characteristics", "cohort_stats", "cohort_counts",
  "attrition", "matching_summary", "relative_risk", "vaccine_records_censor"
)
pre_data <- readData(here("data")) |> mergeData(result_patterns)

# Shiny format ----
data <- list()
data$snapshot <- pre_data$cdm_snapshot
data$cohort_count <- pre_data$cohort_counts |>
  select("cdm_name", "cohort_group", "cohort_name", "number_records", "number_subjects") |>
  mutate(across(starts_with("number"), ~as.numeric(.x)))
data$population_attrition <- pre_data$attrition |>
  select(!"cohort_definition_id") |>
  relocate(c("cdm_name", "cohort_name")) |>
  mutate(
    reason_id = as.numeric(reason_id),
    across(starts_with("number"), ~niceNum(.x)),
    across(starts_with("excluded"), ~niceNum(.x))
  )
data$population_count <- pre_data$relative_risk |>
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
  uniteStrata(c = c("vaccine_brand", "trimester"))
data$weekly_counts <- pre_data$matching_summary |>
  mutate(cohort = paste0(.data$population, "_", .data$covid_cohort)) |>
  select(
    "cdm_name", "cohort", "week_start" = "matching_day", "exposed_pre",
    "unexposed_pre", "exposed_post", "unexposed_post"
  ) |>
  mutate(
    across(contains("exposed"), ~ as.numeric(.x)),
    week_start = as.Date(week_start)
  )
data$index_date <- pre_data$cohort_stats |>
  filter(result_type == "index_date") |>
  mutate("index_date" = as.Date(variable_level)) |>
  splitAll() |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester", "index_date",
    "counts" = "estimate_value"
  ) |>
  mutate(counts = as.numeric(counts)) |>
  uniteStrata(c = c("vaccine_brand", "trimester"))
data$reenrollment <- pre_data$cohort_stats |>
  filter(result_type == "recontributions") |>
  splitAll() |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester",
    "reenrollments" = "estimate_value"
  ) |>
  uniteStrata(c = c("vaccine_brand", "trimester")) |>
  mutate(reenrollments = as.numeric(reenrollments)) |>
  left_join(
    data$population_count,
    by = c("cdm_name", "cohort_name", "strata_name", "strata_level")
  ) |>
  mutate(
    count = reenrollments,
    percentage = reenrollments/(num_control + num_exposed - reenrollments) * 100
  ) |>
  select(!c("reenrollments", "num_control", "num_exposed"))
data$baseline <- pre_data$characteristics |>
  filter(!result_type %in% c("summarised_large_scale_characteristics", "large_scale_differences")) |>
  splitStrata() |>
  filter(exposed != "overall") |>
  select(-starts_with("additional")) |>
  uniteAdditional(cols = c("exposed")) |>
  uniteStrata(cols = c("vaccine_brand", "trimester"))
data$large_scale <- pre_data$characteristics |>
  filter(result_type %in% c("summarised_large_scale_characteristics")) |>
  splitGroup() |>
  splitAdditional() |>
  select(!c("result_id", "result_type", "package_name", "package_version",
            "estimate_type")) |>
  rename("window" = "variable_level", "concept_name" = "variable_name") |>
  mutate(
    estimate_value = as.numeric(estimate_value),
    exposed = case_when(
      exposed == "0" ~ "unexposed",
      exposed == "1" ~ "exposed",
      .default = exposed
    )
  )
data$smd <- pre_data$characteristics |>
  filter(result_type %in% c("large_scale_differences")) |>
  splitGroup() |>
  splitAdditional() |>
  mutate(smd = as.numeric(estimate_value), asmd = abs(smd)) |>
  rename("window" = "variable_level", "concept_name" = "variable_name") |>
  select(!c("result_id", "result_type", "package_name", "package_version",
            "estimate_type", "estimate_name", "estimate_value"))
data$survival_summary <- pre_data$relative_risk |>
  filter(result_type == "followup") |>
  newSummarisedResult() |>
  mutate(estimate_name = gsub("num", "count", estimate_name)) |>
  splitGroup() |>
  splitAdditional() |>
  filter(!(grepl("control", estimate_name) & exposed == 1)) |>
  filter(!(grepl("exposed", estimate_name) & exposed == 0)) |>
  mutate(estimate_type = if_else(grepl("count", estimate_name), "integer", estimate_type)) |>
  mutate(
    estimate_name = gsub("_control|_exposed", "", estimate_name),
    exposed = if_else(exposed == "1", "exposed", "unexposed")
  ) |>
  select(!c("result_id")) |>
  rename("outcome" = "variable_level") |>
  filter(estimate_name != "mean") |>
  mutate(estimate_value = as.numeric(estimate_value)) |>
  group_by(cdm_name, cohort_name, strata_name, strata_level, variable_name, outcome, window, analysis, study_end, exposed) |>
  mutate(suppress = if_else(any(grepl("count", estimate_name) & estimate_value < 5 & estimate_value > 0), TRUE, FALSE)) |>
  ungroup() |>
  mutate(estimate_value = if_else(suppress, NA, estimate_value)) |>
  select(!suppress)
data$risk <- pre_data$relative_risk |>
  filter(result_type %in% c("binomial", "cox")) |>
  splitGroup() |>
  splitAdditional() |>
  rename("outcome" = "variable_level", "regression" = "result_type") |>
  select(!c("package_name", "package_version", "result_id")) |>
  mutate(estimate_value = as.numeric(estimate_value)) |>
  select(
    c("cdm_name", "cohort_name", "strata_name", "strata_level", "regression",
      "analysis", "study_end", "window", "outcome", "variable_name",
      "estimate_type", "estimate_name", "estimate_value")
  )
data$population_count <- data$population_count |>
  mutate(total = num_control + num_exposed)

# Save shiny data ----
save(data, file = here("shinyData.Rdata"))
