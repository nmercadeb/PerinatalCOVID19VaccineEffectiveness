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
  "large_scale_characteristics",  "attrition", "matching_summary", "survival",
  "vaccine_records_censor"
)
pre_data <- readData(here("data")) |> mergeData(result_patterns)

# Shiny format ----
data <- list()
data$snapshot <- pre_data$cdm_snapshot
data$cohort_count <- pre_data$cohort_counts |>
  select("cohort_group", "cohort_name", "number_records", "number_subjects") |>
  mutate(across(starts_with("number"), ~as.numeric(.x)))
data$population_attrition <- pre_data$attrition |>
  select(!"cohort_definition_id") |>
  relocate(c("cdm_name", "cohort_name")) |>
  mutate(
    reason_id = as.numeric(reason_id),
    across(starts_with("number"), ~niceNum(.x)),
    across(starts_with("excluded"), ~niceNum(.x))
  )
data$population_count <- pre_data$survival |>
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
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")
data$weekly_counts <- pre_data$matching_summary |>
  select("population", "covid_cohort", "week_start" = "matching_day",
         "exposed_pre", "unexposed_pre", "exposed_post", "unexposed_post") |>
  mutate(
    across(contains("exposed"), ~ as.numeric(.x)),
    week_start = as.Date(week_start)
  )
data$index_date <- pre_data$cohort_stats |>
  filter(result_type == "index_date") |>
  rename("index_date" = "cohort_start_date") |>
  splitAll() |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester",
    "counts" = "estimate_value"
  ) |>
  mutate(counts = as.numeric(counts))
data$available_followup <- pre_data$cohort_stats |>
  filter(grepl("followup", result_type)) |>
  splitAll() |>
  mutate(followup_end = if_else(
    result_type == "followup_cohort_end", "observation_end", "pregnancy_end"),
    estimate_value = as.numeric(estimate_value)
  ) |>
  select(
    "cdm_name", "cohort_name", "followup_end", "vaccine_brand", "trimester",
    "counts" = "estimate_value"
  )
data$reenrollment <- pre_data$cohort_stats |>
  filter(result_type == "recontributions") |>
  splitAll() |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester",
    "reenrollments" = "estimate_value"
  ) |>
  mutate(reenrollments = as.numeric(reenrollments)) |>
  left_join(
    data$population_count,
    by = c("cdm_name", "cohort_name", "vaccine_brand", "trimester")
  )

