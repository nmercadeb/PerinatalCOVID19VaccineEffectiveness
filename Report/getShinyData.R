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
  "attrition", "matching_summary", "survival", "vaccine_records_censor"
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
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  uniteStrata(c = c("vaccine_brand", "trimester"))
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
  mutate(counts = as.numeric(counts)) |>
  uniteStrata(c = c("vaccine_brand", "trimester"))
data$available_followup <- pre_data$cohort_stats |>
  filter(grepl("followup", result_type)) |>
  select(-"cohort_name", -"cohort_start_date") |>
  mutate(result_id = 1L) |>
  newSummarisedResult()
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
  uniteStrata(cols = c("vaccine_brand", "trimester")) |>
  newSummarisedResult()
data$large_scale <- pre_data$characteristics |>
  filter(result_type %in% c("summarised_large_scale_characteristics", "large_scale_differences")) |>
  newSummarisedResult()
data$survival_summary <- pre_data$survival |>
  filter(result_type == "followup") |>
  newSummarisedResult()
data$survival <- pre_data$survival |>
  filter(grepl("survival", result_type)) |>
  newSummarisedResult()
data$risk <-  pre_data$survival |>
  filter(result_type %in% c("binomial", "cox")) |>
  newSummarisedResult()

# Save shiny data ----
save(data, file = here("shinyData.Rdata"))
