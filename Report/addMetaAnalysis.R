# libraries ----
library(dplyr)
library(readr)
library(here)
library(tidyr)
library(stringr)
library(visOmopResults)
library(omopgenerics)
library(ggplot2)
library(latex2exp)
library(EmpiricalCalibration)
library(egg)
library(gt)
library(flextable)
library(meta)


# load functions
source(here("functions.R"))

# load data
load(here("shinyData.Rdata"))

# add meta analysis
metaData <- data$risk |>
  filter(variable_name == "study") |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

metaanalyses <- expand_grid(
  comparison = metaData$comparison |> unique(),
  covid_definition = metaData$covid_definition |> unique(),
  strata_name = metaData$strata_name |> unique(),
  strata_level = metaData$strata_level |> unique(),
  regression = metaData$regression |> unique(),
  followup_end = metaData$followup_end |> unique(),
  window = metaData$window |> unique(),
  outcome = metaData$outcome |> unique()
)

results <- NULL
for (jj in 1:nrow(metaanalyses)) {
  for (delivery in c("yes", "no")) {
    tempData <- metaData |>
      inner_join(
        metaanalyses[jj, ],
        by = c("comparison", "covid_definition", "strata_name", "strata_level", "regression", "followup_end", "window", "outcome")
      ) %>%
      filter(delivery_excluded %in% c(delivery, "-"))
    if (nrow(tempData) > 0) {
      meta <- metagen(TE = tempData$coef, seTE = tempData$se_coef, sm = "HR", random = TRUE, common = FALSE)
      results <- results |>
        union_all(
          tempData |>
            mutate(
              cdm_name = "Meta-analysis", exp_coef = exp(meta$TE.random),
              coef = meta$TE.random, se_coef = meta$seTE.random,
              lower_ci = exp(meta$lower.random), upper_ci = exp(meta$upper.random),
              i2 = meta$I2, z= NA, p = NA
            ) |>
            distinct()
        )
    }
  }
}

data$risk <- data$risk |>
  union_all(results |>
              pivot_longer(cols = c("coef", "se_coef", "exp_coef", "z", "p", "lower_ci", "upper_ci", "i2"),
                           names_to = "estimate_name", values_to = "estimate_value") |>
              distinct())

save(data, file = here("shinyData.Rdata"))
