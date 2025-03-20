# restore library:
# renv::activate()
# renv::restore()

# libraries ----
library(dplyr)
library(readr)
library(here)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(tidyr)
library(plotly)
library(shinycssloaders)
library(ggplot2)
library(stringr)
library(gt)
library(omopgenerics)
library(visOmopResults)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(glue)
library(stats)
library(PatientProfiles)
library(CohortSurvival)

# load functions
source(here("functions.R"))

# variables
orderVarsName <- tibble(
  variable_name = c(
    "Number records", "Age", "Age group", "Trimester",
    "Vaccine brand", "Number prior pregnancies", "Number visits prior year",
    "Prior observation", "Covid-19 any time prior", "Ohter vaccines any time prior",
    "Conditions any time prior",  "Drugs prior 180 days"
  ),
  Covariate = factor(
    c(
      "Number records", "Age (Years)", "Age Group", "Gestational Trimester",
      "Vaccine Brand", "Previous Pregnancies", "Healthcare Visits (Past Year)", "Days of Prior Observation",
      "COVID-19 Infection (Any Time Prior)", "Other Vaccinations (Any Time Prior)", "Comorbidities (Any Time Prior)",
      "Medications Prescribed (Last 180 Days)"
    ),
    levels = c(
      "Number records", "Age (Years)", "Age Group", "Gestational Trimester",
      "Vaccine Brand", "Previous Pregnancies", "Healthcare Visits (Past Year)", "Days of Prior Observation",
      "COVID-19 Infection (Any Time Prior)", "Other Vaccinations (Any Time Prior)", "Comorbidities (Any Time Prior)",
      "Medications Prescribed (Last 180 Days)"
    ))
)

# load data
load(here("shinyData-mod.Rdata"))

# run shiny
source(here("server.R"))
source(here("ui.R"))
shinyApp(ui, server)
