library(DBI)
library(here)
library(zip)
library(dbplyr)
library(dplyr)
library(CDMConnector)
library(tidyr)
library(readr)
library(PatientProfiles)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(glmnet)
library(log4r)
library(survival)
library(bit64)
library(CodelistGenerator)
library(DrugUtilisation)
library(MatchIt)
library(CirceR)
library(SqlRender)
library(omopgenerics)

database_name <- "CPRD" # "SIDIAP", "UiO", "CPRD

# Connection details
server_dbi <- Sys.getenv("DB_SERVER_DBI_gd")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")
port <- Sys.getenv("DB_PORT")
host <- Sys.getenv("DB_HOST")

db <- dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdm_database_schema <- "public"
results_database_schema <- "results"

# cohort stem where cohorts will be instantiated
table_stem <- "nmb_vax"

cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_database_schema,
  writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
  cdmName = database_name
)

# Pregnancy tables details:
mother_table_schema <- results_database_schema
mother_table_name <- "pregnancy_episode"

# minimum counts to report
minimum_counts <- 5

# output folder
results <- paste0("Results_", cdmName(cdm))

# study dates
study.start <- as.Date("2020-12-08") # date of initiation of the vaccination campaing
study.end   <- as.Date("2023-06-23")

# standard days between vaccine doses
days.booster     <- 90
booster.janssen  <- 90
days.moderna     <- 28
days.astrazeneca <- 28
days.pfizer      <- 21

# Choose code to run
runInstantiateCohorts <- FALSE
runPSMathcing         <- TRUE
runEvaluateCohorts    <- TRUE
runOutcomeModel       <- TRUE

source(here("0_SetUp/RunStudy.R"))

print("Thanks for running the analysis!! :D")
