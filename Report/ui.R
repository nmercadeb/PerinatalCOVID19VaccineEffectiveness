ui <- dashboardPage(
  dashboardHeader(title = "Pregnant COVID-19 vaccine effectiveness"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Database details",
        tabName = "database_details",
        menuSubItem(
          text = "CDM snapshot",
          tabName = "cdm_snapshot"
        )
      ),
      menuItem(
        text = "Cohorts",
        tabName = "cohort_details",
        menuSubItem(
          text = "Cohort count",
          tabName = "cohort_count"
        )
      ),
      menuItem(
        text = "Matching summary",
        tabName = "matching",
        menuSubItem(
          text = "Weekly counts",
          tabName = "weekly_counts"
        ),
        menuSubItem(
          text = "Index date",
          tabName = "index_date"
        ),
        # menuSubItem(
        #   text = "Future observation",
        #   tabName = "available_followup"
        # ),
        menuSubItem(
          text = "Re-enrollment",
          tabName = "reenrollment"
        )
      ),
      menuItem(
        text = "Popultation",
        tabName = "population",
        menuSubItem(
          text = "Attrition",
          tabName = "attrition"
        ),
        menuSubItem(
          text = "Counts",
          tabName = "count"
        ),
        menuSubItem(
          text = "Baseline characteristics",
          tabName = "baseline_characteristics"
        ),
        menuSubItem(
          text = "Large Scale characteristics",
          tabName = "large_scale_characteristics"
        ),
        menuSubItem(
          text = "Standardised mean differences",
          tabName = "smd"
        )
      ),
      menuItem(
        text = "Negative Control Outcomes",
        tabName = "nco",
        menuSubItem(
          text = "Summary",
          tabName = "nco_summary"
        ),
        menuSubItem(
          text = "Forest plot",
          tabName = "nco_forest_plot"
        )
      ),
      menuItem(
        text = "Study outcomes",
        tabName = "outcomes",
        menuSubItem(
          text = "Summary",
          tabName = "study_summary"
        ),
        # menuSubItem(
        #   text = "Proportionality",
        #   tabName = "proportionality"
        # ),
        menuSubItem(
          text = "Kaplan-Meier",
          tabName = "kaplan_meier"
        ),
        menuSubItem(
          text = "Forest plot",
          tabName = "study_forest_plot"
        )
      )
    )
  ),
  ## body ----
  dashboardBody(
    tabItems(
      ### cdm_snapshot ----
      tabItem(
        tabName = "cdm_snapshot",
        h3("Database details"),
        p("See details of CDM snapshot for each database:"),
        DTOutput("cdm_snapshot_table")
      ),
      ### cohort_count ----
      tabItem(
        tabName = "cohort_count",
        h3("Cohort counts"),
        p("Cohort counts for each cohort of the present study"),
        selectors(data = data$cohort_count, prefix = "cohort_count",
                  columns = c("cdm_name", "cohort_group"), multiple = TRUE,
                  default = list("cdm_name" = data$cohort_count$cdm_name[1],
                                 "cohort_group" = data$cohort_count$cohort_group[1])),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("cohort_count_cohort_name_picker")
        ),
        div(
          style = "margin-bottom: 20px;",
          downloadButton("cohort_count_download_table", "Download current table")
        ),
        DTOutput("cohort_count_table") %>% withSpinner()
      ),
      ### weekly counts ----
      tabItem(
        tabName = "weekly_counts",
        h3("Weekly counts"),
        p("Weekly enrollment counts during propensity score matching:"),
        selectors(
          data$weekly_counts, prefix = "weekly_cnts", columns = c("cdm_name", "cohort"),
          default = list(
            "cdm_name" = data$weekly_counts$cdm_name[1],
            "cohort" = data$weekly_counts$cohort[1]
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Summary",
            h5(),
            downloadButton("weekly_counts_summary_download", "Download table in word"),
            gt_output('weekly_counts_summary') %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("weekly_counts_table_download", "Download table as csv"),
            DTOutput('weekly_counts_table') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            div(
              pickerInput(
                inputId = "weekly_plot",
                label = "Matching status",
                choices = c("exposed_pre", "unexposed_post"),
                selected = c("exposed_pre", "unexposed_post"),
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = TRUE,
                inline = TRUE
              )
            ),
            plotSelectors(prefix = "plt_wcounts", choices = c("cdm_name", "cohort", "matching_status"),
                          default = list("color" = "matching_status", "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "wcounts"),
            downloadButton("weekly_counts_plot_download", "Download figure"),
            plotlyOutput('weekly_counts_plot') %>% withSpinner()
          )
        )
      ),
      ## index date ----
      tabItem(
        tabName = "index_date",
        h3("Index date"),
        p("Distribution of index dates (patient follow-up start date)."),
        selectors(
          data$index_date, prefix = "index_dates",
          columns = c("cdm_name", "cohort_name", "strata_name"),
          default = list(
            "cdm_name" = data$index_date$cdm_name[1],
            "cohort_name" = data$index_date$cohort_name[1],
            "strata_name" = data$index_date$strata_name[1]
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("index_date_strata_level")
        ),
        div(
          pickerInput(
            inputId = "index_date_group",
            label = "Group by",
            choices = c("days", "weeks", "months", "years"),
            selected = c("months"),
            options = list(`actions-box` = FALSE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE,
            inline = TRUE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Summary",
            h5(),
            downloadButton("index_date_summary_download", "Download table in word"),
            gt_output('index_date_summary') %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("index_date_table_download", "Download table as csv"),
            DTOutput('index_date_table') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            plotSelectors(prefix = "plt_index", choices = c("cdm_name", "cohort_name", "strata_name", "strata_level"),
                          default = list("color" = NULL, "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "dwn_index"),
            downloadButton("index_date_plot_download", "Download figure"),
            plotlyOutput('index_date_plot') %>% withSpinner()
          )
        )
      ),
      ## future observation ----
      # tabItem(
      #   tabName = "available_followup",
      #   h3("Future observation"),
      #   p("Future observation time in the database from index date."),
      #   selectors(
      #     data$available_followup, prefix = "future_obs",
      #     columns = c("cdm_name", "group_level", "strata_name"),
      #     default = list(
      #       "cdm_name" = data$available_followup$cdm_name[1],
      #       "group_level" = data$available_followup$group_level[1],
      #       "strata_name" = data$available_followup$strata_name[1]
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     uiOutput("future_obs_strata_level")
      #   ),
      #   tabsetPanel(
      #     type = "tabs",
      #     tabPanel(
      #       "Summary",
      #       h5(),
      #       downloadButton("future_obs_summary_download", "Download table in word"),
      #       gt_output('future_obs_summary') %>% withSpinner()
      #     ),
      #     tabPanel(
      #       "Plot",
      #       h5(),
      #       plotSelectors(
      #         prefix = "plt_future", choices = c("cdm_name", "cohort_name", "strata_name", "strata_level", "exposed"),
      #         default = list("color" = "exposed", "facet_by" = "cdm_name")
      #       ),
      #       plotDownloadSelectors(prefix = "dwn_future"),
      #       downloadButton("future_obs_plot_download", "Download figure"),
      #       plotlyOutput('future_obs_plot') %>% withSpinner()
      #     )
      #   )
      # )
      ## re-enrollment ----
      tabItem(
        tabName = "reenrollment",
        h3("Re-enrollments"),
        p("Number of subjects firstly enrolled as controls which later became
          vaccinated an contributed in the exposed group."),
        selectors(
          data$index_date, prefix = "reenrolment",
          columns = c("cdm_name", "cohort_name", "strata_name"),
          default = list(
            "cdm_name" = data$index_date$cdm_name[1],
            "cohort_name" = data$index_date$cohort_name[1],
            "strata_name" = data$index_date$strata_name[1]
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("reenrolment_strata_level")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("reenrolment_table_download", "Download table as csv"),
            DTOutput('reenrolment_table') %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("reenrolment_summary_download", "Download table in word"),
            gt_output('reenrolment_summary') %>% withSpinner()
          )
        )
      ),
      ## population attrition ----
      tabItem(
        tabName = "attrition",
        h3("Population attrition"),
        p("Population enrollment attrition"),
        selectors(
          data$population_attrition, prefix = "attrition",
          columns = c("cdm_name", "cohort_name"),
          default = list(
            "cdm_name" = data$population_attrition$cdm_name[1],
            "cohort_name" = data$population_attrition$cohort_name[1]
          ),
          multiple = FALSE
        ),
        div(
          style = "display: inline-block;vertical-align:center; width: 150px;",
          downloadButton("attrition_table_download", "Download table as csv")
        ),
        DTOutput('attrition_table') %>% withSpinner()
      ),
      tabItem(
        tabName = "count",
        h3("Population counts"),
        p("Population counts"),
        selectors(
          data$population_count, prefix = "pop_count",
          columns = c("cdm_name", "cohort_name", "strata_name"),
          default = list(
            "cdm_name" = data$population_count$cdm_name[1],
            "cohort_name" = data$population_count$cohort_name[1],
            "strata_name" = data$population_count$strata_name[1]
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("population_count_strata_level")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            div(
              style = "display: inline-block;vertical-align:center; width: 150px;",
              downloadButton("population_count_table_download", "Download table as csv")
            ),
            DTOutput('population_count_table') %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("population_count_summary_download", "Download table in word"),
            gt_output('population_count_summary') %>% withSpinner()
          )
        )
      ),
      ## baseline ----
      tabItem(
        tabName = "baseline_characteristics",
        h3("Baseline characteristics"),
        p("Characterisation of the population before index date"),
        selectors(
          data$baseline, prefix = "baseline",
          columns = c("cdm_name", "group_level", "strata_name"),
          default = list(
            "cdm_name" = data$baseline$cdm_name[1],
            "group_level" = data$baseline$group_level[1],
            "strata_name" = data$baseline$strata_name[1]
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("baseline_strata_level")
        ),
        div(
          style = "display: inline-block;vertical-align:center; width: 150px;",
          downloadButton("baseline_table_download", "Download table as word")
        ),
        gt_output('baseline_table') %>% withSpinner()
      ),
      ## large scale characteristics ----
      tabItem(
        tabName = "large_scale_characteristics",
        h3("Large scale characteristics"),
        p("Large scale characeristics for each cohort and strata of the study"),
        selectors(data = data$large_scale,
                  prefix = "large",
                  columns = c("cdm_name", "cohort_name", "strata_name"), multiple = FALSE),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("large_scale_strata_level")
        ),
        selectors(
          data = data$large_scale,
          prefix = "large",
          columns = c("exposed", "window", "estimate_name"),
          default = list(
            "window" = unique(data$large_scale$window)[1],
            "exposed" = "overall",
            "estimate_name" = c("count", "percentage")
          )
        ),
        div(
          style = "display: inline-block;vertical-align:center; width: 150px;",
          downloadButton("large_scale_download_table", "Download current as csv")
        ),
        DTOutput("large_scale_table") %>% withSpinner()
      ),
      # SMD ----
      tabItem(
        tabName = "smd",
        h3("Standardised mean differences"),
        p("Standardised mean differences between exposed and unexposed cohorts"),
        selectors(data = data$smd,
                  prefix = "smd",
                  columns = c("cdm_name", "cohort_name", "strata_name"), multiple = FALSE),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("smd_strata_level")
        ),
        selectors(
          data = data$smd,
          prefix = "smd",
          columns = c("window"),
          default = list("window" = unique(data$large_scale$window)[1])
        ),
        div(
          style = "display: inline-block;vertical-align:center; width: 150px;",
          downloadButton("smd_download_table", "Download current as csv")
        ),
        DTOutput("smd_table") %>% withSpinner()
      ),
      ## NCO summary ----
      tabItem(
        tabName = "nco_summary",
        h3("Survival summary"),
        p("Counts and follor-up for each negative control outcome and analysis"),
        selectors(
          data = data$survival_summary |> filter(variable_name == "nco"),
          prefix = "nco_summ",
          columns = c("cdm_name", "cohort_name", "strata_name"),
          default = list("cdm_name" = "CPRD",
                         "cohort_name" = "none_first_covid_diagnostic_test",
                         "strata_name" = "overall")
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("nco_summary_strata_level")
        ),
        selectors(
          data = data$survival_summary |> filter(variable_name == "nco"),
          prefix = "nco_summ",
          columns = c("outcome"),
          default = list(
            "outcome" = data$survival_summary |> filter(variable_name == "nco") |> pull(outcome) |> unique()
          )
        ),
        selectors(
          data = data$survival_summary |> filter(variable_name == "nco"),
          prefix = "nco_summ",
          columns = c("analysis", "study_end", "window"),
          default = list(
            "window" = "0_Inf",
            "analysis" = "main",
            "study_end" = "cohort_end_date"
          ),
          multiple = FALSE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("nco_summary_download_raw", "Download current as csv"),
            DTOutput("nco_summary_raw") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("nco_summary_download_table", "Download table in word"),
            gt_output('nco_summary_table') %>% withSpinner()
          )
        )
      ),
      ## STUDY summary ----
      tabItem(
        tabName = "study_summary",
        h3("Survival summary"),
        p("Counts and follor-up for each study outcome and analysis"),
        selectors(
          data = data$survival_summary |> filter(variable_name == "study"),
          prefix = "study_summ",
          columns = c("cdm_name", "cohort_name", "strata_name"),
          default = list(
            "cdm_name" = "CPRD",
            "cohort_name" = "none_first_covid_diagnostic_test",
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("study_summary_strata_level")
        ),
        selectors(
          data = data$survival_summary |> filter(variable_name == "study"),
          prefix = "study_summ",
          columns = c("outcome"),
          default = list(
            "outcome" = data$survival_summary |> filter(variable_name == "study") |> pull(outcome) |> unique()
          )
        ),
        selectors(
          data = data$survival_summary |> filter(variable_name == "study"),
          prefix = "study_summ",
          columns = c("analysis", "study_end", "window"),
          default = list(
            "window" = "0_Inf",
            "analysis" = "main",
            "study_end" = "cohort_end_date"
          ),
          multiple = FALSE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("study_summary_download_raw", "Download current as csv"),
            DTOutput("study_summary_raw") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("study_summary_download_table", "Download table in word"),
            gt_output('study_summary_table') %>% withSpinner()
          )
        )
      ),
      ## NCO forest ----
      tabItem(
        tabName = "nco_forest_plot",
        h3("Forest plots"),
        p("Negative control outcomes risk estimates for all populations and analysis"),
        selectors(
          data = data$risk |> filter(variable_name == "nco"),
          prefix = "nco_risk",
          columns = c("cdm_name", "cohort_name", "strata_name"),
          default = list(
            "cdm_name" = "CPRD",
            "cohort_name" = "none_first_covid_diagnostic_test",
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("nco_risk_strata_level")
        ),
        selectors(
          data = data$risk |> filter(variable_name == "nco"),
          prefix = "nco_risk",
          columns = c("regression", "outcome", "analysis", "study_end", "window"),
          default = list(
            "outcome" = data$risk |> filter(variable_name == "nco") |> pull(outcome) |> unique(),
            "window" = "0_Inf",
            "analysis" = "main",
            "study_end" = "cohort_end_date"
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("nco_risk_download_raw", "Download current as csv"),
            DTOutput("nco_risk_raw") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("nco_risk_download_table", "Download table in word"),
            gt_output('nco_risk_table') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            plotSelectors(
              prefix = "plt_nco_risk",
              choices = c("cdm_name", "cohort_name", "strata_name", "strata_level",
                          "regression", "analysis", "study_end", "window", "outcome",
                          "association"),
              default = list("color" = "association", "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "dwn_nco_risk"),
            downloadButton("nco_risk_download_plot", "Download table in word"),
            plotlyOutput('nco_risk_plot') %>% withSpinner()
          )
        )
      ),
      # STUDY FOREST ----
      tabItem(
        tabName = "study_forest_plot",
        h3("Forest plots"),
        p("Study outcomes risk estimates for all populations and analysis"),
        selectors(
          data = data$risk |> filter(variable_name == "study"),
          prefix = "study_risk",
          columns = c("cdm_name", "cohort_name", "strata_name"),
          default = list(
            "cdm_name" = "CPRD",
            "cohort_name" = "none_first_covid_diagnostic_test",
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("study_risk_strata_level")
        ),
        selectors(
          data = data$risk |> filter(variable_name == "study"),
          prefix = "study_risk",
          columns = c("regression", "outcome", "analysis", "study_end", "window"),
          default = list(
            "outcome" = data$risk |> filter(variable_name == "study") |> pull(outcome) |> unique(),
            "window" = "0_Inf",
            "analysis" = "main",
            "study_end" = "cohort_end_date"
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("study_risk_download_raw", "Download current as csv"),
            DTOutput("study_risk_raw") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("study_risk_download_table", "Download table in word"),
            gt_output('study_risk_table') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            plotSelectors(
              prefix = "plt_study_risk",
              choices = c("cdm_name", "cohort_name", "strata_name", "strata_level",
                          "regression", "analysis", "study_end", "window", "outcome",
                          "association"),
              default = list("color" = "outcome", "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "dwn_study_risk"),
            downloadButton("study_risk_download_plot", "Download table in word"),
            plotlyOutput('study_risk_plot') %>% withSpinner()
          )
        )
      )
    )
  )
)
