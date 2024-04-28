ui <- dashboardPage(
  dashboardHeader(title = "P2-C3-002"),
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
        menuSubItem(
          text = "Available follow-up",
          tabName = "available_followup"
        ),
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
        menuSubItem(
          text = "Proportionality",
          tabName = "proportionality"
        ),
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
                  default = list("cdm_name" = data$counts$cdm_name[1],
                                 "cohort_group" = data$counts$cohort_group[1])),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("cohort_count_cohort_name_picker")
        ),
        div(
          style = "margin-bottom: 20px;",
          downloadButton("cohort_count_download_table", "Download current attrition")
        ),
        DTOutput("cohort_count_table") %>% withSpinner()
      ),
      ### matching ----
      tabItem(
        tabName = "cdm_snapshot",
        h3("Database details"),
        p("See details of CDM snapshot for each database:"),
        DTOutput("cdm_snapshot_table")
      ),
      ### baseline_characteristics ----
      tabItem(
        tabName = "baseline_characteristics",
        h3("Characterisation of the participants"),
        h5("Population settings"),
        selectors(data$characteristics, prefix = "baseline",
                  columns = c("cdm_name", "cohort_name", "strata_name"), multiple = TRUE,
                  default = list("cdm_name" = data$characteristics$cdm_name[1],
                                 "cohort_name" = data$characteristics$cohort_name[1],
                                 "strata_name" = data$characteristics$strata_name[1])),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("baseline_strata_level")
        ),
        h5("Variables and estimates"),
        selectors(data = data$characteristics, prefix = "baseline",
                  columns = c("variable_name", "estimate_name")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("baseline_characteristics_tidy_download", "Download current characteristics table"),
            DTOutput('baseline_characteristics_tidy') %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            h5(),
            downloadButton("baseline_characteristics_formatted_download", "Download table in word"),
            gt_output('baseline_characteristics_formatted') %>% withSpinner()
          )
        )
      ),
      ### large scale characteristics ----
      tabItem(
        tabName = "large_scale_characteristics",
        h3("Large scale characteristics"),
        p("Large scale characeristics for each cohort and strata of the present study"),
        selectors(data = data$large_scale_characteristics, prefix = "large",
                  columns = c("cdm_name"), multiple = FALSE),
        selectors(data = data$large_scale_characteristics, prefix = "large",
                  columns = c("concept_domain", "window"),
                  default = list("concept_domain" = unique(data$large_scale_characteristics$concept_domain),
                                 "window" = unique(data$large_scale_characteristics$window)[1])),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            selectors(
              data = data$large_scale_characteristics, prefix = "large_table",
              columns = c("cohort_name", "strata_name"),
              default = list(
                "cohort_name" = data$large_scale_characteristics$cohort_name[1],
                "strata_name" = data$large_scale_characteristics$strata_name[1])
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              uiOutput("large_table_strata_level_picker")
            ),
            selectors(data = data$large_scale_characteristics, prefix = "large_table",
                      columns = c("estimate_name")),
            div(
              style = "display: inline-block;vertical-align:center; width: 150px;",
              downloadButton("large_download_table", "Download current table")
            ),
            DTOutput("ls_characterisation_table") %>% withSpinner()
          ),
          tabPanel(
            "Comparison",
            h4("Cohort reference"),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "comp_large_cohort_name_reference",
                label = "Cohort name",
                choices = unique(data$large_scale_characteristics$cohort_name),
                selected = unique(data$large_scale_characteristics$cohort_name)[1],
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = FALSE
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "comp_large_strata_name_reference",
                label = "Strata name",
                choices = unique(data$large_scale_characteristics$strata_name),
                selected = unique(data$large_scale_characteristics$strata_name)[1],
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = FALSE
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              uiOutput("comp_large_strata_level_reference_picker")
            ),
            h4("Cohort comparator"),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "comp_large_cohort_name_comparator",
                label = "Cohort name",
                choices = unique(data$large_scale_characteristics$cohort_name),
                selected = unique(data$large_scale_characteristics$cohort_name)[2],
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = FALSE
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "comp_large_strata_name_comparator",
                label = "Strata name",
                choices = unique(data$large_scale_characteristics$strata_name),
                selected = unique(data$large_scale_characteristics$strata_name)[1],
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = FALSE
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              uiOutput("comp_large_strata_level_comparator_picker")
            ),
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Table",
                h5(),
                downloadButton("lsc_smd_tidy_download", "Download current table"),
                DTOutput('lsc_smd_tidy') %>% withSpinner()
              ),
              tabPanel(
                "Plot",
                h4("Plotting options"),
                plotSelectors(prefix = "plot_lsc",
                              choices = c("concept_domain", "window", "concept"),
                              default = list("color" = "concept", "facet_by" = NULL)),
                h4("Download options"),
                plotDownloadSelectors("plsc"),
                downloadButton("lsc_smd_plot_download", "Download current figure"),
                plotlyOutput('lsc_smd_plot', height = "800px") %>% withSpinner()
              )
            )
          )
        )
      ),
      ### weekly_counts_distribution ----
      tabItem(
        tabName = "weekly_counts_distribution",
        h3("Weekly counts distribution"),
        p("Weekly counts for each cohort of the present study"),
        h4("Population"),
        selectors(
          data$weekly_counts, prefix = "weekly_counts",
          columns = c("cohort_name", "cdm_name", "strata_name"),
          default = list("cohort_name" = "first_dose",
                         "cdm_name" = data$weekly_counts$cdm_name[1],
                         "strata_name" = data$weekly_counts$strata_name[1])
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("wcounts_strata_level_picker")
        ),
        h4("Dates"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "date_wcounts_start",
            label = "First week",
            choices = as.character(unique(sort(data$weekly_counts$week_start))),
            selected = as.character(min(data$weekly_counts$week_start)),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "date_wcounts_end",
            label = "Last week",
            choices = as.character(unique(sort(data$weekly_counts$week_start))),
            selected = as.character(max(data$weekly_counts$week_start)),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            downloadButton("wcounts_table_download", "Download current table"),
            DTOutput('weekly_counts_table') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h4("Plotting options"),
            plotSelectors(prefix = "plt_wcounts", choices = c("cohort_name", "strata_name", "strata_level"),
                          default = list("color" = "cohort_name", "facet_by" = "cohort_name")),
            h4("Download options"),
            plotDownloadSelectors("wcounts"),
            downloadButton("plot_weekly_counts_download", "Download current figure"),
            plotlyOutput('plot_weekly_counts', height = "800px") %>% withSpinner()
          )
        )
      )
    )
  )
)
