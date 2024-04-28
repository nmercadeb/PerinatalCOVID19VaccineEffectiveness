# server shiny ----
server <- function(input, output, session) {
  ## cdm_snapshot ----
  output$cdm_snapshot_table <- renderDataTable({
    datatable(
      data$snapshot,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  # cohort_count picker ----
  output$cohort_count_cohort_name_picker <-  reactiveSelectors(
    data = data$counts, prefix = "cohort_count", columns = "cohort_name",
    restrictions = "cohort_table_name", input = input, multiple = TRUE
  )
  # cohort_count -----
  getCohortCount <- reactive({
    return(
      data$counts |>
        filterData(prefix = "cohort_count", input = input) %>%
        formatColumnNames()
    )
  })
  output$cohort_count_table <- renderDataTable({
    datatable(
      getCohortCount(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  output$cohort_count_download_table <- downloadHandler(
    filename = function() {
      "cohortCountTable.csv"
    },
    content = function(file) {
      write_csv(getCohortCount(), file)
    }
  )
  ## cohort_attrition picker ----
  output$cohort_attrition_cohort_name_picker <- reactiveSelectors(
    data = data$attrition, prefix = "cohort_attrition", columns = "cohort_name",
    restrictions = "cohort_table_name", input = input, multiple = TRUE,
    default = list("cohort_name" = data$attrition$cohort_name[data$attrition$cohort_table_name == input$cohort_attrition_cohort_table_name][1])
  )
  # cohort_attrition table -----
  getAttrition <- reactive({
    data$attrition |>
      filterData("cohort_attrition", input) |>
      mutate(reason_id = as.numeric(reason_id)) |>
      group_by(cdm_name, cohort_table_name, cohort_name) |>
      arrange(reason_id) |>
      ungroup()
  })
  output$cohort_attrition_table <- renderDataTable({
    datatable(
      getAttrition() %>%
        formatColumnNames() %>%
        select(c("Cdm name", "Cohort table name", "Cohort name", "Reason", -"Reason id", "Number records", "Number subjects", "Excluded records", "Excluded subjects")),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  output$cohort_attrition_download_table <- downloadHandler(
    filename = function() {
      "attritionTable.csv"
    },
    content = function(file) {
      write_csv(getAttrition() %>%
                  rename_with(~stringr::str_to_sentence(gsub("_", " ", .x, fixed = TRUE))) %>%
                  select(c("Cdm name", "Cohort group", "Cohort name", "Reason", -"Reason id", input$cohort_attrition_count)),
                file)
    }
  )
  # cohort attrition chart ----
  output$attrition_diagram <- renderGrViz({
    render_graph(attritionChart(getAttrition()))
  })
  output$cohort_attrition_download_figure <- downloadHandler(
    filename = function() {
      paste0("cohort_attrition_", input$cohort_attrition_cohort_name, ".png")
    },
    content = function(file) {
      table <- getAttrition()
      export_graph(
        graph = attritionChart(table),
        file_name = file,
        file_type = "png",
        width = 800
      )
    }
  )
  # baseline strata levels  ----
  output$baseline_strata_level <- renderUI({
    pickerInput(
      inputId = "baseline_strata_level",
      label = "Strata level",
      choices = unique(data$characteristics$strata_level[data$characteristics$strata_name == input$baseline_strata_name]),
      selected = unique(data$characteristics$strata_level[data$characteristics$strata_name == input$baseline_strata_name]),
      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      multiple = TRUE
    )
  })
  # baseline tidy table ----
  getBaselineTidy <- reactive({
    return(
      filterData(data$characteristics, "baseline", input) |>
        formatEstimateName(
          estimateNameFormat = c(
            "N (%)" = "<count> (<percentage>%)",
            "N" = "<count>",
            "<median> [<q25> - <q75>]",
            "<mean> (<sd>)",
            "[<min> - <max>]",
            "[<q05> - <q95>]"
          )) |>
        splitStrata()
    )
  })
  output$baseline_characteristics_tidy <- renderDataTable({
    datatable(
      getBaselineTidy(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  output$baseline_characteristics_tidy_download <- downloadHandler(
    filename = function() {
      "baselineCharacteristicsTable.csv"
    },
    content = function(file) {
      write_csv(getBaselineTidy(), file)
    }
  )
  # baseline formatted table ----
  getBaselineFormatted <- reactive({
    # row order
    row_order <- c("Number records", "Number subjects", "Age", "Age group",
                   "Age groups", "Sex", "Region",  "Prior observation",
                   "Future observation",  "Cohort start date", "Cohort end date",
                   "Followup", "Stop followup", "Prior vaccine days",
                   "Immunocompromised", "Calendar time", "Brand", "Adapted",
                   "Visit_occurrence count from -365 to -1",
                   "Conditions flag from -365 to -1",
                   "Conditions flag from -30 to -1",
                   "Conditions flag from -inf to -1",
                   "Medications flag from -183 to -1",
                   "Medications flag from -30 to -1")
    # prepare for and convert to gt
    out <- filterData(data$characteristics, "baseline", input) |>
      formatEstimateName(
        estimateNameFormat = c(
          "N (%)" = "<count> (<percentage>%)",
          "N" = "<count>",
          "Median [Q25-Q75]" = "<median> [<q25> - <q75>]",
          "Mean (SD)" = "<mean> (<sd>)",
          "[Min. - Max.]" = "[<min> - <max>]",
          "[Q05-Q95]" = "[<q05> - <q95>]"
        )) |>
      mutate(across(c("strata_name", "strata_level"), ~ stringr::str_to_sentence(gsub("_", " ", .x)))) |>
      formatHeader(header = c("Cohort name", "cohort_name", "Study strata",
                              "strata_name", "strata_level"),
                   includeHeaderName = FALSE) |>
      select(-c("estimate_type")) |>
      mutate(variable_name = factor(variable_name, levels = row_order)) |>
      arrange(variable_name) |>
      mutate(variable_name = as.character(variable_name)) |>
      rename("Variable name" = "variable_name",
             "Variable level" = "variable_level",
             "Estimate name" = "estimate_name") |>
      gtTable(groupNameCol = "cdm_name",
              colsToMergeRows = c("Variable name", "Variable level"))
    return(out)
  })
  output$baseline_characteristics_formatted <- render_gt ({
    getBaselineFormatted() %>%
      tab_options(column_labels.padding = px(5)) %>%
      sub_missing() %>%
      cols_width(everything() ~ px(200), `Variable name` ~ 250, `Variable level` ~ 250, `Estimate name` ~ 250)
  })
  output$baseline_characteristics_formatted_download <- downloadHandler(
    filename = function() {
      "baselineCharacteristicsTable.docx"
    },
    content = function(file) {
      gtsave(data = getBaselineFormatted(),
             filename = file,
             vwidth = 400,
             vheight = 300)
    },
    contentType = "docx"
  )
  # ls characterisation ----
  output$large_table_strata_level_picker <-  reactiveSelectors(
    data = data$large_scale_characteristics, prefix = "large_table", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE,
    default = list("strata_level" = data$large_scale_characteristics$strata_level[data$large_scale_characteristics$strata_name %in% input$large_table_strata_name])
  )
  getLSCharacteristicsTidy <- reactive({
    filterData(data$large_scale_characteristics, "large", input) %>%
      filterData("large_table", input) %>%
      pivot_wider(names_from = estimate_name, values_from = estimate_value) %>%
      select(c("cdm_name", "cohort_name", "strata_name", "strata_level", "window",
               "concept_domain", "concept", "concept_name", input$large_table_estimate_name)) %>%
      formatColumnNames()
  })
  output$ls_characterisation_table <- renderDataTable({
    datatable(
      getLSCharacteristicsTidy(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  output$large_download_table <- downloadHandler(
    filename = function() {
      "largeScaleCharacteristics.csv"
    },
    content = function(file) {
      write_csv(getLSCharacteristicsTidy(), file)
    }
  )
  # lsc + smd table ----
  output$comp_large_strata_level_reference_picker <-  renderUI({
    pickerInput(
      inputId = "comp_large_strata_level_reference",
      label = "Strata level",
      choices = unique(data$large_scale_characteristics$strata_level[data$large_scale_characteristics$strata_name == input$comp_large_strata_name_reference]),
      selected = unique(data$large_scale_characteristics$strata_level[data$large_scale_characteristics$strata_name == input$comp_large_strata_name_reference])[1],
      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      multiple = FALSE
    )
  })
  output$comp_large_strata_level_comparator_picker <-  renderUI({
    pickerInput(
      inputId = "comp_large_strata_level_comparator",
      label = "Strata level",
      choices = unique(data$large_scale_characteristics$strata_level[data$large_scale_characteristics$strata_name == input$comp_large_strata_name_comparator]),
      selected = unique(data$large_scale_characteristics$strata_level[data$large_scale_characteristics$strata_name == input$comp_large_strata_name_comparator])[1],
      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      multiple = FALSE
    )
  })
  getLargeComparisonTable <- reactive({
    filterData(data = data$large_scale_characteristics, prefix = "large", input = input) %>%
      rename_with(.fn = ~ paste0(., "_reference"), .cols = c("cohort_name", "strata_name", "strata_level")) %>%
      mutate(estimate_name = paste0(estimate_name, "_reference"),
             estimate_value = as.numeric(gsub(",", "", estimate_value))) %>%
      filterData(prefix = "comp_large", input = input) %>%
      pivot_wider(names_from = estimate_name, values_from = estimate_value) %>%
      left_join(
        filterData(data = data$large_scale_characteristics, prefix = "large", input = input) %>%
          rename_with(.fn = ~ paste0(., "_comparator"), .cols = c("cohort_name", "strata_name", "strata_level")) %>%
          mutate(estimate_name = paste0(estimate_name, "_comparator"),
                 estimate_value = as.numeric(gsub(",", "", estimate_value))) %>%
          filterData(prefix = "comp_large", input = input) %>%
          pivot_wider(names_from = estimate_name, values_from = estimate_value)
      ) %>%
      mutate(smd = (percentage_comparator/100 - percentage_reference/100) / sqrt((percentage_comparator/100*(1-percentage_comparator/100) + percentage_reference/100*(1-percentage_reference/100))/2),
             smd = round(smd, 3))
  })
  output$lsc_smd_tidy <- renderDataTable({
    datatable(
      getLargeComparisonTable() %>%
        mutate(estimate_reference = paste0(count_reference, " (", percentage_reference, "%)"),
               estimate_comparator = paste0(count_comparator, " (", percentage_comparator, "%)")) %>%
        select(c("cdm_name", "cohort_name_reference", "strata_level_reference", "cohort_name_comparator", "strata_level_comparator",
                 "window", "concept_domain", "concept", "concept_name", "estimate_reference", "estimate_comparator", "smd")) ,
      # formatColumnNames(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  output$lsc_smd_tidy_download <- downloadHandler(
    filename = function() {
      "largeScaleComparison.csv"
    },
    content = function(file) {
      write_csv(getLargeComparisonTable(), file)
    }
  )
  # lsc + smd pot ----
  getLargeComparisonPlot <- reactive({
    table <- getLargeComparisonTable()
    thr <- 0.1

    balance_area <- expand_grid(
      cohort_reference = input$comp_large_cohort_name_reference,
      cohort_comparator = input$comp_large_cohort_name_comparator,
      cdm_name = input$large_cdm_name,
      concept_domain = input$large_concept_domain,
      window = input$large_window,
      concept = NA,
      concept_name = NA,
      count_reference = NA,
      percentage_reference = NA,
      count_comparator = NA,
      percentage_comparator = NA
    ) %>%
      cross_join(
        tibble(p_reference = seq(0, 1, by = 0.01)) %>%
          mutate(
            a = 1 + thr^2/2,
            b = -2*p_reference - thr^2/2,
            c = p_reference*p_reference - (thr^2/2 * (p_reference - p_reference*p_reference)),
            p_comparator_pos = (-b + sqrt(b^2 - 4*a*c))/(2*a) * 100,
            p_comparator_neg = (-b - sqrt(b^2 - 4*a*c))/(2*a) * 100,
            p_reference = p_reference * 100,
            p_comparator = p_reference
          )
      )

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    if(is.null(input$plot_lsc_color)) {
      if(!is.null(input$plot_lsc_facet_by)){
        p <- table %>%
          unite("facet_var",
                c(all_of(input$plot_lsc_facet_by)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x = "percentage_reference", y = "percentage_comparator",
                            label = "concept_domain",
                            label1 = "concept_name",
                            label2 = "concept",
                            label3 = "window",
                            label4 = "count_reference",
                            label5 = "count_comparator"
          )) +
          geom_ribbon(data = balance_area %>%
                        unite("facet_var",
                              c(all_of(input$plot_lsc_facet_by)), remove = FALSE, sep = "; "),
                      mapping = aes(x = p_reference,
                                    y = p_comparator,
                                    ymin = p_comparator_pos,
                                    ymax = p_comparator_neg),
                      alpha = 0.4,
                      color = "#a3b18a",
                      fill = "#a3b18a") +
          geom_point() +
          geom_abline(intercept = 0, slope = 1) +
          facet_wrap(vars(facet_var),nrow = 2) +
          theme_bw() +
          theme(legend.position = "none")
      } else {
        p <- table %>%
          ggplot(aes_string(x = "percentage_reference", y = "percentage_comparator",
                            label = "concept_domain",
                            label1 = "concept_name",
                            label2 = "concept",
                            label3 = "window",
                            label4 = "count_reference",
                            label5 = "count_comparator")) +
          geom_ribbon(data = balance_area,
                      mapping = aes(x = p_reference,
                                    y = p_comparator,
                                    ymin = p_comparator_pos,
                                    ymax = p_comparator_neg),
                      alpha = 0.4,
                      color = "#a3b18a",
                      fill = "#a3b18a") +
          geom_point() +
          geom_abline(intercept = 0, slope = 1) +
          theme_bw() +
          theme(legend.position = "none")
      }
    } else {
      if(!is.null(input$plot_lsc_facet_by)){
        p <- table %>%
          unite("facet_var",
                c(all_of(input$plot_lsc_facet_by)), remove = FALSE, sep = "; ") %>%
          unite("color_var",
                c(all_of(input$plot_lsc_color)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x = "percentage_reference", y = "percentage_comparator",
                            color = "color_var",
                            label = "concept_domain",
                            label1 = "concept_name",
                            label2 = "concept",
                            label3 = "window",
                            label4 = "count_reference",
                            label5 = "count_comparator"
          )) +
          geom_ribbon(data = balance_area %>%
                        unite("facet_var",
                              c(all_of(input$plot_lsc_facet_by)), remove = FALSE, sep = "; ") %>%
                        unite("color_var",
                              c(all_of(input$plot_lsc_color)), remove = FALSE, sep = "; "),
                      mapping = aes(x = p_reference,
                                    y = p_comparator,
                                    ymin = p_comparator_pos,
                                    ymax = p_comparator_neg),
                      alpha = 0.4,
                      color = "#a3b18a",
                      fill = "#a3b18a") +
          geom_point() +
          geom_abline(intercept = 0, slope = 1) +
          facet_wrap(vars(facet_var),nrow = 2) +
          theme_bw() +
          theme(legend.position = "none")
      } else{
        p <- table %>%
          unite("color_var",
                c(all_of(input$plot_lsc_color)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x = "percentage_reference", y = "percentage_comparator",
                            color = "color_var",
                            label = "concept_domain",
                            label1 = "concept_name",
                            label2 = "concept",
                            label3 = "window",
                            label4 = "count_reference",
                            label5 = "count_comparator")) +
          geom_ribbon(data = balance_area %>%
                        unite("color_var",
                              c(all_of(input$plot_lsc_color)), remove = FALSE, sep = "; "),
                      mapping = aes(x = p_reference,
                                    y = p_comparator,
                                    ymin = p_comparator_pos,
                                    ymax = p_comparator_neg),
                      alpha = 0.4,
                      color = "#a3b18a",
                      fill = "#a3b18a") +
          geom_point() +
          geom_abline(intercept = 0, slope = 1) +
          theme_bw() +
          theme(legend.position = "none")
      }
    }

    p +
      xlab("Reference") +
      ylab("Comparator")
  })
  output$lsc_smd_plot <- renderPlotly({
    getLargeComparisonPlot()
  })
  output$lsc_smd_plot_download <- downloadHandler(
    filename = function() {
      paste0("largeComparisonPlot.", input$plsc_device)
    },
    content = function(file) {
      ggsave(file, getLargeComparisonPlot(),
             width = as.numeric(input$plsc_width),
             height = as.numeric(input$plsc_height))
    }
  )
  #  weekly counts table  ----
  output$wcounts_strata_level_picker <-  reactiveSelectors(
    data = data$large_scale_characteristics, prefix = "weekly_counts", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  getWeeklyCounts <- reactive({
    data$weekly_counts %>%
      filterData(prefix = "weekly_counts", input = input) %>%
      filter(.data$week_start >= input$date_wcounts_start) %>%
      filter(.data$week_start <= input$date_wcounts_end) %>%
      arrange(.data$week_start)
  })
  output$weekly_counts_table <- renderDataTable({
    datatable(
      getWeeklyCounts() %>%
        mutate(n = if_else(is.na(n), "<5", as.character(n))) %>%
        select("CDM name" = "cdm_name",
               "Cohort name" = "cohort_name",
               "Week start" = "week_start",
               "N" = "n"),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  output$wcounts_table_download <- downloadHandler(
    filename = function() {
      "weeklyCounts.csv"
    },
    content = function(file) {
      write_csv(getWeeklyCounts() %>%
                  select("CDM name" = "cdm_name",
                         "Cohort name" = "cohort_name",
                         "Week start" = "week_start",
                         "N" = "n"),
                file)
    }
  )
  # weekly counts plot ----
  getWeeklyCountsPlot <- reactive({
    table <- getWeeklyCounts()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    if(is.null(input$plt_wcounts_color)){
      if(!is.null(input$plt_wcounts_facet_by)){
        p<-table %>%
          filter(!is.na(n)) %>%
          unite("facet_var",
                c(all_of(input$plt_wcounts_facet_by)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x= "week_start", y = "n"
          )) +
          geom_point() +
          geom_line() +
          scale_x_date(date_breaks = "months", date_labels = "%b%y") +
          facet_wrap(vars(facet_var),nrow = 2) +
          theme_bw()
      } else{
        p<-table %>%
          filter(!is.na(n)) %>%
          ggplot(aes_string(x= "week_start", y="n")) +
          geom_point() +
          geom_line() +
          scale_x_date(date_breaks = "months", date_labels = "%b%y") +
          theme_bw()
      }
    }


    if(!is.null(input$plt_wcounts_color) ){
      if(is.null(input$plt_wcounts_facet_by) ){
        p<-table %>%
          filter(!is.na(n)) %>%
          unite("Group",
                c(all_of(input$plt_wcounts_color)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x= "week_start", y="n",
                            group="Group",
                            fill = "Group",
                            colour="Group")) +
          geom_point() +
          geom_line() +
          scale_x_date(date_breaks = "months", date_labels = "%b%y") +
          theme_bw()
      }

      if(!is.null(input$plt_wcounts_facet_by) ){
        if(!is.null(input$plt_wcounts_color) ){
          p<-table %>%
            filter(!is.na(n)) %>%
            unite("Group",
                  c(all_of(input$plt_wcounts_color)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$plt_wcounts_facet_by)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x= "week_start", y="n",
                              group="Group",
                              fill = "Group",
                              colour="Group")) +
            geom_point() +
            geom_line() +
            scale_x_date(date_breaks = "months", date_labels = "%b%y") +
            facet_wrap(vars(facet_var),ncol = 2)+
            theme_bw()
        }
      }
    }

    p +
      xlab("Week start") +
      ylab("N")
  })
  output$plot_weekly_counts <- renderPlotly({
    getWeeklyCountsPlot()
  })
  output$plot_weekly_counts_download <- downloadHandler(
    filename = function() {
      paste0("weeklyCountsPlot.", input$wcounts_device)
    },
    content = function(file) {
      ggsave(file, getWeeklyCountsPlot(),
             width = as.numeric(input$wcounts_width),
             height = as.numeric(input$wcounts_height))
    }
  )
}
