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

# data ----
source(here("prepare_data.R"))

# plots ----
comparisons <- unique(smd$comparison)
covid <- unique(smd$covid_definition)
stratifications <- unique(risk$strata_name)
regression <- "cox" # unique(risk$regression)
censoring <-  "none" # unique(risk$exposed_censoring)
followup <- unique(risk$followup_end)
outcome <- unique(kaplan_meier$outcome)

## ID ----
for (comp in comparisons) {
  for (cov in covid) {
    index_date %>%
      filter(comparison == comp, covid_definition == cov) |>
      ggplot(aes(x = date, y = nn,
                 fill = strata_name, color = strata_name)) +
      geom_col(position = "identity") +
      facet_wrap(~ strata_name + strata_level) +
      theme_bw() +
      scale_x_date(date_labels = "%m/%y",
                   breaks = as.Date(c("2021-01-01", "2021-06-01", "2022-01-01", "2022-06-01", "2023-01-01")),
                   limits = as.Date(c("2021-01-01", "2023-01-01"))) +
      scale_fill_manual(values=c("#dda15e", "#778da9", "#87b38d")) +
      scale_color_manual(values=c("#bc6c25", "#4a4e69", "#4a7c59")) +
      ylab("Counts") +
      xlab("Index date") +
      theme(legend.position = "none")
    ggsave(
      paste0(comp, "_", cov, "_id.png"),
      plot = last_plot(),
      path = here::here("figures", "index_date"),
      scale = 1,
      width = 3000,
      height = 1300,
      units = "px",
      dpi = 300
    )
  }
}

## ASMD ----
for (comp in comparisons) {
  for (cov in covid) {
    smd %>%
      filter(covid_definition == cov, comparison == comp) |>
      ggplot(aes(x = asmd, color = balance, fill = balance)) +
      geom_histogram() +
      facet_wrap(~ strata_name + strata_level) +
      scale_color_manual(values = c("#226f54", "#52796f", "#e57c04"),
                         labels = unname(TeX(c("ASMD $\\leq 0.1$", "0.1 < AMSD $\\leq 0.2$", "ASMD $> 0.2$"))))+
      scale_fill_manual(values = c("#87c38f", "#84a98c", "#f6ae2d"),
                        labels = unname(TeX(c("ASMD $\\leq 0.1$", "0.1 < AMSD $\\leq 0.2$", "ASMD $> 0.2$"))))+
      geom_hline(yintercept = 0, color = "white") +
      geom_vline(xintercept = 0.1, linetype = 'dashed') +
      geom_vline(xintercept = 0.2, linetype = 'dashed') +
      ylab("Counts") +
      xlab("ASMD") +
      theme_bw() +
      theme(legend.title = element_blank())
    ggsave(
      paste0(comp, "_", cov, "_asmd.png"),
      plot = last_plot(),
      path = here::here("figures", "asmd"),
      scale = 1,
      width = 3000,
      height = 1300,
      units = "px",
      dpi = 300
    )
  }
}


for (cov in covid) {
  smd %>%
    filter(covid_definition == cov) |>
    group_by(comparison, strata_name, strata_level, balance) |>
    tally() |>
    left_join(
      smd %>%
        filter(covid_definition == cov) |>
        group_by(comparison, strata_name, strata_level) |>
        tally(name = "total")
    ) |>
    mutate(
      n = paste0(niceNum(n), " (", niceNum(n/total*100, dec = 2), "%)"),
      ASMD = case_when(
        balance == "ASMD $\\leq$ 0.1" ~ "ASMD \U2264 0.1",
        balance == "ASMD > 0.2" ~ "ASMD > 0.2",
        balance == "0.1 < ASMD $\\leq$ 0.2" ~ "0.1 < ASMD \U2264 0.2"
      ),
      comparison = factor(comparison,
                          levels = c("none_first", "complete_booster"),
                          labels = c("1st dose vs. Unvaccinated", "3rd dose vs. 2nd dose"))
    ) |>
    select("comparison", "Strata name" = "strata_name", "Strata value" = "strata_level",
           "ASMD", "Counts, N(%)" = "n") |>
    ungroup() |>
    pivot_wider(names_from = c("ASMD"), values_from = "Counts, N(%)") |>
    relocate("ASMD ≤ 0.1", .before = "0.1 < ASMD ≤ 0.2") |>
    rename_with(~paste0("[header]Counts, N(%)\n[header_level]", .x), contains("ASMD")) |>
    gtTable(groupNameCol = "comparison",
            style = list(
              "header" = list(gt::cell_fill(color = "#e1e1e1"),
                              gt::cell_text(weight = "bold")),
              "header_name" = list(gt::cell_fill(color = "#e1e1e1"),
                                   gt::cell_text(weight = "bold")),
              "header_level" = list(gt::cell_fill(color = "#e1e1e1"),
                                    gt::cell_text(weight = "bold")),
              "column_name" = list(gt::cell_fill(color = "#e1e1e1"),
                                   gt::cell_text(weight = "bold")),
              "group_label" = list(gt::cell_fill(color = "#f2f2f2"), gt::cell_text(weight = "bold")),
              "body" = list(gt::cell_borders(color = "#D3D3D3", sides = c("left", "right")))
            ),
            colsToMergeRows = "all_columns") |>
    gtsave(filename = paste0(cov, "_smd_unbalance.png"), path = here::here("figures", "counts"), vwidth = 1500, expand = 50)
}


# NCO ----
for (comp in comparisons) {
  for (cov in covid) {
    for (cens in censoring) {
      for (fup in followup) {
        for (reg in regression) {
          listPlots <- list()
          ii <- 1
          for (strata_level in c("overall", "pfizer",  "moderna", "T1", "T2", "T3")) {
            dataNCO <- risk %>%
              filter(variable_name == "nco") %>%
              filter(covid_definition == cov, comparison == comp,
                     regression == reg, exposed_censoring == cens,
                     followup_end == fup, strata_level == .env$strata_level)
            gg <- plotCiCalibrationEffect_NMB(dataNCO$coef,
                                              dataNCO$se_coef,
                                              rep(0, nrow(dataNCO)),
                                              legacy = FALSE,
                                              model = NULL,
                                              xLabel = "Hazard ratio",
                                              fileName = NULL)
            listPlots[[ii]] <- gg
            ii <- ii + 1
          }

          gg <- ggarrange(
            plots = listPlots,
            ncol = 3,
            nrow = 2,
            labels = as.list(c("Overall", "Pfizer",  "Moderna", "Trimester 1", "Trimester 2", "Trimester 3")),
            label.args = list(gp = grid::gpar(cex = 1.2, fontface = "bold"))
          )
          ggsave(
            paste0(comp, "_", cov, "_", fup, "_nco.png"),
            plot = gg,
            path = here::here("figures", "nco"),
            scale = 1,
            width = 3100,
            height = 2000,
            units = "px",
            dpi = 300
          )
        }
      }
    }
  }
}


# KM ----
for (comp in comparisons) {
  for (cov in covid) {
    for (fup in followup) {
      for (strata in stratifications) {
        for (out in outcome) {
          gg <- kaplan_meier |>
            filter(covid_definition == cov, comparison == comp, followup_end == fup,
                   strata_name == strata, outcome == out, delivery_excluded == "-" | delivery_excluded == "yes") |>
            ggplot(aes(x = time, y = estimate, color = Cohort, fill = Cohort, ymin = estimate_95CI_lower, ymax = estimate_95CI_upper)) +
            geom_step(size = 1) +
            geom_ribbon(alpha = 0.3, colour = NA) +
            scale_color_manual(values = c("#87b38d", "#0d3b66")) +
            scale_fill_manual(values = c("#87b38d", "#0d3b66")) +
            theme_bw() +
            xlab("Time (days)") +
            ylab("Survival probability")
          if (strata == "overall") {
            width = 2900
            height = 1500
          } else if (strata == "trimester") {
            gg <- gg +
              facet_wrap(. ~ strata_level)
            width = 3500
            height = 1500
          } else if (strata == "vaccine_brand") {
            gg <- gg +
              facet_wrap(. ~ strata_level)
            width = 3100
            height = 1500
          }
          ggsave(
            paste0(comp, "_", cov, "_", fup, "_", strata, ".png"),
            plot = gg,
            path = here::here("figures", "kaplan-meier", out),
            scale = 1,
            width = width,
            height = height,
            units = "px",
            dpi = 300
          )
        }
      }
    }
  }
}


for (cov in covid) {
  for (fup in followup) {
    for (strata in stratifications) {
      for (out in outcome) {
        gg <- kaplan_meier |>
          filter(covid_definition == cov, followup_end == fup,
                 strata_name == strata, outcome == out, delivery_excluded == "-" | delivery_excluded == "yes") |>
          mutate(
            comparison = factor(comparison,
                                levels = c("none_first", "complete_booster"),
                                labels = c("1st dose vs. Unvaccinated", "3rd dose vs. 2nd dose"))
          ) |>
          ggplot(aes(x = time, y = estimate, color = Cohort, fill = Cohort, ymin = estimate_95CI_lower, ymax = estimate_95CI_upper)) +
          geom_step(size = 1) +
          geom_ribbon(alpha = 0.3, colour = NA) +
          scale_color_manual(values = c("#87b38d", "#0d3b66")) +
          scale_fill_manual(values = c("#87b38d", "#0d3b66")) +
          theme_bw() +
          theme(
            strip.text = element_text(size = 15),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 15)
          ) +
          xlab("Time (days)") +
          ylab("Survival probability")
        if (strata == "overall") {
          gg <- gg +
            facet_grid(rows = vars(comparison))
          width = 2900
          height = 2000
        } else if (strata == "trimester") {
          gg <- gg +
            facet_grid(rows = vars(comparison), cols = vars(strata_level))
          width = 3500
          height = 2000
        } else if (strata == "vaccine_brand") {
          gg <- gg +
            facet_grid(rows = vars(comparison), cols = vars(strata_level))
          width = 3100
          height = 2000
        }
        ggsave(
          paste0("both_", cov, "_", fup, "_", strata, ".png"),
          plot = gg,
          path = here::here("figures", "kaplan-meier", out),
          scale = 1,
          width = width,
          height = height,
          units = "px",
          dpi = 300
        )
      }
    }
  }
}



# COX: 0 - Inf ----
y_breaks <- c(7, 5.5, 4.5, 3, 2, 1)
strata_level <- c("Overall", "Pfizer", "Moderna", "T1", "T2", "T3")
xlim <- c(0, 10)
for (cov in covid) {
  for (fup in followup) {
    gg_data <- risk |>
      filter(variable_name == "study",
             regression == "cox",
             outcome %in% c("covid"),
             window %in% c("0_Inf"),
             covid_definition == cov,
             delivery_excluded == "yes" | delivery_excluded == "-",
             followup_end == fup, exposed_censoring == cens) |>
      mutate(strata_level = factor(strata_level,
                                   levels = c("overall", "pfizer", "moderna", "T1", "T2", "T3"),
                                   label = c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3"))
      ) %>%
      inner_join(tibble(y_breaks = y_breaks, strata_level = strata_level))  %>%
      mutate(Strata = factor(strata_name,
                             levels = c("overall", "vaccine_brand", "trimester"),
                             labels = c("Overall", "Vaccine brand", "Trimester")),
             Outcome = factor(outcome,
                              levels = c("covid"),
                              labels = c("SARS-CoV-2 infection")),
             comparison = factor(comparison,
                                 levels = c("none_first", "complete_booster"),
                                 labels = c("1st dose vs. Unvaccinated", "3rd dose vs. 2nd dose"))) %>%
      filter(!(exp_coef < 0.1 & abs(lower_ci-upper_ci) > 1))
    gg_min <- min(gg_data$lower_ci)
    gg_max <- max(gg_data$upper_ci)
    gg <- gg_data %>%
      ggplot(aes(y = y_breaks, color = Strata)) +
      geom_point(aes(x = exp_coef, shape = Outcome), size = 2) +
      geom_linerange(aes(xmin = lower_ci, xmax = upper_ci)) +
      scale_y_continuous(breaks = y_breaks, labels = strata_level,
                         limits = c(0.5, max(y_breaks)+0.5) ) +
      scale_x_continuous(limits = c(0.2, 1.3), breaks = c(0.25, 0.5, 0.75, 1, 1.5), trans = "log",
                         oob=scales::rescale_none) +
      geom_vline(xintercept = 1) +
      ggplot2::theme_bw() +
      facet_wrap(. ~ comparison) +
      theme(
        axis.title.y = element_blank()
      ) +
      scale_color_manual(values = c("#FF7F0FFF", "#3CB7CCFF","#32A251FF",  "#ACD98DFF", "#B85A0DFF", "#B85A0DFF")) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 4, ymax = 6),
                fill = "#a6e1fa",
                alpha = 0.02,
                color = "#a6e1fa",
                linetype = 0) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 0.5, ymax = 3.5),
                fill = "#d9ed92",
                alpha = 0.02,
                color = "#d9ed92",
                linetype = 0) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 6.5, ymax = 7.5),
                fill = "#ff9b54",
                alpha = 0.02,
                color = "#ff9b54",
                linetype = 0) +
      geom_point(aes(x = exp_coef, shape = Outcome), size = 2) +
      geom_linerange(aes(xmin = lower_ci, xmax = upper_ci)) +
      xlab("Hazard Ratio")
  }
}


# COX: windows ----
y_breaks <- c(7, 5.5, 4.5, 3, 2, 1)
strata_level <- c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3")
xlim <- c(0, 10)
for (cov in covid) {
  for (fup in followup) {
    gg_data <- risk |>
      filter(variable_name == "study",
             regression == "cox",
             outcome %in% c("covid"),
             window %in% c("0_10", "11_90", "91_Inf"),
             covid_definition == cov,
             delivery_excluded == "yes" | delivery_excluded == "-",
             followup_end == fup, exposed_censoring == cens) |>
      mutate(strata_level = factor(strata_level,
                                   levels = c("overall", "pfizer", "moderna", "T1", "T2", "T3"),
                                   label = c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3"))
      ) %>%
      inner_join(tibble(y_breaks = y_breaks, strata_level = strata_level))  %>%
      mutate(Strata = factor(strata_name,
                             levels = c("overall", "vaccine_brand", "trimester"),
                             labels = c("Overall", "Vaccine brand", "Trimester")),
             Outcome = factor(outcome,
                              levels = c("covid"),
                              labels = c("SARS-CoV-2 infection")),
             window = factor(window,
                             levels = c("0_10", "11_90", "91_Inf"),
                             labels = c("0 to 10 days", "11 to 90 days", "More than 90 days")),
             comparison = factor(comparison,
                                 levels = c("none_first", "complete_booster"),
                                 labels = c("1st dose vs. Unvaccinated", "3rd dose vs. 2nd dose"))) %>%
      filter(!(exp_coef < 0.1 & abs(lower_ci-upper_ci) > 1))
    gg_min <- min(gg_data$lower_ci)
    gg_max <- max(gg_data$upper_ci)
    gg <- gg_data %>%
      ggplot(aes(y = y_breaks, color = Strata)) +
      geom_point(aes(x = exp_coef, shape = Outcome), size = 4) +
      geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), size = 1) +
      scale_y_continuous(breaks = y_breaks, labels = strata_level,
                         limits = c(0.5, max(y_breaks) + 0.5) ) +
      scale_x_continuous(limits = c(0.25, 2), breaks = c(0.25, 0.5, 1, 2), trans = "log",
                         oob=scales::rescale_none) +
      geom_vline(xintercept = 1) +
      ggplot2::theme_bw() +
      theme(
        axis.title.y = element_blank()
      ) +
      scale_color_manual(values = c("#FF7F0FFF", "#3CB7CCFF","#32A251FF",  "#ACD98DFF", "#B85A0DFF", "#B85A0DFF")) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 4, ymax = 6),
                fill = "#a6e1fa",
                alpha = 0.02,
                color = "#a6e1fa",
                linetype = 0) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 0.5, ymax = 3.5),
                fill = "#d9ed92",
                alpha = 0.02,
                color = "#d9ed92",
                linetype = 0) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 6.5, ymax = 7.5),
                fill = "#ff9b54",
                alpha = 0.02,
                color = "#ff9b54",
                linetype = 0) +
      geom_point(aes(x = exp_coef, shape = Outcome), size = 2) +
      geom_linerange(aes(xmin = lower_ci, xmax = upper_ci)) +
      facet_grid(vars(comparison), vars(window)) +
      xlab("Hazard Ratio") +
      theme(
        strip.text = element_text(size = 15),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        panel.spacing = unit(2, "lines")
      )

    ggsave(
      paste0(cov, "_", fup, "_", ".png"),
      plot = gg,
      path = here::here("figures", "cox-windows"),
      scale = 1,
      width = 3700,
      height = 2000,
      units = "px",
      dpi = 300
    )
  }
}

for (cov in covid) {
  for (fup in followup) {
    gg_data <- risk |>
      filter(variable_name == "study",
             regression == "cox",
             outcome %in% c("covid"),
             window %in% c("0_10", "11_27", "28_90", "91_150", "150_Inf"),
             covid_definition == cov,
             delivery_excluded == "yes" | delivery_excluded == "-",
             followup_end == fup, exposed_censoring == cens) |>
      mutate(strata_level = factor(strata_level,
                                   levels = c("overall", "pfizer", "moderna", "T1", "T2", "T3"),
                                   label = c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3"))
      ) %>%
      inner_join(tibble(y_breaks = y_breaks, strata_level = strata_level))  %>%
      mutate(Strata = factor(strata_name,
                             levels = c("overall", "vaccine_brand", "trimester"),
                             labels = c("Overall", "Vaccine brand", "Trimester")),
             Outcome = factor(outcome,
                              levels = c("covid"),
                              labels = c("SARS-CoV-2 infection")),
             window = factor(window,
                             levels = c("0_10", "11_27", "28_90", "91_150", "150_Inf"),
                             labels = c("0_10", "11_27", "28_90", "91_150", "150_Inf")),
             comparison = factor(comparison,
                                 levels = c("none_first", "complete_booster"),
                                 labels = c("1st dose vs. Unvaccinated", "3rd dose vs. 2nd dose"))) %>%
      filter(!(exp_coef < 0.1 & abs(lower_ci-upper_ci) > 1))
    gg_min <- min(gg_data$lower_ci)
    gg_max <- max(gg_data$upper_ci)
    gg <- gg_data %>%
      ggplot(aes(y = y_breaks, color = Strata)) +
      geom_point(aes(x = exp_coef, shape = Outcome), size = 2) +
      geom_linerange(aes(xmin = lower_ci, xmax = upper_ci)) +
      scale_y_continuous(breaks = y_breaks, labels = strata_level,
                         limits = c(0.5, max(y_breaks) + 0.5) ) +
      scale_x_continuous(limits = c(0.25, 2), breaks = c(0.25, 0.5, 1, 2), trans = "log",
                         oob=scales::rescale_none) +
      geom_vline(xintercept = 1) +
      ggplot2::theme_bw() +
      theme(
        axis.title.y = element_blank()
      ) +
      scale_color_manual(values = c("#FF7F0FFF", "#3CB7CCFF","#32A251FF",  "#ACD98DFF", "#B85A0DFF", "#B85A0DFF")) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 4, ymax = 6),
                fill = "#a6e1fa",
                alpha = 0.02,
                color = "#a6e1fa",
                linetype = 0) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 0.5, ymax = 3.5),
                fill = "#d9ed92",
                alpha = 0.02,
                color = "#d9ed92",
                linetype = 0) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 6.5, ymax = 7.5),
                fill = "#ff9b54",
                alpha = 0.02,
                color = "#ff9b54",
                linetype = 0) +
      geom_point(aes(x = exp_coef, shape = Outcome), size = 2) +
      geom_linerange(aes(xmin = lower_ci, xmax = upper_ci)) +
      facet_grid(vars(comparison), vars(window)) +
      xlab("Hazard Ratio")

    ggsave(
      paste0(cov, "_", fup, "_", ".png"),
      plot = gg,
      path = here::here("figures", "cox-windows"),
      scale = 1,
      width = width,
      height = height,
      units = "px",
      dpi = 300
    )
  }
}

# Outcome counts ----
# for (comp in comparisons) {
for (cov in covid) {
  for (fup in followup) {
    for (strata in stratifications) {
      data <- survival_summary %>%
        filter(covid_definition == cov,
               delivery_excluded == "yes" | delivery_excluded == "-",
               followup_end == fup, exposed_censoring == cens, tolower(strata_name) == strata) |>
        splitStrata()

      if (strata == "overall") {
        data <- data |>
          select(all_of(c("Comparison" = "comparison", "Outcome" = "outcome", "exposed", "Subjects, N" = "count", "Events, N(%)" = "count_events"))) |>
          mutate(Outcome = factor(Outcome,
                                  levels = c("covid", "inpatient_covid", "icu_covid", "death_covid"),
                                  labels = c("SARS-CoV-2 infection", "COVID-19 hospitalisation", "COVID-19 ICU admission", "COVID-19 death"))) |>
          arrange(Outcome)
      } else if (strata == "trimester") {
        data <- data |>
          mutate(trimester = factor(trimester, levels = c("T1", "T2", "T3"), labels = c("Trimester 1", "Trimester 2", "Trimester 3")),
                 outcome = factor(outcome,
                                  levels = c("covid", "inpatient_covid", "icu_covid", "death_covid"),
                                  labels = c("SARS-CoV-2 infection", "COVID-19 hospitalisation", "COVID-19 ICU admission", "COVID-19 death"))) |>
          arrange(outcome, trimester) |>
          select("Comparison" = "comparison", "Outcome" = "outcome", "Trimester" = "trimester", "exposed", "Subjects, N" = "count", "Events, N(%)" = "count_events")
      } else if (strata == "vaccine_brand") {
        data <- data |>
          mutate(vaccine_brand = factor(vaccine_brand, levels = c("pfizer", "moderna"), labels = c("Pfizer", "Moderna")),
                 outcome = factor(outcome,
                                  levels = c("covid", "inpatient_covid", "icu_covid", "death_covid"),
                                  labels = c("SARS-CoV-2 infection", "COVID-19 hospitalisation", "COVID-19 ICU admission", "COVID-19 death"))) |>
          arrange(outcome, vaccine_brand) |>
          select("Comparison" = "comparison", "Outcome" = "outcome", "Vaccine brand" = "vaccine_brand", "exposed", "Subjects, N" = "count", "Events, N(%)" = "count_events")
      }

      data <- data |>
        pivot_longer(cols = c("Events, N(%)"), names_to = "estimate_name", values_to = "estimate_value")  |>
        mutate(
          "Subjects, N" = niceNum(as.numeric(gsub(",", "", .data[["Subjects, N"]]))*2)
        )

      if (strata == "overall") {
        subjects <- data |>
          select(Comparison, Outcome, exposed, "Subjects, N")
      } else if (strata == "trimester") {
        subjects <- data |>
          select(Comparison, Outcome, Trimester, exposed, "Subjects, N")
      } else if (strata == "vaccine_brand") {
        subjects <- data |>
          select(Comparison, Outcome, "Vaccine brand", exposed, "Subjects, N")
      }

      data |>
        select(!"Subjects, N") |>
        formatHeader(header = c("estimate_name", "exposed"), includeHeaderName = FALSE) |>
        left_join(subjects |>
                    filter(exposed == "Exposed") |>
                    select(!exposed)) |>
        mutate(Comparison = factor(Comparison,
                                   levels = c("none_first", "complete_booster"),
                                   labels = c("1st dose vs. Unvaccinated", "3rd dose vs. 2nd dose"))) |>
        relocate("Subjects, N", .before = "[header_level]Events, N(%)\n[header_level]Unexposed") |>
        gtTable(style = list(
          "header" = list(gt::cell_fill(color = "#e1e1e1"),
                          gt::cell_text(weight = "bold"),
                          gt::cell_borders(color = "#D3D3D3", weight = px(1))),
          "header_name" = list(gt::cell_fill(color = "#e1e1e1"),
                               gt::cell_text(weight = "bold")),
          "header_level" = list(gt::cell_fill(color = "#e1e1e1"),
                                gt::cell_text(weight = "bold")),
          "column_name" = list(gt::cell_fill(color = "#e1e1e1"),
                               gt::cell_text(weight = "bold")),
          "group_label" = list(gt::cell_fill(color = "#f2f2f2"), gt::cell_text(weight = "bold")),
          "body" = list(gt::cell_borders(color = "#D3D3D3", sides = c("left", "right")))
        ),
        colsToMergeRows = "all_columns",
        groupNameCol = "Comparison") |>
        gtsave(filename = paste0(cov, "_", fup, "_", strata, ".png"), path = here::here("figures", "outcome_counts"), expand = 50)
    }
  }
}
# }

# Table 1 ----
for (comp in comparisons) {
  for (cov in covid) {
    for (strata in stratifications) {
      if (strata != "overall") {
        variables <- c(
          "Number records", "Age", "Age group", "Vaccine brand", "Trimester", "Number prior pregnancies", "Number visits prior year",
          "Conditions any time prior", "Drugs prior 180 days"
        )

        exclude <- c("Immunosupressants", "Corticosteroids", "Antiacids", "Antiseptics and desinfectants", "Drugs ulcer and gords",
                     "Obstructive respiratory diseases", "Pregnancy induced hypertension", "Epilepsy", "Hiv", "Cerebrovascular disease",
                     "Bronchiectasis", "Hematological malignancies", "Interstitial lung disease", "Stroke", "Pulmonary hypertension",
                     "Diabetes mellitus", "Chronic liver disease", "Renal impairment", "Venous thromboembolism", "Pcos",
                     "Schizophrenia spectrum disorder", "Pulmonary embolism")

        data <- baseline %>%
          filter(covid_definition == cov, comparison == comp, tolower(strata_name) == strata) |>
          filter(variable_name %in% variables,
                 !estimate_name %in% c("min", "max", "q05", "q95", "mean", "sd"),
                 !variable_level %in% exclude) |>
          mutate(
            variable_level = case_when(
              variable_name == "Number prior pregnancies" ~ NA,
              variable_level %in% c("T1", "T2", "T3") ~ paste0("Trimester ", gsub("T", "", variable_level)),
              .default = variable_level
            ),
            variable_name = factor(variable_name,
                                   level = c("Number records", "Age", "Age group",
                                             "Vaccine brand", "Trimester", "Number visits prior year",
                                             "Number prior pregnancies", "Conditions any time prior",
                                             "Drugs prior 180 days")),
            additional_level = if_else(additional_level == "0", "Unexposed", "Exposed"),

          ) |>
          arrange(variable_name) |>
          select(omopgenerics::resultColumns())

        data |>
          PatientProfiles::tableCharacteristics(
            header = c("strata", "additional"),
            excludeColumns = c("result_id", "result_type", "package_name", "package_version", "estimate_type", "cdm_name", "cohort_name")) |>
          cols_width(everything() ~ px(100), `Variable name` ~ 150, `Variable level` ~ 200) |>
          gtsave(filename = paste0(comp, "_", cov, "_", strata, ".docx"), path = here::here("figures", "baseline"), vwidth = 2000, expand = 50)
      }
    }
  }
}


for (cov in covid) {
  variables <- c(
    "Number records", "Age", "Age group", "Vaccine brand", "Trimester", "Number prior pregnancies", "Number visits prior year",
    "Conditions any time prior", "Drugs prior 180 days"
  )

  exclude <- c("Immunosupressants", "Corticosteroids", "Antiacids", "Antiseptics and desinfectants", "Drugs ulcer and gords",
               "Obstructive respiratory diseases", "Pregnancy induced hypertension", "Epilepsy", "Hiv", "Cerebrovascular disease",
               "Bronchiectasis", "Hematological malignancies", "Interstitial lung disease", "Stroke", "Pulmonary hypertension",
               "Diabetes mellitus", "Chronic liver disease", "Renal impairment", "Venous thromboembolism", "Pcos",
               "Schizophrenia spectrum disorder", "Pulmonary embolism")

  data <- baseline %>%
    filter(covid_definition == cov, tolower(strata_name) == "overall") |>
    filter(variable_name %in% variables,
           !estimate_name %in% c("min", "max", "q05", "q95", "mean", "sd"),
           !variable_level %in% exclude) |>
    mutate(
      variable_level = case_when(
        variable_name == "Number prior pregnancies" ~ NA,
        variable_level %in% c("T1", "T2", "T3") ~ paste0("Trimester ", gsub("T", "", variable_level)),
        .default = variable_level
      ),
      variable_name = factor(variable_name,
                             level = c("Number records", "Age", "Age group",
                                       "Vaccine brand", "Trimester", "Number visits prior year",
                                       "Number prior pregnancies", "Conditions any time prior",
                                       "Drugs prior 180 days")),
      additional_level = if_else(additional_level == "0", "Unexposed", "Exposed"),
      additional_name = if_else(grepl("none_first", comparison), "1st dose vs. Unvaccinated", "3rd dose vs. 2nd dose")
    ) |>
    arrange(variable_name) |>
    select(omopgenerics::resultColumns())

  data |>
    arrange(additional_name) |>
    PatientProfiles::tableCharacteristics(
      header = c("strata", "additional"),
      excludeColumns = c("result_id", "result_type", "package_name", "package_version", "estimate_type", "cdm_name", "cohort_name")) |>
    cols_width(everything() ~ px(100), `Variable name` ~ 150, `Variable level` ~ 200) |>
    gtsave(filename = paste0("both_", cov, "_overall.docx"), path = here::here("figures", "baseline"), vwidth = 2000, expand = 50)
}

## Cohort counts ----
population_count |>
  filter(covid_definition == "diagnostic_test") |>
  mutate(
    comparison = factor(comparison,
                        levels = c("none_first", "complete_booster"),
                        labels = c("1st dose vs. Unvaccinated", "3rd dose vs. 2nd dose")),
    strata_level = factor(strata_level,
                          levels = c("overall", "pfizer", "moderna", "T1", "T2", "T3"),
                          label = c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3")),
    strata_name = factor(strata_name,
                         levels = c("overall", "vaccine_brand", "trimester"),
                         labels = c("Overall", "Vaccine brand", "Trimester")),
    total = niceNum(total),
    num_control = niceNum(num_control),
    num_exposed = niceNum(num_exposed)
  ) |>
  arrange(strata_level) |>
  select("comparison", "Strata name" = "strata_name", "Strata value" = "strata_level",
         "Total pregnancies (N)" = "total", "Exposed (N)" = "num_control",
         "Controls (N)" = "num_exposed") |>
  gtTable(groupNameCol = "comparison",
          style = list(
            "header" = list(gt::cell_fill(color = "#e1e1e1"),
                            gt::cell_text(weight = "bold")),
            "header_name" = list(gt::cell_fill(color = "#e1e1e1"),
                                 gt::cell_text(weight = "bold")),
            "header_level" = list(gt::cell_fill(color = "#e1e1e1"),
                                  gt::cell_text(weight = "bold")),
            "column_name" = list(gt::cell_fill(color = "#e1e1e1"),
                                 gt::cell_text(weight = "bold")),
            "group_label" = list(gt::cell_fill(color = "#f2f2f2"), gt::cell_text(weight = "bold")),
            "body" = list(gt::cell_borders(color = "#D3D3D3", sides = c("left", "right")))
          ),
          colsToMergeRows = "all_columns") |>
  gtsave(filename = paste0("main_population_counts.png"), path = here::here("figures", "counts"), vwidth = 1500, expand = 50)
