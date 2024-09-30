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


# load functions
source(here("functions.R"))

# load data
load(here("shinyData.Rdata"))

# counts ----
gtTab <- data$population_count |>
  filter(strata_level == "overall") |>
  filter(cdm_name %in% c("SIDIAP", "UiO", "CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  mutate(
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccination schema vs. Unvaccinated", "3 vaccine doses vs. 2 vaccine doses")
    ),
    cdm_name = factor(
      cdm_name,
      level = c("SIDIAP", "UiO", "CPRD Gold"),
      labels = c("SIDIAP", "UiO", "CPRD Gold")
    ),
    num = niceNum(num_control + num_exposed)
  ) |>
  arrange(cdm_name, comparison) |>
  select(
    "Database name" = "cdm_name", "Comparison" = "comparison",
    "N" = "num"
  ) |>
  pivot_wider(names_from = Comparison, values_from = N) |>
  gtTable(groupNameCol = "Database name", groupNameAsColumn = TRUE)
gtTab |>
  gtsave(filename = "icpeCounts.png", path = here::here("figures", "icpe"), vwidth = 1500, expand = 50)


# asmd ----
data$smd |>
  filter(strata_level == "overall") |>
  filter(cdm_name %in% c("SIDIAP", "UiO", "CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  mutate(
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccine schema vs.\n Unvaccinated", "3 vaccine doses vs. \n 2 vaccine doses")
    ),
    balance = case_when(
      asmd <= 0.2 ~ "ASMD $\\leq$ 0.2",
      asmd > 0.2 ~ "ASMD > 0.2",
      # .default = "0.1 < ASMD $\\leq$ 0.2"
    ),
    cdm_name = factor(
      cdm_name,
      level = c("SIDIAP", "UiO", "CPRD Gold"),
      labels = c("SIDIAP", "UiO", "CPRD Gold")
    ),
    asmd = abs(as.numeric(smd))
  ) |>
  arrange(cdm_name, comparison) |>
  ggplot(aes(x = asmd, color = balance, fill = balance)) +
  geom_histogram(binwidth = 0.01) +
  facet_grid(cols = vars(cdm_name), rows = vars(comparison)) +
  scale_color_manual(values = c("#52796f", "#e57c04"),
                     labels = c(TeX("ASMD \\leq 0.2"), TeX("ASMD > 0.2"))) +
  scale_fill_manual(values = c("#84a98c", "#f6ae2d"),
                    labels = c(TeX("ASMD \\leq 0.2"), TeX("ASMD > 0.2"))) +
  scale_x_continuous(limits = c(0, 0.4)) +
  # scale_y_continuous(limits = c(0, 400)) +
  geom_hline(yintercept = 0, color = "white") +
  # geom_vline(xintercept = 0.1, linetype = 'dashed') +
  geom_vline(xintercept = 0.2, linetype = 'dashed') +
  ylab("Counts") +
  xlab("ASMD") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.spacing = unit(1, "lines")
  )
ggsave(
  "icpeASMD.png",
  plot = last_plot(),
  path = here::here("figures", "icpe"),
  scale = 1,
  width = 2500,
  height = 1400,
  units = "px",
  dpi = 300
)

# NCO ----
dataNCO <- data$risk %>%
  filter(variable_name == "nco") %>%
  filter(strata_level == "overall") |>
  filter(cdm_name %in% c("SIDIAP", "UiO", "CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  filter(followup_end == "cohort_end_date") |>
  filter(estimate_name %in% c("se_coef", "coef")) |>
  mutate(
    cdm_name = factor(
      cdm_name,
      level = c("SIDIAP", "UiO", "CPRD Gold"),
      labels = c("SIDIAP", "UiO", "CPRD Gold")
    ),
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccine schema vs.\n Unvaccinated", "3 vaccine doses vs. \n 2 vaccine doses")
    )
  ) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  mutate(
    logRr = coef, seLogRr = se_coef, trueRr = 0
  )
breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
themeRA <- ggplot2::element_text(colour = "#000000", size = 10, hjust = 1)

ggplot2::ggplot(dataNCO, ggplot2::aes(x = .data$logRr, y = .data$seLogRr)) +
  ggplot2::geom_area(
    fill = "#cce3de",
    color = "#cce3de",
    size = 1,
    alpha = 0.5,
    data = tibble(logRr = 0:1000, seLogRr =  -(0:1000)/qnorm(0.025))) +
  ggplot2::geom_area(
    fill = "#cce3de",
    color = "#cce3de",
    size = 1,
    alpha = 0.5,
    data = tibble(logRr = -(0.001:100), seLogRr =  -(0.001:100)/qnorm(0.025))) +
  # ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$trueRr)) / qnorm(0.025), slope = 1 / qnorm(0.025)), colour = rgb(0, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dataNCO) +
  # ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$trueRr)) / qnorm(0.975), slope = 1 / qnorm(0.975)), colour = rgb(0, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dataNCO) +
  ggplot2::geom_vline(xintercept = log(breaks), colour = "#dddddd", lty = 1, size = 0.5) +
  ggplot2::geom_point(
    shape = 16,
    size = 1,
    alpha = 0.5,
    color = rgb(0, 0, 0.8)
  ) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::scale_x_continuous("Hazard ratio", limits = log(c(0.1, 10)), breaks = log(breaks), labels = breaks) +
  ggplot2::scale_y_continuous("Standard Error") +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  facet_grid(cols = vars(cdm_name), rows = vars(comparison)) +
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  )

ggsave(
  "icpeNCO.png",
  plot = last_plot(),
  path = here::here("figures", "icpe"),
  scale = 1,
  width = 2000,
  height = 1300,#2000,
  units = "px",
  dpi = 300
)

# forest end ----
windows <- c("0_7", "8_90", "91_180", "181_365", "366_Inf")
x_breaks = rev(1:length(windows))
dataHR <- data$risk %>%
  filter(variable_name == "study") %>%
  filter(strata_level == "overall") |>
  filter(cdm_name %in% c("Meta-analysis")) |>
  # filter(cdm_name %in% c("CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  filter(followup_end == "cohort_end_date") |>
  # filter(followup_end == "pregnancy_end_date") |>
  filter(regression %in% c("cox")) |>
  filter(outcome %in% c("covid", "inpatient_covid")) |>
  filter(delivery_excluded %in% c("-", "yes")) |>
  # filter(delivery_excluded %in% c("-", "no")) |>
  filter(window %in% windows) |>
  inner_join(tibble(x_breaks = x_breaks, window = windows))  |>
  mutate(
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccine schema vs.\n Unvaccinated", "3 vaccine doses vs. \n 2 vaccine doses")
    ),
    # cdm_name = factor(
    #   cdm_name,
    #   level = c("SIDIAP", "UiO", "CPRD Gold"),
    #   labels = c("SIDIAP", "UiO", "CPRD Gold")
    # ),
    window = factor(
      window,
      levels = windows,
      labels = paste0(gsub("_", " to ", c("0_7", "8_90", "91_180", "181_365", "> 365")), " days")
    ),
    Outcome = factor(outcome,
                     levels = c("covid", "inpatient_covid"),
                     labels = c("SARS-CoV-2 infection", "COVID-19 related hospitalisation")),
    x_breaks = if_else(outcome == "inpatient_covid", x_breaks-0.15, x_breaks+0.15),
    "Database name" = cdm_name
  ) |>
  arrange(cdm_name, comparison, window) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

dataHR |>
  ggplot(aes(y = x_breaks, color = Outcome)) +
  geom_point(aes(x = exp_coef), size = 3) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), size = 1) +
  scale_y_continuous(breaks = x_breaks, labels = dataHR$window |> levels(),
                     limits = c(0.5, max(dataHR$x_breaks) + 0.5) ) +
  scale_x_continuous(limits = c(0.1, 4), breaks = c(0.1, 0.25, 0.5, 1, 2, 4), trans = "log",
                     oob=scales::rescale_none) +
  geom_vline(xintercept = 1) +
  facet_grid(cols = vars(comparison)) +
  scale_color_manual(values = c("#0d3b66", "#87b38d", "#ACD98DFF", "#B85A0DFF", "#B85A0DFF")) +
  ggplot2::theme_bw() +
  theme(
    # axis.title.y = element_blank(),
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Time-windows") +
  xlab("Hazard Ratios")

ggsave(
  "icpeHR.png",
  plot = last_plot(),
  path = here::here("figures", "icpe"),
  scale = 1,
  width = 3000,
  height = 1500,#2000,
  units = "px",
  dpi = 300
)

# pregnancy end forest ----
windows <- c("0_7", "8_90", "91_180", "181_365", "366_Inf")
x_breaks = rev(1:length(windows))
dataHR <- data$risk %>%
  filter(variable_name == "study") %>%
  filter(strata_level == "overall") |>
  filter(cdm_name %in% c("Meta-analysis")) |>
  # filter(cdm_name %in% c("CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  # filter(followup_end == "cohort_end_date") |>
  filter(followup_end == "pregnancy_end_date") |>
  filter(regression %in% c("cox")) |>
  filter(outcome %in% c("covid", "inpatient_covid")) |>
  filter(delivery_excluded %in% c("-", "yes")) |>
  # filter(delivery_excluded %in% c("-", "no")) |>
  filter(window %in% windows) |>
  inner_join(tibble(x_breaks = x_breaks, window = windows))  |>
  mutate(
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccine schema vs.\n Unvaccinated", "3 vaccine doses vs. \n 2 vaccine doses")
    ),
    # cdm_name = factor(
    #   cdm_name,
    #   level = c("SIDIAP", "UiO", "CPRD Gold"),
    #   labels = c("SIDIAP", "UiO", "CPRD Gold")
    # ),
    window = factor(
      window,
      levels = windows,
      labels = paste0(gsub("_", " to ", c("0_7", "8_90", "91_180", "181_365", "> 365")), " days")
    ),
    Outcome = factor(outcome,
                     levels = c("covid", "inpatient_covid"),
                     labels = c("SARS-CoV-2 infection", "COVID-19 related hospitalisation")),
    x_breaks = if_else(outcome == "inpatient_covid", x_breaks-0.15, x_breaks+0.15),
    "Database name" = cdm_name
  ) |>
  arrange(cdm_name, comparison, window) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

dataHR |>
  ggplot(aes(y = x_breaks, color = Outcome)) +
  geom_point(aes(x = exp_coef), size = 3) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), size = 1) +
  scale_y_continuous(breaks = x_breaks, labels = dataHR$window |> levels(),
                     limits = c(0.5, max(dataHR$x_breaks) + 0.5) ) +
  scale_x_continuous(limits = c(0.1, 4), breaks = c(0.1, 0.25, 0.5, 1, 2, 4), trans = "log",
                     oob=scales::rescale_none) +
  geom_vline(xintercept = 1) +
  facet_grid(cols = vars(comparison)) +
  scale_color_manual(values = c("#0d3b66", "#87b38d", "#ACD98DFF", "#B85A0DFF", "#B85A0DFF")) +
  ggplot2::theme_bw() +
  theme(
    # axis.title.y = element_blank(),
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Time-windows") +
  xlab("Hazard Ratios")

ggsave(
  "icpeHR_pregnancy_end.png",
  plot = last_plot(),
  path = here::here("figures", "icpe"),
  scale = 1,
  width = 3000,
  height = 1500,#2000,
  units = "px",
  dpi = 300
)

# across databases covid ----
windows <- c("0_7", "8_90", "91_180", "181_365", "366_Inf")
x_breaks = rev(1:length(windows))
dataHR <- data$risk %>%
  filter(variable_name == "study") %>%
  filter(strata_level == "overall") |>
  filter(cdm_name != c("Meta-analysis")) |>
  # filter(cdm_name %in% c("CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  filter(followup_end == "cohort_end_date") |>
  # filter(followup_end == "pregnancy_end_date") |>
  filter(regression %in% c("cox")) |>
  filter(outcome %in% c("covid")) |>
  filter(delivery_excluded %in% c("-", "yes")) |>
  # filter(delivery_excluded %in% c("-", "no")) |>
  filter(window %in% windows) |>
  inner_join(tibble(x_breaks = x_breaks, window = windows))  |>
  mutate(
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccine schema vs.\n Unvaccinated", "3 vaccine doses vs. \n 2 vaccine doses")
    ),
    cdm_name = factor(
      cdm_name,
      level = c("SIDIAP", "UiO", "CPRD Gold"),
      labels = c("SIDIAP", "UiO", "CPRD Gold")
    ),
    window = factor(
      window,
      levels = windows,
      labels = paste0(gsub("_", " to ", c("0_7", "8_90", "91_180", "181_365", "> 365")), " days")
    ),
    Outcome = factor(outcome,
                     levels = c("covid", "inpatient_covid"),
                     labels = c("SARS-CoV-2 infection", "COVID-19 related hospitalisation")),
    x_breaks = case_when(
      cdm_name == "SIDIAP" ~ x_breaks+0.15,
      cdm_name == "CPRD Gold" ~ x_breaks-0.15,
      cdm_name == "UiO" ~ x_breaks,
      ),
    "Database name" = cdm_name
  ) |>
  arrange(cdm_name, comparison, window) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

dataHR |>
  ggplot(aes(y = x_breaks, color = cdm_name)) +
  geom_point(aes(x = exp_coef), size = 3) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), size = 1) +
  scale_y_continuous(breaks = x_breaks, labels = dataHR$window |> levels(),
                     limits = c(0.5, max(dataHR$x_breaks) + 0.5) ) +
  scale_x_continuous(limits = c(0.1, 4), breaks = c(0.1, 0.25, 0.5, 1, 2, 4), trans = "log",
                     oob=scales::rescale_none) +
  geom_vline(xintercept = 1) +
  facet_grid(cols = vars(comparison)) +
  scale_color_manual(values = c("#141b41", "#306bac", "#586f7c", "#B85A0DFF")) +
  ggplot2::theme_bw() +
  theme(
    # axis.title.y = element_blank(),
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Time-windows") +
  xlab("Hazard Ratios") +
  guides(fill = guide_legend(title = "Database name"))

ggsave(
  "icpeHR_infection.png",
  plot = last_plot(),
  path = here::here("figures", "icpe"),
  scale = 1,
  width = 3000,
  height = 1500,#2000,
  units = "px",
  dpi = 300
)

# across databases inpatient ----
windows <- c("0_7", "8_90", "91_180", "181_365", "366_Inf")
x_breaks = rev(1:length(windows))
dataHR <- data$risk %>%
  filter(variable_name == "study") %>%
  filter(strata_level == "overall") |>
  filter(!cdm_name %in% c("Meta-analysis", "CPRD Gold")) |>
  # filter(cdm_name %in% c("CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  filter(followup_end == "cohort_end_date") |>
  # filter(followup_end == "pregnancy_end_date") |>
  filter(regression %in% c("cox")) |>
  filter(outcome %in% c("inpatient_covid")) |>
  filter(delivery_excluded %in% c("-", "yes")) |>
  # filter(delivery_excluded %in% c("-", "no")) |>
  filter(window %in% windows) |>
  inner_join(tibble(x_breaks = x_breaks, window = windows))  |>
  mutate(
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccine schema vs.\n Unvaccinated", "3 vaccine doses vs. \n 2 vaccine doses")
    ),
    cdm_name = factor(
      cdm_name,
      level = c("SIDIAP", "UiO", "CPRD Gold"),
      labels = c("SIDIAP", "UiO", "CPRD Gold")
    ),
    window = factor(
      window,
      levels = windows,
      labels = paste0(gsub("_", " to ", c("0_7", "8_90", "91_180", "181_365", "> 365")), " days")
    ),
    Outcome = factor(outcome,
                     levels = c("covid", "inpatient_covid"),
                     labels = c("SARS-CoV-2 infection", "COVID-19 related hospitalisation")),
    x_breaks = case_when(
      cdm_name == "SIDIAP" ~ x_breaks+0.15,
      cdm_name == "CPRD Gold" ~ x_breaks,
      cdm_name == "UiO" ~ x_breaks-0.15,
    ),
    "Database name" = cdm_name
  ) |>
  arrange(cdm_name, comparison, window) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

dataHR |>
  ggplot(aes(y = x_breaks, color = cdm_name)) +
  geom_point(aes(x = exp_coef), size = 3) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), size = 1) +
  scale_y_continuous(breaks = x_breaks, labels = dataHR$window |> levels(),
                     limits = c(0.5, max(dataHR$x_breaks) + 0.5) ) +
  scale_x_continuous(limits = c(0.1, 4), breaks = c(0.1, 0.25, 0.5, 1, 2, 4), trans = "log",
                     oob=scales::rescale_none) +
  geom_vline(xintercept = 1) +
  facet_grid(cols = vars(comparison)) +
  scale_color_manual(values = c("#141b41", "#306bac", "#B85A0DFF", "#B85A0DFF")) +
  ggplot2::theme_bw() +
  theme(
    # axis.title.y = element_blank(),
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Time-windows") +
  xlab("Hazard Ratios") +
  guides(fill = guide_legend(title = "Database name"))

ggsave(
  "icpeHR_hosp.png",
  plot = last_plot(),
  path = here::here("figures", "icpe"),
  scale = 1,
  width = 3000,
  height = 1500,#2000,
  units = "px",
  dpi = 300
)

# pregnancy end across databases covid ----
windows <- c("0_7", "8_90", "91_180", "181_365", "366_Inf")
x_breaks = rev(1:length(windows))
dataHR <- data$risk %>%
  filter(variable_name == "study") %>%
  filter(strata_level == "overall") |>
  filter(cdm_name != c("Meta-analysis")) |>
  # filter(cdm_name %in% c("CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  # filter(followup_end == "cohort_end_date") |>
  filter(followup_end == "pregnancy_end_date") |>
  filter(regression %in% c("cox")) |>
  filter(outcome %in% c("covid")) |>
  filter(delivery_excluded %in% c("-", "yes")) |>
  # filter(delivery_excluded %in% c("-", "no")) |>
  filter(window %in% windows) |>
  inner_join(tibble(x_breaks = x_breaks, window = windows))  |>
  mutate(
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccine schema vs.\n Unvaccinated", "3 vaccine doses vs. \n 2 vaccine doses")
    ),
    cdm_name = factor(
      cdm_name,
      level = c("SIDIAP", "UiO", "CPRD Gold"),
      labels = c("SIDIAP", "UiO", "CPRD Gold")
    ),
    window = factor(
      window,
      levels = windows,
      labels = paste0(gsub("_", " to ", c("0_7", "8_90", "91_180", "181_365", "> 365")), " days")
    ),
    Outcome = factor(outcome,
                     levels = c("covid", "inpatient_covid"),
                     labels = c("SARS-CoV-2 infection", "COVID-19 related hospitalisation")),
    x_breaks = case_when(
      cdm_name == "SIDIAP" ~ x_breaks+0.15,
      cdm_name == "CPRD Gold" ~ x_breaks-0.15,
      cdm_name == "UiO" ~ x_breaks,
    ),
    "Database name" = cdm_name
  ) |>
  arrange(cdm_name, comparison, window) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

dataHR |>
  ggplot(aes(y = x_breaks, color = cdm_name)) +
  geom_point(aes(x = exp_coef), size = 3) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), size = 1) +
  scale_y_continuous(breaks = x_breaks, labels = dataHR$window |> levels(),
                     limits = c(0.5, max(dataHR$x_breaks) + 0.5) ) +
  scale_x_continuous(limits = c(0.1, 4), breaks = c(0.1, 0.25, 0.5, 1, 2, 4), trans = "log",
                     oob=scales::rescale_none) +
  geom_vline(xintercept = 1) +
  facet_grid(cols = vars(comparison)) +
  scale_color_manual(values = c("#141b41", "#306bac", "#586f7c", "#B85A0DFF")) +
  ggplot2::theme_bw() +
  theme(
    # axis.title.y = element_blank(),
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Time-windows") +
  xlab("Hazard Ratios") +
  guides(color = guide_legend(title = "Database name")) +
  ggtitle("SARS-CoV-2 infection")

ggsave(
  "icpeHR_infection_pregnancy_end.png",
  plot = last_plot(),
  path = here::here("figures", "icpe"),
  scale = 1,
  width = 3000,
  height = 1500,#2000,
  units = "px",
  dpi = 300
)

# across databases inpatient ----
windows <- c("0_7", "8_90", "91_180", "181_365", "366_Inf")
x_breaks = rev(1:length(windows))
dataHR <- data$risk %>%
  filter(variable_name == "study") %>%
  filter(strata_level == "overall") |>
  filter(!cdm_name %in% c("Meta-analysis", "CPRD Gold")) |>
  # filter(cdm_name %in% c("CPRD Gold")) |>
  filter(covid_definition == "diagnostic_test") |>
  # filter(followup_end == "cohort_end_date") |>
  filter(followup_end == "pregnancy_end_date") |>
  filter(regression %in% c("cox")) |>
  filter(outcome %in% c("inpatient_covid")) |>
  filter(delivery_excluded %in% c("-", "yes")) |>
  # filter(delivery_excluded %in% c("-", "no")) |>
  filter(window %in% windows) |>
  inner_join(tibble(x_breaks = x_breaks, window = windows))  |>
  mutate(
    comparison = factor(
      comparison,
      level = c("none_first", "complete_booster"),
      labels = c("Primary vaccine schema vs.\n Unvaccinated", "3 vaccine doses vs. \n 2 vaccine doses")
    ),
    cdm_name = factor(
      cdm_name,
      level = c("SIDIAP", "UiO", "CPRD Gold"),
      labels = c("SIDIAP", "UiO", "CPRD Gold")
    ),
    window = factor(
      window,
      levels = windows,
      labels = paste0(gsub("_", " to ", c("0_7", "8_90", "91_180", "181_365", "> 365")), " days")
    ),
    Outcome = factor(outcome,
                     levels = c("covid", "inpatient_covid"),
                     labels = c("SARS-CoV-2 infection", "COVID-19 related hospitalisation")),
    x_breaks = case_when(
      cdm_name == "SIDIAP" ~ x_breaks+0.15,
      cdm_name == "CPRD Gold" ~ x_breaks,
      cdm_name == "UiO" ~ x_breaks-0.15,
    ),
    "Database name" = cdm_name
  ) |>
  arrange(cdm_name, comparison, window) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")

dataHR |>
  ggplot(aes(y = x_breaks, color = cdm_name)) +
  geom_point(aes(x = exp_coef), size = 3) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), size = 1) +
  scale_y_continuous(breaks = x_breaks, labels = dataHR$window |> levels(),
                     limits = c(0.5, max(dataHR$x_breaks) + 0.5) ) +
  scale_x_continuous(limits = c(0.1, 4), breaks = c(0.1, 0.25, 0.5, 1, 2, 4), trans = "log",
                     oob=scales::rescale_none) +
  geom_vline(xintercept = 1) +
  facet_grid(cols = vars(comparison)) +
  scale_color_manual(values = c("#141b41", "#306bac", "#B85A0DFF", "#B85A0DFF")) +
  ggplot2::theme_bw() +
  theme(
    # axis.title.y = element_blank(),
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Time-windows") +
  xlab("Hazard Ratios") +
  guides(color = guide_legend(title = "Database name")) +
  ggtitle("COVID-19 related hospitalisation")

ggsave(
  "icpeHR_hosp_pregnancy_end.png",
  plot = last_plot(),
  path = here::here("figures", "icpe"),
  scale = 1,
  width = 3000,
  height = 1500,#2000,
  units = "px",
  dpi = 300
)





