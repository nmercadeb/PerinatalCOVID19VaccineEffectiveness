getId <- function(x, name) {
  settings(x) %>%
    filter(.data$cohort_name == name) %>%
    pull("cohort_definition_id")
}
