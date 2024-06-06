## Read data ----
readData <- function(path) {
  x <- list.files(path = path)
  csvFiles <- x[tools::file_ext(x) == "csv"]
  zipFiles <- x[tools::file_ext(x) == "zip"]
  tempfolder <- tempdir()
  data <- readFiles(file.path(path, csvFiles))
  for (file in zipFiles) {
    file <- file.path(path, file)
    fname = unzip(file, list=TRUE)$Name
    fname <- fname[tools::file_ext(fname) == "csv"]
    unzip(file, files=fname, exdir=tempfolder, overwrite=TRUE)
    files <- file.path(tempfolder, fname)
    data <- c(data, readFiles(files))
  }
  return(data)
}

readFiles <- function(files) {
  data <- list()
  for (file in files) {
    data[[file]] <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()))
  }
  names(data) <- basename(tools::file_path_sans_ext(names(data)))
  return(data)
}

mergeData <- function(data, patterns) {
  x <- list()
  for (pat in patterns) {
    x[[pat]] <- data[grepl(pat, names(data))] %>% dplyr::bind_rows() %>% distinct()
  }
  return(x)
}

## Shiny creation ----
selectors <- function(data, prefix, columns, multiple = TRUE, default = list()) {
  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col)
      if (!multiple) {
        x <- first(x)
      }
    }
    return(x)
  }
  choic <- function(col) {
    data[[col]] %>% unique() %>% sort()
  }
  purrr::map(columns, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choic(.),
    selected = def(.),
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}

plotSelectors <- function(prefix, choices, multiple = TRUE, default = list(), selectors = c("color", "facet_by")) {
  purrr::map(selectors, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choices,
    selected = default[[.]],
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}

reactiveSelectors <- function(data, prefix, columns, restrictions, input,
                              multiple = TRUE, default = list()) {
  if (length(restrictions) != length(columns)) {
    if (length(restrictions) == 1) {
      restrictions <- rep(restrictions, length(columns))
    } else {
      cli::cli_abort("Revise columns and restrictions arguments.")
    }
  }

  names(columns) <- restrictions

  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col, dict = columns, input = input)
      if (!multiple) {
        x <- first(x)
      }
    }
    return(x)
  }
  choic <- function(col, dict, input) {
    filterCol <- names(dict)[dict == col]
    return(sort(unique(data[[col]][data[[filterCol]] %in% input[[paste0(prefix, "_", filterCol)]]])))
  }
  renderUI({
    purrr::map(columns, ~ pickerInput(
      inputId = paste0(prefix, "_", .),
      label = stringr::str_to_sentence(gsub("_", " ", .)),
      choices = choic(., dict = columns, input = input),
      selected = def(.),
      options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
      multiple = multiple,
      inline = TRUE
    ))
  })
}

plotDownloadSelectors <- function(prefix,
                                  choices = list("device" = c("png", "jpeg", "tiff", "pdf"),
                                                 "width" = 1:50,
                                                 "height"= 1:50),
                                  multiple = FALSE,
                                  default = list("device" = "png", "width" = 10, "height" = 10),
                                  types = c("device", "width", "height")) {
  purrr::map(types, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choices[[.]],
    selected = default[[.]],
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}

serverPlotDownload <- function(prefix, name, plot, input) {
  downloadHandler(
    filename = function() {
      paste0(name, ".", input[[paste0(prefix, "_device")]])
    },
    content = function(file) {
      ggsave(file, plot,
             width = as.numeric(input[[paste0(prefix, "_width")]]),
             height = as.numeric(input[[paste0(prefix, "_height")]]))
    }
  )
}

serverGTDownload <- function(name, gt) {
  downloadHandler(
    filename = function() {
      paste0(name, ".docx")
    },
    content = function(file) {
      gtsave(data = gt,
             filename = file,
             vwidth = 400,
             vheight = 300)
    },
    contentType = "docx"
  )
}

serverCSVDownload <- function(name, table) {
  downloadHandler(
    filename = function() {
      paste0(name, ".csv")
    },
    content = function(file) {
      write_csv(table, file)
    }
  )
}

filterData <- function(data, prefix, input) {
  cols <- colnames(data)
  cols <- cols[paste0(prefix, "_", cols) %in% names(input)]
  for (col in cols) {
    data <- data %>%
      dplyr::filter(.data[[col]] %in% .env$input[[paste0(prefix, "_", col)]])
  }
  return(data)
}

niceChar <- function(x, cols = everything()) {
  x %>%
    rename_with(.fn = ~ stringr::str_to_sentence(gsub("_", " ", .x)), .cols = cols) %>%
    rename("CDM name" = "Cdm name")
}

niceNum <- function(x, dec = 0) {
  trimws(format(round(as.numeric(x), dec), big.mark = ",", nsmall = dec, scientific = FALSE))
}

niceCohortName <- function(x, col = "cohort_name", removeCol = TRUE) {
  x <- x |>
    mutate(
      covid_definition = if_else(
        grepl("covid_diagnostic_test", .data[[col]]), "diagnostic_test", "test"),
      comparison = gsub("_covid_diagnostic_test|_covid_test", "", .data[[col]])
    ) |>
    relocate("covid_definition", .after = "comparison")
  if (removeCol) {
    x <- x |> select(!all_of(col))
  }
  return(x)
}

niceOutcomeName <- function(x) {
  x |>
    mutate(
      covid_definition_out = if_else(
        grepl("covid_diagnostic_test", outcome), "diagnostic_test", "test"),
      delivery_excluded = case_when(
        variable_name == "nco" ~ NA,
        grepl("no_delivery", outcome) ~ "yes",
        !grepl("no_delivery", outcome) & grepl("inpatient|icu", outcome) ~ "no",
        !grepl("no_delivery", outcome) & !grepl("inpatient|icu", outcome) ~ "-"
      ),
      outcome = gsub("_diagnostic_test|_test|_no_delivery", "", outcome)
    ) |>
    filter(covid_definition == covid_definition_out | variable_name == "nco")  |>
    relocate("delivery_excluded", .after = "outcome") |>
    select(!covid_definition_out)
}
