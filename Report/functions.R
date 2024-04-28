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
  x %>% rename_with(.fn = ~ stringr::str_to_sentence(gsub("_", " ", .x)), .cols = cols)
}

niceNum <- function(x, dec = 0) {
  trimws(format(round(as.numeric(x), dec), big.mark = ",", nsmall = dec, scientific = FALSE))
}
