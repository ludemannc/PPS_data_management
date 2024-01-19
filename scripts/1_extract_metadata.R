### -------------------------------------------------------------------------- ###
# PPS Data Management Plan companion script to create excel workbooks
# including proper meta data
### -------------------------------------------------------------------------- ###

library(openxlsx)

### Functions ------------------------------------------------------------------ #


#' Create template of metadata sheet
#'
#' Once the data process the user can manually update the metadata sheet
#' in the resulting excel workbook.
#'
#' @return A data.frame with columns field, field_name and values.
#' This last is to be filled manually.
instantiate_meta_data_template <- function() {

  field <- c("Data ID", "Official title of the dataset", "Project name",
             "Description of project", "Author", "Author ID(ORCID)",
             "Contributor(s)", "Subject matter of research/Vocabulary", "Data origin", "Funder(s) or sponsor(s) of project",
             "creation date (m/d/yyyy)", "Embargo end date", "Citation",
             "keywords (AGROVOC)", "Country(ies) covered", "Point longitude coord. in Dec. Degrees",
             "Agro-Ecological Zone(s)(FAO) covered", "Years covered by data", "Crops covered by data",
             "Animals covered by data", "Start date of data collection", "End date of data collection",
             "License (default=CC-BY)", "Permission given by email", "Rights", "Contact email")

  field_name <- c("data.id", "data.title", "project.name", "project.description",
                  "author", "orcid", "contributors", "subject.research",
                  "data.origin", "donor", "date.creation", "date.embargo",
                  "citation", "keywords.agrovoc", "countries", "longitude",
                  "aez", "years", "crops", "animals",
                  "date.collect.start", "date.collect.end", "licence", "permission",
                  "rights", "contact.mail")

  meta_data <- data.frame(field = field,
                          field_name = field_name,
                          values = NA,
                          stringsAsFactors = FALSE)
  return(meta_data)
}


#' Extract file extension
#'
#' @param file A file object yielded by `read_raw_data_file()`.
#' @return A character indicating the file extension.
extract_file_extension <- function(file) {
  regmatches(file, regexpr("(?<=\\.)[a-z]+$", file, perl = TRUE))
}


#' Make a list of raw data files in projects
#'
#' @param path A character string indicating the relative path
#' to the folder where raw data is stored.
#' By default raw data is assumed to be in "data/raw/".
#' @return A list of data.frame. Each data.frame as two columns: path and extension.
#' Respectively holding the relative path to the file and the file extension.
#' Later this list of data.frame is referred to as a *file object*.
list_raw_data_files <- function(path = "data/raw/") {
  paths <- list.files(path, full.names = TRUE)
  extensions <- vapply(paths, extract_file_extension,
                       FUN.VALUE = character(1), USE.NAMES = FALSE)
  raw_data_files <- data.frame(path = paths,
                               extension = extensions)
  raw_data_files <- split(raw_data_files, 1:nrow(raw_data_files))
  return(raw_data_files)
}


#' Extract file name
#' file name <=> string before file extension
#' @param file A file object yielded by read_raw_data_file().
#' @param supported_extensions A character vector of supported extensions.
#' @return A character corresponding to the string before file extension.
extract_file_name <- function(file, supported_extensions = c("csv", "xlsx")) {
  extension_pattern <- paste0(supported_extensions, collapse = "|")
  pattern <- sprintf("\\.(%s)$", extension_pattern)
  gsub(pattern, "", basename(file$path))
}


#' Given a file extract the data sheets
#'
#' @param file A file object yielded by read_raw_data_file().
#' @return Depending on the file$extension:
#' * if file$extension is "csv", a character string "data"
#' * if file$extension is "xlsx", a character vector with all sheet names.
extract_data_sheets <- function(file) {
  switch(file$extension,
         "csv" = "data",
         "xlsx" = openxlsx::getSheetNames(file$path)
  )
}


#' Instantiate a workbook object
#'
#' This is the most important function of the script. It defines the one major
#' object which is passed from function to function and encapsulate all required data.
#' The workbook object if the R skeleton of the future excel workbook that will contain
#' data, meta data and variable definitions.
#'
#' @param file A file object yielded by read_raw_data_file().
#' @param output_path A character string. Path where final workbook will written.
#' Default to "data/processed/"
#'
#' @return A list later referred to as *workbook object*, containing the
#' following fields:
#' * original_name [character]: name of the original file without extension.
#' * extension [character]: original file extension.
#' * path [charcater]: entire path including file name where workbook should be saved.
#' * workbook [openxlsx Workbook Object]
#' * variables [named list]: holding 3 fields: name, unit and sheet used to construct
#'  variable definitions.
#' * sheets [character vector]: name of the workbook sheets to be processed.
instantiate_workbook <- function(file, output_path = "data/processed/") {

  file_name <- extract_file_name(file)
  workbook_name <- paste0(file_name, "_metadata")
  full_name <- paste0(workbook_name, ".xlsx")

  variables <- list(name =  vector("character"),
                    unit = vector("character"),
                    sheet = vector("character"))

  workbook <- list(original_name = file_name,
                  extension = file$extension,
                  path = file.path(output_path, full_name),
                  wb = createWorkbook(workbook_name),
                  variables = variables,
                  sheets =  extract_data_sheets(file))
  return(workbook)
}


#' Read data from a given sheet
#' @param file A file object yielded by read_raw_data_file().
#' @param sheet A character indicating sheet name.
#' @return A data.frame
read_data_sheet <- function(file, sheet) {
  ## Properly read in the file depending on its extension.
  # The first line is skipped. Otherwise the special place holder '$'
  # separating variable name and unit might get coerced to a '.'
  switch(file$extension,
         "csv" = read.csv(file$path, check.names = FALSE),
         "xlsx" = read.xlsx(file$path, sheet = sheet)
  )
}


#' Process a given data sheet
#'
#' @param file A file object yielded by read_raw_data_file().
#' @param sheet A character indicating sheet name.
#'
#' @return A list later referred to as a *processed sheet object* containing
#' the following fields:
#' * name [character]: sheet name.
#' * variables [character vector]: variable names.
#' * units [character vector]: variables units.
#' * data [data.frame]: holding sheet data.
process_sheet <- function(file, sheet) {

  dat <- read_data_sheet(file, sheet)
  header <- strsplit(colnames(dat), "\\$")

  empty_columns <- vapply(header, length, FUN.VALUE = integer(1)) == 0
  if (any(empty_columns)) {
    stop(sprintf("Empty column name in %s", basename(file$path)),
         call. = FALSE)
  }

  # From header extract...
  variables <- vapply(header, `[`, 1, FUN.VALUE = character(1)) # variable names
  units <- vapply(header, `[`, 2, FUN.VALUE = character(1)) # units

  # Assign variable names as column names -> get rid of $<unit>
  colnames(dat) <- variables

  processed_sheet <- list(
    name = sheet,
    variables = variables,
    units = units,
    data = dat
  )
  return(processed_sheet)
}


#' Update workbook object with a given processed sheet
#' @param workbook A work book object.
#' @param processed_sheet A processed sheet object.
#' @return A workbook object updated with data from `processed_sheet`.
update_workbook_sheet_data <- function(workbook, processed_sheet) {
  ## When several sheets are present in your workbook
  # append the relevant values one after the other
  workbook$variables$name <- c(workbook$variables$name, processed_sheet$variables)
  workbook$variables$unit <- c(workbook$variables$unit, processed_sheet$units)
  sheet_vector <- rep(processed_sheet$name, length(processed_sheet$variables))
  workbook$variables$sheet <- c(workbook$variables$sheet, sheet_vector)

  addWorksheet(workbook$wb, processed_sheet$name)
  writeData(workbook$wb, processed_sheet$name, processed_sheet$data)

  return(workbook)
}


#' Create variable definition data.frame
#' @param workbook A work book object.
#' @return A data.frame holding variable and units to be updated manually later on.
create_variable_definition_df <- function(workbook) {
  var_definitions <- data.frame("workbook" = workbook$original_name,
                                "sheet" = workbook$variables$sheet,
                                "variable" = workbook$variables$name,
                                "unit" = workbook$variables$unit,
                                "definition" = NA,
                                "unique identifier" = 0,
                                "personal information" = 0,
                                stringsAsFactors = FALSE)
  return(var_definitions)
}

#' Update existing metadata and variable definition.
#'
#' In practice this function is called only if the final workbook already exists.
#' Its main goal is to prevent the necessary manual updates in metadata and variable
#' definition sheet from being discarded when the script is run again.
#'
#' @param workbook A work book object.
#' @param meta_data A data.frame containing metadata template.
#' @param var_definitions A data.frame containing variable definitions template.
#'
#' @return A list with the following fields:
#'  * meta_data [data.frame]
#'  * var_definitions [data.frame]
update_meta_data <- function(workbook, meta_data, var_definitions) {

  current_var_definitions <- read.xlsx(workbook$path,
                                       sheet = "variable definitions")
  current_meta_data <- read.xlsx(workbook$path,
                                 sheet = "meta data")

  # Update current meta_data and var_definitions with the potential new values
  var_definitions <- merge(current_var_definitions, var_definitions, all.x = TRUE)
  meta_data <- merge(current_meta_data, meta_data, all.x = TRUE)

  # Unfortunately merge() does not preserve the original order
  # We need to set that back manually
  order_var_definitions <- match(current_var_definitions$variable,
                                 var_definitions$variable)
  var_definitions <- var_definitions[order_var_definitions, ]
  order_meta_data <- match(current_meta_data$field, meta_data$field)
  meta_data <- meta_data[order_meta_data, ]

  updated_meta_data <- list(var_definitions = var_definitions,
                            meta_data = meta_data)
  return(updated_meta_data)
}

#' Save workbook as a xlsx file
#'
#' Workbook is saved at the path specified in `workbook$path`.
#' By default "data/processed/". To modify the location where the workbook
#' is saved, you must modified the `output_path` argument when instantiating
#' the workbook with `instantiate_workbook()`.
#'
#' @param workbook A work book object.
#' @param meta_data A data.frame containing metadata template.
#' @param var_definitions A data.frame containing variable definitions template.
save_excel_file <- function(workbook, meta_data, var_definitions) {

  addWorksheet(workbook$wb, "meta data")
  addWorksheet(workbook$wb, "variable definitions")
  writeData(workbook$wb, "meta data", meta_data)
  writeData(workbook$wb, "variable definitions", var_definitions)

  saveWorkbook(wb = workbook$wb,
               file = workbook$path,
               overwrite = TRUE)
}



### Raw file processing -------------------------------------------------------- #

raw_data_files <- list_raw_data_files()

for (file in raw_data_files) {

  print(sprintf("Processing: %s", file$path))

  if (!(file$extension %in% c("csv", "xlsx"))) {
    warning(sprintf("File extension not supported: %s, file skipped.", file$extension))
  }

  workbook <- instantiate_workbook(file)

  for (sheet in workbook$sheets) {
    processed_sheet <- process_sheet(file, sheet)
    workbook <- update_workbook_sheet_data(workbook, processed_sheet)
  }

  var_definitions <- create_variable_definition_df(workbook)
  meta_data <- instantiate_meta_data_template()

  if (file.exists(workbook$path)) {
    updated_meta_data <- update_meta_data(workbook, meta_data, var_definitions)
    meta_data <- updated_meta_data$meta_data
    var_definitions <- updated_meta_data$var_definitions
  }

  save_excel_file(workbook = workbook,
                  meta_data = meta_data,
                  var_definitions = var_definitions)
}



