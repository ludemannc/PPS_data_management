
#' Read metadata workbook from disk
#'
#' @param path_file A character sting, path to metadata workbook.
#' @return A list of data.frames corresponding to each sheet of the workbook.
read_metadata_wb <- function(path_file) {
  sheet_names <- openxlsx::getSheetNames(path_file)
  metadata_wb <- lapply(sheet_names, function(sheet) {
    openxlsx::read.xlsx(xlsxFile = path_file, sheet = sheet)
  })
  names(metadata_wb) <- sheet_names
  return(metadata_wb)
}

#' Remove dir path and extension
#'
#' @param path_file A character sting, path to metadata workbook.
#' @return A character sting, metadata workbook file name stripped of its extension.
get_metadata_wb_core_name <- function(path_file) {
  sub("\\.xlsx", "", basename(path_file))
}

#' Save workbook content as Rdata file
save_metadata_wb_snapshot <- function(path_file) {
  metadata_file_wb <- read_metadata_wb(path_file)
  core_file_name <- get_metadata_wb_core_name(path_file)
  save(metadata_file_wb,
       file = file.path("scripts/tests/", paste0(core_file_name, ".RData"))
  )
}

#' Take a snapshot of the whole data/processed/ directory
take_snapshot_processed_data <- function(path = "data/processed/") {
  path_files <- list.files(path,
                           full.names = TRUE)
  for (wb in path_files) {
    save_metadata_wb_snapshot(wb)
    message("Snapshot taken: ", paste0(basename(wb)))
  }
}

#' Load workbook snapshot form disk
#'
#' @param path_corresponding_file A character sting, path to metadata workbook
#' corresponding to the snapshot.
load_snapshot <- function(path_corresponding_file) {
  metadata_wb_core_name <- get_metadata_wb_core_name(path_corresponding_file)
  pattern <- paste0("^", metadata_wb_core_name, "\\.RData$")
  path_snapshot <- list.files(file.path("scripts/tests/"),
                              pattern = pattern,
                              full.names = TRUE)
  # Loads metadata_file_wb within function env
  load(path_snapshot)
  return(metadata_file_wb)
}

#' Custom testthat expectation
#'
#' @param file A character string, name of the metadata workbook to check.
expect_match_snapshot <- function(file) {
  path_file <- file.path("data/processed/", file)
  snapshot <- load_snapshot(path_file)
  wb <- read_metadata_wb(path_file)
  testthat::expect_identical(wb, snapshot)
}

compare_with_snapshot <- function(file) {
  path_file <- file.path("data/processed/", file)
  snapshot <- load_snapshot(path_file)
  wb <- read_metadata_wb(path_file)
  waldo::compare(snapshot, wb)
}
