### Download and unzip project on user's computer


## Check working directory is empty if not prompt user with message.
current_wd <- getwd()
current_files <- list.files(current_wd)

if (length(current_files) != 0) {

  message(paste0("Working directory is not empty. You might loose existing files.",
                 " \n",
                 "Are you sure you want to continue ?"))

  valid_answer <- FALSE

  while (valid_answer == FALSE) {

    answer <- readline()

    valid_answer <- answer %in% c("Yes", "No")
    if (!valid_answer) {
      message("Yes/No:")
    }
  }

  if (answer == "No") {
    stop("Please back up your files or set up the project in another directory.",
         call. = FALSE)
  }

}

## Download and unzip project
url_repo <- paste0("https://git.wageningenur.nl/pps/PPS_data_management/-/archive/",
                   "master/PPS_data_management-master.zip")
download.file(url = url_repo,
              destfile = file.path(current_wd, "project.zip"))
unzip("project.zip")


## Copy project file to working directory
list_files <- list.files("./PPS_data_management-master/",
                         full.names = TRUE)
file.copy(from = list_files,
          to = "./",
          recursive = TRUE)

## Get rid of un necessary files
unlink(file.path(current_wd, "./PPS_data_management-master/"),
       recursive = TRUE)
unlink(c("project.zip", "download_project.R", "README.md",
         "scripts/tests/"))

## Run project building script
source("./scripts/0_build_project.R")
