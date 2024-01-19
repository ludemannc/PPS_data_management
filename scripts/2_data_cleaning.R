##### Minimal Reproducible Analysis Example
##### 2 - data cleaning

library(openxlsx)


##### Inputs ---------------------------------------------------------------------------------------

output_worbook_name <- "WUR_ferlilizer_trials"
output_sheet_name <- "data"

##### Load data ------------------------------------------------------------------------------------

# Raw (initial processed) data set
path_data <- "data/processed/Meststof proef WUR_metadata.xlsx"
d <- read.xlsx(path_data, sheet = "data")
metadata <- read.xlsx(path_data, sheet = "meta data")
variable_definition <- read.xlsx(path_data, sheet = "variable definitions")

##### Clean data -----------------------------------------------------------------------------------

## Inspect data
str(d)
# Here we can see that:
# - Opbrengst.Maïs: has multiple place holders for missing values
# - Boederij: some farm names are not properly capitalized
lapply(d, table)


## Set proper English variable names
names(d) <- c("field_id", "farm", "fertilizer", "yield")

## Correct values
d$farm[grep("de jong", d$farm)] <- "de Jong"
d$farm[grep("van de boer", d$farm)] <- "van de Boer"

## Make missing value placeholder uniform for yield
id_NA <- grep("^[^-]?\\d+([.]\\d+)?$", d$yield, invert = TRUE)
d$yield[id_NA] <- NA


## Transform variables

# Convert yield in ton per ha (tha),
d$yield <-  as.numeric(d$yield) / 10


##### Save data workbook ---------------------------------------------------------------------------

wb <- createWorkbook()

sheet_names <- c("data","meta data","variable definitions")
sheet_data <- list(d, metadata, variable_definition)

# Add the sheet to the workbook template
lapply(sheet_names, function(x) addWorksheet(wb, sheetName = x))
# Write data to the proper sheets
mapply(function(name, data) writeData(wb, name, data), sheet_names, sheet_data)

saveWorkbook(wb = wb,
             file = "./data/cleaned/fertilizer_trial_WUR_cleaned.xlsx",
             overwrite = TRUE)


