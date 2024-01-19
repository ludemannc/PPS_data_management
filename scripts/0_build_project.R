##### Minimal Reproducible Analysis Example
##### 0 - build project


### Install required packages if necessary
required_packages <- c("ggplot2", "openxlsx", "emmeans")

for (package in required_packages) {
  if (package %in% rownames(installed.packages()) == FALSE) {
    install.packages(package)
  }
}

### Run all scripts in the correct order

all_scripts <- list.files("./scripts", full.names = TRUE)
target_scripts <- all_scripts[grep("[1-9]", all_scripts)]

for (script in target_scripts) source(script, local = TRUE)

