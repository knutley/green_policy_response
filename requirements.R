# requirements.R

required_packages <- c(
  "data.table",
  "stringr",
  "furrr",
  "googleLanguageR",
  "testthat",
  "progressr",
  "progress",
  "future.apply",
  "tidyverse",
  "tidytext",
  "topicmodels",
  "reshape2"
)

installed <- installed.packages()[, "Package"]
to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install)
} else {
  message("All required packages are already installed.")
}
