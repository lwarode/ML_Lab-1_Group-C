# install packages
if (!"devtools" %in% install.packages() & !"vdemdata" %in% installed.packages()) {
  install.packages("devtools") 
  devtools::install_github("vdeminstitute/vdemdata")
  # library(vdemdata)
} else if ("devtools" %in% install.packages() & !"vdemdata" %in% installed.packages()) {
  devtools::install_github("vdeminstitute/vdemdata")
  # library(vdemdata)
} else {
  # library(vdemdata)
}

# file name
file_name_fun <- function(name, year) {
  paste0(name, "_",  year + 1, ".csv")
}

vdem_file_name <- file_name_fun(
  "vdem_version", max(vdemdata::vdem$year)
)

vdem_file_folder <- paste0("data/", vdem_file_name)

# custom data function
vdem_data_fun <- function(data, file_name) {
  write.csv(
    data, file_name
  ) 
}

# store data in folder
if (!file.exists(vdem_file_folder)) {
  dir.create("data")
  vdem_data_fun(vdemdata::vdem, vdem_file_folder)
} else {
  print("File already exists.")
}