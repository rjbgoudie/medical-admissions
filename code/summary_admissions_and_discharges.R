# 2021-08-07 Code for counting total number of patients
# Robert Goudie/Sarah Cowan

csv_directory <- "//cuh_nas120/eau/mseu/Shared/morning_report/"
analysis_directory <- "//cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the summary graph will be placed (within 
# analysis_directory)
output_folder_name <- "summary_admissions_and_discharges"


filepaths_to_load_admitted <- filepaths_all(csv_directory = csv_directory,
                                   type = "admitted")

filepaths_to_load_discharged <- filepaths_all(csv_directory = csv_directory,
                                   type = "discharged")

# Hack to remove post 2nd April 2020 (since 3rd April data file is empty)
filepaths_to_load_admitted <- filepaths_to_load_admitted[1:699]
filepaths_to_load_discharged <- filepaths_to_load_discharged[1:699]

mr_data_admitted <- load_mr_data(filepaths_to_load_admitted)
mr_data_discharged <- load_mr_data(filepaths_to_load_discharged)

# There may be overlap in Patient_MRNs betwen admitted and discharged
# This is because a patient may be both admitted and discharged in a
# single day
mr_data <- bind_rows(mr_data_admitted, mr_data_discharged)


mr_data_filtered <- mr_data %>%
  filter(date >= as.Date("2019-08-01") & date <= as.Date("2020-03-17"))

# Number of patients involved in audit
nrow(mr_data_filtered)
