# 2020-01-05 Basic working version
# 2020-02-03 Add footer with caveats
# 2020-02-23 Load discharged data, and present this as well
# Robert Goudie/Sarah Cowan

csv_directory <- "//cuh_nas120/eau/mseu/Shared/morning_report/"
analysis_directory <- "//cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the daily staff reports will be placed,
# with each day within a folder with date format YYYY-MM-DD
output_folder_name <- "staff_reports_daily"

# minimum number of admissions that a staff member has seen for a report to
# be created (needs to be >= this)
minimum_admissions_for_report <- 4

filepaths_to_load <- filepaths_most_recent(csv_directory = csv_directory,
                                           type = "admitted")
mr_data_admitted <- load_mr_data(filepaths_to_load)

filepaths_to_load <- filepaths_most_recent(csv_directory = csv_directory,
                                           type = "discharged")
mr_data_discharged <- load_mr_data(filepaths_to_load)

mr_data_discharged <- mr_data_discharged %>%
  mutate(VTE = NA,
         ReSPECT = NA,
         Medications_reconciliation = NA,
         Allergies = NA,
         Problem_list = NA,
         Summary = "Discharged")

# There may be overlap in Patient_MRNs betwen admitted and discharged
# This is because a patient may be both admitted and discharged in a
# single day
mr_data <- bind_rows(mr_data_admitted, mr_data_discharged)

# Create a data frame in which each row corresponds to one Doctor's interaction
# with one patient admission
mr_data_staff <- reshape_mr_table_staff(mr_data)

# Most recent date
#most_recent_date <- as.POSIXct("2019-11-21")
most_recent_date <- sort(mr_data$date, decreasing = TRUE)[1]

# filter to only most_recent_date
mr_data_staff_most_recent_date <- mr_data_staff %>%
  filter(date == most_recent_date)

# identify all staff working on most_recent_date
staff_most_recent_date <- unique(mr_data_staff_most_recent_date$Treatment_team)

# form path to output folder and check if it exists
output_folder_name_date <-
  file.path(output_folder_name, as.character(most_recent_date))

if (dir.exists(output_folder_name_date)){
  warning("Note directory for output already exists")
} else {
  dir.create(output_folder_name_date, recursive = TRUE)
}

# for each staff name in staff_most_recent_date, call
# staff_report_table_pdf function, with person set to each staff name in turn
void <- sapply(staff_most_recent_date,
               FUN = staff_report_table_pdf,
               x = mr_data_staff_most_recent_date,
               output_directory = output_folder_name_date,
               minimum_admissions_for_report = minimum_admissions_for_report)
