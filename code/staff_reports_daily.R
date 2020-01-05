csv_directory <- "//cuh_nas120/eau/mseu/Shared/morning_report/"
analysis_directory <- "//cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the daily staff reports will be placed,
# with each day within a folder with date format YYYY-MM-DD
output_folder_name <- "staff_reports_daily"

minimum_admissions_for_report <- 4

filepaths_to_load <- filepaths_most_recent(csv_directory = csv_directory)
mr_data <- load_mr_data(filepaths_to_load)

# Most recent date
#most_recent_date <- as.POSIXct("2019-11-21")
most_recent_date <- sort(mr_data$date, decreasing = TRUE)[1]

mr_data_staff <- mr_data %>%
  mutate(id = row_number()) %>%
  gather(key = "key",
         value = "value",
         Treatment_team_active,
         Treatment_team_active_jobtitle,
         Treatment_team,
         Treatment_team_jobtitle) %>%
  separate_rows(value, sep = "\n") %>%
  group_by(key) %>%
  mutate(temp_id = row_number()) %>%
  spread(key, value) %>%
  select(-temp_id) %>%
  filter(Treatment_team_jobtitle %in%
           c("Core Trainee", "Foundation Trainee", "ST3+", "Clinical Fellow", 
             "GP Trainee")) %>%
  distinct
# distinct needed as if doctor named twice in treatment team
# this creates a duplication

mr_data_staff_most_recent_date <- mr_data_staff %>%
  filter(date == most_recent_date)

staff_most_recent_date <- unique(mr_data_staff_most_recent_date$Treatment_team)

output_folder_name_date <-
  file.path(output_folder_name, as.character(most_recent_date))

if (dir.exists(output_folder_name_date)){
  warning("Note directory for output already exists")
} else {
  dir.create(output_folder_name_date, recursive = TRUE)
}

void <- sapply(staff_most_recent_date,
               FUN = staff_report_table_pdf,
               x = mr_data_staff_most_recent_date,
               output_directory = output_folder_name_date)
