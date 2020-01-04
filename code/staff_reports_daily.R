csv_directory <- "\\\\cuh_nas120/eau/mseu/Shared/morning_report"
analysis_directory <- "\\\\cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the daily staff reports will be placed,
# with each day within a folder with date format YYYY-MM-DD
output_folder_name <- "staff_reports_daily"

minimum_admissions_for_report <- 4

filepaths_to_load <- filepaths_most_recent(csv_directory = csv_directory)
simple_data <- load_simple_data(filepaths_to_load)

# Most recent date
#most_recent_date <- as.POSIXct("2019-11-21")
most_recent_date <- sort(simple_data$date, decreasing = TRUE)[1]

simple_data_staff <- simple_data %>%
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

simple_data_staff_most_recent_date <- simple_data_staff %>%
  filter(date == most_recent_date)

staff_most_recent_date <- unique(simple_data_staff_most_recent_date$Treatment_team)

directory <- file.path(output_folder_name, as.character(most_recent_date))

if (dir.exists(directory)){
  warning("Note directory for output already exists")
} else {
  dir.create(directory, recursive = TRUE)
}

void <- sapply(staff_most_recent_date,
       create_report_for_staff_member,
       x = simple_data_staff_most_recent_date,
       directory = directory)
