csv_directory <- "//cuh_nas120/eau/mseu/Shared/morning_report/"
analysis_directory <- "//cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the monthly staff reports will be placed,
# with each month within a folder with date format YYYY-MM
output_folder_name <- "staff_reports_monthly"

minimum_admissions_for_report <- 5

# This should be any date within the month of interest
date_in_month <- as.Date("2019-11-01")

filepaths_to_load <- filepaths_from_month(csv_directory = csv_directory,
                                          date_in_month = date_in_month)
mr_data <- load_mr_data(filepaths_to_load)

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

directory <- file.path(output_folder_name, format(date_in_month, "%Y-%m"))

if (dir.exists(directory)){
  warning("Note directory for output already exists")
} else {
  dir.create(directory, recursive = TRUE)
}

mr_data_staff_npatients <-  mr_data_staff %>%
  group_by(Treatment_team) %>%
  summarise(n_patients = n())

mr_data_staff_npatients_exceed_minimum <- mr_data_staff_npatients %>%
  filter(n_patients >= minimum_admissions_for_report) %>%
  pull(Treatment_team)

# some of this is copied from daily - can we avoid repeat
mr_data_staff_percents <- mr_data_staff %>%
  group_by(Treatment_team) %>%
  filter(Treatment_team %in% mr_data_staff_npatients_exceed_minimum) %>%
  select(MRN = Patient_MRN,
         Age = Age,
         Gender = Patient_gender,
         Summary = Summary,
         VTE = VTE,
         ReSPECT = ReSPECT,
         Meds_Rec = Medications_reconciliation,
         Problem_list = Problem_list,
         Allergies = Allergies) %>%
  summarise_at(c("VTE",
                 "ReSPECT",
                 "Meds_Rec",
                 "Problem_list",
                 "Allergies"),
               ~ round(mean(., na.rm = TRUE), 2) * 100) %>%
  left_join(mr_data_staff_npatients) %>%
  mutate(performance = (VTE + Meds_Rec)/2) %>%
  arrange(desc(performance)) %>%
  print(n = Inf)
