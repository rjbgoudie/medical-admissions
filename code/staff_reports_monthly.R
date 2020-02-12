# 2020-01-05 Basic working version
# 2020-02-12 Fixup loading, and change performance metric to VTE
# Robert Goudie/Sarah Cowan

csv_directory <- "//cuh_nas120/eau/mseu/Shared/morning_report/"
analysis_directory <- "//cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the monthly staff reports will be placed,
# with each month within a folder with date format YYYY-MM
output_folder_name <- "staff_reports_monthly"

# minimum number of admissions that a staff member has seen for them to be
# included in the table (needs to be >= this)
minimum_admissions_for_report <- 10

# This should be any date within the month of interest
date_in_month <- as.Date("2020-01-01")

filepaths_to_load <- filepaths_from_month(csv_directory = csv_directory,
                                          date_in_month = date_in_month)
mr_data <- load_mr_data(filepaths_to_load)

# Create a data frame in which each row corresponds to one Doctor's interaction
# with one patient admission
mr_data_staff <- reshape_mr_table_staff(mr_data)

directory <- file.path(output_folder_name, format(date_in_month, "%Y-%m"))

if (dir.exists(directory)){
  warning("Note directory for output already exists")
} else {
  dir.create(directory, recursive = TRUE)
}

# create table of number of patients seen by each staff member
mr_data_staff_npatients <-  mr_data_staff %>%
  group_by(Treatment_team) %>%
  summarise(n_patients = n())

# create character vector of the names of staff who saw enough patients
mr_data_staff_npatients_exceed_minimum <- mr_data_staff_npatients %>%
  filter(n_patients >= minimum_admissions_for_report) %>%
  pull(Treatment_team)

# filter to staff who saw enough patients,
# then select and rename desired columns,
# then summarise and then merge with data on the number of patients seen
# finally create overall performance measure, and sort the rows by this
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
  mutate(performance = VTE) %>%
  arrange(desc(performance)) %>%
  print(n = Inf)

write.csv(mr_data_staff_percents,
          row.names = FALSE,
          file = file.path(directory, "table_vte.csv"))