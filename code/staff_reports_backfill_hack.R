dates <- seq(from = as.Date("2019-08-07"),
             to = as.Date("2020-01-03"),
             by = 1)


filepaths_to_load <- filepaths_all(csv_directory = csv_directory)
mr_data <- load_mr_data(filepaths_to_load)

# Create a data frame in which each row corresponds to one Doctor's interaction
# with one patient admission
mr_data_staff <- reshape_mr_table_staff(mr_data)

for (i in 1:length(dates)){

  
  most_recent_date <- dates[i]
  
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

cat("done", dates[i], staff_most_recent_date, "\n")
}