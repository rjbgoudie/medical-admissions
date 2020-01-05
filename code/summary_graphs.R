csv_directory <- "//cuh_nas120/eau/mseu/Shared/morning_report/"
analysis_directory <- "//cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the daily staff reports will be placed,
# with each day within a folder with date format YYYY-MM-DD
output_folder_name <- "summary_graphs"

# load ALL the CSV files, so we have all the history
filepaths_to_load <- filepaths_all(csv_directory = csv_directory)
mr_data <- load_mr_data(filepaths_to_load)

# Reshape the data, so that each indicator has a separate row
# for each admission - the "indicator" column records the name
# of the indicator that that row corresponds to; and the
# "value" column contains its value
mr_data_long <- mr_data %>%
  gather(key = "indicator",
         value = "value",
         -filename,
         -date,
         -Age,
         -Patient_gender,
         -Patient_DOB,
         -Patient_MRN,
         -Treatment_team_active,
         -Treatment_team_active_jobtitle,
         -Treatment_team,
         -Treatment_team_jobtitle,
         -Summary)

# calculates the proportion of 1s in "value" for each date and indicator
mr_data_perday <- mr_data_long %>%
  group_by(date, indicator) %>%
  summarise(proportion = mean(value))

### TODO need to change POSIXct to Date below


pdf(file = file.path(output_folder_name, "indicators_graph.pdf"),
    height = 20/cm(1),
    width = 30/cm(1))
ggplot(mr_data_perday, aes(x = date, y = proportion, colour = indicator)) +
  geom_smooth(span = 0.25, se = FALSE) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_date(limits = c(as.Date("2019-07-01"),
                          as.Date("2020-09-01")),
               date_minor_breaks ="months") +
  labs(title = "",
       x = "Date",
       y = "Percentage completed",
       colour = "",
       caption = "") +
  theme_bw() + 
  geom_vline(xintercept = as.Date("2019-08-07")) +
  annotate("text", 
           x = as.Date("2019-08-04"), 
           y = 0.9, 
           label = "August changeover",
           angle = 90,
           vjust = "bottom") +
  geom_vline(xintercept = as.Date("2019-11-07")) +
  annotate("text", 
           x = as.Date("2019-11-04"), 
           y = 0.9, 
           label = "Emails commenced",
           angle = 90,
           vjust = "bottom") +
  geom_vline(xintercept = as.Date("2019-12-04")) +
  annotate("text", 
           x = as.Date("2019-12-01"), 
           y = 0.9, 
           label = "Foundation changeover",
           angle = 90,
           vjust = "bottom")
graphics.off()
