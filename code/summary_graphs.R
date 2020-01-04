csv_directory <- "\\\\cuh_nas120/eau/mseu/Shared/morning_report"
analysis_directory <- "\\\\cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the daily staff reports will be placed,
# with each day within a folder with date format YYYY-MM-DD
output_folder_name <- "summary_graphs"

# load ALL the CSV files, so we have all the history
# only keep some of the columns
filepaths_to_load <- filepaths_all(csv_directory = csv_directory)
simple_data <- load_simple_data(filepaths_to_load)

# Reshape the data, so that each indicator has a separate row
# for each admission - the "indicator" column records the name
# of the indicator that that row corresponds to; and the
# "value" column contains its value
simple_data_long <- simple_data %>%
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
simple_data_perday <- simple_data_long %>%
  group_by(date, indicator) %>%
  summarise(proportion = mean(value))

pdf(file = file.path(output_folder_name, "indicators_graph.pdf"),
    height = 20/cm(1),
    width = 30/cm(1))
ggplot(simple_data_perday, aes(x = date, y = proportion, colour = indicator)) +
  geom_smooth(span = 0.25, se = FALSE) + 
  geom_vline(xintercept = as.POSIXct("2019-08-07 00:00:00",
                                     format = "%Y-%m-%d %H:%M:%S"))+
  annotate("text", 
           x = as.POSIXct("2019-08-04 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S"), 
           y = 0.9, 
           label = "August changeover",
           angle = 90,
           vjust = "bottom") +
  geom_vline(xintercept = as.POSIXct("2019-11-07 00:00:00",
                                     format = "%Y-%m-%d %H:%M:%S"))+
  annotate("text", 
           x = as.POSIXct("2019-11-04 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S"), 
           y = 0.9, 
           label = "Emails commenced",
           angle = 90,
           vjust = "bottom") +
  geom_vline(xintercept = as.POSIXct("2019-12-04 00:00:00",
                                     format = "%Y-%m-%d %H:%M:%S"))+
  annotate("text", 
           x = as.POSIXct("2019-12-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S"), 
           y = 0.9, 
           label = "Foundation changeover",
           angle = 90,
           vjust = "bottom") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_datetime(
    limits = c(as.POSIXct("2019-07-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"),
               as.POSIXct("2020-09-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")),
    date_minor_breaks ="months") +
  labs(title = "",
       x = "Date",
       y = "Percentage completed",
       colour = "",
       caption = "")+
  theme_bw()
graphics.off()
