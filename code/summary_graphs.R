# 2020-01-05 Basic working version
# Robert Goudie/Sarah Cowan

csv_directory <- "//cuh_nas120/eau/mseu/Shared/morning_report/"
analysis_directory <- "//cuh_nas120/eau/mseu/Shared/reports/medical-admissions/"

# Output is created relative to this working directory
setwd(analysis_directory)

# load various shared functions
source("code/functions.R")

# Subdirectory in which the summary graph will be placed (within 
# analysis_directory)
output_folder_name <- "summary_graphs"

# load ALL the CSV files
filepaths_to_load <- filepaths_all(csv_directory = csv_directory,
                                   type = "admitted")

# Hack to remove post 2nd April 2020 (since 3rd April data file is empty)
filepaths_to_load <- filepaths_to_load[1:699]

# This takes some time, since loads hundreds of CSV files
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
         -Treatment_team,
         -Treatment_team_jobtitle,
         -Summary)

# calculates the proportion of 1s in "value" for each date and indicator
# Note that NAs are deleted from the data before calculating proportions
mr_data_perday <- mr_data_long %>%
  group_by(date, indicator) %>%
  summarise(proportion = mean(value, na.rm = TRUE))

saveRDS(mr_data_perday,
        file = file.path(output_folder_name, "mr_data_perday.rds"))

# graph without raw data (only shows smoothed curve)

pdf(file = file.path(output_folder_name, "indicators_graph.pdf"),
    height = 20/cm(1),
    width = 30/cm(1))
ggplot(mr_data_perday, aes(x = date, y = proportion, colour = indicator)) +
  geom_smooth(span = 0.25, se = FALSE, method = "loess") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1),
                     breaks = seq(from = 0, to = 1, by = 0.2),
                     expand = c(0, 0)) +
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
           vjust = "bottom") +
  geom_vline(xintercept = as.Date("2020-02-03")) +
  annotate("text",
           x = as.Date("2020-01-31"),
           y = 0.9,
           label = "Individual feedback commenced",
           angle = 90,
           vjust = "bottom") +
  geom_vline(xintercept = as.Date("2020-02-17")) +
  annotate("text",
           x = as.Date("2020-02-14"),
           y = 0.9,
           label = "Certificates commenced",
           angle = 90,
           vjust = "bottom") +
  geom_vline(xintercept = as.Date("2020-03-17")) +
  annotate("text",
           x = as.Date("2020-03-14"),
           y = 0.9,
           label = "Stopped (COVID-19)",
           angle = 90,
           vjust = "bottom")
graphics.off()

# graph with both raw data and smoothed curve

pdf(file = file.path(output_folder_name, "indicators_graph_unsmoothed.pdf"),
    height = 20/cm(1),
    width = 30/cm(1))
ggplot(mr_data_perday, aes(x = date, y = proportion, colour = indicator)) +
  geom_point(alpha = 0.5) +
  geom_smooth(span = 0.25, se = FALSE, method = "loess") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1),
                     breaks = seq(from = 0, to = 1, by = 0.2),
                     expand = c(0, 0)) +
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
           vjust = "bottom") +
  geom_vline(xintercept = as.Date("2020-02-03")) +
  annotate("text",
           x = as.Date("2020-01-31"),
           y = 0.9,
           label = "Individual feedback commenced",
           angle = 90,
           vjust = "bottom") +
geom_vline(xintercept = as.Date("2020-02-17")) +
  annotate("text",
           x = as.Date("2020-02-14"),
           y = 0.9,
           label = "Certificates commenced",
           angle = 90,
           vjust = "bottom") +
geom_vline(xintercept = as.Date("2020-03-17")) +
annotate("text",
           x = as.Date("2020-03-14"),
           y = 0.9,
           label = "Stopped (COVID-19)",
           angle = 90,
           vjust = "bottom")
graphics.off()
  
