source("common.R")

# Output is created relative to this working directory
setwd("H:/problem_list")

simple_data <- load_simple_data(load_all_files = TRUE)

simple_data_long <- simple_data %>%
  gather("indicator",
         "value",
         -filename,
         -date,
         -Age,
         -Patient_DOB,
         -Patient_MRN,
         -Treatment_team_active,
         -Treatment_team_active_jobtitle,
         -Treatment_team,
         -Treatment_team_jobtitle,
         -Summary)

# WHY ARE THERE NAs in value???? To look at
simple_data_perday <- simple_data_long %>%
  group_by(date, indicator) %>%
  summarise(proportion = mean(value, na.rm = TRUE))

pdf("bigdata.pdf", height = 10/cm(1), width = 15/cm(1))
ggplot(simple_data_long, aes(x = date, y = value, colour = indicator)) + 
  stat_summary(fun.y = "mean", geom = "line") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(title = "",
       x = "Date",
       y = "Percentage completed",
       colour = "",
       caption = "")+
  theme_bw()
graphics.off()

pdf("data_overview.pdf", height = 20/cm(1), width = 30/cm(1))
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
