source("common.R")

# Output is created relative to this working directory
setwd("H:/problem_list")

simple_data <- load_simple_data(load_all_files = FALSE)

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

create_report_for_staff_member <- function(person,
                                           directory){
  display_table <- simple_data_staff_most_recent_date %>%
    filter(Treatment_team == person) %>%
    select(Patient_MRN,
           Age,
           Patient_gender,
           Summary,
           VTE,
           ReSPECT,
           Medications_reconciliation,
           Problem_list,
           Allergies) %>%
    rename(MRN = Patient_MRN,
           Gender = Patient_gender,
           Meds_Rec = Medications_reconciliation) %>%
    rowwise %>%
    mutate(Summary = sapply(strwrap(Summary, width = 80, simplify = FALSE),
                            paste, collapse = "\n"))
  
  display_table_summary <- display_table %>%
    ungroup %>%
    summarise_at(c("VTE",
                   "ReSPECT",
                   "Meds_Rec",
                   "Problem_list",
                   "Allergies"),
                 ~ paste0(round(mean(., na.rm = TRUE), 2) * 100, "%"))
  
  display_table_char <- display_table %>%
    mutate_all(as.character) %>%
    mutate_at(c("VTE",
                "ReSPECT",
                "Meds_Rec",
                "Problem_list",
                "Allergies"),
              ~ case_when(. == "1" ~ "Yes",
                          . == "0" ~ "No",
                          TRUE ~ "?"))
  
  display_table_all <- bind_rows(display_table_char, display_table_summary)
  
  # DELETE Problem_list column for now
  # Also note Problem_list mutate_at in colour_char has been commented out
  display_table_all <- display_table_all %>%
    select(-Problem_list)
  
  colour_char <- display_table_all %>%
    mutate_all(as.character) %>%
    mutate_at(c("VTE",
                "ReSPECT",
                "Meds_Rec",
                ###### "Problem_list", #######
                "Allergies"),
              ~ case_when(. == "Yes" ~ "green3",
                          . == "No" ~ "red",
                          TRUE ~ "black")) %>%
    mutate_at(c("MRN", "Gender", "Age", "Summary"),
              ~ "black") %>%
    unlist
  
  n_minus_1 <- nrow(display_table_all) - 1
  
  t1 <- ttheme_default(core=list(
    fg_params=list(fontface=c(rep("plain", n_minus_1), "bold"),
                   col = colour_char,
                   hjust = 0,
                   x = 0.01,
                   vjust = 1,
                   y = 0.99),
    bg_params = list(fill=c(rep(c("white"),
                                length.out=n_minus_1), "white"))))
  
  role <- (simple_data_staff_most_recent_date %>%
             filter(Treatment_team == person) %>%
             pull(Treatment_team_jobtitle))[1]
  
  if (nrow(display_table) > 3){
    g <- tableGrob(display_table_all, rows=NULL, theme = t1)
    
    n_newlines <- sum(sapply(display_table$Summary, function(x){
      str_count(x, "\n")
    }))
    lines <- n_newlines + nrow(display_table) + 3
    height <- max(20, ceiling(lines/1.25))
    
    date_char <- as.character(most_recent_date)
    date_prev_char <- as.character(most_recent_date - ddays(1))
    
    heading <- paste0(person, "     ",
                      role, "     ",
                      date_prev_char, " to ", date_char)
    
    pdf(file = file.path(directory, paste0(person, ".pdf")),
        width = 30/cm(1),
        height = height/cm(1))
    grid.arrange(g,
                 top = heading)
    graphics.off()
  }
  cat("Report for", person, "output\n")
}

directory <- file.path("staff_output", as.character(most_recent_date))

if (dir.exists(directory)){
  warning("Note directory for output already exists")
} else {
  dir.create(directory, recursive = TRUE)
}

void <- sapply(staff_most_recent_date,
       create_report_for_staff_member,
       directory = directory)
