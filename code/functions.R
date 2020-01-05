library(lubridate)
library(tidyverse)
library(grid)
library(gridExtra)

################################################################################
# filepath functions
################################################################################

# basic

filepaths_base <- function(csv_directory){
  # has to be recursive since old csv files are filed in subdirectories
  filepaths_to_load <- list.files(path = csv_directory,
                                  pattern="*1.csv",
                                  recursive = TRUE)
  
  # exclude files in the "MR audit FT" folder
  is_in_unwanted_folder <- str_detect(filepaths_to_load, "^MR audit FT/")
  filepaths_to_load[!is_in_unwanted_folder]
}

filepaths_all <- function(csv_directory){
  filepaths_to_load <- filepaths_base(csv_directory)
  paste0(csv_directory, filepaths_to_load)
}

filepaths_most_recent <- function(csv_directory){
  filepaths_to_load <- filepaths_base(csv_directory)
  filepaths_to_load <- filepaths_filter_most_recent(filepaths_to_load)
  paste0(csv_directory, filepaths_to_load)
}

filepaths_from_month <- function(csv_directory, date_in_month){
  filepaths_to_load <- filepaths_base(csv_directory)
  filepaths_to_load <- filepaths_filter_within_month(filepaths_to_load,
                                                     date_in_month)
  paste0(csv_directory, filepaths_to_load)
}

# filtering filepaths

filepaths_filter_most_recent <- function(x){
  file_dates <- extract_date_from_filepath(x)
  is_most_recent <- sort(file_dates, decreasing = TRUE)[1] == file_dates
  x[is_most_recent]
}

filepaths_filter_within_month <- function(x, date_in_month){
  file_dates <- extract_date_from_filepath(x)
  start_date <- floor_date(date_in_month, unit = "month")
  end_date <- ceiling_date(date_in_month, unit = "month")
  is_within_month <- file_dates >= start_date & file_dates < end_date
  x[is_within_month]
}

################################################################################
# Extract data from strings
################################################################################

extract_gender_from_patient_name <- function(x){
  # Assumes the format "DUCK, Donald M (96 y.o. F)"
  # and simply extracts the second last letter
  str_sub(x, -2, -2) 
}

extract_date_from_filepath <- function(filename){
  as.Date(strptime(basename(filename), "%Y%m%d"))
}

################################################################################
# Loading morning report csv files
################################################################################

load_mr_data_single <- function(file){
  mr_data <- read_csv(file = file,
                      col_types = cols(DOB = col_date("%d/%m/%Y"),
                                       "Prob List Updated?" = col_integer(),
                                       .default = col_character()))
  
  file_date <- extract_date_from_filepath(file)

  # On 21st October 2018, the trailing space from UFTO column is not present
  # if (file_date == as.Date("2018-10-21")){
  #   mr_data <- mr_data %>%
  #     rename("UFTO order placed?" = "UFTO order placed? ")
  # }
  
  # On 27th Nov 2019, the column name changed to "VTE Assessment done?"
  # (ie "done" became lowercase)
  if (file_date < as.Date("2019-11-27")){
    mr_data <- mr_data %>%
      rename("VTE Assessment done?" = "VTE Assessment Done?")
  }
  
  mr_data <- mr_data %>%
    select(Patient_name = "Patient Name", # for extracting gender - remove after
           Patient_MRN = "MRN",
           Patient_DOB = "DOB",
           Treatment_team_active = "Active Treatment Team",
           Treatment_team_active_jobtitle = "Active Treatment Team Relationships",
           Treatment_team = "Treatment Team",
           Treatment_team_jobtitle = "Treatment Team Relationship",
           VTE = "VTE Assessment done?",
           ReSPECT = "UFTO order placed?",
           Medications_reconciliation = "Admission Medication Reconciliation Complete?",
           Problem_list = "Prob List Updated?",
           Allergies = "Allergy Review Status",
           Summary = "Summary - all services")
  
  mr_data <- mr_data %>%
    mutate(Patient_gender = extract_gender_from_patient_name(Patient_name),
           filename = basename(file),
           date = extract_date_from_filepath(file),
           Age = floor(time_length(date - Patient_DOB, unit = "year"))) %>%
    select(-Patient_name)
  
  # Check for unexpected values in gender column
  if (any(!mr_data$Patient_gender %in% c("M", "F", "U"))){
    stop(file_date, ": Unexpected Patient gender data")
  }
  
  if (any(!is.na(mr_data$VTE) & mr_data$VTE != "Yes")){
    stop(file_date, ": Unexpected VTE data")
  }
  
  if (any(!is.na(mr_data$ReSPECT) & mr_data$ReSPECT != "Has UFTO order")){
    stop(file_date, ": Unexpected ReSPECT data")
  }
  
  if (any(!is.na(mr_data$Medications_reconciliation) &
          !mr_data$Medications_reconciliation %in% c("Partially",
                                                     "No",
                                                     "Yes"))){
    stop(file_date, ": Unexpected Medications_reconciliation data")
  }
  
  if (any(!is.na(mr_data$Allergies) &
          !mr_data$Allergies %in% c("Reviewed", "Unable to Assess"))){
    stop(file_date, ": Unexpected Allergies data")
  }
  
  if (any(!mr_data$Problem_list %in% c(0, 1))){
    stop(file_date, ": Unexpected Problem_list data")
  }
  
  # Recode to 1s and 0s
  mr_data %>%
    mutate(VTE = if_else(VTE == "Yes",
                         true = 1,
                         false = 0,
                         missing = 0),
           ReSPECT = if_else(ReSPECT == "Has UFTO order",
                             true = 1,
                             false = 0,
                             missing = 0),
           Medications_reconciliation =
             if_else(Medications_reconciliation == "Yes" |
                       Medications_reconciliation == "Partially",
                     true = 1,
                     false = 0,
                     missing = 0),
           Allergies = if_else(Allergies == "Reviewed",
                               true = 1,
                               false = 0,
                               missing = 0))
}

#' Load (potentially several) morning report csv files
#' 
#' Each separate file is joined together to form a tall data frame 
#' containing all the data
#' 
#' @param filepaths_to_load A character vector of filepaths to morning report
#' csv files
load_mr_data <- function(filepaths_to_load){
  mr_data_list <- lapply(filepaths_to_load, load_mr_data_single)
  bind_rows(mr_data_list)
}

################################################################################
# Generate PDF report
################################################################################

staff_report_table_pdf <- function(x,
                                   person,
                                   output_directory){
  # extract records for this person
  display_table <- x %>%
    filter(Treatment_team == person) %>%
    select(MRN = Patient_MRN,
           Age = Age,
           Gender = Patient_gender,
           Summary = Summary,
           VTE = VTE,
           ReSPECT = ReSPECT,
           Meds_Rec = Medications_reconciliation,
           Problem_list = Problem_list,
           Allergies = Allergies) %>%
    rowwise %>%
    mutate(Summary = sapply(strwrap(Summary, width = 80, simplify = FALSE),
                            paste, collapse = "\n"))
  
  if (nrow(display_table) < minimum_admissions_for_report){
    cat(person, "skipped; only", nrow(display_table), "admission(s)\n")
  } else {
    # calculate %s for last row of table
    display_table_percents <- display_table %>%
      ungroup %>%
      summarise_at(c("VTE",
                     "ReSPECT",
                     "Meds_Rec",
                     "Problem_list",
                     "Allergies"),
                   ~ paste0(round(mean(., na.rm = TRUE), 2) * 100, "%"))
    
    # convert 1s and 0s to Yes and No
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

    # Join both together
    display_table_full <- bind_rows(display_table_char, display_table_percents)
    
    # DELETE Problem_list column for now
    # Also note Problem_list mutate_at in text_colours has been commented out
    display_table_full <- display_table_full %>%
      select(-Problem_list)

    # create a character vector with the text colour of each column of
    # display_table_full in turn
    text_colours <- display_table_full %>%
      mutate_all(as.character) %>%
      mutate_at(c("VTE",
                  "ReSPECT",
                  "Meds_Rec",
                  # "Problem_list",
                  "Allergies"),
                ~ case_when(. == "Yes" ~ "green3",
                            . == "No" ~ "red",
                            TRUE ~ "black")) %>%
      mutate_at(c("MRN", "Gender", "Age", "Summary"),
                ~ "black") %>%
      unlist
    
    # standard fontface for all rows except the last row
    tablegrob_theme <- ttheme_default(
      core =
        list(fg_params =
               list(fontface = c(rep("plain", nrow(display_table_full) - 1),
                                 "bold"),
                    col = text_colours,
                    hjust = 0,
                    x = 0.01,
                    vjust = 1,
                    y = 0.99),
             bg_params =
               list(fill = c(rep(c("white"), nrow(display_table_full) - 1),
                             "white"))))
    
    # generate graphical object ("grob") of the table
    grob <- tableGrob(display_table_full,
                      rows = NULL,
                      theme = tablegrob_theme)
    
    
    # calculate the height of the table
    # need to count number of newlines in the summary, since this can be
    # long sometimes
    n_newlines <- sum(sapply(display_table$Summary, function(x){
      str_count(x, "\n")
    }))
    lines <- n_newlines + nrow(display_table) + 3
    height <- max(20, ceiling(lines/1.25))
    
    # Create heading of the table
    jobtitle <- (x %>%
                   filter(Treatment_team == person) %>%
                   pull(Treatment_team_jobtitle))[1]
    date_char <- as.character(most_recent_date)
    date_prev_char <- as.character(most_recent_date - ddays(1))
    heading <- paste0(person, "     ",
                      jobtitle, "     ",
                      date_prev_char, " to ", date_char)
    
    # Output as PDF
    pdf(file = file.path(output_directory, paste0(person, ".pdf")),
        width = 30/cm(1),
        height = height/cm(1))
    grid.arrange(grob, top = heading)
    graphics.off()
    cat(person, "done\n")
  }
}