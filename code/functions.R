library(lubridate)
library(tidyverse)
library(grid)
library(gridExtra)

# filepaths

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


setClass("slashseparateddob")
setAs("character",
      "slashseparateddob",
      function(from){
        as.POSIXct(from, format = "%d/%m/%Y")
      })

extract_gender_from_patient_name <- function(x){
  # Assumes the format "DUCK, Donald M (96 y.o. F)"
  # and simply extracts the second last letter
  str_sub(x, -2, -2) 
}

extract_date_from_filepath <- function(filename){
  as.POSIXct(strptime(basename(filename), "%Y%m%d"))
}

read_morning_report_csv <- function(file){
  colClasses <- c("MRN" = "character",
                  "DOB" = "slashseparateddob",
                  "VTE.Assessment.Done." = "character",
                  "UFTO.order.placed.." = "character",
                  "Active.Treatment.Team" = "character",
                  "Active.Treatment.Team.Relationships" = "character",          
                  "Treatment.Team" = "character",                                
                  "Treatment.Team.Relationship" = "character",
                  "Admission.Medication.Reconciliation.Complete." = "character",
                  "FRAILTY.complete" = "character",
                  "ECG." = "character",
                  "Prob.List.Updated." = "character",
                  "Allergy.Review.Status" = "character")
  
  file_date <- extract_date_from_filepath(file)
  
  # On 21st Oct 2018, there is an extra trailing space in "UFTO order placed  "
  # column name
  if (file_date == as.POSIXct("2018-10-21")){
    is_ufto <- names(colClasses) == "UFTO.order.placed.."
    names(colClasses)[is_ufto] <- "UFTO.order.placed."
  }
  
  # On 27th Nov 2019, the column name changed to "VTE.Assessment.done."
  if (file_date >= as.POSIXct("2019-11-27")){
    is_vte <- names(colClasses) == "VTE.Assessment.Done."
    names(colClasses)[is_vte] <- "VTE.Assessment.done."
  }
  
  x <- read.csv(file = file,
                colClasses = colClasses,
                stringsAsFactors = FALSE)
  
  if (file_date == as.POSIXct("2018-10-21")){
    is_ufto <- names(x) == "UFTO.order.placed."
    colnames(x)[is_ufto] <- "UFTO.order.placed.."
  }
  
  # On 27th Nov 2019, the column name changed to "VTE.Assessment.done."
  if (file_date >= as.POSIXct("2019-11-27")){
    is_vte <- names(x) == "VTE.Assessment.done."
    names(x)[is_vte] <- "VTE.Assessment.Done."
  }
  
  # Rename columns
  x <- x %>%
    rename(Patient_name = "Patient.Name",
           Patient_MRN = "MRN",
           Patient_DOB = "DOB",
           Ward_room = "Ward.and.Room",
           VTE = "VTE.Assessment.Done.",
           ReSPECT = "UFTO.order.placed..",
           Treatment_team_active = "Active.Treatment.Team",
           Treatment_team_active_jobtitle = "Active.Treatment.Team.Relationships",
           Treatment_team = "Treatment.Team",
           Treatment_team_jobtitle = "Treatment.Team.Relationship",
           Date_admission_decision = "Decision.to.Admit.Time",
           Medications_reconciliation = "Admission.Medication.Reconciliation.Complete.",
           Frailty = "FRAILTY.complete",
           Consultant_lead = "Lead.Consultant",
           Date_arrival = "Arrival.date.and.time",
           Decision_maker_first = "First.Decision.Maker",
           Senior_review_start = "Senior.Review.Start",
           Time_departure = "Departure.time",
           ECG = "ECG.",
           Problem_list = "Prob.List.Updated.",
           PTA_meds_reviewed = "PTA.Meds.Rwd.",
           Allergies = "Allergy.Review.Status",
           Drug_allergies = "Allergies",
           Problem_list_text = "Problem.List",
           Handover = "Handover",
           Summary = "Summary...all.services",
           pochgb = "POCHGB",
           pocmcv = "POCMCV",
           pocwbc = "POCWBC",
           pocplt = "POCPLT",
           inr = "INR",
           pocddimer = "POCDDIMER",
           pocna = "POCNA",
           pock = "POCK",
           pocurea = "POCUREA",
           poccre = "POCCRE",
           pocalb = "POCALB",
           poctbil = "POCTBIL",
           pocalt = "POCALT",
           pocalp = "POCALP",
           pocica = "POCICA",
           pocglubg = "POCGLUBG",
           poccrp = "POCCRP",
           pocamylase = "POCAMYLASE",
           poctini = "POCTINI",
           poctrop = "POCTROP",
           pocph = "POCPH",
           pocbe = "POCBE",
           poclacbg = "POCLACBG")
  
  # Create Patient_gender, filename, and date columns
  x <- x %>%
    mutate(Patient_gender = extract_gender_from_patient_name(Patient_name),
           filename = basename(file),
           date = extract_date_from_filepath(file))
  
  # Check for unexpected values in gender column
  if (any(!x$Patient_gender %in% c("M", "F", "U"))){
    stop("Unexpected Patient gender data")
  }
  
  # delete column that only appears in half the files
  if ("Disch.Date.Time" %in% colnames(x)){
    x$Disch.Date.Time <- NULL
  }
  
  x$Problem_list <- as.numeric(as.character(x$Problem_list))
  x$ECG <- as.numeric(as.character(x$ECG))
  
  columns_to_keep <- c("filename",
                       "date",
                       "Patient_gender",
                       "Patient_DOB",
                       "Patient_MRN",
                       "Treatment_team_active",
                       "Treatment_team_active_jobtitle",
                       "Treatment_team",                                
                       "Treatment_team_jobtitle",
                       "VTE",
                       "ReSPECT",
                       "Medications_reconciliation",
                       "Frailty",
                       "ECG",
                       "Problem_list",
                       "Allergies",
                       "Summary")
  as_tibble(x[, columns_to_keep])
}

#' Load (potentially several) morning report csv files
#' 
#' @param filepaths_to_load A character vector of filepaths to morning report
#' csv files
load_simple_data <- function(filepaths_to_load){
  list_csv <- lapply(filepaths_to_load, read_morning_report_csv)
  
  simple_data <- bind_rows(list_csv)
  
  # Recode the data to 1s and 0s
  simple_data <- simple_data %>% 
    mutate(Age = floor(as.numeric(as.duration(date - Patient_DOB),
                                  units = "years")),
           VTE = if_else(VTE == "Yes",
                         true = 1, 
                         false = 0),
           ReSPECT = if_else(ReSPECT == "Has UFTO order",
                             true = 1,
                             false = 0),
           Frailty = case_when(Frailty == "Yes" ~ 1L, 
                               Frailty == "No" ~ 0L,
                               Frailty == "N/A" ~ NA_integer_),
           Allergies = if_else(Allergies == "Reviewed",
                               true = 1,
                               false = 0),
           Medications_reconciliation = if_else(Medications_reconciliation == "No",
                                                true = 0,
                                                false = 1))
  
  # Delete the Frailty and ECG columns, since not interested in these
  simple_data %>%
    select(-Frailty, -ECG)
}

create_report_for_staff_member <- function(x, person,
                                           directory){
  display_table <- x %>%
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
  
  role <- (x %>%
             filter(Treatment_team == person) %>%
             pull(Treatment_team_jobtitle))[1]
  
  if (nrow(display_table) >= minimum_admissions_for_report){
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
    cat(person, "done\n")
  } else {
    cat(person, "skipped; only", nrow(display_table), "admission(s)\n")
  }
}