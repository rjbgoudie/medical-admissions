library(lubridate)
library(ggplot2)
library(tidyverse)
library(scales)
library(grid)
library(gridExtra)

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


check_for_colname <- function(file){
  "UFTO.order.placed." %in% colnames(read.csv(file))
}

read_epic_file <- function(file){
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
  x <- x%>%
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
           poclacbg = "POCLACBG") %>%
    mutate(Patient_gender = extract_gender_from_patient_name(Patient_name))
  
  if (any(!x$Patient_gender %in% c("M", "F", "U"))){
    stop("Unexpected Patient gender data")
  }
  
  x$filename <- basename(file)
  x$date <- extract_date_from_filepath(file)
  
  # delete column that only appears in half the files
  if ("Disch.Date.Time" %in% colnames(x)){
    x$Disch.Date.Time <- NULL
  }
  
  x$Problem_list <- as.numeric(as.character(x$Problem_list))
  x$ECG <- as.numeric(as.character(x$ECG))
  
  # Relevant columns
  simple_data_colnames <- c("filename",
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
  as_tibble(x[, simple_data_colnames])
}


load_simple_data <- function(load_all_files){
  all_csv <- list.files(path = "Z:/", pattern="*1.csv", recursive = TRUE)
  is_in_unwanted_folder <- str_detect(all_csv, "MR audit FT")
  all_csv <- all_csv[!is_in_unwanted_folder]
  
  if (!load_all_files){
    dates <- extract_date_from_filepath(all_csv)
    which_is_most_recent <- which(sort(dates, decreasing = TRUE)[1] == dates)
    all_csv <- all_csv[which_is_most_recent]
  }
  
  all_csv <- paste0("Z:/", all_csv)
  
  list_csv <- lapply(all_csv, read_epic_file)
  
  simple_data <- bind_rows(list_csv)
  
  simple_data <- simple_data %>% 
    mutate(Age = floor(as.numeric(as.duration(date - Patient_DOB),
                                  units = "years")),
           VTE = if_else(VTE == "Yes",
                         1, 
                         0),
           ReSPECT = if_else(ReSPECT == "Has UFTO order",
                             1,
                             0),
           Frailty = case_when(Frailty == "Yes" ~ 1L, 
                               Frailty == "No" ~ 0L,
                               Frailty == "N/A" ~ NA_integer_),
           Allergies = if_else(Allergies == "Reviewed",
                               1,
                               0),
           Medications_reconciliation = if_else(Medications_reconciliation == "No",
                                                0,
                                                1))
  
  # Never interested in these
  simple_data %>%
    select(-Frailty, -ECG)
}