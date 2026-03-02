##########################
# Loading Packages
##########################
library(sdtm.oak) # package for creating sdtm datasets
library(pharmaverseraw) # package for loading data
library(dplyr) # package for data frame transformation
library(stringr) # package for string manipulation


#######################
# Loading the data
#######################

ds_raw <- pharmaverseraw::ds_raw # importing the raw data

dm <- pharmaversesdtm::dm # importing the dm domain dataset for calculating DSSTDY

study_ct <- read.csv("sdtm_ct.csv") # study controlled terms

#########################
# Create oak id variables
#########################

ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )
########################
# Map the topic variable
########################

# Setting DSTERM to the reason for completion/discontinuation
ds_raw$DSTERM <- ifelse(is.na(ds_raw$OTHERSP), ds_raw$IT.DSTERM, 
                    ds_raw$OTHERSP)
ds <-
  # Derive topic variable
  # Map DSTERM using assign_no_ct, raw_var=IT.DSTERM, tgt_var=DSTERM
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )
########################################
# Mapping the remainder of the variables
########################################

# Creating the standardized disposition term variable
ds_raw$DSDECOD_collected <- ifelse(is.na(ds_raw$OTHERSP), ds_raw$IT.DSDECOD,
                     ds_raw$OTHERSP)

# Fixing minor case or spelling issues in the collected values
ds_raw$DSDECOD_collected <- case_when(
  ds_raw$DSDECOD_collected == "Completed" ~ "Complete",
  ds_raw$DSDECOD_collected == "Screen Failure" ~ "Trial Screen Failure",
  ds_raw$DSDECOD_collected == "Lost to Follow-Up" ~ "Lost To Follow-Up",
  ds_raw$DSDECOD_collected == "Study Terminated by Sponsor" ~ "Study Terminated By Sponsor",
  TRUE ~ ds_raw$DSDECOD_collected
)
# mapping the standardized disposition term using the study ct
ds <- ds %>% assign_ct(
  raw_dat = ds_raw,
  raw_var = "DSDECOD_collected",
  tgt_var = "DSDECOD",
  ct_spec = study_ct,
  ct_clst = "C66727",
  id_vars = oak_id_vars())

# creating the category for disposition event variable without ct
ds_raw$DSCAT_collected <- ifelse(is.na(ds_raw$OTHERSP), ifelse(ds_raw$IT.DSDECOD=="Randomized",
                                                 "PROTOCOL MILESTONE",
                                                 "DISPOSITION EVENT"), 
                   "OTHER EVENT")

# mapping the category for disposition event using study_ct
ds <- ds %>% assign_ct(
  raw_dat = ds_raw,
  raw_var = "DSCAT_collected",
  tgt_var = "DSCAT",
  ct_spec = study_ct,
  ct_clst = "C74558",
  id_vars = oak_id_vars())

ds <- ds %>% assign_datetime(
  tgt_var = "DSSTDTC",
  raw_dat = ds_raw,
  raw_var = "IT.DSSTDAT",
  raw_fmt = "m-d-y"
)

ds <- ds %>% 
  # creating the collection date and time of the disposition observation in ISO8601 format
  assign_datetime(
    tgt_var = "DSDTC",
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    raw_fmt = c("m-d-y", "H:M")
  ) %>%
  # for DSDTCOL with only m-d-y to avoid generating NA's
  assign_datetime(
    tgt_var = "DSDTC_DATE",
    raw_dat = ds_raw,
    raw_var = "DSDTCOL",
    raw_fmt = "m-d-y"
  ) %>%
  # setting DSDTC to be in the form m-d-y, H:M when hours and minutes are available
  # and to d-m-y when hours and minutes are unavailable
  mutate(
    DSDTC = coalesce(DSDTC, DSDTC_DATE)
  ) %>%
  select(-DSDTC_DATE)

# Fixing minor case or spelling issues in the collected values
ds_raw$INSTANCE <- case_when(
  ds_raw$INSTANCE == "Ambul Ecg Removal" ~ "Ambul ECG Removal",
  TRUE ~ ds_raw$INSTANCE
)


# Map VISIT from INSTANCE using assign_ct
ds <- ds %>% assign_ct(
  raw_dat = ds_raw,
  raw_var = "INSTANCE",
  tgt_var = "VISIT",
  ct_spec = study_ct,
  ct_clst = "VISIT",
  id_vars = oak_id_vars()
)
  # Map VISITNUM from INSTANCE using assign_ct
ds <- ds %>% assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )

# adding the INSTANCE variable to the ds dataset to properly map the unscheduled visits
ds <- ds %>% assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "INSTANCE",
  tgt_var = "INSTANCE",
  id_vars = oak_id_vars()
)
# handling the unscheduled visits that are not in the study
ds <- ds %>%
  mutate(
    # converting VISITNUM to numeric
    VISITNUM = as.numeric(VISITNUM),
    # if the visit is unscheduled, overwrite the value in VISITNUM to just contain the numeric visit number
    VISITNUM = ifelse(
      str_detect(INSTANCE, "^Unscheduled"),
      as.numeric(str_extract(INSTANCE, "[0-9]+\\.?[0-9]*")),
      VISITNUM
    )
  )
###############################
# Create SDTM Derived Variables
###############################

ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    DSTERM = toupper(DSTERM),
    VISIT = toupper(VISIT), # uppercase for the unscheduled visits not included in the study ct
    
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) %>%
  # deriving the study day of start of event relative to the sponsor defined RFSTDTC
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY",
    merge_key = "USUBJID"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
  )


#############################
# Save the data to a csv file
#############################

write.csv(ds, "ds_sdtm.csv")