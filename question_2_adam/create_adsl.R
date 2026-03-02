###################
# Loading Packages
###################

library(admiral) # package for creating ADaM datasets
library(dplyr, warn.conflicts = FALSE) # package for data manipulation
library(pharmaversesdtm) # package for importing sdtm datasets
library(lubridate)
library(stringr)

##################
# Loading the data
##################
dm <- pharmaversesdtm::dm 
vs <- pharmaversesdtm::vs 
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds 
ae <- pharmaversesdtm::ae

dm <- convert_blanks_to_na(dm) # demographics domain
vs <- convert_blanks_to_na(vs) # vital signs domain
ex <- convert_blanks_to_na(ex) # exposure domain
ds <- convert_blanks_to_na(ds) # disposition domain
ae <- convert_blanks_to_na(ae) # adverse event domain

#######################################
# Assigning DM Domain to an adsl object
#######################################

adsl <- dm %>%
  select(-DOMAIN)

############################
# Creating Derived Variables
############################

# deriving the age groupings into the categories "<18", "18-50", ">=50"
adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18-50",
      AGE >= 50 ~ ">=50",
      TRUE ~ NA_character_ # Handles missing values
    ),AGEGR9N = case_when(
      AGE < 18 ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE >= 50 ~ 3
    )
  )

# deriving the treatment start date
# Impute start and end time of exposure to first and last respectively,
# Do not impute date
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC, # Start Date/Time of Treatment
    new_vars_prefix = "EXST", # start of variable names that are created by function
    time_imputation = "first", # if time is missing, impute the earliest possible time
    # example: "2020-01-15" -> "2020-01-15 00:00:00"
    ignore_seconds_flag = TRUE # if only seconds are missing, do not populate imputation flag
  )%>%
  derive_vars_dtm(
    dtc = EXENDTC, # End date/Time of Treatment
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

# TRTSDTM is the patients treatment start data-time

adsl <- adsl %>%
  # TREATMENT START DATE
  derive_vars_merged(
    # adding variables to adsl from ex_ext
    dataset_add = ex_ext,
    # choosing which observations to include
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    # setting the names of the new variables to be added to adsl
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    
    order = exprs(EXSTDTM, EXSEQ), # sort in date-time order and exposure sequence
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )%>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# deriving the ITTFL randomization flag
adsl <- adsl %>%
  derive_vars_merged(
    # adding variable to adsl based on data in dm
    dataset_add = dm, 
    new_vars = exprs(
      # if there is a value in ARM, set randomization flag to "Y"
      # if there is no value in ARM, set randomization flag to "N"
      ITTFL = if_else(!is.na(ARM), "Y", "N")
    ),
    by_vars = exprs(STUDYID, USUBJID) # variables to join on
  )

# deriving the last known date alive

adsl <- adsl %>%
  # using this function to look at multiple event dates
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      # last complete date of vital assessment with a valid test result
      # ([VS.VSSTRESN] and [VS.VSSTRESC] not both missing) and datepart of
      # [VS.VSDTC] not missing.
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = !is.na(VSSTRESN) | !is.na(VSSTRESC) & !is.na(convert_dtc_to_dt(VSDTC)), 
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "M"),
          seq = VSSEQ
        ),
      ),
      # last complete onset date of AEs (datepart of Start Date/Time of Adverse Event.
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(AESTDTC), # use records where adverse event start date is not missing
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "M"),
          seq = AESEQ
        ),
      ),
      # last complete disposition date (datepart of Start Date/Time of Disposition Even.
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M"),
          seq = DSSEQ
        ),
      ),
      # Last date of treatment administration where patient received a valid
      #dose (datepart of Datetime of Last Exposure to Treatment.
      event(
        dataset_name = "adsl",
        condition = !is.na(as.Date(TRTEDTM)), # converting date-time to date
        set_values_to = exprs(
          LSTALVDT = as.Date(TRTEDTM), 
          seq = 0),
      )
    ),
    source_datasets = list(ae = ae, vs = vs, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTALVDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTALVDT) # in the coding assessment documentation this variable is spelled as LSTAVLDT,
                               # but in all other official documentation the variable is spelled as LSTALVDT
  )
#################################
# Save adsl dataset to a csv file
#################################

write.csv(adsl, "adsl.csv")





