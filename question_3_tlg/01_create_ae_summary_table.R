###############
# Load packages
###############
library(pharmaverseadam)
library(gtsummary) # package for creating tables
library(dplyr) # package for manipulating data 
library(gt) # package for saving table

############
# Load data
############
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

####################
# Data preprocessing
####################

adae <- adae |>
  filter( # filtering for
    # Treatment-emergent adverse effects
    TRTEMFL == "Y"
  )

#############################################################
# Creating summary table of treatment-emergent adverse events
#############################################################

tbl <- adae |>
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = ACTARM, # stratifying TEAE's by treatment group
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Treatment-Emergent AEs"
  ) |>
  add_overall(col_label = "**All Patients**  \nN = {style_number(N)}", ) |>
  sort_hierarchical() # sorting table by decreasing frequency based on the 'All Patients' column

tbl

###############################
# Outputting the table as a PDF
###############################

tbl |>
  as_gt() |>
  gtsave("ae_summary_table.pdf")
