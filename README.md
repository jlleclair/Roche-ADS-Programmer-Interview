## Description of Folders and Files

### `question_1_sdtm/`
- Contains a script and files related to **SDTM DS domain creation**.
- `01_create_ds_domain.R` uses the `sdtm.oak` package to map raw variables to SDTM-compliant variables (DSTERM, DSDECOD, DSCAT, etc.).
- `study_ct.csv` contains controlled terminology used to standardize variables.
- The resulting dataset (`ds_sdtm.csv`), and log file (`log.txt`) are included for reference.

### `question_2_adam/`
- Contains a script and files related to **ADaM ADSL Dataset Creation**.
- `01_create_adsl.R` uses the `admiral` package to create the ADSL dataset and derives key study variables (e.g., randomized flag, treatment dates).
- The resulting dataset (`adsl.csv`), and log file (`log.txt`) are included for reference.

### `question_3_tlg/`
- Contains scripts and files related to **TLG - Adverse Events Reporting**.
- `01_create_ae_summary_table.R` uses the `gtsummary` package to create a summary table of treatment-emergent adverse events.
  - The resulting table (`ae_summary_table.pdf`), and log file (`01_log.txt`) are included for reference.
- `02_create_visualizations.R` uses the `ggplot2` package to create two adverse events visualizations.
  - The `aesev_plot.png` file contains a bar chart of the adverse event severity by treatment arm.
  - The `top10_AE_plot.png` file contains a forest plot of the top 10 most frequent adverse events along with the 95% confidence intervals for incidence rates.
  - The resulting log file (`02_log.txt`) is included for reference.

## How to Run

1. Clone this repository.
2. Open R or RStudio and set the working directory to the repo root.
3. Run the scripts in any order:
   1. `question_1_sdtm/01_create_ds_domain.R`
   2. `question_2_adam/01_create_adsl.R`
   3. `question_3_tlg/01_create_ae_summary_table.R`
   4. `question_3_tlg/02_create_visualizations.R`

## Packages Used
- `sdtm.oak` - SDTM dataset creation
- `pharmaverseraw` - Raw datasets
- `pharmaversesdtm` - SDTM datasets
- `pharmaverseadam` - ADaM datasets
- `dplyr` - Data manipulation
- `ggplot2` - Visualizations
- `gtsummary` - TEAE summary tables
- `gt` - Saving tables
- `stringr` - String manipulation
- `lubridate` - Date/time manipulation
- `admiral` - ADaM derivations (dates, flags, etc.)
- `GenBinomApps` - Calculating confidence intervals

## Decisions

### Question 1: SDTM

- Unscheduled visits not in the controlled terminology were kept as-is, with VISIT showing the original label and VISITNUM derived from the numeric portion.

### Question 2: ADaM
- The coding assessment documentation spells the Last Known Alive Date variable as LSTAVLDT. To align with official ADaM/SDTM standards and other documentation, the variable name has been corrected to LSTALVDT throughout the code and datasets.
