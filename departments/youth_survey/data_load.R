# ── YOUTH SURVEY DATA LOAD & PREPROCESSING ────────────────────────────────
# Source: Alumni_Data1.csv (KoboToolbox export)
#
# Base filter: consent_given == 1 only.
# Invalid codes are mapped to NA so KPI means use na.rm = TRUE.
#
# Column key
#   S3S1Q1            Employment  1=Employed  0=Not employed  8=N/A
#   S1S2Q3            Gender      1=Male  2=Female
#   S1S2Q1            Marital     1=Single  2=Married
#   S1S2Q2            DOB         date string "m/d/yyyy"
#   S1S3Q4            Education   1=None 2=Primary 3=Secondary 4=Vocational
#                                 5=University 6=Other
#   province_interview Province   1=Kigali City 2=Southern 3=Western
#                                 4=Northern 5=Eastern
#   consent_given     Consent     1=given

ys_raw <- readr::read_csv(
  "departments/alumini/data/Alumni_Data1.csv",
  show_col_types = FALSE
)

ys_df <- ys_raw %>%
  dplyr::filter(consent_given == 1) %>%
  dplyr::mutate(
    gender_label = factor(
      dplyr::case_when(
        S1S2Q3 == 1 ~ "Male",
        S1S2Q3 == 2 ~ "Female",
        TRUE        ~ NA_character_
      ),
      levels = c("Male", "Female")
    ),
    marital_label = factor(
      dplyr::case_when(
        S1S2Q1 == 1 ~ "Single",
        S1S2Q1 == 2 ~ "Married",
        TRUE        ~ NA_character_
      ),
      levels = c("Single", "Married")
    ),
    employment_label = factor(
      dplyr::case_when(
        S3S1Q1 == 1 ~ "Employed",
        S3S1Q1 == 0 ~ "Not Employed",
        TRUE        ~ NA_character_
      ),
      levels = c("Employed", "Not Employed")
    ),
    education_label = factor(
      dplyr::case_when(
        S1S3Q4 == 1 ~ "None",
        S1S3Q4 == 2 ~ "Primary",
        S1S3Q4 == 3 ~ "Secondary",
        S1S3Q4 == 4 ~ "Vocational",
        S1S3Q4 == 5 ~ "University",
        S1S3Q4 == 6 ~ "Other",
        TRUE        ~ NA_character_
      ),
      levels = c("None", "Primary", "Secondary", "Vocational", "University", "Other")
    ),
    province_label = factor(
      dplyr::case_when(
        province_interview == 1 ~ "Kigali City",
        province_interview == 2 ~ "Southern",
        province_interview == 3 ~ "Western",
        province_interview == 4 ~ "Northern",
        province_interview == 5 ~ "Eastern",
        TRUE                   ~ NA_character_
      ),
      levels = c("Kigali City", "Southern", "Western", "Northern", "Eastern")
    ),
    # DOB: try date string first, fall back to Excel serial number
    dob_str  = as.Date(as.character(S1S2Q2), format = "%m/%d/%Y"),
    dob_num  = suppressWarnings(
      as.Date(as.numeric(as.character(S1S2Q2)), origin = "1899-12-30")
    ),
    dob_date = dplyr::coalesce(dob_str, dob_num),
    age      = as.integer((Sys.Date() - dob_date) / 365.25)
  ) %>%
  dplyr::select(-dob_str, -dob_num)
