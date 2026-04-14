# ── ACADEMIC PERFORMANCE — SYNTHETIC DATA ───────────────────
set.seed(42)

.ac_cohorts <- c("Intwari", "Ishami", "Ijabo", "Grade2028", "Grade2029")
.ac_stages  <- c("EY (Entry Year)", "S4", "S5", "S6")
.ac_n       <- 500L

.ac_gender  <- sample(c("Female", "Male"), .ac_n, replace = TRUE)
.ac_cohort  <- sample(.ac_cohorts,         .ac_n, replace = TRUE)
.ac_stage   <- sample(.ac_stages,          .ac_n, replace = TRUE)
.ac_term    <- sample(c("Term 1", "Term 2", "Term 3"), .ac_n, replace = TRUE)

.ac_base    <- ifelse(.ac_gender == "Female", 63L, 59L)
.ac_bonus   <- c("EY (Entry Year)" = 0, S4 = 3, S5 = 5, S6 = 7)[.ac_stage]

.clip <- function(x) pmax(0L, pmin(100L, round(x)))

.ac_math    <- .clip(rnorm(.ac_n, .ac_base + .ac_bonus - 2,  12))
.ac_english <- .clip(rnorm(.ac_n, .ac_base + .ac_bonus + 1,  10))
.ac_science <- .clip(rnorm(.ac_n, .ac_base + .ac_bonus - 1,  11))
.ac_kirundi <- .clip(rnorm(.ac_n, .ac_base + .ac_bonus + 4,   9))
.ac_gpa     <- round((.ac_math + .ac_english + .ac_science + .ac_kirundi) / 4, 1)

ac_data <- data.frame(
  StudentID   = seq_len(.ac_n),
  Cohort      = .ac_cohort,
  Stage       = factor(.ac_stage, levels = .ac_stages),
  Gender      = .ac_gender,
  Term        = .ac_term,
  Math        = .ac_math,
  English     = .ac_english,
  Science     = .ac_science,
  Kinyarwanda = .ac_kirundi,
  GPA         = .ac_gpa,
  Status      = ifelse(.ac_gpa >= 75, "Honors", ifelse(.ac_gpa >= 50, "Pass", "Fail")),
  stringsAsFactors = FALSE
)

# clean up temp vars
rm(list = ls(pattern = "^\\.ac_"))

AC_CLR <- list(
  blue   = "#378ADD", blue_l  = "#B5D4F4",
  green  = "#0E6B52", green_l = "#D1FAE5",
  gold   = "#D4A843", gold_l  = "#F3E8C8",
  red    = "#C75B39", red_l   = "#F5C4B3",
  purple = "#534AB7"
)
