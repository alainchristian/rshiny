library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

# ── Load raw data ──────────────────────────────────────────────────────────────
al_raw <- read.csv(
  "departments/alumini/data/Alumni_Data1.csv",
  stringsAsFactors = FALSE
)

# ── Filter out non-cohort entries ──────────────────────────────────────────────
Alumni1 <- al_raw %>%
  filter(!(S1S1Q4 %in% c("Icyizere", "Urumuli", "Intwari")))

# ── Grade ──────────────────────────────────────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    grade = factor(
      na_if(S1S1Q4, ""),
      levels = c("Imena", "Isonga", "Umurage", "Umucyo", "Ishyaka", "Inganji")
    )
  )

# ── Gender ─────────────────────────────────────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    gender = factor(
      case_when(
        as.numeric(S1S2Q1) == 1 ~ "Male",
        as.numeric(S1S2Q1) == 2 ~ "Female",
        TRUE ~ NA_character_
      ),
      levels = c("Male", "Female")
    )
  )

# ── Health Status ──────────────────────────────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    Health_Status = case_when(
      S4S2Q1 == 0 ~ "Healthy",
      S4S2Q1 == 1 ~ "Unwell",
      TRUE        ~ NA_character_
    ),
    Health_Status = factor(Health_Status, levels = c("Healthy", "Unwell"))
  )

# ── Mental Health: frequency of symptoms ──────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    Status = factor(
      as.numeric(S4S2Q28),
      levels  = c(1, 2, 3, 4, 5),
      labels  = c("Never", "Seldom", "Quite often", "Very often", "Always"),
      ordered = TRUE
    )
  )

Alumni1 <- Alumni1 %>%
  mutate(
    Ment_Dis = case_when(
      Status %in% c("Never", "Seldom", "Quite often") ~ 1,
      Status %in% c("Very often", "Always")           ~ 0,
      TRUE ~ NA_real_
    ),
    Ment_Dis = factor(
      Ment_Dis,
      levels = c(1, 0),
      labels = c("Positive Feelings", "Negative Feelings")
    )
  )

# ── QoL ratings ────────────────────────────────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    QOL_rating = factor(
      as.numeric(S4S2Q3),
      levels  = c(1, 2, 3, 4, 5),
      labels  = c("Very poor", "Poor", "Average", "Good", "Very good"),
      ordered = TRUE
    ),
    H_satisf_rating = factor(
      as.numeric(S4S2Q4),
      levels  = c(1, 2, 3, 4, 5),
      labels  = c("Very poor", "Poor", "Average", "Good", "Very good"),
      ordered = TRUE
    )
  )

QOL_rating_items <- Alumni1 %>%
  dplyr::select(QOL_rating, H_satisf_rating)

# ── WHOQOL-BREF reverse scoring ────────────────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    S4S2Q5_rev  = 6 - S4S2Q5,
    S4S2Q6_rev  = 6 - S4S2Q6,
    S4S2Q28_rev = 6 - S4S2Q28
  )

# ── WHOQOL domain means ────────────────────────────────────────────────────────
Alumni1$phys_mean <- rowMeans(
  Alumni1[, c("S4S2Q5_rev","S4S2Q6_rev","S4S2Q12",
              "S4S2Q17","S4S2Q18","S4S2Q19","S4S2Q20")],
  na.rm = TRUE
)
Alumni1$psych_mean <- rowMeans(
  Alumni1[, c("S4S2Q7","S4S2Q8","S4S2Q9",
              "S4S2Q13","S4S2Q21","S4S2Q28_rev")],
  na.rm = TRUE
)
Alumni1$social_mean <- rowMeans(
  Alumni1[, c("S4S2Q22","S4S2Q23","S4S2Q24")],
  na.rm = TRUE
)
Alumni1$env_mean <- rowMeans(
  Alumni1[, c("S4S2Q10","S4S2Q11","S4S2Q14","S4S2Q15",
              "S4S2Q16","S4S2Q25","S4S2Q26","S4S2Q27")],
  na.rm = TRUE
)

# ── Transform to WHO 0-100 scale ───────────────────────────────────────────────
Alumni1$phys_QOL_perc  <- 20 * Alumni1$phys_mean
Alumni1$psych_QOL_perc <- 20 * Alumni1$psych_mean
Alumni1$social_QOL_perc <- 20 * Alumni1$social_mean
Alumni1$env_QOL_perc   <- 20 * Alumni1$env_mean

# ── Binary QoL indicators ──────────────────────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    physQOL_bin    = factor(if_else(phys_QOL_perc   >= 60, "Good", "Poor"), levels = c("Poor","Good")),
    psyQOL_bin     = factor(if_else(psych_QOL_perc  >= 60, "Good", "Poor"), levels = c("Poor","Good")),
    social_QOL_bin = factor(if_else(social_QOL_perc >= 60, "Good", "Poor"), levels = c("Poor","Good"))
  )

# ── Employment ─────────────────────────────────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    Employment = factor(
      case_when(
        as.numeric(S3S1Q6) == 1 ~ "Employed",
        as.numeric(S3S1Q6) == 0 ~ "Not employed",
        TRUE ~ NA_character_
      ),
      levels = c("Employed", "Not employed")
    )
  )

# ── Scholarship ────────────────────────────────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    scholarship = factor(
      dplyr::recode(as.character(S2S2Q12), `0` = "No", `1` = "Yes"),
      levels = c("No", "Yes")
    )
  )

# ── Entrepreneurship mindset items ────────────────────────────────────────────
EntrepreneurMindset_items <- Alumni1 %>%
  dplyr::select(S3S3Q1, S3S3Q2, S3S3Q3, S3S3Q4, S3S3Q5, S3S3Q6) %>%
  dplyr::rename(
    Problem_solving = S3S3Q1,
    Managing_money  = S3S3Q2,
    Creativity      = S3S3Q3,
    Persuasion      = S3S3Q4,
    Leadership      = S3S3Q5,
    Decision_making = S3S3Q6
  )

# ── Average entrepreneurship mindset score ────────────────────────────────────
Alumni1 <- Alumni1 %>%
  mutate(
    EntMindset_av_score = rowMeans(
      dplyr::select(., S3S3Q1:S3S3Q6),
      na.rm = TRUE
    )
  )

# ── Aggregated means by grade/gender (for Fig 6 & 7) ─────────────────────────
df_plot <- Alumni1 %>%
  drop_na(grade, EntMindset_av_score, gender) %>%
  group_by(grade, gender) %>%
  summarise(mean_score = mean(EntMindset_av_score, na.rm = TRUE), .groups = "drop")

df_dot <- df_plot %>%
  dplyr::rename(EntMindset_av_score = mean_score)

# ── Resilience items ───────────────────────────────────────────────────────────
Resilience_items <- Alumni1 %>%
  dplyr::select(
    S4S3Q1, S4S3Q2, S4S3Q3, S4S3Q4, S4S3Q5,
    S4S3Q6, S4S3Q7, S4S3Q8, S4S3Q9, S4S3Q10
  )
