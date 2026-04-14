# ── MENTAL HEALTH DATA ───────────────────────────────────────
old <- read_csv("departments/mental_health/data/data_old.csv", show_col_types = FALSE) %>%
  mutate(Source="Old questionnaire",
         Gender=ifelse(Gender %in% c("Male","Female"), Gender, NA_character_),
         Grade =ifelse(is.na(Grade)|Grade=="---", NA_character_, Grade),
         Category=ifelse(is.na(Category)|Category=="---", NA_character_, Category),
         Dep_pct=round(Depression/Dep_max*100,1), Anx_pct=round(Anxiety/Anx_max*100,1))

new <- read_csv("departments/mental_health/data/data_new.csv", show_col_types = FALSE) %>%
  mutate(Source="All questionnaire",
         Dep_pct=round(Depression/Dep_max*100,1), Anx_pct=round(Anxiety/Anx_max*100,1),
         Stage=recode(Category, "ey_nitial_assessment"="EY (Entry Year)",
                      "S4_Reassessment"="S4","S5_Reassessment"="S5","s6_reassessment"="S6"))

item_labels <- read_csv("departments/mental_health/data/item_labels.csv", show_col_types = FALSE)

CLR <- list(coral="#D85A30",coral_l="#F5C4B3",blue="#378ADD",blue_l="#B5D4F4",
            purple="#534AB7",teal="#1D9E75",amber="#EF9F27",green="#639922",gray="#888780")

sev_dep <- function(x) cut(x,c(-Inf,4,9,14,19,Inf),
                           labels=c("None (0-4)","Mild (5-9)","Moderate (10-14)","Mod-severe (15-19)","Severe (20+)"))
sev_anx <- function(x) cut(x,c(-Inf,4,9,14,Inf),
                           labels=c("Minimal (0-4)","Mild (5-9)","Moderate (10-14)","Severe (15+)"))
