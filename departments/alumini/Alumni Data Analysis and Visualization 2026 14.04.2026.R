#**Installing packages***
install.packages("shiny")
library(shiny)

install.packages(c(
  "gapminder", "ggforce", "gh", "globals", "openintro", "profvis", 
  "RSQLite", "shiny", "shinycssloaders", "shinyFeedback", 
  "shinythemes", "testthat", "thematic", "tidyverse", "vroom", 
  "waiter", "xml2", "zeallot" 
))

install.packages("gmodels")
library(gmodels)
#Loading packages
library(gapminder)       # Example global dataset
library(ggforce)         # Advanced ggplot tools
library(gh)              # GitHub API access
library(globals)         # Manage global objects
library(openintro)       # Intro statistics datasets
library(profvis)         # Profile code performance
library(RSQLite)         # SQLite database interface
library(shiny)           # Interactive web apps
library(shinycssloaders) # Shiny loading spinners
library(shinyFeedback)   # Shiny input feedback
library(shinythemes)     # Shiny app themes
library(testthat)        # Unit testing framework
library(thematic)        # Automatic plot theming
library(tidyverse)       # Data science tools
library(vroom)           # Fast file reading
library(waiter)          # Shiny loading screens
library(xml2)            # XML HTML parsing
library(zeallot)         # Multiple value assignment

library(tidyverse)
library(psych)
library(GGally)
library(scales)
library(ggpubr)       
library(shiny)

#Set a working directory

setwd("~/ASYV Data Analytics 2026/ASYV_Project_2026")

Alumni<-read.csv("Alumni_Data1.csv")
head(Alumni)

summary(Alumni)

# Tabulate a vector

table(Alumni$S1S1Q4)
Alumni1 <- subset(Alumni, !(S1S1Q4 %in% c("Icyizere", "Urumuli", "Intwari")))

table(Alumni1$S1S1Q4)

head(Alumni1$S1S1Q4)

table(Alumni1$S1S2Q1)

#Grade
Alumni1 <- Alumni1 %>%
  mutate(grade = factor(na_if(S1S1Q4),
                        `1` = "Imena",
                        `2` = "Isonga",
                        `3` = "Umurage",
                        `4` = "Umucyo",
                        `5` = "Ishyaka",
                        `6` = "Inganji"),
         levels=c("Imena","Isonga","Umurage",
                  "Umucyo","Ishyaka","Inganji"))

library(dplyr)

Alumni1 <- Alumni1 %>%
  mutate(
    grade = factor(
      na_if(S1S1Q4, ""),
      levels = c("Imena", "Isonga", "Umurage", "Umucyo", "Ishyaka", "Inganji")
    )
  )


#Gender
Alumni1 <- Alumni1 %>%
  mutate(gender = factor(
    recode(as.character(S1S2Q1),
           `1` = "Male",
           `2` = "Female"),
    levels = c("Male", "Female")
  ))

###################################################################
#WHO Quality of Life
#------------------------------------------------------------------

# Load required packages
library(dplyr)
library(labelled)
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(scales)

# Recode and create Health Status variable

Alumni1 <- Alumni1 %>%
  mutate(
    Health_Status = case_when(
      S4S2Q1 == 0 ~ "Healthy",
      S4S2Q1 == 1 ~ "Unwell",
      S4S2Q1 == 8 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    Health_Status = factor(
      Health_Status,
      levels = c("Healthy", "Unwell")
    )
  )














#Cross tabulation

attach(Alumni1)

#Cross tabulation of of gender by grade
CrossTable(x=grade, y=gender,prop.c = F,prop.t = F,
           prop.chisq = F,chisq =T,format ="SPSS",digits = 1)



#Cross tabulation of scholarship by grade and sex

table(S2S2Q12)
str(S2S2Q12)
Alumni1 <- Alumni1 %>%
  mutate(scholarship= factor(
    recode(as.character(S2S2Q12),
           `0` = "No",
           `1` = "Yes"),
    levels = c("No", "Yes")
  ))

table(Alumni1$scholarship)

str(Alumni1$scholarship)

#Entrepreneurship Mindset

EntrepreneurMindset_items <- Alumni1 |>
  select(
    S3S3Q1,
    S3S3Q2,
    S3S3Q3,
    S3S3Q4,
    S3S3Q5,
    S3S3Q6
  ) |>
  rename(
    Problem_solving = S3S3Q1,
    Managing_money  = S3S3Q2,
    Creativity      = S3S3Q3,
    Persuasion      = S3S3Q4,
    Leadership      = S3S3Q5,
    Decision_making = S3S3Q6
  )

psych::describe(EntrepreneurMindset_items)



#Tablulation
attach(Alumni1)
CrossTable(x=grade, y=scholarship,prop.c = F,prop.t = F,prop.r = T,
           prop.chisq = F,chisq =T,format ="SPSS",digits = 1)

#Full scholarship
head(Alumni1$S2S6Q2)



#Employment

Alumni1 <- Alumni1 %>%
  mutate(employment= factor(
    recode(as.character(S3S1Q6),
           `0` = "No",
           `1` = "Yes"),
    levels = c("No", "Yes")
  ))

table(Alumni1$employment)

str(Alumni1$employment)


#Mental Health
Alumni1 <- Alumni1 %>%
  mutate(
    Status = factor(
      as.numeric(S4S2Q28),
      levels = c(1, 2, 3, 4, 5),
      labels = c("Never", "Seldom", "Quite often", "Very often", "Always"),
      ordered = TRUE
    )
  )


Alumni1<-Alumni1 %>%
  mutate(
  Ment_Dis = case_when(
    Status %in% c("Never", "Seldom", "Quite often") ~ 1,
    Status %in% c("Very often", "Always") ~ 0,
    TRUE ~ NA_real_
  )
)

Alumni1 <- Alumni1 %>%
  mutate(
    Ment_Dis = factor(
      Ment_Dis,
      levels = c(1, 0),
      labels = c("Positive Feelings", "Negative Feelings")
    )
  )


#Rating of quality of life
#S4S2Q3. How would you rate your quality of life?
#Mental Health
Alumni1 <- Alumni1 %>%
  mutate(
    QOL_rating = factor(
      as.numeric(S4S2Q3),
      levels = c(1, 2, 3, 4, 5),
      labels = c("Very poor", "Poor", "Average", "Good", "Very good"),
      ordered = TRUE))
      
#Health Satisfaction
Alumni1 <- Alumni1 %>%
  mutate(
    H_satisf_rating = factor(
      as.numeric(S4S2Q4),
      levels = c(1, 2, 3, 4, 5),
      labels = c("Very poor", "Poor", "Average", "Good", "Very good"),
      ordered = TRUE))


#Reverse scoring

Alumni1 <- Alumni1 |>
  mutate(
    S4S2Q5_rev  = 6 - S4S2Q5,   # Pain
    S4S2Q6_rev  = 6 - S4S2Q6,   # Need medical treatment
    S4S2Q28_rev = 6 - S4S2Q28   # Negative feelings
  )

#Domain score calculation

#Physical Health

Alumni1$phys_mean <- rowMeans(
  Alumni1[, c(
    "S4S2Q5_rev", "S4S2Q6_rev", "S4S2Q12",
    "S4S2Q17", "S4S2Q18", "S4S2Q19", "S4S2Q20"
  )],
  na.rm = TRUE
)


#Psychological Health
Alumni1$psych_mean <- rowMeans(
  Alumni1[, c(
    "S4S2Q7", "S4S2Q8", "S4S2Q9",
    "S4S2Q13", "S4S2Q21", "S4S2Q28_rev"
  )],
  na.rm = TRUE
)


# Social Relationships (3 items)
Alumni1$social_mean <- rowMeans(
  Alumni1[, c(
    "S4S2Q22", "S4S2Q23", "S4S2Q24"
  )],
  na.rm = TRUE
)



Alumni1$env_mean <- rowMeans(
  Alumni1[, c(
    "S4S2Q10", "S4S2Q11", "S4S2Q14", "S4S2Q15",
    "S4S2Q16", "S4S2Q25", "S4S2Q26", "S4S2Q27"
  )],
  na.rm = TRUE
)


# 3. Transform to WHO 0–100 scale
# --------------------------------------------------
Alumni1$phys_QOL_perc   <- 20 * Alumni1$phys_mean
Alumni1$psych_QOL_perc  <- 20 * Alumni1$psych_mean
Alumni1$social_QOL_perc  <- 20 * Alumni1$social_mean
Alumni1$env_QOL_perc    <- 20 * Alumni1$env_mean

# Create binary Physical QOL indicator (>= 60 = 1, otherwise = 0)
#library(dplyr)

Alumni1 <- Alumni1 %>%
  mutate(
    physQOL_bin = if_else(phys_QOL_perc >= 60, 1, 0),
    physQOL_bin = factor(
      physQOL_bin,
      levels = c(0, 1),
      labels = c("Poor", "Good")
    )
  )

# Create binary Psychological QOL indicator (>= 60 = 1, otherwise = 0)
Alumni1 <- Alumni1 %>%
  mutate(
    psyQOL_bin = if_else(psych_QOL_perc >= 60, 1, 0),
    psyQOL_bin = factor(
      psyQOL_bin,
      levels = c(0, 1),
      labels = c("Poor", "Good")
    )
  )


# Create binary Social QOL indicator (>= 60 = 1, otherwise = 0)
Alumni1 <- Alumni1 %>%
  mutate(
    social_QOL_bin = if_else(social_QOL_perc >= 60, 1, 0),
    social_QOL_bin = factor(
      social_QOL_bin,
      levels = c(0, 1),
      labels = c("Poor", "Good")
    )
  )



#Self-reported employment

Alumni1 <- Alumni1 %>%
  mutate(
    Employment = factor(
      S3S1Q6,
      levels = c(1, 0),
      labels = c("Employed", "Not employed")
    )
  )
#Average score for Entrepreneurship mindset
Alumni1 <- Alumni1 %>%
  mutate(
    EntMindset_av_score = rowMeans(
      dplyr::select(., S3S3Q1:S3S3Q6),
      na.rm = TRUE
    )
  )


##################################################
##    Psychosocial Assessment
##################################################

##Category of Assessment

library(dplyr)

Psycho <- Psycho %>%
  mutate(
    Category = case_when(
      Category == "ey_nitial_assessment" ~ "EY",
      Category == "S4_Reassessment" ~ "Grade 10",
      Category == "s6_reassessment" ~ "Grade 12",
      TRUE ~ NA_character_
    ),
    
    Category = factor(
      Category,
      levels = c("EY", "Grade 10", "Grade 12")
    )
  )
#Depression with PHQ-9

# 2. Calculate Total Score
# PHQ-9 sum is simply the row-wise sum of all 8 items
Psycho <- Psycho %>%
  mutate(
    PHQ8_score = rowSums(
      select(., phq9_anhedonia:phq9_suicidal_ideation),
      na.rm = TRUE
    ),
    
    PHQ9_score = round(PHQ8_score * (9 / 8)) %>% as.integer()#Bring to 9 items
  )

# 3. Categorize Severity
# Cutoffs: <5 None, 5-9 Mild, 10-14 Moderate, 15-19 Mod. Severe, 20-27 Severe
Psycho <- Psycho %>%
  mutate(PHQ_severity = case_when(
    PHQ9_score < 5 ~ "None-Minimal",
    PHQ9_score < 10 ~ "Mild",
    PHQ9_score < 15 ~ "Moderate",
    PHQ9_score < 20 ~ "Moderately Severe",
    PHQ9_score >= 20 ~ "Severe"
  ))


attach(Alumni1)
install.packages(c("socviz", "tidyverse"))

library(socviz)
library(tidyverse)
ment_by_grade <- Alumni1 |>
  group_by(grade, Status) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n),
         pct = round((freq*100), 0))

#Figure 1: Percentage of alumni by level mental health symptoms experienced overtime

fig1 <- ggplot(
  ment_by_grade |> drop_na(Status),
  aes(x = grade, y = pct)
) +
  geom_bar(
    aes(fill = Status),
    stat = "identity",
    position = "stack",
    width = 0.7
  ) +
  geom_text(
    aes(
      label = percent(pct / 100, accuracy = 1),
      group = Status
    ),
    position = position_stack(vjust = 0.8),
    color = "white",
    size = 3,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1)
  ) +
  scale_fill_manual(
    values = c(
      "#0f2f36",
      "#48705f",
      "navy",
      "#f49b45",
      "#C00000"
    )
  ) +
  labs(
    x = "Cohort",
    y = "Percentage",
    fill = "Frequency of Symptoms",
    title = "Mental health symptoms",
    subtitle = "Percentage (%) of Alumni self-reported currently experiencing of blue moods and anxiety",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 1",fontface = "bold"
  ) +
  theme(
    legend.position = "right"
  )

fig1




##Fig 2: Percentage of Alumni classified as healthy or Ill for blue mood or anxiety



# Use position = position_dodge() 

disord_by_grade <- Alumni1 |>
  group_by(grade, Ment_Dis) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n),
         pct = round((freq*100), 0))

Fig2<-ggplot(
  disord_by_grade |> drop_na(Ment_Dis),
  aes(x = grade, y = pct, group = Ment_Dis)
) +
  geom_hline(
    yintercept = 50,
    lty ="solid",
    color = "darkred",
    linewidth = 3
  ) +
  geom_linerange(
    aes(ymin = 0, ymax = 100),
    color = "white",
    size = 10,
    position = position_dodge(width = 0.7)
  ) +
  geom_point(
    aes(color = Ment_Dis),
    position = position_dodge(width = 0.7),
    size = 20
  ) +
  geom_text(
    aes(label = percent(pct / 100, accuracy = 1)),
    position = position_dodge(width = 0.7),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "Positive Feelings" = "#48705f",
      "Negative Feelings" = "#f49b45"
    )
  ) +
  labs(
    x = "Cohort (Grade of 2018-2024)",
    y = "Percentage",
    color = "Prevalence of positive feelings",
    title = "Status of experienced feelings",
    subtitle = "Percentage (%) of Alumni self-reported currently experiencing of blue moods and anxiety",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 2"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "left",
    plot.title = element_text(face = "bold"),
    plot.tag   = element_text(face = "bold")
  )

Fig2


##Employment rate

employment_by_grade <- Alumni1 |>
  group_by(grade, Employment) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n),
         pct = round((freq*100), 0))

Fig3<-ggplot(
  employment_by_grade |> drop_na(Employment),
  aes(x = grade, y = pct, group = Employment)
) +
  geom_hline(
    yintercept = 50,
    lty =1,
    color = "darkred",
    linewidth = 0.1
  ) +
  geom_linerange(
    aes(ymin = 0, ymax = 100),
    color = "white",
    size = 10,
    position = position_dodge(width = 0.7)
  ) +
  geom_point(
    aes(color = Employment),
    position = position_dodge(width = 0.7),
    size = 20
  ) +
  geom_text(
    aes(label = percent(pct / 100, accuracy = 1)),
    position = position_dodge(width = 0.7),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "Employed" = "#48705f",
      "Not employed" = "black"
    )
  ) +
  labs(
    x = "Cohort (Grade of 2018-2024)",
    y = "Percentage",
    color = "Employment",
    title = "Employment rate",
    subtitle = "Percentage (%) of Alumni (grade of 2018-2024) self-reported currently employed",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 3"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "left",
    plot.title = element_text(face = "bold"),
    plot.tag   = element_text(face = "bold")
  )

Fig3



# Entrepreneurship mindset
Fig4 <- EntrepreneurMindset_items |>
  pivot_longer(everything()) |>
  filter(!is.na(value)) |>
  group_by(name, value) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(name) |>
  mutate(pct = n / sum(n)) |>
  ggplot(
    aes(
      x = factor(value),
      y = pct,
      fill = factor(value)
    )
  ) +
  geom_col(width = 0.98) +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.85),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  facet_wrap(
    ~ name,
    ncol = 3,
    labeller = as_labeller(c(
      Problem_solving = "Problem Solving",
      Managing_money  = "Managing Money",
      Creativity      = "Creativity",
      Persuasion      = "Persuasion",
      Leadership      = "Leadership",
      Decision_making = "Decision Making"
    ))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_discrete(
    labels = c("1", "2", "3", "4", "5")
  ) +
  scale_fill_manual(
    name = "Level of ability",
    values = c(
      "1" = "#C00000",
      "2" = "#f49b45",
      "3" = "navy",
      "4" = "#48705f",
      "5" = "#0f2f36"
    ),
    labels = c(
      "1" = "Much worse",
      "2" = "A little worse",
      "3" = "About the same",
      "4" = "A little better",
      "5" = "Much better"
    )
  ) +
  labs(
    x = "Level (1 = Much worse … 5 = Much better)",
    y = "Percentage of alumni",
    title = "Perceived Entrepreneurial Mindset",
    subtitle = "Percentage of alumni by level of perceived entrepreneurial ability",
    tag = "Figure 4"
  ) +
  theme_minimal() +
  theme(
    strip.text   = element_text(face = "bold"),
    axis.text.x  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.title   = element_text(face = "bold"),
    plot.tag     = element_text(face = "bold")
  )

Fig4


# Average score for Entrepreneurship mindset
# Color by gender with custom palette
Fig5<-ggerrorplot(
  Alumni1 |>
    drop_na(grade, EntMindset_av_score, gender),
  x = "grade",
  y = "EntMindset_av_score",
  desc_stat = "mean_sd",
  color = "gender",
  width = 2,                      # thicker bars
  position = position_dodge(0.8)  # tighter spacing
) +
  # Sample mean line
  geom_hline(
    yintercept = 3.875,
    linetype = "dashed",
    color = "darkred"
  ) +
  # Label for the sample mean
  annotate(
    "text",
    x = Inf,                      # place at right edge
    y = 3.875,
    label = "Sample mean = 3.875",
    hjust = 1.05,
    vjust = -0.5,
    color = "darkred",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "Male"   = "#48705f",
      "Female" = "#f49b45"
    )
  ) +
  labs(
    x = "Cohort (Grade)",
    y = "Average score",
    color = "Gender",
    title = "Average score for Entrepreneurial Mindset by grade and gender",
    subtitle = "Values above 3 indicate perceived improvement (scale 1–5)",
    tag = "Figure 5",
    caption = "Data source: Alumni Survey 2025"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title   = element_text(face = "bold"),
    plot.tag     = element_text(face = "bold")
  )
Fig5


Fig6 <- ggplot(
  df_plot,
  aes(x = grade, y = mean_score, fill = gender)
) +
  geom_bar(
    stat = "identity",
    color = c("#48705f","#f49b45")
    width = 0.7,
    position = position_dodge(0.8)
  ) +
  geom_hline(
    yintercept = 3.875,
    linetype = "dashed",
    color = "darkred"
  ) +
  annotate(
    "text",
    x = Inf,
    y = 3.875,
    label = "Sample mean = 3.875",
    hjust = 1.05,
    vjust = -0.5,
    color = "darkred",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Male"   = "#48705f",
      "Female" = "#f49b45"
    )
  ) +
  ylim(0, 5) +   # ✅ fix y-axis range
  labs(
    x = "Cohort (Grade)",
    y = "Average Entrepreneurial Mindset score",
    fill = "Gender",
    title = "Average score for Entrepreneurial Mindset by grade and gender",
    subtitle = "Values above 3 indicate perceived improvement (scale 1–5)",
    tag = "Figure 4",
    caption = "Data source: Alumni Survey 2025"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title   = element_text(face = "bold"),
    plot.tag     = element_text(face = "bold")
  )

Fig6

Fig6 <- ggplot(
  df_plot,
  aes(x = grade, y = mean_score, fill = gender)
) +
  geom_bar(
    stat = "identity",
    color = "black",     # outline color only
    width = 0.7,
    position = position_dodge(0.8)
  ) +
  geom_hline(
    yintercept = 3.875,
    linetype = "dashed",
    color = "darkred"
  ) +
  annotate(
    "text",
    x = Inf,
    y = 3.875,
    label = "Sample mean = 3.875",
    hjust = 1.05,
    vjust = -0.5,
    color = "darkred",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Male"   = "#48705f",
      "Female" = "#f49b45"
    )
  ) +
  ylim(0, 5) +
  labs(
    x = "Cohort (Grade)",
    y = "Average Entrepreneurial Mindset score",
    fill = "Gender",
    title = "Average score for Entrepreneurial Mindset by grade and gender",
    subtitle = "Values above 3 indicate perceived improvement (scale 1–5)",
    tag = "Figure 6",
    caption = "Data source: Alumni Survey 2025"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title   = element_text(face = "bold"),
    plot.tag     = element_text(face = "bold")
  )

Fig6


Fig7 <- ggdotchart(
  df_dot,
  x = "grade",
  y = "EntMindset_av_score",
  color = "gender",
  palette = c("#48705f", "#f49b45"),   # Male / Female
  add = "segments",
  dot.size = 20,
  segment.size = 11,
  
  # ✅ VALUE LABELS INSIDE DOTS
  
  label = round(df_dot$EntMindset_av_score, 1),
  label.pos = "center",
  font.label = list(color = "white", size = 10, face = "bold"),
  
  ylab = "Average score (1-5)",
  xlab = FALSE,
  ggtheme = theme_pubr()
) +
  scale_y_continuous(limits = c(0, 5)) +
  geom_hline(
    yintercept = 3.875,
    linetype = "dashed",
    color = "darkred"
  ) +
  annotate(
    "text",
    x = Inf,
    y = 3.875,
    label = "Sample mean = 3.875",
    hjust = 1.5,
    vjust = -0.5,
    color = "darkred",
    fontface = "bold"
  ) +
  labs(
    title = "Average Entrepreneurial Mindset by grade and gender",
    subtitle = "Dots represent mean scores; values above 3 indicate significantly  improved mindset towards work readiness and entrepreneurship mindset, likely to enable the graduate to acquire a job or an income generating activity (IGA).",
    color = "Gender",
    tag = "Figure 7",
    caption = "Data source: Alumni Survey 2025"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 0.5),
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.tag   = element_text(face = "bold")
  )

Fig7

#Health Status

##Health Status

Healstat_by_grade <- Alumni1 |>
  drop_na(Health_Status) |>
  group_by(grade, Health_Status) |>
  summarize(n = n(), .groups = "drop_last") |>
  mutate(
    freq = n / sum(n),
    pct = round((freq * 100), 0)
  )

Fig8<-ggplot(Healstat_by_grade, aes(x = grade, y = pct, group =Health_Status)
) +
  geom_hline(
    yintercept = 50,
    lty ="solid",
    color = "darkred",
    linewidth = 0.5
  ) +
  geom_linerange(
    aes(ymin = 0, ymax = 100),
    color = "white",
    size = 10,
    position = position_dodge(width = 0.7)
  ) +
  geom_point(
    aes(color = Health_Status),
    position = position_dodge(width = 0.7),
    size = 20
  ) +
  geom_text(
    aes(label = percent(pct / 100, accuracy = 1)),
    position = position_dodge(width = 0.7),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "Healthy" = "#48705f",
      "Unwell" = "#f49b45"
    )
  ) +
  labs(
    x = "Cohort (Grade of 2018-2024)",
    y = "Percentage",
    color = "Health Status",
    title = "Health Status",
    subtitle = "Percentage (%) of Alumni (grade of 2018-2024) self-reported Healthy Status",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 8"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "left",
    plot.title = element_text(face = "bold"),
    plot.tag   = element_text(face = "bold")
  )

Fig8


###############################################################
##Rating of quality of life
###############################################################
Fig9 <- QOL_rating_items |>
  pivot_longer(everything()) |>
  filter(!is.na(value)) |>
  mutate(score = as.numeric(value)) |>
  group_by(name, score) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(name) |>
  mutate(pct = n / sum(n)) |>
  ggplot(
    aes(
      x = score,
      y = pct,
      fill = factor(score)
    )
  ) +
  geom_col(width = 0.98) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.85),
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  facet_wrap(
    ~ name,
    ncol = 3,
    labeller = as_labeller(c(
      QOL_rating      = "Quality of Life Rating (%)",
      H_satisf_rating = "Health Satisfaction Rating (%)"
    ))
  ) +
  scale_x_continuous(
    breaks = 1:5,
    limits = c(0.5, 5.5)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    name = "Rating",
    values = c(
      "1" = "#C00000",
      "2" = "#f49b45",
      "3" = "navy",
      "4" = "#48705f",
      "5" = "#0f2f36"
    ),
    labels = c(
      "1" = "Very poor",
      "2" = "Poor",
      "3" = "Average",
      "4" = "Good",
      "5" = "Very good"
    )
  ) +
  labs(
    x = "Rating (1 = Very poor … 5 = Very good)",
    y = "Percentage of alumni",
    title = "Perceived Quality of Life and Health Satisfaction",
    subtitle = "Percentage of alumni by level of life and health satisfaction",
    tag = "Figure 9",
    size=11
  ) +
  theme_minimal() +
  theme(
    strip.text   = element_text(face = "bold"),
    axis.text.x  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.title   = element_text(face = "bold"),
    plot.tag     = element_text(face = "bold")
  )

Fig9


##Physical quality of life

Fig10<-ggerrorplot(
  Alumni1 |> dplyr::filter(!is.na(psych_QOL_perc)),
  x = "grade",
  y = "phys_QOL_perc",
  desc_stat = "mean_sd",
  color = "gender",
  position = position_dodge(0.3),
  dot.size = 6,
  errorbar.width = 0.25
) +
  scale_color_manual(
    values = c(
      "Male"   = "#48705f",
      "Female" = "#f49b45"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = "Cohort",
    y = "WHOQOL score (0–100)",
    color = "Gender",
    title = "Physical Health Quality of Life",
    subtitle = "Mean WHOQOL-BREF physical health scores by cohort and gender",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 10"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    plot.caption  = element_text(face = "bold"),
    plot.tag      = element_text(face = "bold"),
    legend.title  = element_text(face = "bold"),
    axis.title    = element_text(face = "bold")
  )

Fig10

##Psychological quality of life

Fig11<-ggerrorplot(
  Alumni1 |> dplyr::filter(!is.na(psych_QOL_perc)),
  x = "grade",
  y = "psych_QOL_perc",
  desc_stat = "mean_sd",
  color = "gender",
  position = position_dodge(0.3),
  dot.size = 10,
  errorbar.width = 0.1
) +
  scale_color_manual(
    values = c(
      "Male"   = "#48705f",
      "Female" = "#f49b45"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = "Cohort",
    y = "WHOQOL score (0–100)",
    color = "Gender",
    title = "Psychological Health Quality of Life",
    subtitle = "Mean WHOQOL-BREF Psychological health scores by cohort and gender",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 11"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    plot.caption  = element_text(face = "bold"),
    plot.tag      = element_text(face = "bold"),
    legend.title  = element_text(face = "bold"),
    axis.title    = element_text(face = "bold")
  )

Fig11

##Environmental quality of life

Fig12<-ggerrorplot(
  Alumni1 |> dplyr::filter(!is.na(env_QOL_perc)),
  x = "grade",
  y = "env_QOL_perc",
  desc_stat = "mean_sd",
  color = "gender",
  position = position_dodge(0.3),
  dot.size = 10,
  errorbar.width = 0.1
) +
  scale_color_manual(
    values = c(
      "Male"   = "#48705f",
      "Female" = "#f49b45"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = "Cohort",
    y = "WHOQOL score (0–100)",
    color = "Gender",
    title = "Environmental Quality of Life",
    subtitle = "Mean WHOQOL-BREF environmental health scores by cohort and gender",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 12"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    plot.caption  = element_text(face = "bold"),
    plot.tag      = element_text(face = "bold"),
    legend.title  = element_text(face = "bold"),
    axis.title    = element_text(face = "bold")
  )

Fig12


##Social quality of life

Fig13<-ggerrorplot(
  Alumni1 |> dplyr::filter(!is.na(social_QOL_perc)),
  x = "grade",
  y = "social_QOL_perc",
  desc_stat = "mean_sd",
  color = "gender",
  position = position_dodge(0.3),
  dot.size = 10,
  errorbar.width = 0.1
) +
  scale_color_manual(
    values = c(
      "Male"   = "#48705f",
      "Female" = "#f49b45"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = "Cohort",
    y = "WHOQOL score (0–100)",
    color = "Gender",
    title = "Social and Relational Quality of Life",
    subtitle = "Mean WHOQOL-BREF Social and Relational health scores by cohort and gender",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 13"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    plot.caption  = element_text(face = "bold"),
    plot.tag      = element_text(face = "bold"),
    legend.title  = element_text(face = "bold"),
    axis.title    = element_text(face = "bold")
  )

Fig13

# ------------------------------------------------------------
# Required packages
# ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# Select resilience scale items (S4S3Q1–S4S3Q10)
# ------------------------------------------------------------
Resilience_items <- Alumni1 |>
  select(
    S4S3Q1, S4S3Q2, S4S3Q3, S4S3Q4, S4S3Q5,
    S4S3Q6, S4S3Q7, S4S3Q8, S4S3Q9, S4S3Q10
  )

# ------------------------------------------------------------
# Analyse and visualise Likert distributions (Figure 9 style)
# ------------------------------------------------------------
Fig14 <- Resilience_items |>
  pivot_longer(everything(), names_to = "item", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(score = as.numeric(value)) |>
  group_by(item, score) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(item) |>
  mutate(pct = n / sum(n)) |>
  ggplot(
    aes(
      x = score,
      y = pct,
      fill = factor(score)
    )
  ) +
  geom_col(width = 0.98) +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.85),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  facet_wrap(
    ~ item,
    ncol = 5,
    labeller = as_labeller(c(
      S4S3Q1  = "Adapt to change",
      S4S3Q2  = "Deal with challenges",
      S4S3Q3  = "See humor in problems",
      S4S3Q4  = "Stress makes me stronger",
      S4S3Q5  = "Bounce back in hardship",
      S4S3Q6  = "Achieve goals despite obstacles",
      S4S3Q7  = "Stay focused",
      S4S3Q8  = "Stay courageous",
      S4S3Q9  = "Strong in life challenges",
      S4S3Q10 = "Handle painful feelings"
    ))
  ) +
  scale_x_continuous(
    breaks = 1:5,
    limits = c(0.5, 5.5)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    name = "Response level",
    values = c(
      "1" = "#C00000",
      "2" = "#f49b45",
      "3" = "navy",
      "4" = "#48705f",
      "5" = "#0f2f36"
    ),
    labels = c(
      "1" = "Strongly disagree",
      "2" = "Disagree",
      "3" = "Neutral",
      "4" = "Agree",
      "5" = "Strongly agree"
    )
  ) +
  labs(
    x = "Agreement level (1 = Strongly disagree … 5 = Strongly agree)",
    y = "Percentage of alumni",
    title = "Resilience and Coping Capacity",
    subtitle = "Percentage distribution of alumni responses to resilience and coping statements",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 14"
  ) +
  theme_minimal() +
  theme(
    strip.text   = element_text(face = "bold"),
    axis.text.x  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.title   = element_text(face = "bold"),
    plot.subtitle= element_text(face = "bold"),
    plot.caption = element_text(face = "bold"),
    plot.tag     = element_text(face = "bold")
  )

# ------------------------------------------------------------
# Print figure
# ------------------------------------------------------------
Fig14


##Category of Physical Quality of life
# ================================

library(dplyr)
library(ggplot2)
library(scales)

physQOLbin_by_grade <- Alumni1 |>
  drop_na(physQOL_bin) |>
  group_by(grade, physQOL_bin) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(grade) |>
  mutate(
    freq = n / sum(n),
    pct = freq * 100
  )

Fig15 <- ggplot(
  physQOLbin_by_grade,
  aes(x = grade, y = pct, color = physQOL_bin, group = physQOL_bin)
) +
  geom_hline(
    yintercept = 60,
    linetype = 1,
    color = "darkred",
    linewidth = 2
  ) +
  geom_point(
    position = position_dodge(width = 0.75),
    size = 20
  ) +
  geom_text(
    aes(label = percent(pct / 100, accuracy = 1)),
    position = position_dodge(width = 0.90),
    vjust = -0.5,
    size = 4,
    col = "white"
  ) +
  scale_color_manual(
    values = c(
      "Poor" = "#f49b45",
      "Good" = "#48705f"
    )
  ) +
  scale_y_continuous(
    breaks = c(20, 40, 60, 80, 100),
    limits = c(0, 100)
  ) +
  labs(
    x = "Cohort (Grade of 2018-2024)",
    y = "Percentage",
    color = "Physical QOL",
    title = "Perceived Physical Quality of Life",
    subtitle = "Percentage (%) of Alumni self-reporting Physical QOL status",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 15"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "left",
    plot.title = element_text(face = "bold"),
    plot.tag = element_text(face = "bold")
  )

Fig15


##Category of Psychological Quality of life
# ================================
# Step 1: Ensure consistent factor labels
Alumni1 <- Alumni1 %>%
  mutate(
    psyQOL_bin = if_else(psych_QOL_perc >= 60, "Good", "Poor"),
    psyQOL_bin = factor(psyQOL_bin, levels = c("Poor", "Good"))
  )
psyQOLbin_by_grade <- Alumni1 |>
  drop_na(psyQOL_bin) |>
  group_by(grade, psyQOL_bin) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(grade) |>
  mutate(
    freq = n / sum(n),
    pct = freq * 100
  )

Fig16 <- ggplot(
  psyQOLbin_by_grade,
  aes(
    x = grade,
    y = pct,
    color = psyQOL_bin,
    group = psyQOL_bin
  )
) +
  geom_hline(
    yintercept = 60,
    linetype = 1,
    color = "darkred",
    linewidth = 1.5
  ) +
  geom_point(
    position = position_dodge(width = 0.75),
    size = 18
  ) +
  geom_text(
    aes(label = scales::percent(pct / 100, accuracy = 1)),
    position = position_dodge(width = 0.75),
    vjust = -0.6,
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "Poor" = "#f49b45",
      "Good" = "#48705f"
    )
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80, 100),
    limits = c(0, 100)
  ) +
  labs(
    x = "Cohort (Grade of 2018–2024)",
    y = "Percentage",
    color = "Psychological QOL",
    title = "Perceived Psychological Quality of Life",
    subtitle = "Percentage (%) of alumni self-reporting psychological QOL status",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 16"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "left",
    plot.title = element_text(face = "bold"),
    plot.tag   = element_text(face = "bold")
  )

Fig16



##Category of Social and Relational Quality of life
# ================================

# Step 2: Prepare aggregated data
SocQOLbin_by_grade <- Alumni1 |>
  drop_na(social_QOL_bin) |>
  group_by(grade, social_QOL_bin) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(grade) |>
  mutate(
    freq = n / sum(n),
    pct = freq * 100
  )
Fig17 <- ggplot(
  SocQOLbin_by_grade,
  aes(x = grade, y = pct, color = social_QOL_bin, group = social_QOL_bin)
) +
  geom_hline(
    yintercept = 60,
    linetype = 1,
    color = "darkred",
    linewidth = 2
  ) +
  geom_point(
    position = position_dodge(width = 0.75),
    size = 20
  ) +
  geom_text(
    aes(label = percent(pct / 100, accuracy = 1)),
    position = position_dodge(width = 0.90),
    vjust = -0.5,
    size = 4,
    col = "white"
  ) +
  scale_color_manual(
    values = c(
      "Poor" = "#f49b45",
      "Good" = "#48705f"
    )
  ) +
  scale_y_continuous(
    breaks = c(20, 40, 60, 80, 100),
    limits = c(0, 100)
  ) +
  labs(
    x = "Cohort (Grade of 2018-2024)",
    y = "Percentage",
    color = "Social and Relational QOL",
    title = "Perceived Social and Relational Quality of Life",
    subtitle = "Percentage (%) of Alumni self-reporting Social and relational QOL status",
    caption = "Data source: Alumni Survey 2025",
    tag = "Figure 17"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "left",
    plot.title = element_text(face = "bold"),
    plot.tag = element_text(face = "bold")
  )
Fig17




# Visualize Depression Severity Distribution





