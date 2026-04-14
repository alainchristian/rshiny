# ══════════════════════════════════════════════════════════════════════════════
# Alumni Department — Server Logic
# ══════════════════════════════════════════════════════════════════════════════

# ── OVERVIEW: info boxes ──────────────────────────────────────────────────────
output$al_total_respondents <- renderbs4InfoBox({
  bs4InfoBox(
    title    = "Total Respondents",
    value    = nrow(al_raw),
    icon     = icon("users"),
    color    = "success",
    fill     = TRUE
  )
})

output$al_pct_employed <- renderbs4InfoBox({
  employed_n <- sum(Alumni1$Employment == "Employed", na.rm = TRUE)
  valid_n    <- sum(!is.na(Alumni1$Employment))   # excludes code-8 & blanks
  pct        <- round(employed_n / valid_n * 100, 1)
  bs4InfoBox(
    title    = "Employment Rate",
    value    = paste0(pct, "%"),
    icon     = icon("briefcase"),
    color    = "success",
    fill     = TRUE
  )
})

output$al_n_grades <- renderbs4InfoBox({
  n <- Alumni1 %>% drop_na(grade) %>% pull(grade) %>% droplevels() %>% nlevels()
  bs4InfoBox(
    title    = "Cohorts Represented",
    value    = n,
    icon     = icon("graduation-cap"),
    color    = "warning",
    fill     = TRUE
  )
})

output$al_pct_female <- renderbs4InfoBox({
  female_n <- sum(Alumni1$gender == "Female", na.rm = TRUE)
  valid_n  <- sum(!is.na(Alumni1$gender))   # excludes blanks & invalid codes
  pct      <- round(female_n / valid_n * 100, 1)
  bs4InfoBox(
    title    = "Female Alumni",
    value    = paste0(pct, "%"),
    icon     = icon("person-dress"),
    color    = "info",
    fill     = TRUE
  )
})

# ── OVERVIEW: plots ───────────────────────────────────────────────────────────
output$al_grade_bar <- renderPlot({
  grade_counts <- Alumni1 %>%
    drop_na(grade) %>%
    count(grade) %>%
    mutate(pct = round(n / sum(n) * 100, 1))

  ggplot(grade_counts, aes(x = grade, y = n, fill = grade)) +
    geom_col(width = 0.7, color = "white") +
    geom_text(
      aes(label = paste0(n, "\n(", pct, "%)")),
      vjust = -0.3, size = 3.5, fontface = "bold"
    ) +
    scale_fill_manual(values = c(
      "Imena"   = "#0f2f36",
      "Isonga"  = "#48705f",
      "Umurage" = "navy",
      "Umucyo"  = "#378ADD",
      "Ishyaka" = "#f49b45",
      "Inganji" = "#C00000"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Cohort", y = "Number of respondents",
         caption = "Data source: Alumni Survey 2025") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text  = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      plot.caption = element_text(color = "#64748B")
    )
})

output$al_gender_bar <- renderPlot({
  gender_counts <- Alumni1 %>%
    drop_na(gender) %>%
    count(gender) %>%
    mutate(pct = round(n / sum(n) * 100, 1))

  ggplot(gender_counts, aes(x = gender, y = n, fill = gender)) +
    geom_col(width = 0.5, color = "white") +
    geom_text(
      aes(label = paste0(n, "\n(", pct, "%)")),
      vjust = -0.3, size = 4, fontface = "bold"
    ) +
    scale_fill_manual(values = c("Male" = "#48705f", "Female" = "#f49b45")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = NULL, y = "Number of respondents",
         caption = "Data source: Alumni Survey 2025") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text  = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      plot.caption = element_text(color = "#64748B")
    )
})

# ── FIGURE 1: Mental health symptoms by grade (stacked bar) ──────────────────
output$al_fig1 <- renderPlot({
  ment_by_grade <- Alumni1 %>%
    group_by(grade, Status) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(freq = n / sum(n), pct = round(freq * 100, 0))

  ggplot(
    ment_by_grade %>% drop_na(Status),
    aes(x = grade, y = pct)
  ) +
    geom_bar(aes(fill = Status), stat = "identity", position = "stack", width = 0.7) +
    geom_text(
      aes(label = percent(pct / 100, accuracy = 1), group = Status),
      position = position_stack(vjust = 0.8),
      color = "white", size = 3, fontface = "bold"
    ) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_fill_manual(values = c(
      "Never"       = "#0f2f36",
      "Seldom"      = "#48705f",
      "Quite often" = "navy",
      "Very often"  = "#f49b45",
      "Always"      = "#C00000"
    )) +
    labs(
      x       = "Cohort",
      y       = "Percentage",
      fill    = "Frequency of Symptoms",
      title   = "Mental health symptoms",
      subtitle = "Percentage (%) of Alumni self-reported currently experiencing blue moods and anxiety",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 1"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title   = element_text(face = "bold"),
      plot.tag     = element_text(face = "bold")
    )
})

# ── FIGURE 2: Positive vs. negative feelings by grade ────────────────────────
output$al_fig2 <- renderPlot({
  disord_by_grade <- Alumni1 %>%
    group_by(grade, Ment_Dis) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(freq = n / sum(n), pct = round(freq * 100, 0))

  ggplot(
    disord_by_grade %>% drop_na(Ment_Dis),
    aes(x = grade, y = pct, group = Ment_Dis)
  ) +
    geom_hline(yintercept = 50, lty = "solid", color = "darkred", linewidth = 3) +
    geom_linerange(
      aes(ymin = 0, ymax = 100), color = "white",
      linewidth = 10, position = position_dodge(width = 0.7)
    ) +
    geom_point(
      aes(color = Ment_Dis),
      position = position_dodge(width = 0.7), size = 20
    ) +
    geom_text(
      aes(label = percent(pct / 100, accuracy = 1)),
      position = position_dodge(width = 0.7),
      color = "white", fontface = "bold", size = 4
    ) +
    scale_color_manual(values = c(
      "Positive Feelings" = "#48705f",
      "Negative Feelings" = "#f49b45"
    )) +
    labs(
      x       = "Cohort (Grade of 2018-2024)",
      y       = "Percentage",
      color   = "Prevalence of positive feelings",
      title   = "Status of experienced feelings",
      subtitle = "Percentage (%) of Alumni self-reported currently experiencing blue moods and anxiety",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 2"
    ) +
    theme_pubclean() +
    theme(
      legend.position = "left",
      plot.title = element_text(face = "bold"),
      plot.tag   = element_text(face = "bold")
    )
})

# ── FIGURE 3: Employment rate by grade ───────────────────────────────────────
output$al_fig3 <- renderPlot({
  employment_by_grade <- Alumni1 %>%
    group_by(grade, Employment) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(freq = n / sum(n), pct = round(freq * 100, 0))

  ggplot(
    employment_by_grade %>% drop_na(Employment),
    aes(x = grade, y = pct, group = Employment)
  ) +
    geom_hline(yintercept = 50, lty = 1, color = "darkred", linewidth = 0.1) +
    geom_linerange(
      aes(ymin = 0, ymax = 100), color = "white",
      linewidth = 10, position = position_dodge(width = 0.7)
    ) +
    geom_point(
      aes(color = Employment),
      position = position_dodge(width = 0.7), size = 20
    ) +
    geom_text(
      aes(label = percent(pct / 100, accuracy = 1)),
      position = position_dodge(width = 0.7),
      color = "white", fontface = "bold", size = 4
    ) +
    scale_color_manual(values = c(
      "Employed"     = "#48705f",
      "Not employed" = "black"
    )) +
    labs(
      x       = "Cohort (Grade of 2018-2024)",
      y       = "Percentage",
      color   = "Employment",
      title   = "Employment rate",
      subtitle = "Percentage (%) of Alumni (grade of 2018-2024) self-reported currently employed",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 3"
    ) +
    theme_pubclean() +
    theme(
      legend.position = "left",
      plot.title = element_text(face = "bold"),
      plot.tag   = element_text(face = "bold")
    )
})

# ── FIGURE 4: Entrepreneurial mindset item distributions (faceted) ────────────
output$al_fig4 <- renderPlot({
  EntrepreneurMindset_items %>%
    pivot_longer(everything()) %>%
    filter(!is.na(value)) %>%
    group_by(name, value) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(name) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(x = factor(value), y = pct, fill = factor(value))) +
    geom_col(width = 0.98) +
    geom_text(
      aes(label = percent(pct, accuracy = 1)),
      position = position_stack(vjust = 0.85),
      color = "white", fontface = "bold", size = 3.5
    ) +
    facet_wrap(
      ~ name, ncol = 3,
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
    scale_x_discrete(labels = c("1","2","3","4","5")) +
    scale_fill_manual(
      name   = "Level of ability",
      values = c("1"="#C00000","2"="#f49b45","3"="navy","4"="#48705f","5"="#0f2f36"),
      labels = c(
        "1" = "Much worse",     "2" = "A little worse",
        "3" = "About the same", "4" = "A little better",
        "5" = "Much better"
      )
    ) +
    labs(
      x        = "Level (1 = Much worse … 5 = Much better)",
      y        = "Percentage of alumni",
      title    = "Perceived Entrepreneurial Mindset",
      subtitle = "Percentage of alumni by level of perceived entrepreneurial ability",
      tag      = "Figure 4"
    ) +
    theme_minimal() +
    theme(
      strip.text   = element_text(face = "bold"),
      axis.text.x  = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      plot.title   = element_text(face = "bold"),
      plot.tag     = element_text(face = "bold")
    )
})

# ── FIGURE 5: Average entrepreneurial mindset – error plot ───────────────────
output$al_fig5 <- renderPlot({
  sample_mean <- mean(Alumni1$EntMindset_av_score, na.rm = TRUE)

  ggerrorplot(
    Alumni1 %>% drop_na(grade, EntMindset_av_score, gender),
    x = "grade", y = "EntMindset_av_score",
    desc_stat = "mean_sd",
    color     = "gender",
    width     = 2,
    position  = position_dodge(0.8)
  ) +
    geom_hline(yintercept = sample_mean, linetype = "dashed", color = "darkred") +
    annotate(
      "text", x = Inf, y = sample_mean,
      label    = paste0("Sample mean = ", round(sample_mean, 3)),
      hjust    = 1.05, vjust = -0.5,
      color    = "darkred", fontface = "bold", size = 4
    ) +
    scale_color_manual(values = c("Male" = "#48705f", "Female" = "#f49b45")) +
    labs(
      x        = "Cohort (Grade)",
      y        = "Average score",
      color    = "Gender",
      title    = "Average score for Entrepreneurial Mindset by grade and gender",
      subtitle = "Values above 3 indicate perceived improvement (scale 1–5)",
      caption  = "Data source: Alumni Survey 2025",
      tag      = "Figure 5"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.title   = element_text(face = "bold"),
      plot.tag     = element_text(face = "bold")
    )
})

# ── FIGURE 6: Average entrepreneurial mindset – bar chart ────────────────────
output$al_fig6 <- renderPlot({
  sample_mean <- mean(Alumni1$EntMindset_av_score, na.rm = TRUE)

  ggplot(df_plot, aes(x = grade, y = mean_score, fill = gender)) +
    geom_bar(stat = "identity", color = "black", width = 0.7,
             position = position_dodge(0.8)) +
    geom_hline(yintercept = sample_mean, linetype = "dashed", color = "darkred") +
    annotate(
      "text", x = Inf, y = sample_mean,
      label    = paste0("Sample mean = ", round(sample_mean, 3)),
      hjust    = 1.05, vjust = -0.5,
      color    = "darkred", fontface = "bold", size = 4
    ) +
    scale_fill_manual(values = c("Male" = "#48705f", "Female" = "#f49b45")) +
    ylim(0, 5) +
    labs(
      x        = "Cohort (Grade)",
      y        = "Average Entrepreneurial Mindset score",
      fill     = "Gender",
      title    = "Average score for Entrepreneurial Mindset by grade and gender",
      subtitle = "Values above 3 indicate perceived improvement (scale 1–5)",
      caption  = "Data source: Alumni Survey 2025",
      tag      = "Figure 6"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.title   = element_text(face = "bold"),
      plot.tag     = element_text(face = "bold")
    )
})

# ── FIGURE 7: Average entrepreneurial mindset – dot chart ────────────────────
output$al_fig7 <- renderPlot({
  sample_mean <- mean(Alumni1$EntMindset_av_score, na.rm = TRUE)

  ggdotchart(
    df_dot,
    x          = "grade",
    y          = "EntMindset_av_score",
    color      = "gender",
    palette    = c("#48705f", "#f49b45"),
    add        = "segments",
    dot.size   = 20,
    segment.size = 11,
    label      = round(df_dot$EntMindset_av_score, 1),
    label.pos  = "center",
    font.label = list(color = "white", size = 10, face = "bold"),
    ylab       = "Average score (1-5)",
    xlab       = FALSE,
    ggtheme    = theme_pubr()
  ) +
    scale_y_continuous(limits = c(0, 5)) +
    geom_hline(yintercept = sample_mean, linetype = "dashed", color = "darkred") +
    annotate(
      "text", x = Inf, y = sample_mean,
      label    = paste0("Sample mean = ", round(sample_mean, 3)),
      hjust    = 1.5, vjust = -0.5,
      color    = "darkred", fontface = "bold"
    ) +
    labs(
      title    = "Average Entrepreneurial Mindset by grade and gender",
      subtitle = "Dots represent mean scores; values above 3 indicate significantly improved mindset towards work readiness and entrepreneurship.",
      color    = "Gender",
      caption  = "Data source: Alumni Survey 2025",
      tag      = "Figure 7"
    ) +
    theme(
      axis.text.x     = element_text(angle = 60, vjust = 0.5),
      legend.position = "right",
      plot.title      = element_text(face = "bold"),
      plot.tag        = element_text(face = "bold")
    )
})

# ── FIGURE 8: Health status by grade ─────────────────────────────────────────
output$al_fig8 <- renderPlot({
  Healstat_by_grade <- Alumni1 %>%
    drop_na(Health_Status) %>%
    group_by(grade, Health_Status) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(freq = n / sum(n), pct = round(freq * 100, 0))

  ggplot(Healstat_by_grade, aes(x = grade, y = pct, group = Health_Status)) +
    geom_hline(yintercept = 50, lty = "solid", color = "darkred", linewidth = 0.5) +
    geom_linerange(
      aes(ymin = 0, ymax = 100), color = "white",
      linewidth = 10, position = position_dodge(width = 0.7)
    ) +
    geom_point(
      aes(color = Health_Status),
      position = position_dodge(width = 0.7), size = 20
    ) +
    geom_text(
      aes(label = percent(pct / 100, accuracy = 1)),
      position = position_dodge(width = 0.7),
      color = "white", fontface = "bold", size = 4
    ) +
    scale_color_manual(values = c("Healthy" = "#48705f", "Unwell" = "#f49b45")) +
    labs(
      x       = "Cohort (Grade of 2018-2024)",
      y       = "Percentage",
      color   = "Health Status",
      title   = "Health Status",
      subtitle = "Percentage (%) of Alumni (grade of 2018-2024) self-reported Healthy Status",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 8"
    ) +
    theme_pubclean() +
    theme(
      legend.position = "left",
      plot.title = element_text(face = "bold"),
      plot.tag   = element_text(face = "bold")
    )
})

# ── FIGURE 9: QoL rating and health satisfaction (faceted) ───────────────────
output$al_fig9 <- renderPlot({
  QOL_rating_items %>%
    pivot_longer(everything()) %>%
    filter(!is.na(value)) %>%
    mutate(score = as.numeric(value)) %>%
    group_by(name, score) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(name) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(x = score, y = pct, fill = factor(score))) +
    geom_col(width = 0.98) +
    geom_text(
      aes(label = percent(pct, accuracy = 1)),
      position = position_stack(vjust = 0.85),
      color = "white", fontface = "bold", size = 5
    ) +
    facet_wrap(
      ~ name, ncol = 3,
      labeller = as_labeller(c(
        QOL_rating      = "Quality of Life Rating (%)",
        H_satisf_rating = "Health Satisfaction Rating (%)"
      ))
    ) +
    scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(
      name   = "Rating",
      values = c("1"="#C00000","2"="#f49b45","3"="navy","4"="#48705f","5"="#0f2f36"),
      labels = c(
        "1" = "Very poor", "2" = "Poor", "3" = "Average",
        "4" = "Good",      "5" = "Very good"
      )
    ) +
    labs(
      x        = "Rating (1 = Very poor … 5 = Very good)",
      y        = "Percentage of alumni",
      title    = "Perceived Quality of Life and Health Satisfaction",
      subtitle = "Percentage of alumni by level of life and health satisfaction",
      tag      = "Figure 9"
    ) +
    theme_minimal() +
    theme(
      strip.text   = element_text(face = "bold"),
      axis.text.x  = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      plot.title   = element_text(face = "bold"),
      plot.tag     = element_text(face = "bold")
    )
})

# ── FIGURE 10: Physical QoL by grade and gender ───────────────────────────────
output$al_fig10 <- renderPlot({
  ggerrorplot(
    Alumni1 %>% dplyr::filter(!is.na(phys_QOL_perc)),
    x = "grade", y = "phys_QOL_perc",
    desc_stat = "mean_sd", color = "gender",
    position = position_dodge(0.3),
    dot.size = 6, errorbar.width = 0.25
  ) +
    scale_color_manual(values = c("Male" = "#48705f", "Female" = "#f49b45")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(
      x       = "Cohort",
      y       = "WHOQOL score (0–100)",
      color   = "Gender",
      title   = "Physical Health Quality of Life",
      subtitle = "Mean WHOQOL-BREF physical health scores by cohort and gender",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 10"
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
})

# ── FIGURE 11: Psychological QoL by grade and gender ─────────────────────────
output$al_fig11 <- renderPlot({
  ggerrorplot(
    Alumni1 %>% dplyr::filter(!is.na(psych_QOL_perc)),
    x = "grade", y = "psych_QOL_perc",
    desc_stat = "mean_sd", color = "gender",
    position = position_dodge(0.3),
    dot.size = 10, errorbar.width = 0.1
  ) +
    scale_color_manual(values = c("Male" = "#48705f", "Female" = "#f49b45")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(
      x       = "Cohort",
      y       = "WHOQOL score (0–100)",
      color   = "Gender",
      title   = "Psychological Health Quality of Life",
      subtitle = "Mean WHOQOL-BREF Psychological health scores by cohort and gender",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 11"
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
})

# ── FIGURE 12: Environmental QoL by grade and gender ─────────────────────────
output$al_fig12 <- renderPlot({
  ggerrorplot(
    Alumni1 %>% dplyr::filter(!is.na(env_QOL_perc)),
    x = "grade", y = "env_QOL_perc",
    desc_stat = "mean_sd", color = "gender",
    position = position_dodge(0.3),
    dot.size = 10, errorbar.width = 0.1
  ) +
    scale_color_manual(values = c("Male" = "#48705f", "Female" = "#f49b45")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(
      x       = "Cohort",
      y       = "WHOQOL score (0–100)",
      color   = "Gender",
      title   = "Environmental Quality of Life",
      subtitle = "Mean WHOQOL-BREF environmental health scores by cohort and gender",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 12"
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
})

# ── FIGURE 13: Social QoL by grade and gender ────────────────────────────────
output$al_fig13 <- renderPlot({
  ggerrorplot(
    Alumni1 %>% dplyr::filter(!is.na(social_QOL_perc)),
    x = "grade", y = "social_QOL_perc",
    desc_stat = "mean_sd", color = "gender",
    position = position_dodge(0.3),
    dot.size = 10, errorbar.width = 0.1
  ) +
    scale_color_manual(values = c("Male" = "#48705f", "Female" = "#f49b45")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(
      x       = "Cohort",
      y       = "WHOQOL score (0–100)",
      color   = "Gender",
      title   = "Social and Relational Quality of Life",
      subtitle = "Mean WHOQOL-BREF Social and Relational health scores by cohort and gender",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 13"
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
})

# ── FIGURE 14: Resilience scale item distributions (faceted) ─────────────────
output$al_fig14 <- renderPlot({
  Resilience_items %>%
    pivot_longer(everything(), names_to = "item", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(score = as.numeric(value)) %>%
    group_by(item, score) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(item) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(x = score, y = pct, fill = factor(score))) +
    geom_col(width = 0.98) +
    geom_text(
      aes(label = percent(pct, accuracy = 1)),
      position = position_stack(vjust = 0.85),
      color = "white", fontface = "bold", size = 3
    ) +
    facet_wrap(
      ~ item, ncol = 5,
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
    scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(
      name   = "Response level",
      values = c("1"="#C00000","2"="#f49b45","3"="navy","4"="#48705f","5"="#0f2f36"),
      labels = c(
        "1" = "Strongly disagree", "2" = "Disagree", "3" = "Neutral",
        "4" = "Agree",             "5" = "Strongly agree"
      )
    ) +
    labs(
      x        = "Agreement level (1 = Strongly disagree … 5 = Strongly agree)",
      y        = "Percentage of alumni",
      title    = "Resilience and Coping Capacity",
      subtitle = "Percentage distribution of alumni responses to resilience and coping statements",
      caption  = "Data source: Alumni Survey 2025",
      tag      = "Figure 14"
    ) +
    theme_minimal() +
    theme(
      strip.text    = element_text(face = "bold"),
      axis.text.x   = element_text(face = "bold"),
      legend.title  = element_text(face = "bold"),
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold"),
      plot.caption  = element_text(face = "bold"),
      plot.tag      = element_text(face = "bold")
    )
})

# ── FIGURE 15: Physical QoL categories by grade ──────────────────────────────
output$al_fig15 <- renderPlot({
  physQOLbin_by_grade <- Alumni1 %>%
    drop_na(physQOL_bin) %>%
    group_by(grade, physQOL_bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(grade) %>%
    mutate(freq = n / sum(n), pct = freq * 100)

  ggplot(physQOLbin_by_grade,
         aes(x = grade, y = pct, color = physQOL_bin, group = physQOL_bin)) +
    geom_hline(yintercept = 60, linetype = 1, color = "darkred", linewidth = 2) +
    geom_point(position = position_dodge(width = 0.75), size = 20) +
    geom_text(
      aes(label = percent(pct / 100, accuracy = 1)),
      position = position_dodge(width = 0.90),
      vjust = -0.5, size = 4, color = "white"
    ) +
    scale_color_manual(values = c("Poor" = "#f49b45", "Good" = "#48705f")) +
    scale_y_continuous(breaks = c(20, 40, 60, 80, 100), limits = c(0, 100)) +
    labs(
      x       = "Cohort (Grade of 2018-2024)",
      y       = "Percentage",
      color   = "Physical QOL",
      title   = "Perceived Physical Quality of Life",
      subtitle = "Percentage (%) of Alumni self-reporting Physical QOL status",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 15"
    ) +
    theme_pubclean() +
    theme(
      legend.position = "left",
      plot.title = element_text(face = "bold"),
      plot.tag   = element_text(face = "bold")
    )
})

# ── FIGURE 16: Psychological QoL categories by grade ─────────────────────────
output$al_fig16 <- renderPlot({
  psyQOLbin_by_grade <- Alumni1 %>%
    drop_na(psyQOL_bin) %>%
    group_by(grade, psyQOL_bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(grade) %>%
    mutate(freq = n / sum(n), pct = freq * 100)

  ggplot(psyQOLbin_by_grade,
         aes(x = grade, y = pct, color = psyQOL_bin, group = psyQOL_bin)) +
    geom_hline(yintercept = 60, linetype = 1, color = "darkred", linewidth = 1.5) +
    geom_point(position = position_dodge(width = 0.75), size = 18) +
    geom_text(
      aes(label = percent(pct / 100, accuracy = 1)),
      position = position_dodge(width = 0.75),
      vjust = -0.6, size = 4, color = "white", fontface = "bold"
    ) +
    scale_color_manual(values = c("Poor" = "#f49b45", "Good" = "#48705f")) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
    labs(
      x       = "Cohort (Grade of 2018–2024)",
      y       = "Percentage",
      color   = "Psychological QOL",
      title   = "Perceived Psychological Quality of Life",
      subtitle = "Percentage (%) of alumni self-reporting psychological QOL status",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 16"
    ) +
    theme_pubclean() +
    theme(
      legend.position = "left",
      plot.title = element_text(face = "bold"),
      plot.tag   = element_text(face = "bold")
    )
})

# ── FIGURE 17: Social QoL categories by grade ────────────────────────────────
output$al_fig17 <- renderPlot({
  SocQOLbin_by_grade <- Alumni1 %>%
    drop_na(social_QOL_bin) %>%
    group_by(grade, social_QOL_bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(grade) %>%
    mutate(freq = n / sum(n), pct = freq * 100)

  ggplot(SocQOLbin_by_grade,
         aes(x = grade, y = pct, color = social_QOL_bin, group = social_QOL_bin)) +
    geom_hline(yintercept = 60, linetype = 1, color = "darkred", linewidth = 2) +
    geom_point(position = position_dodge(width = 0.75), size = 20) +
    geom_text(
      aes(label = percent(pct / 100, accuracy = 1)),
      position = position_dodge(width = 0.90),
      vjust = -0.5, size = 4, color = "white"
    ) +
    scale_color_manual(values = c("Poor" = "#f49b45", "Good" = "#48705f")) +
    scale_y_continuous(breaks = c(20, 40, 60, 80, 100), limits = c(0, 100)) +
    labs(
      x       = "Cohort (Grade of 2018-2024)",
      y       = "Percentage",
      color   = "Social and Relational QOL",
      title   = "Perceived Social and Relational Quality of Life",
      subtitle = "Percentage (%) of Alumni self-reporting Social and relational QOL status",
      caption = "Data source: Alumni Survey 2025",
      tag     = "Figure 17"
    ) +
    theme_pubclean() +
    theme(
      legend.position = "left",
      plot.title = element_text(face = "bold"),
      plot.tag   = element_text(face = "bold")
    )
})
