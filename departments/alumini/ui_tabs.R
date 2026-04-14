dept_ui_tabs <- list(

  # ── OVERVIEW ──────────────────────────────────────────────────────────────────
  bs4TabItem(
    tabName = "al_overview",
    tags$head(tags$style(HTML("
      .info-box .info-box-text {
        color: #ffffff !important;
      }
    "))),
    fluidRow(
      bs4InfoBoxOutput("al_total_respondents", width = 3),
      bs4InfoBoxOutput("al_pct_employed",      width = 3),
      bs4InfoBoxOutput("al_n_grades",          width = 3),
      bs4InfoBoxOutput("al_pct_female",        width = 3)
    ),
    fluidRow(
      bs4Card(
        title = "Respondents by Cohort (Grade)", status = "success",
        solidHeader = TRUE, width = 6,
        plotOutput("al_grade_bar", height = "340px")
      ),
      bs4Card(
        title = "Gender Distribution", status = "success",
        solidHeader = TRUE, width = 6,
        plotOutput("al_gender_bar", height = "340px")
      )
    )
  ),

  # ── MENTAL HEALTH ─────────────────────────────────────────────────────────────
  bs4TabItem(
    tabName = "al_mentalhealth",
    fluidRow(
      bs4Card(
        title = "Figure 1 — Mental Health Symptoms by Cohort",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig1", height = "420px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 2 — Positive vs. Negative Feelings by Cohort",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig2", height = "420px")
      )
    )
  ),

  # ── EMPLOYMENT & ENTREPRENEURSHIP ─────────────────────────────────────────────
  bs4TabItem(
    tabName = "al_employment",
    fluidRow(
      bs4Card(
        title = "Figure 3 — Employment Rate by Cohort",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig3", height = "420px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 4 — Perceived Entrepreneurial Mindset (Item Distributions)",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig4", height = "500px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 5 — Average Entrepreneurial Mindset by Cohort & Gender (Error Plot)",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig5", height = "420px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 6 — Average Entrepreneurial Mindset by Cohort & Gender (Bar Chart)",
        status = "success", solidHeader = TRUE, width = 6,
        plotOutput("al_fig6", height = "420px")
      ),
      bs4Card(
        title = "Figure 7 — Average Entrepreneurial Mindset by Cohort & Gender (Dot Chart)",
        status = "success", solidHeader = TRUE, width = 6,
        plotOutput("al_fig7", height = "420px")
      )
    )
  ),

  # ── HEALTH & QUALITY OF LIFE ──────────────────────────────────────────────────
  bs4TabItem(
    tabName = "al_health",
    fluidRow(
      bs4Card(
        title = "Figure 8 — Health Status by Cohort",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig8", height = "420px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 9 — Quality of Life & Health Satisfaction Ratings",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig9", height = "460px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 10 — Physical Health QoL by Cohort & Gender",
        status = "success", solidHeader = TRUE, width = 6,
        plotOutput("al_fig10", height = "400px")
      ),
      bs4Card(
        title = "Figure 11 — Psychological Health QoL by Cohort & Gender",
        status = "success", solidHeader = TRUE, width = 6,
        plotOutput("al_fig11", height = "400px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 12 — Environmental QoL by Cohort & Gender",
        status = "success", solidHeader = TRUE, width = 6,
        plotOutput("al_fig12", height = "400px")
      ),
      bs4Card(
        title = "Figure 13 — Social & Relational QoL by Cohort & Gender",
        status = "success", solidHeader = TRUE, width = 6,
        plotOutput("al_fig13", height = "400px")
      )
    )
  ),

  # ── QOL CATEGORIES ────────────────────────────────────────────────────────────
  bs4TabItem(
    tabName = "al_qolbin",
    fluidRow(
      bs4Card(
        title = "Figure 15 — Physical QoL Category by Cohort",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig15", height = "420px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 16 — Psychological QoL Category by Cohort",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig16", height = "420px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Figure 17 — Social & Relational QoL Category by Cohort",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig17", height = "420px")
      )
    )
  ),

  # ── RESILIENCE ────────────────────────────────────────────────────────────────
  bs4TabItem(
    tabName = "al_resilience",
    fluidRow(
      bs4Card(
        title = "Figure 14 — Resilience & Coping Capacity (Item Distributions)",
        status = "success", solidHeader = TRUE, width = 12,
        plotOutput("al_fig14", height = "600px")
      )
    )
  )
)
