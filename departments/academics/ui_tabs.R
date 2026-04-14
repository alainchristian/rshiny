dept_ui_tabs <- list(

  # OVERVIEW ──────────────────────────────────────────────────
  bs4TabItem(tabName = "ac_overview",
    fluidRow(
      bs4InfoBox("Total Records",    nrow(ac_data),                                 icon = icon("database"),        color = "primary", width = 3),
      bs4InfoBox("Students",         length(unique(ac_data$StudentID)),              icon = icon("users"),           color = "success", width = 3),
      bs4InfoBox("Avg GPA",          round(mean(ac_data$GPA, na.rm = TRUE), 1),      icon = icon("graduation-cap"),  color = "info",    width = 3),
      bs4InfoBox("Honours Rate",     paste0(round(mean(ac_data$Status == "Honors") * 100, 1), "%"),
                                                                                     icon = icon("star"),            color = "warning", width = 3)
    ),
    fluidRow(
      bs4Card(title = "Cohort & stage summary", width = 12, collapsible = TRUE,
              DTOutput("ac_overview_table"))
    ),
    fluidRow(
      bs4Card(title = "GPA by cohort", width = 7, collapsible = TRUE,
              plotlyOutput("ac_overview_gpa_plot", height = "300px")),
      bs4Card(title = "Status breakdown", width = 5, collapsible = TRUE,
              plotlyOutput("ac_overview_status_plot", height = "300px"))
    )
  ),

  # SCORE TRENDS ──────────────────────────────────────────────
  bs4TabItem(tabName = "ac_trends",
    fluidRow(
      column(3, bs4Card(title = "Filters", width = 12, collapsible = FALSE,
        selectInput("ac_tr_cohort", "Cohort",
                    choices = c("All", sort(unique(ac_data$Cohort)))),
        selectInput("ac_tr_gender", "Gender",
                    choices = c("All", "Female", "Male")),
        selectInput("ac_tr_metric", "Subject",
                    choices = c("GPA", "Math", "English", "Science", "Kinyarwanda"))
      )),
      column(9,
        fluidRow(
          bs4ValueBoxOutput("ac_tr_n",       width = 3),
          bs4ValueBoxOutput("ac_tr_avg",     width = 3),
          bs4ValueBoxOutput("ac_tr_highest", width = 3),
          bs4ValueBoxOutput("ac_tr_lowest",  width = 3)
        ),
        fluidRow(
          bs4Card(title = "Cohort & stage breakdown", width = 12, collapsible = TRUE,
                  DTOutput("ac_tr_summary_table"))
        ),
        fluidRow(
          bs4Card(title = "Score across stages", width = 12, collapsible = TRUE,
                  plotlyOutput("ac_trends_stage_plot", height = "280px"))
        ),
        fluidRow(
          bs4Card(title = "Score by term", width = 5, collapsible = TRUE,
                  plotlyOutput("ac_trends_term_plot", height = "260px")),
          bs4Card(title = "Score distribution", width = 7, collapsible = TRUE,
                  plotlyOutput("ac_trends_dist_plot", height = "260px"))
        )
      )
    )
  ),

  # SUBJECT BREAKDOWN ─────────────────────────────────────────
  bs4TabItem(tabName = "ac_subjects",
    fluidRow(
      column(3, bs4Card(title = "Filters", width = 12, collapsible = FALSE,
        selectInput("ac_sb_stage",  "Stage",
                    choices = c("All", "EY (Entry Year)", "S4", "S5", "S6")),
        selectInput("ac_sb_gender", "Gender",
                    choices = c("All", "Female", "Male")),
        selectInput("ac_sb_term",   "Term",
                    choices = c("All", "Term 1", "Term 2", "Term 3"))
      )),
      column(9,
        fluidRow(
          bs4ValueBoxOutput("ac_sb_math",    width = 3),
          bs4ValueBoxOutput("ac_sb_english", width = 3),
          bs4ValueBoxOutput("ac_sb_science", width = 3),
          bs4ValueBoxOutput("ac_sb_kirundi", width = 3)
        ),
        fluidRow(
          bs4Card(title = "Cohort subject summary", width = 12, collapsible = TRUE,
                  DTOutput("ac_sb_summary_table"))
        ),
        fluidRow(
          bs4Card(title = "Average score by subject", width = 5, collapsible = TRUE,
                  plotlyOutput("ac_subjects_bar_plot", height = "280px")),
          bs4Card(title = "Subject score distributions", width = 7, collapsible = TRUE,
                  plotlyOutput("ac_subjects_box_plot", height = "280px"))
        ),
        fluidRow(
          bs4Card(title = "Subject scores by gender", width = 12, collapsible = TRUE,
                  plotlyOutput("ac_subjects_gender_plot", height = "260px"))
        )
      )
    )
  ),

  # COHORT COMPARISON ─────────────────────────────────────────
  bs4TabItem(tabName = "ac_cohorts",
    fluidRow(
      column(3, bs4Card(title = "Filters", width = 12, collapsible = FALSE,
        selectInput("ac_co_stage",  "Stage",
                    choices = c("All", "EY (Entry Year)", "S4", "S5", "S6")),
        selectInput("ac_co_metric", "Metric",
                    choices = c("GPA", "Math", "English", "Science", "Kinyarwanda")),
        hr(),
        p(style = "font-size:11px;color:var(--text-muted);",
          "Honors \u2265 75", br(),
          "Pass \u2265 50", br(),
          "Fail < 50")
      )),
      column(9,
        fluidRow(
          bs4ValueBoxOutput("ac_co_best",   width = 4),
          bs4ValueBoxOutput("ac_co_worst",  width = 4),
          bs4ValueBoxOutput("ac_co_spread", width = 4)
        ),
        fluidRow(
          bs4Card(title = "Cohort comparison table", width = 12, collapsible = TRUE,
                  DTOutput("ac_co_table"))
        ),
        fluidRow(
          bs4Card(title = "Score by cohort", width = 7, collapsible = TRUE,
                  plotlyOutput("ac_cohorts_bar_plot", height = "280px")),
          bs4Card(title = "Honors rate by cohort", width = 5, collapsible = TRUE,
                  plotlyOutput("ac_cohorts_honors_plot", height = "280px"))
        ),
        fluidRow(
          bs4Card(title = "Subject radar \u2014 cohort averages", width = 12, collapsible = TRUE,
                  plotlyOutput("ac_cohorts_radar_plot", height = "300px"))
        )
      )
    )
  )

)
