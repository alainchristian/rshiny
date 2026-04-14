# ════════════════════════════════════════════════════════
# ACADEMIC PERFORMANCE — SERVER
# ════════════════════════════════════════════════════════

# ── OVERVIEW ─────────────────────────────────────────────
output$ac_overview_gpa_plot <- renderPlotly({
  ac_data %>%
    group_by(Cohort) %>%
    summarise(Avg = round(mean(GPA, na.rm = TRUE), 1),
              SE  = sd(GPA, na.rm = TRUE) / sqrt(n()),
              .groups = "drop") %>%
    plot_ly(x = ~reorder(Cohort, -Avg), y = ~Avg, type = "bar",
            error_y = list(array = ~SE * 1.96, color = "#64748B"),
            marker = list(color = "rgba(55,138,221,.25)",
                          line  = list(color = "#378ADD", width = 1.5)),
            text = ~Avg, textposition = "outside") %>%
    plotly_layout(
      xaxis = list(title = "Cohort", tickangle = -20),
      yaxis = list(title = "Avg GPA", range = list(0, 100)),
      margin = list(t = 10, b = 50)
    )
})

output$ac_overview_status_plot <- renderPlotly({
  ac_data %>%
    count(Status) %>%
    plot_ly(labels = ~Status, values = ~n, type = "pie",
            marker = list(colors = c("#D4A843", "#C75B39", "#0E6B52")),
            texttemplate = "%{label}<br>%{percent}", hole = 0.4) %>%
    plotly_layout(showlegend = TRUE,
                  legend = list(orientation = "h", y = -0.1),
                  margin = list(t = 10, b = 10))
})

output$ac_overview_table <- renderDT({
  ac_data %>%
    group_by(Cohort, Stage) %>%
    summarise(
      N       = n(),
      Avg_GPA = round(mean(GPA, na.rm = TRUE), 1),
      Math    = round(mean(Math, na.rm = TRUE), 1),
      English = round(mean(English, na.rm = TRUE), 1),
      Science = round(mean(Science, na.rm = TRUE), 1),
      Honors_pct = paste0(round(mean(Status == "Honors") * 100, 1), "%"),
      .groups = "drop"
    ) %>%
    arrange(Cohort, Stage) %>%
    datatable(class = "nowrap", options = list(dom = "tp", pageLength = 10, scrollX = TRUE), rownames = FALSE)
})

# ── SCORE TRENDS ─────────────────────────────────────────
ac_tr_data <- reactive({
  df <- ac_data
  if (!is.null(input$ac_tr_cohort) && input$ac_tr_cohort != "All")
    df <- df %>% filter(Cohort == input$ac_tr_cohort)
  if (!is.null(input$ac_tr_gender) && input$ac_tr_gender != "All")
    df <- df %>% filter(Gender == input$ac_tr_gender)
  df
})

output$ac_tr_n <- renderbs4ValueBox(
  bs4ValueBox(nrow(ac_tr_data()), "Records", color = "primary",
              icon = icon("database"), width = 3))

output$ac_tr_avg <- renderbs4ValueBox({
  met <- input$ac_tr_metric %||% "GPA"
  bs4ValueBox(round(mean(ac_tr_data()[[met]], na.rm = TRUE), 1),
              paste("Avg", met), color = "info",
              icon = icon("chart-bar"), width = 3)
})

output$ac_tr_highest <- renderbs4ValueBox({
  met <- input$ac_tr_metric %||% "GPA"
  top <- ac_tr_data() %>%
    group_by(Cohort) %>%
    summarise(Score = mean(get(met), na.rm = TRUE), .groups = "drop") %>%
    slice_max(Score, n = 1)
  bs4ValueBox(paste0(top$Cohort, " (", round(top$Score, 1), ")"),
              "Highest cohort", color = "success",
              icon = icon("arrow-up"), width = 3)
})

output$ac_tr_lowest <- renderbs4ValueBox({
  met <- input$ac_tr_metric %||% "GPA"
  bot <- ac_tr_data() %>%
    group_by(Cohort) %>%
    summarise(Score = mean(get(met), na.rm = TRUE), .groups = "drop") %>%
    slice_min(Score, n = 1)
  bs4ValueBox(paste0(bot$Cohort, " (", round(bot$Score, 1), ")"),
              "Lowest cohort", color = "warning",
              icon = icon("arrow-down"), width = 3)
})

output$ac_tr_summary_table <- renderDT({
  met <- input$ac_tr_metric %||% "GPA"
  ac_tr_data() %>%
    filter(!is.na(Stage)) %>%
    group_by(Cohort, Stage) %>%
    summarise(
      N          = n(),
      `Avg score`= round(mean(get(met), na.rm = TRUE), 1),
      Honors     = paste0(round(mean(Status == "Honors") * 100, 1), "%"),
      Pass       = paste0(round(mean(Status == "Pass")   * 100, 1), "%"),
      Fail       = paste0(round(mean(Status == "Fail")   * 100, 1), "%"),
      .groups    = "drop"
    ) %>%
    arrange(Cohort, Stage) %>%
    datatable(class = "nowrap",
              options = list(dom = "tp", pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
})

output$ac_trends_stage_plot <- renderPlotly({
  met <- input$ac_tr_metric %||% "GPA"
  ac_tr_data() %>%
    filter(!is.na(Stage)) %>%
    group_by(Stage) %>%
    summarise(Score = mean(get(met), na.rm = TRUE), .groups = "drop") %>%
    plot_ly(x = ~Stage, y = ~Score, type = "scatter", mode = "lines+markers",
            line   = list(color = "#378ADD", width = 3, shape = "spline"),
            marker = list(color = "#378ADD", size = 10,
                          line  = list(color = "#fff", width = 2))) %>%
    plotly_layout(
      xaxis = list(title = "Stage"),
      yaxis = list(title = met, range = list(0, 100)),
      margin = list(t = 10)
    )
})

output$ac_trends_term_plot <- renderPlotly({
  met <- input$ac_tr_metric %||% "GPA"
  ac_tr_data() %>%
    group_by(Term) %>%
    summarise(Score = mean(get(met), na.rm = TRUE), .groups = "drop") %>%
    plot_ly(x = ~Term, y = ~Score, type = "bar",
            marker = list(color = "rgba(55,138,221,.25)",
                          line  = list(color = "#378ADD", width = 1.5)),
            text = ~round(Score, 1), textposition = "outside") %>%
    plotly_layout(
      xaxis = list(title = "Term"),
      yaxis = list(title = met, range = list(0, 100)),
      margin = list(t = 10)
    )
})

output$ac_trends_dist_plot <- renderPlotly({
  met <- input$ac_tr_metric %||% "GPA"
  plot_ly(x = ac_tr_data()[[met]], type = "histogram", nbinsx = 20,
          marker = list(color = "rgba(55,138,221,.35)",
                        line  = list(color = "#378ADD", width = 1))) %>%
    plotly_layout(
      xaxis = list(title = met),
      yaxis = list(title = "Count"),
      margin = list(t = 10)
    )
})

# ── SUBJECT BREAKDOWN ─────────────────────────────────────
ac_sb_data <- reactive({
  df <- ac_data
  if (!is.null(input$ac_sb_stage) && input$ac_sb_stage != "All")
    df <- df %>% filter(Stage == input$ac_sb_stage)
  if (!is.null(input$ac_sb_gender) && input$ac_sb_gender != "All")
    df <- df %>% filter(Gender == input$ac_sb_gender)
  if (!is.null(input$ac_sb_term) && input$ac_sb_term != "All")
    df <- df %>% filter(Term == input$ac_sb_term)
  df
})

output$ac_sb_math    <- renderbs4ValueBox(bs4ValueBox(
  round(mean(ac_sb_data()$Math,        na.rm = TRUE), 1),
  "Avg Math",        color = "primary", icon = icon("calculator"), width = 3))
output$ac_sb_english <- renderbs4ValueBox(bs4ValueBox(
  round(mean(ac_sb_data()$English,     na.rm = TRUE), 1),
  "Avg English",     color = "success", icon = icon("book"),       width = 3))
output$ac_sb_science <- renderbs4ValueBox(bs4ValueBox(
  round(mean(ac_sb_data()$Science,     na.rm = TRUE), 1),
  "Avg Science",     color = "info",    icon = icon("flask"),      width = 3))
output$ac_sb_kirundi <- renderbs4ValueBox(bs4ValueBox(
  round(mean(ac_sb_data()$Kinyarwanda, na.rm = TRUE), 1),
  "Avg Kinyarwanda", color = "warning", icon = icon("language"),   width = 3))

output$ac_sb_summary_table <- renderDT({
  ac_sb_data() %>%
    filter(!is.na(Cohort)) %>%
    group_by(Cohort) %>%
    summarise(
      N               = n(),
      `Avg GPA`       = round(mean(GPA,         na.rm = TRUE), 1),
      `Avg Math`      = round(mean(Math,         na.rm = TRUE), 1),
      `Avg English`   = round(mean(English,      na.rm = TRUE), 1),
      `Avg Science`   = round(mean(Science,      na.rm = TRUE), 1),
      `Avg Kinyarwanda` = round(mean(Kinyarwanda,na.rm = TRUE), 1),
      Honors          = paste0(round(mean(Status == "Honors") * 100, 1), "%"),
      Pass            = paste0(round(mean(Status == "Pass")   * 100, 1), "%"),
      Fail            = paste0(round(mean(Status == "Fail")   * 100, 1), "%"),
      .groups         = "drop"
    ) %>%
    arrange(desc(`Avg GPA`)) %>%
    datatable(class = "nowrap",
              options = list(dom = "tp", pageLength = 8, scrollX = TRUE),
              rownames = FALSE)
})

output$ac_subjects_bar_plot <- renderPlotly({
  df <- ac_sb_data()
  subjects <- c("Math", "English", "Science", "Kinyarwanda")
  avgs <- sapply(subjects, function(s) round(mean(df[[s]], na.rm = TRUE), 1))
  colors <- c("#378ADD", "#0E6B52", "#534AB7", "#D4A843")
  plot_ly(x = avgs, y = subjects, type = "bar", orientation = "h",
          marker = list(color = paste0(colors, "44"),
                        line  = list(color = colors, width = 1.5)),
          text = avgs, textposition = "outside") %>%
    plotly_layout(
      xaxis = list(title = "Avg score", range = list(0, 100)),
      yaxis = list(title = ""),
      margin = list(t = 10, l = 100)
    )
})

output$ac_subjects_box_plot <- renderPlotly({
  df <- ac_sb_data()
  colors <- c("#378ADD", "#0E6B52", "#534AB7", "#D4A843")
  plot_ly(type = "box", quartilemethod = "linear") %>%
    add_trace(y = df$Math,        name = "Math",        fillcolor = "rgba(55,138,221,.15)",  line = list(color = "#378ADD"), marker = list(color = "#378ADD")) %>%
    add_trace(y = df$English,     name = "English",     fillcolor = "rgba(14,107,82,.15)",   line = list(color = "#0E6B52"), marker = list(color = "#0E6B52")) %>%
    add_trace(y = df$Science,     name = "Science",     fillcolor = "rgba(83,74,183,.15)",   line = list(color = "#534AB7"), marker = list(color = "#534AB7")) %>%
    add_trace(y = df$Kinyarwanda, name = "Kinyarwanda", fillcolor = "rgba(212,168,67,.15)",  line = list(color = "#D4A843"), marker = list(color = "#D4A843")) %>%
    plotly_layout(showlegend = FALSE, margin = list(t = 10))
})

output$ac_subjects_gender_plot <- renderPlotly({
  df <- ac_sb_data()
  df %>%
    filter(!is.na(Gender)) %>%
    group_by(Gender) %>%
    summarise(Math = mean(Math, na.rm = TRUE), English = mean(English, na.rm = TRUE),
              Science = mean(Science, na.rm = TRUE), Kinyarwanda = mean(Kinyarwanda, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_longer(-Gender, names_to = "Subject", values_to = "Score") %>%
    plot_ly(x = ~Subject, y = ~Score, color = ~Gender,
            colors = c("#C75B39", "#0369A1"), type = "bar",
            text = ~round(Score, 1), textposition = "outside") %>%
    plotly_layout(barmode = "group",
                  yaxis  = list(title = "Avg score", range = list(0, 100)),
                  legend = list(orientation = "h", y = -0.15),
                  margin = list(t = 10))
})

# ── COHORT COMPARISON ─────────────────────────────────────
ac_co_data <- reactive({
  df <- ac_data
  if (!is.null(input$ac_co_stage) && input$ac_co_stage != "All")
    df <- df %>% filter(Stage == input$ac_co_stage)
  df
})

ac_co_summary <- reactive({
  met <- input$ac_co_metric %||% "GPA"
  ac_co_data() %>%
    group_by(Cohort) %>%
    summarise(Score       = round(mean(get(met), na.rm = TRUE), 1),
              Honors_pct  = round(mean(Status == "Honors") * 100, 1),
              N           = n(),
              .groups     = "drop") %>%
    arrange(desc(Score))
})

output$ac_co_best <- renderbs4ValueBox({
  d <- ac_co_summary() %>% slice_max(Score, n = 1)
  bs4ValueBox(paste0(d$Cohort, " (", d$Score, ")"),
              paste("Highest", input$ac_co_metric %||% "GPA"),
              color = "success", icon = icon("trophy"), width = 4)
})
output$ac_co_worst <- renderbs4ValueBox({
  d <- ac_co_summary() %>% slice_min(Score, n = 1)
  bs4ValueBox(paste0(d$Cohort, " (", d$Score, ")"),
              paste("Lowest", input$ac_co_metric %||% "GPA"),
              color = "danger", icon = icon("arrow-down"), width = 4)
})
output$ac_co_spread <- renderbs4ValueBox({
  s <- ac_co_summary()
  spread <- round(max(s$Score) - min(s$Score), 1)
  bs4ValueBox(spread, "Score spread (max \u2212 min)",
              color = "info", icon = icon("arrows-left-right"), width = 4)
})

output$ac_co_table <- renderDT({
  met <- input$ac_co_metric %||% "GPA"
  ac_co_data() %>%
    group_by(Cohort) %>%
    summarise(
      N               = n(),
      `Avg score`     = round(mean(get(met),     na.rm = TRUE), 1),
      Honors          = paste0(round(mean(Status == "Honors") * 100, 1), "%"),
      Pass            = paste0(round(mean(Status == "Pass")   * 100, 1), "%"),
      Fail            = paste0(round(mean(Status == "Fail")   * 100, 1), "%"),
      `Avg Math`      = round(mean(Math,          na.rm = TRUE), 1),
      `Avg English`   = round(mean(English,       na.rm = TRUE), 1),
      `Avg Science`   = round(mean(Science,       na.rm = TRUE), 1),
      `Avg Kinyarwanda` = round(mean(Kinyarwanda, na.rm = TRUE), 1),
      .groups         = "drop"
    ) %>%
    arrange(desc(`Avg score`)) %>%
    datatable(class = "nowrap",
              options = list(dom = "tp", pageLength = 8, scrollX = TRUE),
              rownames = FALSE)
})

output$ac_cohorts_bar_plot <- renderPlotly({
  s <- ac_co_summary()
  plot_ly(s, x = ~reorder(Cohort, -Score), y = ~Score, type = "bar",
          marker = list(color = "rgba(14,107,82,.25)",
                        line  = list(color = "#0E6B52", width = 1.5)),
          text = ~Score, textposition = "outside") %>%
    plotly_layout(
      xaxis = list(title = "Cohort", tickangle = -15),
      yaxis = list(title = input$ac_co_metric %||% "GPA", range = list(0, 100)),
      margin = list(t = 10, b = 50)
    )
})

output$ac_cohorts_honors_plot <- renderPlotly({
  s <- ac_co_summary()
  plot_ly(s, x = ~reorder(Cohort, -Honors_pct), y = ~Honors_pct, type = "bar",
          marker = list(color = "rgba(212,168,67,.35)",
                        line  = list(color = "#D4A843", width = 1.5)),
          text = ~paste0(Honors_pct, "%"), textposition = "outside") %>%
    plotly_layout(
      xaxis = list(title = "Cohort", tickangle = -15),
      yaxis = list(title = "Honors %"),
      margin = list(t = 10, b = 50)
    )
})

output$ac_cohorts_radar_plot <- renderPlotly({
  subjects <- c("Math", "English", "Science", "Kinyarwanda")
  cohort_avgs <- ac_co_data() %>%
    group_by(Cohort) %>%
    summarise(across(all_of(subjects), ~round(mean(.x, na.rm = TRUE), 1)),
              .groups = "drop")

  colors <- c("#378ADD", "#0E6B52", "#534AB7", "#D4A843", "#C75B39")
  p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
  for (i in seq_len(nrow(cohort_avgs))) {
    row <- cohort_avgs[i, ]
    vals <- as.numeric(row[subjects])
    p <- p %>% add_trace(
      r     = c(vals, vals[1]),
      theta = c(subjects, subjects[1]),
      name  = row$Cohort,
      line  = list(color = colors[i], width = 2),
      fillcolor = paste0(gsub("#", "rgba(", colors[i]), "22)")
    )
  }
  p %>% plotly_layout(
    polar  = list(radialaxis = list(visible = TRUE, range = list(0, 100))),
    legend = list(orientation = "h", y = -0.1),
    margin = list(t = 10)
  )
})

# ── NULL-COALESCING HELPER (local to this dept) ───────────
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
