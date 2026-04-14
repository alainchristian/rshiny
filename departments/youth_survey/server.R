# ── YOUTH SURVEY SERVER ───────────────────────────────────────────────────
# Inputs:  ys_province | ys_gender | ys_marital | ys_employment
# Outputs: 4 KPI cards, 3 plotly charts, 2 DT tables, 2 download handlers

# ── HELPER: KPI card HTML ─────────────────────────────────────────────────
ys_kpi_card <- function(bg, icon_nm, title, value, subtitle) {
  tags$div(
    class = "ys-kpi-card",
    style = paste0("background:", bg, ";"),
    tags$div(
      class = "ys-kpi-header",
      shiny::icon(icon_nm),
      tags$span(class = "ys-kpi-title", title)
    ),
    tags$div(class = "ys-kpi-value", value),
    tags$div(class = "ys-kpi-subtitle", subtitle)
  )
}

# ── 1. REACTIVE FILTERED DATASET ─────────────────────────────────────────
ys_filtered <- reactive({
  req(isTRUE(auth()$logged_in))

  df <- ys_df

  if (!is.null(input$ys_province) && input$ys_province != "All") {
    df <- df %>% dplyr::filter(as.character(province_label) == input$ys_province)
  }
  if (!is.null(input$ys_gender) && input$ys_gender != "All") {
    df <- df %>% dplyr::filter(as.character(gender_label) == input$ys_gender)
  }
  if (!is.null(input$ys_marital) && length(input$ys_marital) > 0) {
    df <- df %>% dplyr::filter(as.character(marital_label) %in% input$ys_marital)
  }
  if (!is.null(input$ys_employment) && input$ys_employment != "All") {
    df <- df %>% dplyr::filter(as.character(employment_label) == input$ys_employment)
  }

  df
})

# ── 2. KPI CARDS ─────────────────────────────────────────────────────────

output$ys_kpi_total <- renderUI({
  df <- ys_filtered()
  ys_kpi_card(
    bg       = "#1F3864",
    icon_nm  = "clipboard-list",
    title    = "Total Respondents",
    value    = format(nrow(df), big.mark = ","),
    subtitle = "Total surveys collected"
  )
})

output$ys_kpi_employment <- renderUI({
  df  <- ys_filtered()
  emp <- df %>% dplyr::filter(!is.na(employment_label))
  n_emp   <- sum(emp$employment_label == "Employed",     na.rm = TRUE)
  n_valid <- nrow(emp)
  rate    <- if (n_valid > 0) n_emp / n_valid else 0
  ys_kpi_card(
    bg       = "#375623",
    icon_nm  = "check-square",
    title    = "Employment Rate",
    value    = scales::percent(rate, accuracy = 0.1),
    subtitle = paste0(n_emp, " employed of ", n_valid, " valid responses")
  )
})

output$ys_kpi_female <- renderUI({
  df   <- ys_filtered()
  gen  <- df %>% dplyr::filter(!is.na(gender_label))
  n_f  <- sum(gen$gender_label == "Female", na.rm = TRUE)
  n_m  <- sum(gen$gender_label == "Male",   na.rm = TRUE)
  n_tot <- n_f + n_m
  rate  <- if (n_tot > 0) n_f / n_tot else 0
  ys_kpi_card(
    bg       = "#843C0C",
    icon_nm  = "venus",
    title    = "Female Share",
    value    = scales::percent(rate, accuracy = 0.1),
    subtitle = paste0(n_f, " female \u00a0|\u00a0 ", n_m, " male")
  )
})

output$ys_kpi_married <- renderUI({
  df   <- ys_filtered()
  mar  <- df %>% dplyr::filter(!is.na(marital_label))
  n_ma <- sum(mar$marital_label == "Married", na.rm = TRUE)
  n_si <- sum(mar$marital_label == "Single",  na.rm = TRUE)
  n_tot <- n_ma + n_si
  rate  <- if (n_tot > 0) n_ma / n_tot else 0
  ys_kpi_card(
    bg       = "#4B0082",
    icon_nm  = "ring",
    title    = "Married Rate",
    value    = scales::percent(rate, accuracy = 0.1),
    subtitle = paste0(n_ma, " married \u00a0|\u00a0 ", n_si, " single")
  )
})

# ── 3. CHART 1 — Respondents by Province (horizontal bar) ────────────────

output$ys_chart_province <- renderPlotly({
  df <- ys_filtered()
  req(nrow(df) > 0)

  d <- df %>%
    dplyr::filter(!is.na(province_label)) %>%
    dplyr::count(province_label) %>%
    dplyr::arrange(n)

  req(nrow(d) > 0)

  plot_ly(
    d,
    x           = ~n,
    y           = ~province_label,
    type        = "bar",
    orientation = "h",
    marker      = list(
      color = "rgba(11,77,59,.18)",
      line  = list(color = "#0B4D3B", width = 1.5)
    ),
    text          = ~n,
    textposition  = "outside",
    textfont      = list(size = 11, color = "#374151"),
    hovertemplate = "<b>%{y}</b><br>Respondents: <b>%{x:,}</b><extra></extra>"
  ) %>%
    plotly_layout(
      xaxis  = list(title = "Number of Respondents", tickfont = list(size = 11)),
      yaxis  = list(title = "", tickfont = list(size = 11)),
      margin = list(l = 10, r = 60, t = 20, b = 40)
    )
})

# ── 4. CHART 2 — Education Level Distribution (vertical bar) ─────────────

output$ys_chart_education <- renderPlotly({
  df <- ys_filtered()
  req(nrow(df) > 0)

  lev_order <- c("None", "Primary", "Secondary", "Vocational", "University", "Other")

  d <- df %>%
    dplyr::filter(!is.na(education_label)) %>%
    dplyr::count(education_label) %>%
    dplyr::mutate(education_label = factor(education_label, levels = lev_order)) %>%
    dplyr::arrange(education_label)

  req(nrow(d) > 0)

  plot_ly(
    d,
    x            = ~education_label,
    y            = ~n,
    type         = "bar",
    marker       = list(
      color = "rgba(212,168,67,.22)",
      line  = list(color = "#D4A843", width = 1.5)
    ),
    text          = ~n,
    textposition  = "outside",
    textfont      = list(size = 11, color = "#374151"),
    hovertemplate = "<b>%{x}</b><br>Count: <b>%{y:,}</b><extra></extra>"
  ) %>%
    plotly_layout(
      xaxis  = list(title = "Education Level", tickfont = list(size = 11)),
      yaxis  = list(title = "Number of Respondents", tickfont = list(size = 11)),
      margin = list(l = 50, r = 20, t = 20, b = 65)
    )
})

# ── 5. CHART 3 — Employment by Gender ────────────────────────────────────
# Y-axis = employment status; bars stacked by gender (Male vs Female)

output$ys_chart_emp_gender <- renderPlotly({
  df <- ys_filtered()
  req(nrow(df) > 0)

  d <- df %>%
    dplyr::filter(!is.na(employment_label), !is.na(gender_label)) %>%
    dplyr::count(employment_label, gender_label)

  req(nrow(d) > 0)

  male_d   <- d %>% dplyr::filter(gender_label == "Male")
  female_d <- d %>% dplyr::filter(gender_label == "Female")

  # Ensure both employment levels appear even if one gender is absent
  emp_levels <- c("Not Employed", "Employed")

  male_d   <- dplyr::left_join(
    tibble::tibble(employment_label = factor(emp_levels, levels = emp_levels)),
    male_d, by = "employment_label") %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0))

  female_d <- dplyr::left_join(
    tibble::tibble(employment_label = factor(emp_levels, levels = emp_levels)),
    female_d, by = "employment_label") %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0))

  plot_ly() %>%
    add_trace(
      data           = male_d,
      x              = ~n,
      y              = ~employment_label,
      type           = "bar",
      orientation    = "h",
      name           = "Male",
      marker         = list(color = "#1F3864"),
      text           = ~ifelse(n > 0, as.character(n), ""),
      textposition   = "inside",
      insidetextfont = list(color = "white", size = 12, family = "Plus Jakarta Sans"),
      hovertemplate  = "<b>%{y}</b><br>Male: <b>%{x:,}</b><extra></extra>"
    ) %>%
    add_trace(
      data           = female_d,
      x              = ~n,
      y              = ~employment_label,
      type           = "bar",
      orientation    = "h",
      name           = "Female",
      marker         = list(color = "#C75B39"),
      text           = ~ifelse(n > 0, as.character(n), ""),
      textposition   = "inside",
      insidetextfont = list(color = "white", size = 12, family = "Plus Jakarta Sans"),
      hovertemplate  = "<b>%{y}</b><br>Female: <b>%{x:,}</b><extra></extra>"
    ) %>%
    plotly_layout(
      barmode = "stack",
      xaxis   = list(title = "Number of Respondents", tickfont = list(size = 11)),
      yaxis   = list(title = "", tickfont = list(size = 12)),
      legend  = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.28,
                     font = list(size = 12)),
      margin  = list(l = 10, r = 20, t = 10, b = 65)
    )
})

# ── 6. TABLE 1 — Employment Rate by Province ─────────────────────────────

ys_province_tbl <- reactive({
  df <- ys_filtered()
  req(nrow(df) > 0)

  d <- df %>%
    dplyr::filter(!is.na(province_label), !is.na(employment_label)) %>%
    dplyr::group_by(Province = province_label) %>%
    dplyr::summarise(
      Total    = dplyr::n(),
      Employed = sum(employment_label == "Employed", na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(Total)) %>%
    dplyr::mutate(`Employment Rate` = Employed / Total)

  total_row <- tibble::tibble(
    Province         = "TOTAL",
    Total            = sum(d$Total),
    Employed         = sum(d$Employed),
    `Employment Rate` = sum(d$Employed) / sum(d$Total)
  )

  dplyr::bind_rows(d, total_row)
})

output$ys_table_province <- DT::renderDT({
  d <- ys_province_tbl()
  req(nrow(d) > 0)

  DT::datatable(
    d,
    class    = "nowrap compact stripe hover",
    rownames = FALSE,
    options  = list(dom = "t", scrollX = TRUE, pageLength = 20,
                    language = list(emptyTable = "No data available"))
  ) %>%
    DT::formatPercentage("Employment Rate", digits = 1) %>%
    DT::formatStyle(
      "Employment Rate",
      background         = DT::styleColorBar(c(0, 1), "#0B4D3B"),
      backgroundSize     = "100% 80%",
      backgroundRepeat   = "no-repeat",
      backgroundPosition = "center"
    ) %>%
    DT::formatStyle(
      "Province",
      target     = "row",
      fontWeight = DT::styleEqual("TOTAL", "bold")
    )
})

output$ys_dl_province <- downloadHandler(
  filename = function() paste0("employment_by_province_", Sys.Date(), ".csv"),
  content  = function(file) {
    d <- ys_province_tbl()
    d$`Employment Rate` <- scales::percent(d$`Employment Rate`, accuracy = 0.1)
    write.csv(d, file, row.names = FALSE)
  }
)

# ── 7. TABLE 2 — Employment by Marital Status & Gender ───────────────────

ys_marital_tbl <- reactive({
  df <- ys_filtered()
  req(nrow(df) > 0)

  d <- df %>%
    dplyr::filter(!is.na(marital_label), !is.na(gender_label), !is.na(employment_label)) %>%
    dplyr::mutate(Group = paste0(marital_label, " \u2014 ", gender_label)) %>%
    dplyr::group_by(Group) %>%
    dplyr::summarise(
      Total          = dplyr::n(),
      Employed       = sum(employment_label == "Employed",     na.rm = TRUE),
      `Not Employed` = sum(employment_label == "Not Employed", na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    dplyr::mutate(`Emp. Rate` = Employed / Total)

  ord <- c("Single \u2014 Male", "Single \u2014 Female",
           "Married \u2014 Male", "Married \u2014 Female")
  d <- d[order(match(d$Group, ord)), ]

  total_row <- tibble::tibble(
    Group          = "TOTAL",
    Total          = sum(d$Total),
    Employed       = sum(d$Employed),
    `Not Employed` = sum(d$`Not Employed`),
    `Emp. Rate`    = sum(d$Employed) / sum(d$Total)
  )

  dplyr::bind_rows(d, total_row)
})

output$ys_table_marital <- DT::renderDT({
  d <- ys_marital_tbl()
  req(nrow(d) > 0)

  DT::datatable(
    d,
    class    = "nowrap compact stripe hover",
    rownames = FALSE,
    options  = list(dom = "t", scrollX = TRUE,
                    language = list(emptyTable = "No data available"))
  ) %>%
    DT::formatPercentage("Emp. Rate", digits = 1) %>%
    DT::formatStyle(
      "Emp. Rate",
      background         = DT::styleColorBar(c(0, 1), "#0B4D3B"),
      backgroundSize     = "100% 80%",
      backgroundRepeat   = "no-repeat",
      backgroundPosition = "center"
    ) %>%
    DT::formatStyle(
      "Group",
      target     = "row",
      fontWeight = DT::styleEqual("TOTAL", "bold")
    )
})

output$ys_dl_marital <- downloadHandler(
  filename = function() paste0("employment_by_marital_gender_", Sys.Date(), ".csv"),
  content  = function(file) {
    d <- ys_marital_tbl()
    d$`Emp. Rate` <- scales::percent(d$`Emp. Rate`, accuracy = 0.1)
    write.csv(d, file, row.names = FALSE)
  }
)
