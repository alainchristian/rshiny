dept_ui_tabs <- list(

  # ── YOUTH SURVEY DASHBOARD ────────────────────────────────────────────────
  bs4TabItem(
    tabName = "ys_dashboard",

    tags$head(tags$style(HTML("

      /* ── Typography ─────────────────────────────────────────── */
      .ys-kpi-row, .ys-card, .card.ys-card .card-body,
      #ys_table_province, #ys_table_marital {
        font-family: 'Plus Jakarta Sans', 'Segoe UI', system-ui, sans-serif;
      }

      /* ── KPI Row wrapper ────────────────────────────────────── */
      .ys-kpi-row {
        display: flex;
        width: 100%;
        margin-bottom: 22px;
        gap: 0;
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 4px 18px rgba(0,0,0,0.14);
      }

      /* ── Individual KPI card ────────────────────────────────── */
      .ys-kpi-card {
        flex: 1;
        padding: 20px 24px 16px;
        min-height: 122px;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        border-right: 1px solid rgba(255,255,255,0.14);
        cursor: default;
        transition: filter 0.18s ease;
        position: relative;
        overflow: hidden;
      }
      .ys-kpi-card:hover { filter: brightness(1.09); }
      .ys-kpi-card:last-child { border-right: none; }

      /* shimmer bar at base of each card */
      .ys-kpi-card::after {
        content: '';
        position: absolute;
        bottom: 0; left: 0; right: 0;
        height: 3px;
        background: rgba(255,255,255,0.18);
      }

      /* ── KPI header (icon + label row) ─────────────────────── */
      .ys-kpi-header {
        display: flex;
        align-items: center;
        gap: 9px;
        margin-bottom: 6px;
      }
      .ys-kpi-header i {
        color: rgba(255,255,255,0.95);
        font-size: 13px;
        background: rgba(255,255,255,0.16);
        padding: 6px 7px;
        border-radius: 7px;
        width: 30px;
        text-align: center;
        flex-shrink: 0;
      }
      .ys-kpi-title {
        color: rgba(255,255,255,0.92);
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 0.11em;
        text-transform: uppercase;
        line-height: 1.3;
      }

      /* ── KPI value ──────────────────────────────────────────── */
      .ys-kpi-value {
        color: #FFD700;
        font-size: 2.75em;
        font-weight: 800;
        line-height: 1;
        margin: 5px 0 2px;
        text-shadow: 0 1px 4px rgba(0,0,0,0.22);
        letter-spacing: -0.01em;
      }

      /* ── KPI subtitle ───────────────────────────────────────── */
      .ys-kpi-subtitle {
        color: rgba(255,255,255,0.68);
        font-size: 10.5px;
        font-style: italic;
        margin-top: 3px;
        line-height: 1.4;
      }

      /* ── Chart / Table card polish ──────────────────────────── */
      .ys-card {
        border-radius: 10px !important;
        box-shadow: 0 2px 14px rgba(0,0,0,0.08) !important;
        overflow: hidden;
        border: 1px solid rgba(0,0,0,0.055) !important;
      }
      .ys-card .card-header {
        font-family: 'Plus Jakarta Sans', 'Segoe UI', sans-serif;
        font-weight: 700;
        font-size: 13.5px;
        letter-spacing: 0.01em;
      }

      /* ── Filter card label styling ──────────────────────────── */
      .ys-filters .control-label,
      .ys-filters label.control-label {
        font-family: 'Plus Jakarta Sans', 'Segoe UI', sans-serif;
        font-size: 11px;
        font-weight: 700;
        color: #374151;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        margin-bottom: 5px;
      }
      .ys-filters .form-control,
      .ys-filters select.form-control {
        border-radius: 6px;
        border: 1px solid #d1d5db;
        font-size: 13px;
        color: #1f2937;
        transition: border-color 0.15s ease, box-shadow 0.15s ease;
      }
      .ys-filters .form-control:focus,
      .ys-filters select.form-control:focus {
        border-color: #0B4D3B;
        box-shadow: 0 0 0 2px rgba(11,77,59,0.14);
        outline: none;
      }
      .ys-filters .radio-inline label,
      .ys-filters .checkbox-inline label {
        font-size: 13px;
        color: #374151;
        font-weight: 500;
      }

      /* ── DT: table header colours ───────────────────────────── */
      #ys_table_province thead tr th,
      #ys_table_marital  thead tr th {
        background-color: #0B4D3B !important;
        color: #ffffff !important;
        font-family: 'Plus Jakarta Sans', 'Segoe UI', sans-serif !important;
        font-size: 12px !important;
        font-weight: 700 !important;
        letter-spacing: 0.05em !important;
        padding: 10px 8px !important;
        border-bottom: 2px solid #0a4033 !important;
      }

      /* ── DT: zebra striping ─────────────────────────────────── */
      #ys_table_province table.dataTable tbody tr:nth-child(even) td,
      #ys_table_marital  table.dataTable tbody tr:nth-child(even) td {
        background-color: #f2f7f5 !important;
      }
      #ys_table_province table.dataTable tbody tr:nth-child(odd) td,
      #ys_table_marital  table.dataTable tbody tr:nth-child(odd) td {
        background-color: #ffffff !important;
      }

      /* ── DT: row hover ──────────────────────────────────────── */
      #ys_table_province table.dataTable tbody tr:hover td,
      #ys_table_marital  table.dataTable tbody tr:hover td {
        background-color: #d5ece5 !important;
        transition: background-color 0.12s ease;
      }

      /* ── DT: cell typography ────────────────────────────────── */
      #ys_table_province table.dataTable td,
      #ys_table_marital  table.dataTable td {
        font-family: 'Plus Jakarta Sans', 'Segoe UI', sans-serif;
        font-size: 12.5px;
        color: #1f2937;
        padding: 7px 8px !important;
        vertical-align: middle;
      }

      /* ── DT: TOTAL row accent ───────────────────────────────── */
      #ys_table_province table.dataTable tbody tr:last-child td,
      #ys_table_marital  table.dataTable tbody tr:last-child td {
        border-top: 2px solid #0B4D3B !important;
        background-color: #e8f5f0 !important;
      }

      /* ── Download button ─────────────────────────────────────── */
      .ys-dl-btn {
        font-family: 'Plus Jakarta Sans', 'Segoe UI', sans-serif;
        font-size: 0.74rem;
        font-weight: 600;
        padding: 3px 10px;
        border-radius: 5px;
        vertical-align: middle;
        line-height: 1.45;
        border-color: #0B4D3B !important;
        color: #0B4D3B !important;
        transition: background-color 0.15s ease, color 0.15s ease;
      }
      .ys-dl-btn:hover {
        background-color: #0B4D3B !important;
        color: #ffffff !important;
      }

      /* ── Responsive: tablet ──────────────────────────────────── */
      @media (max-width: 991px) {
        .ys-kpi-value { font-size: 2.3em; }
        .ys-kpi-card  { padding: 16px 18px 13px; min-height: 108px; }
      }

      /* ── Responsive: mobile ──────────────────────────────────── */
      @media (max-width: 767px) {
        .ys-kpi-row {
          flex-direction: column;
          border-radius: 8px;
        }
        .ys-kpi-card {
          border-right: none;
          border-bottom: 1px solid rgba(255,255,255,0.15);
          min-height: 88px;
          padding: 14px 18px 12px;
        }
        .ys-kpi-card:last-child { border-bottom: none; }
        .ys-kpi-value { font-size: 2.1em; }
        .ys-kpi-title { font-size: 9.5px; }
      }
      @media (max-width: 480px) {
        .ys-kpi-value { font-size: 1.85em; }
      }
    "))),

    # ── FILTER PANEL ───────────────────────────────────────────────────────
    bs4Card(
      title       = tagList(shiny::icon("filter"), " Dashboard Filters"),
      status      = "success",
      solidHeader = FALSE,
      width       = 12,
      collapsible = TRUE,
      class       = "ys-filters",
      fluidRow(
        column(3,
          selectInput(
            "ys_province", "Province",
            choices  = c("All", "Kigali City", "Southern", "Western", "Northern", "Eastern"),
            selected = "All"
          )
        ),
        column(3,
          radioButtons(
            "ys_gender", "Gender",
            choices  = c("All", "Male", "Female"),
            selected = "All",
            inline   = TRUE
          )
        ),
        column(3,
          checkboxGroupInput(
            "ys_marital", "Marital Status",
            choices  = c("Single", "Married"),
            selected = c("Single", "Married"),
            inline   = TRUE
          )
        ),
        column(3,
          radioButtons(
            "ys_employment", "Employment Status",
            choices  = c("All", "Employed", "Not Employed"),
            selected = "All",
            inline   = TRUE
          )
        )
      )
    ),

    # ── KPI ROW (4 custom full-background cards) ────────────────────────────
    tags$div(
      class = "ys-kpi-row",
      uiOutput("ys_kpi_total"),
      uiOutput("ys_kpi_employment"),
      uiOutput("ys_kpi_female"),
      uiOutput("ys_kpi_married")
    ),

    # ── CHARTS ROW 1  (Province | Education) ───────────────────────────────
    fluidRow(
      bs4Card(
        title       = tagList(shiny::icon("map-marker-alt"), " Respondents by Province"),
        status      = "success",
        solidHeader = TRUE,
        width       = 6,
        class       = "ys-card",
        shinycssloaders::withSpinner(
          plotlyOutput("ys_chart_province", height = "320px"),
          color = "#0B4D3B", type = 4, size = 0.8
        )
      ),
      bs4Card(
        title       = tagList(shiny::icon("graduation-cap"), " Education Level Distribution"),
        status      = "success",
        solidHeader = TRUE,
        width       = 6,
        class       = "ys-card",
        shinycssloaders::withSpinner(
          plotlyOutput("ys_chart_education", height = "320px"),
          color = "#0B4D3B", type = 4, size = 0.8
        )
      )
    ),

    # ── CHARTS ROW 2  (Employment by Gender — full width) ──────────────────
    fluidRow(
      bs4Card(
        title       = tagList(shiny::icon("venus-mars"), " Employment by Gender"),
        status      = "success",
        solidHeader = TRUE,
        width       = 12,
        class       = "ys-card",
        shinycssloaders::withSpinner(
          plotlyOutput("ys_chart_emp_gender", height = "240px"),
          color = "#0B4D3B", type = 4, size = 0.8
        )
      )
    ),

    # ── TABLES ROW ─────────────────────────────────────────────────────────
    fluidRow(
      bs4Card(
        title = tagList(
          shiny::icon("table"), " Employment Rate by Province",
          tags$span(
            downloadButton("ys_dl_province", "Download CSV",
                           class = "btn btn-outline-secondary ys-dl-btn"),
            style = "float:right; margin-top:-4px;"
          )
        ),
        status      = "info",
        solidHeader = TRUE,
        width       = 6,
        class       = "ys-card",
        shinycssloaders::withSpinner(
          DT::dataTableOutput("ys_table_province"),
          color = "#0B4D3B", type = 4, size = 0.8
        )
      ),
      bs4Card(
        title = tagList(
          shiny::icon("table"), " Employment by Marital Status & Gender",
          tags$span(
            downloadButton("ys_dl_marital", "Download CSV",
                           class = "btn btn-outline-secondary ys-dl-btn"),
            style = "float:right; margin-top:-4px;"
          )
        ),
        status      = "info",
        solidHeader = TRUE,
        width       = 6,
        class       = "ys-card",
        shinycssloaders::withSpinner(
          DT::dataTableOutput("ys_table_marital"),
          color = "#0B4D3B", type = 4, size = 0.8
        )
      )
    )
  )
)
