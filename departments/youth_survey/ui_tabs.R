dept_ui_tabs <- list(

  # ── YOUTH SURVEY DASHBOARD ────────────────────────────────────────────────
  bs4TabItem(
    tabName = "ys_dashboard",

    tags$head(tags$style(HTML("

      /* ── KPI Cards ─────────────────────────────────────────── */
      .ys-kpi-row { display: flex; width: 100%; margin-bottom: 20px; gap: 0; }

      .ys-kpi-card {
        flex: 1;
        padding: 18px 22px 14px;
        min-height: 115px;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        border-right: 1px solid rgba(255,255,255,0.15);
        cursor: default;
      }
      .ys-kpi-card:last-child { border-right: none; }

      .ys-kpi-header {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 4px;
      }
      .ys-kpi-header i {
        color: rgba(255,255,255,0.85);
        font-size: 12px;
      }
      .ys-kpi-title {
        color: rgba(255,255,255,0.88);
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 0.1em;
        text-transform: uppercase;
      }
      .ys-kpi-value {
        color: #FFD700;
        font-size: 2.6em;
        font-weight: 800;
        line-height: 1;
        margin: 2px 0;
      }
      .ys-kpi-subtitle {
        color: rgba(255,255,255,0.65);
        font-size: 10px;
        font-style: italic;
        margin-top: 2px;
      }

      /* ── Chart / Table card polish ──────────────────────────── */
      .ys-card {
        border-radius: 8px !important;
        box-shadow: 0 2px 8px rgba(0,0,0,.09) !important;
        overflow: hidden;
      }

      /* ── DT header colours ──────────────────────────────────── */
      #ys_table_province thead tr th,
      #ys_table_marital  thead tr th {
        background-color: #1F3864 !important;
        color: #ffffff !important;
        font-size: 12px !important;
      }

      /* ── Download button ────────────────────────────────────── */
      .ys-dl-btn {
        font-size: 0.76rem;
        padding: 2px 9px;
        border-radius: 4px;
        vertical-align: middle;
        line-height: 1.4;
      }
    "))),

    # ── FILTER PANEL ───────────────────────────────────────────────────────
    bs4Card(
      title       = tagList(shiny::icon("filter"), " Dashboard Filters"),
      status      = "success",
      solidHeader = FALSE,
      width       = 12,
      collapsible = TRUE,
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
        title       = "Respondents by Province",
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
        title       = "Education Level Distribution",
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
        title       = "Employment by Gender",
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
          "Employment Rate by Province",
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
          "Employment by Marital Status & Gender",
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
