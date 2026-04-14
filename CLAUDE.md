# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running & Deploying

**Run locally** (in RStudio with `asyvdb.Rproj` open):
```r
shiny::runApp()
# or Ctrl+Shift+Enter
```

**Install packages** (first time only):
```r
source("deploy.R")  # runs only lines 1-28 to install
```

**Deploy to shinyapps.io:**
```r
library(rsconnect)
rsconnect::deployApp(
  appDir = "C:/Users/Christian/Desktop/asyvdb",
  appName = "asyvdb", account = "asyv", forceUpdate = TRUE
)
```
Live URL: https://asyv.shinyapps.io/asyvdb/

---

## Architecture

### Startup sequence
1. `global.R` — loads libraries, defines helpers (`hash_pw`, `check_pw`, `plotly_layout`, `load_users`, `save_users`), discovers and loads all departments into `DEPARTMENTS`
2. `ui.R` — iterates `DEPARTMENTS` to assemble tabs and layout; includes all CSS inline
3. `server.R` — per-session: sources `server/server_auth.R`, `server/server_users.R`, then loops over `DEPARTMENTS` sourcing each `server.R` into the server environment

### Department plugin system
Adding a department = creating a folder under `departments/` with four files:

```
departments/{dept_id}/
├── config.R      → must assign: dept_config <- list(id, name, icon, menu_items=list(...))
├── data_load.R   → sourced into globalenv(); put data frames here
├── ui_tabs.R     → must assign: dept_ui_tabs <- list(bs4TabItem(...), ...)
└── server.R      → output$*, reactive(), observeEvent() — has full access to input/output/session/auth/users_rv
```

**`config.R` shape:**
```r
dept_config <- list(
  id         = "my_dept",
  name       = "My Department",
  icon       = "chart-line",          # Font Awesome icon name
  menu_items = list(
    list(label = "Overview", tabName = "md_overview", icon = "house"),
    ...
  )
)
```

**TabName namespacing:** prefix all `tabName` values with a short dept code (`ac_`, `mh_`, `md_`, …) to avoid collisions.

**Sourcing contexts:**
- `config.R` → isolated `new.env(parent = globalenv())` — only `dept_config` escapes
- `data_load.R` → `globalenv()` — all variables become globally available
- `ui_tabs.R` → isolated `new.env(parent = globalenv())` — only `dept_ui_tabs` escapes
- `server.R` → server function's local environment — sees `input`, `output`, `session`, `auth`, `users_rv`, and all global data

---

## Key conventions

### Plotly charts
Always wrap with `plotly_layout()` — it merges caller args with the ASYV theme (font, grid color, transparent background, custom modebar). xaxis/yaxis args are deep-merged, not replaced:
```r
plot_ly(...) %>%
  plotly_layout(
    xaxis = list(title = "Cohort", tickangle = -20),
    yaxis = list(title = "Avg GPA", range = list(0, 100)),
    margin = list(t = 10, b = 50)
  )
```

**Standard bar style** (use consistently):
```r
marker = list(color = "rgba(R,G,B,.18)", line = list(color = "#HEX", width = 1.5))
text = ~value, textposition = "outside", textfont = list(size = 11, color = "#64748B")
```

### DataTables
All tables must use:
```r
datatable(df, class = "nowrap",
          options = list(dom = "tp", pageLength = 8, scrollX = TRUE),
          rownames = FALSE)
```
- `dom = "t"` for static tables (no pagination), `"tp"` for paginated
- `class = "nowrap"` prevents text wrapping when `scrollX` is on

### Sidebar treeview
The sidebar uses delegated jQuery (not AdminLTE's built-in plugin) because `renderUI` re-renders the DOM. The handler lives in `ui.R` and a `setTimeout` style-reset runs in `server/server_auth.R` after each render. Do not switch to AdminLTE's native treeview without updating both locations.

### Authentication
- `auth()` reactive holds `list(logged_in, username, role, full_name)` — check `isTRUE(auth()$logged_in)` before rendering sensitive outputs
- Roles: `"admin"` sees User Management tab; `"viewer"` sees only My Account
- Users persist in `data/users.csv`; passwords are SHA-256 hashed via `digest`
- Default credentials: `admin` / `Admin@ASYV2024`

---

## Existing departments

### Mental Health (`mh_` prefix)
- Data: two CSVs in `departments/mental_health/data/` — `data_old.csv` (2018–2024, 2-item dep / 3-item anx / 13 binary subscales) and `data_new.csv` (2022–2025, PHQ-8 / GAD-7 / Grit / Gratitude / SWLS / Meaning)
- Global vars: `old`, `new`, `item_labels`, `sev_dep()`, `sev_anx()`, `CLR`
- 6 tabs: `mh_overview`, `mh_dep_anx`, `mh_cohort`, `mh_baseline`, `mh_risk`, `mh_wellbeing`

### Academics (`ac_` prefix)
- Data: 500-row synthetic frame generated in `data_load.R` with `set.seed(42)`
- Global var: `ac_data` (StudentID, Cohort, Stage, Gender, Term, Math, English, Science, Kinyarwanda, GPA, Status)
- Status thresholds: Honors ≥ 75, Pass ≥ 50, Fail < 50
- 4 tabs: `ac_overview`, `ac_trends`, `ac_subjects`, `ac_cohorts`
- Local `%||%` null-coalescing operator defined at the bottom of `academics/server.R`

---

## Design tokens (CSS variables, set in `ui.R`)

| Variable | Value | Usage |
|---|---|---|
| `--asyv-forest` | `#0B4D3B` | Primary green |
| `--asyv-emerald` | `#10B981` | Accent / active states |
| `--asyv-gold` | `#D4A843` | Warning / tertiary |
| `--asyv-navy` | `#0F172A` | Navbar background |
| `--asyv-terracotta` | `#C75B39` | Depression / danger charts |

Chart palette for multi-series: `#378ADD` (blue), `#0E6B52` (green), `#534AB7` (purple), `#D4A843` (gold), `#C75B39` (terracotta).
