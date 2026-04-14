library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(tidyr)
library(readr)
library(digest)    # password hashing
library(shinyjs)         # JS helpers
library(ggpubr)          # error plots, dot charts, pub themes
library(shinycssloaders) # loading spinners

# ── PATHS ────────────────────────────────────────────────────
USERS_FILE <- "data/users.csv"

# ── PASSWORD UTILITIES ───────────────────────────────────────
hash_pw  <- function(pw) digest(pw, algo = "sha256", serialize = FALSE)
check_pw <- function(pw, hash) identical(hash_pw(pw), hash)

# ── USER STORE ───────────────────────────────────────────────
load_users <- function() {
  if (!file.exists(USERS_FILE)) {
    tibble(username="admin", password_hash=hash_pw("Admin@ASYV2024"),
           role="admin", full_name="ASYV Administrator",
           email="admin@asyv.org", created_at=as.character(Sys.Date()), active="TRUE")
  } else {
    read_csv(USERS_FILE, show_col_types = FALSE,
             col_types = cols(created_at = col_character(),
                              active     = col_character()))
  }
}
save_users <- function(df) write_csv(df, USERS_FILE)

# ── Plotly theme helper ───────────────────────────────────────
# Uses modifyList so caller's xaxis/yaxis args MERGE with (not replace) defaults.
plotly_layout <- function(p, ...) {
  args <- list(...)

  axis_base <- list(
    gridcolor = "#F1F5F9", gridwidth = 1,
    zeroline  = FALSE,     showline  = FALSE,
    tickfont  = list(size = 11.5, color = "#94A3B8"),
    titlefont = list(size = 12,   color = "#64748B")
  )

  args$xaxis <- modifyList(axis_base, if (!is.null(args$xaxis)) args$xaxis else list())
  args$yaxis <- modifyList(axis_base, if (!is.null(args$yaxis)) args$yaxis else list())

  base <- list(
    font          = list(family = "'Plus Jakarta Sans', -apple-system, sans-serif",
                         size = 12.5, color = "#334155"),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    hoverlabel    = list(
      bgcolor     = "#1E293B",
      bordercolor = "#334155",
      font        = list(color = "#F1F5F9", size = 12,
                         family = "'Plus Jakarta Sans', sans-serif")
    )
  )

  do.call(layout, c(list(p), modifyList(base, args))) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverCompareCartesian", "hoverClosestCartesian", "toggleSpikelines"
      ),
      displaylogo          = FALSE,
      toImageButtonOptions = list(format = "png", width = 960, height = 520,
                                  filename = "asyv_chart")
    )
}

# ── DEPARTMENT DISCOVERY ─────────────────────────────────────
dept_dirs  <- list.dirs("departments", full.names = TRUE, recursive = FALSE)
dept_order <- c("alumini", "youth_survey")
dept_dirs  <- dept_dirs[basename(dept_dirs) %in% dept_order]
dept_dirs  <- dept_dirs[order(match(basename(dept_dirs), dept_order))]
DEPARTMENTS <- lapply(dept_dirs, function(d) {
  cfg_env <- new.env(parent = globalenv())
  source(file.path(d, "config.R"),    local = cfg_env)
  source(file.path(d, "data_load.R"), local = globalenv())
  list(id = basename(d), config = cfg_env$dept_config, dir = d)
})
