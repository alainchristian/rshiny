# ============================================================
# ASYV Dashboard — Setup & Deployment Script
# Run this script ONCE in RStudio before deploying
# ============================================================

# ── Step 1: Install required packages ───────────────────────
packages <- c(
  "shiny",
  "bs4Dash",
  "dplyr",
  "ggplot2",
  "plotly",
  "DT",
  "scales",
  "tidyr",
  "readr",
  "rsconnect",        # needed for shinyapps.io deployment
  "ggpubr",           # error plots, dot charts, pub-ready themes (alumni dept)
  "shinycssloaders"   # loading spinners (youth survey dept)
)

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install, dependencies = TRUE)
} else {
  message("All packages already installed.")
}

# ── Step 2: Connect to shinyapps.io ─────────────────────────
# After running this, a browser window will open asking you
# to authorise. Log in with your 'asyv' account credentials.

library(rsconnect)

rsconnect::setAccountInfo(name='asyv', token='EFBE1B1B5F4885C26A6A510769EAD5EE', secret='ZKn/MbPZKxUYzQG/W+7m8nYzpVSurwlBNzisaECR')

# ── Step 3: Deploy ──────────────────────────────────────────
# Set working directory to the app folder first, then run:

app_dir <- "C:/Users/Christian/Desktop/asyvdb"   # <-- update this path

rsconnect::deployApp(
  appDir  = app_dir,
  appName = "asyvdb",
  account = "asyv",
  forceUpdate = TRUE
)

# After deployment the URL will be:
# https://asyv.shinyapps.io/asyvdb/
