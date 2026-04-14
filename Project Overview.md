Project Overview
Build an R Shiny dashboard that replicates an Excel survey analysis dashboard. The data source is a KoboToolbox survey export (CSV or Excel file). The app should be clean, professional, and fully reactive.

Data Source & Key Columns
Load the survey data from a CSV/Excel file. The relevant columns are:

Column Name Description Codes
S3S1Q1 Employment status 1=Employed, 0=Not employed, 8=N/A
S1S2Q3 Gender 1=Male, 2=Female
S1S2Q1 Marital status 1=Single, 2=Married
S1S2Q2 Date of birth Excel serial date number
S1S3Q4 Education level 1=None, 2=Primary, 3=Secondary, 4=Vocational, 5=University, 6=Other
province_interview Province 1=Kigali City, 2=Southern, 3=Western, 4=Northern, 5=Eastern
consent_given Consent 1=given, 0=not given
Step 1 — Setup & Dependencies

# Install and load these packages:

library(shiny)
library(shinydashboard) # or bslib for modern UI
library(tidyverse)
library(readxl) # or readr for CSV
library(plotly) # for interactive charts
library(DT) # for interactive tables
library(scales) # for percent formatting
Step 2 — Data Preprocessing

# On load, clean the data:

# 1. Filter only valid employment responses: S3S1Q1 %in% c(0, 1)

# 2. Filter only valid gender responses: S1S2Q3 %in% c(1, 2)

# 3. Map codes to labels:

# - Gender: 1 → "Male", 2 → "Female"

# - Marital: 1 → "Single", 2 → "Married"

# - Education: 1→"None", 2→"Primary", 3→"Secondary",

# 4→"Vocational", 5→"University", 6→"Other"

# - Province: 1→"Kigali City", 2→"Southern", 3→"Western",

# 4→"Northern", 5→"Eastern"

# - Employment: 1→"Employed", 0→"Not Employed"

# 4. Convert DOB from Excel serial to Date:

# dob_date = as.Date(S1S2Q2, origin = "1899-12-30")

# age = as.integer((Sys.Date() - dob_date) / 365.25)

# Filter ages between 15 and 60 (exclude invalid serials)

Step 3 — Sidebar Filters (reactive)
Add a sidebar with these filter inputs:

Province — selectInput, choices = all provinces + "All"
Gender — radioButtons, choices = Male / Female / All
Marital Status — checkboxGroupInput, choices = Single, Married
Employment Status — radioButtons, choices = Employed / Not Employed / All
All charts and tables must react to these filters using a single reactive({}) filtered dataset.

Step 4 — 4 KPI Value Boxes
Use valueBox() from shinydashboard (or value_box() from bslib):

Box Formula Color
Total Respondents nrow(df) Blue (#1F3864)
Employment Rate mean(df$S3S1Q1 == 1, na.rm=T) formatted as %	Green (#375623)
Female Share	mean(df$S1S2Q3 == 2, na.rm=T) formatted as % Orange (#843C0C)
Married Rate mean(df$S1S2Q1 == 2, na.rm=T) formatted as % Purple (#4B0082)
Step 5 — 3 Charts (use plotly for interactivity)
Chart 1 — Respondents by Province (horizontal bar)

# Group by province_label, count, sort descending

# plotly: plot_ly(type="bar", orientation="h")

# Color: #2E4D87

# Title: "Respondents by Province"

Chart 2 — Education Level Distribution (vertical bar / column)

# Group by education_label, count

# Order: None → Primary → Secondary → Vocational → University → Other

# plotly: plot_ly(type="bar")

# Color: #375623

# Title: "Education Level Distribution"

Chart 3 — Employment by Gender (stacked horizontal bar)

# Group by gender_label + employment_label, count

# plotly: plot_ly(type="bar", orientation="h", barmode="stack")

# Colors: Employed=#375623, Not Employed=#C00000

# Title: "Employment by Gender"

# Add % annotation on each bar segment

Step 6 — 2 Interactive Tables (use DT::datatable)
Table 1 — Employment Rate by Province

# Columns: Province | Total | Employed | Employment Rate

# Group by province, compute counts and rate

# Format rate column as percentage (scales::percent)

# Add conditional color bar on Employment Rate column using DT::formatStyle + styleColorBar

Table 2 — Employment by Marital Status & Gender

# Columns: Group | Total | Employed | Not Employed | Emp. Rate

# Groups: Single–Male, Single–Female, Married–Male, Married–Female, TOTAL

# Use dplyr::group_by(marital_label, gender_label) %>% summarise(...)

# Add a TOTAL row using bind_rows()

# Bold the TOTAL row using DT formatStyle

Step 7 — UI Layout

# Use shinydashboard::dashboardPage or bslib::page_sidebar

# Header: "YOUTH SURVEY DASHBOARD — Rwanda"

# Sidebar: all 4 filters (Step 3)

# Body layout (fluidRow / layout_columns):

# Row 1: 4 valueBoxes side by side

# Row 2: Chart 1 (left 50%) + Chart 2 (right 50%)

# Row 3: Chart 3 (full width or left 50%)

# Row 4: Table 1 (left 50%) + Table 2 (right 50%)

Step 8 — Styling

# Custom CSS in tags$style(HTML("...")):

.skin-blue .main-header .logo { background-color: #1F3864; }
.skin-blue .main-header .navbar { background-color: #2E4D87; }
.small-box.bg-blue { background-color: #1F3864 !important; }

# Value box numbers: font-size: 2.2em, font-weight: bold, color: #FFD700

# Chart backgrounds: white with subtle border-radius: 8px box-shadow

# Table header: background #1F3864, color white

Step 9 — File Structure
/shiny-dashboard/ ├── app.R # main app (or split into ui.R + server.R) ├── data/ │ └── survey_data.xlsx # raw KoboToolbox export ├── R/ │ ├── data_prep.R # all cleaning/labelling logic │ └── chart_helpers.R # plotly chart functions └── www/ └── custom.css # all custom styles
Final Notes for Claude Code
Make all 4 KPI cards, all 3 charts, and both tables fully reactive to the sidebar filters
Use req() guards before any computation to avoid errors on empty filtered data
Add a download button to export each table as CSV
Use shinycssloaders::withSpinner() on charts and tables for loading states
Test with shiny::runApp() and confirm all filters work correctly
