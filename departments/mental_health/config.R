dept_config <- list(
  id          = "mental_health",
  name        = "Mental Health",
  description = "PHQ-8, GAD-7, grit, gratitude — cohort & clinical tracking",
  icon        = "heart-pulse",
  menu_items  = list(
    list(label = "Overview",             tabName = "mh_overview",  icon = "house"),
    list(label = "Depression & Anxiety", tabName = "mh_dep_anx",   icon = "heart-pulse"),
    list(label = "Cohort Progression",   tabName = "mh_cohort",    icon = "arrow-trend-up"),
    list(label = "EY Baseline",          tabName = "mh_baseline",  icon = "flag"),
    list(label = "Clinical Risk",        tabName = "mh_risk",      icon = "triangle-exclamation"),
    list(label = "Wellbeing",            tabName = "mh_wellbeing", icon = "star")
  )
)
