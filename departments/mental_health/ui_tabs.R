dept_ui_tabs <- list(

  # OVERVIEW ──────────────────────────────────────────
  bs4TabItem(tabName="mh_overview",
             fluidRow(
               bs4InfoBox("Old questionnaire records",nrow(old),icon=icon("database"),color="primary",width=3),
               bs4InfoBox("All questionnaire records",nrow(new),icon=icon("database"),color="success",width=3),
               bs4InfoBox("Total records",nrow(old)+nrow(new),icon=icon("users"),color="info",width=3),
               bs4InfoBox("Date range",paste(min(c(old$Year,new$Year),na.rm=T),"\u2013",max(c(old$Year,new$Year),na.rm=T)),
                          icon=icon("calendar"),color="warning",width=3)),
             fluidRow(
               bs4Card(title="Dataset summary",width=6,collapsible=TRUE,DTOutput("overview_table")),
               bs4Card(title="Assessments per year",width=6,collapsible=TRUE,plotlyOutput("overview_year_plot",height="300px"))),
             fluidRow(
               bs4Card(title="All questionnaire — cohort summary",width=7,collapsible=TRUE,DTOutput("overview_cohort_table")),
               bs4Card(title="Gender breakdown",width=5,collapsible=TRUE,DTOutput("overview_gender_table"))),
             fluidRow(
               bs4Card(title="PHQ-8 severity by stage",width=12,collapsible=TRUE,DTOutput("overview_stage_sev_table"))),
             fluidRow(bs4Card(title="About",width=12,collapsible=TRUE,
                              p(strong("Old questionnaire (2018\u20132024):")," 2-item depression, 3-item anxiety, 13 binary clinical subscales."),
                              p(strong("All questionnaire (2022\u20132025):")," PHQ-8, GAD-7, grit, gratitude, SWLS, meaning/purpose.")))
  ),

  # DEPRESSION & ANXIETY ──────────────────────────────
  bs4TabItem(tabName="mh_dep_anx",
             fluidRow(
               column(3,bs4Card(title="Filters",width=12,collapsible=FALSE,
                                selectInput("da_source","Dataset",choices=c("All questionnaire (PHQ-8/GAD-7)"="new","Old questionnaire (2-item/3-item)"="old")),
                                selectInput("da_gender","Gender",choices=c("All","Female","Male")),
                                uiOutput("da_grade_ui"),
                                hr(),
                                p(style="font-size:11px;color:var(--text-muted);",
                                  strong("PHQ-8:"),br(),
                                  "None 0\u20134 \u00b7 Mild 5\u20139",br(),
                                  "Moderate 10\u201314 \u00b7 Mod-severe 15\u201319",br(),
                                  "Severe 20+"),
                                p(style="font-size:11px;color:var(--text-muted);margin-top:6px;",
                                  strong("GAD-7:"),br(),
                                  "Minimal 0\u20134 \u00b7 Mild 5\u20139",br(),
                                  "Moderate 10\u201314 \u00b7 Severe 15+"))),
               column(9,
                      fluidRow(
                        bs4ValueBoxOutput("da_n",      width=3),
                        bs4ValueBoxOutput("da_dep_avg",width=3),
                        bs4ValueBoxOutput("da_anx_avg",width=3),
                        bs4ValueBoxOutput("da_dep_pct",width=3)),
                      fluidRow(
                        bs4Card(title="Grade / cohort summary",width=12,collapsible=TRUE,
                                DTOutput("da_summary_table"))),
                      fluidRow(
                        bs4Card(title="Severity distribution",width=5,collapsible=TRUE,
                                plotlyOutput("da_sev_plot",height="280px")),
                        bs4Card(title="Score by gender",width=7,collapsible=TRUE,
                                plotlyOutput("da_gender_plot",height="280px"))),
                      fluidRow(
                        bs4Card(title="Score by grade / cohort",width=12,collapsible=TRUE,
                                plotlyOutput("da_grade_plot",height="300px"))),
                      fluidRow(
                        bs4Card(title="Score distribution",width=5,collapsible=TRUE,
                                plotlyOutput("da_hist_plot",height="260px")),
                        bs4Card(title="Item-level breakdown",width=7,collapsible=TRUE,
                                plotlyOutput("da_items_plot",height="260px"))))
             )
  ),

  # COHORT PROGRESSION ────────────────────────────────
  bs4TabItem(tabName="mh_cohort",
             fluidRow(
               column(3,bs4Card(title="Filters",width=12,collapsible=FALSE,
                                selectInput("cp_grade","Cohort",choices=c("Intwari","Ishami","Ijabo","Grade2028","Grade of 2027","Ey2022","Grade_2029")),
                                selectInput("cp_gender","Gender",choices=c("All","Female","Male")),
                                selectInput("cp_metric","Metric",choices=c("Depression % of max"="Dep_pct","Anxiety % of max"="Anx_pct","Depression (raw)"="Depression","Anxiety (raw)"="Anxiety")))),
               column(9,
                      fluidRow(
                        bs4ValueBoxOutput("cp_n_ey",width=3),
                        bs4ValueBoxOutput("cp_n_s4",width=3),
                        bs4ValueBoxOutput("cp_n_s5",width=3),
                        bs4ValueBoxOutput("cp_n_s6",width=3)),
                      fluidRow(
                        bs4Card(title="Stage summary",width=12,collapsible=TRUE,
                                DTOutput("cp_summary_table"))),
                      fluidRow(
                        bs4Card(title="Progression across stages",width=12,collapsible=TRUE,
                                plotlyOutput("cp_line_plot",height="300px"))),
                      fluidRow(
                        bs4Card(title="By stage and gender",width=5,collapsible=TRUE,
                                plotlyOutput("cp_gender_plot",height="280px")),
                        bs4Card(title="Cross-cohort at same stage",width=7,collapsible=TRUE,
                                plotlyOutput("cp_cross_plot",height="280px"))),
                      fluidRow(
                        bs4Card(title="PHQ-8 item breakdown by stage",width=12,collapsible=TRUE,
                                plotlyOutput("cp_items_plot",height="300px"))))
             )
  ),

  # EY BASELINE ───────────────────────────────────────
  bs4TabItem(tabName="mh_baseline",
             fluidRow(
               column(3,bs4Card(title="Filters",width=12,collapsible=FALSE,
                                selectInput("bl_metric","Metric",choices=c("Depression % of max"="Dep_pct","Anxiety % of max"="Anx_pct","Depression (raw)"="Depression","Anxiety (raw)"="Anxiety")),
                                selectInput("bl_gender","Gender",choices=c("All","Female","Male")))),
               column(9,
                      fluidRow(
                        bs4ValueBoxOutput("bl_cohorts", width=3),
                        bs4ValueBoxOutput("bl_highest", width=3),
                        bs4ValueBoxOutput("bl_lowest",  width=3),
                        bs4ValueBoxOutput("bl_total_n", width=3)),
                      fluidRow(
                        bs4Card(title="EY cohort summary",width=12,collapsible=TRUE,
                                DTOutput("bl_table"))),
                      fluidRow(
                        bs4Card(title="EY baseline by cohort",width=7,collapsible=TRUE,
                                plotlyOutput("bl_bar_plot",height="280px")),
                        bs4Card(title="Depression vs anxiety",width=5,collapsible=TRUE,
                                plotlyOutput("bl_scatter_plot",height="280px"))),
                      fluidRow(
                        bs4Card(title="Gender breakdown at EY",width=6,collapsible=TRUE,
                                plotlyOutput("bl_gender_plot",height="260px")),
                        bs4Card(title="EY \u2192 S4 (Grade2028)",width=6,collapsible=TRUE,
                                plotlyOutput("bl_ey_s4_plot",height="260px"))))
             )
  ),

  # CLINICAL RISK ─────────────────────────────────────
  bs4TabItem(tabName="mh_risk",
             fluidRow(
               column(3,bs4Card(title="Filters",width=12,collapsible=FALSE,
                                selectInput("ri_source","Dataset",choices=c("All questionnaire (PHQ item)"="new","Old questionnaire (subscale flags)"="old")),
                                selectInput("ri_gender","Gender",choices=c("All","Female","Male")),
                                uiOutput("ri_grade_ui"),
                                uiOutput("ri_cat_ui"))),
               column(9,
                      fluidRow(
                        bs4ValueBoxOutput("ri_sui_n",   width=3),
                        bs4ValueBoxOutput("ri_sui_pct", width=3),
                        bs4ValueBoxOutput("ri_psy_n",   width=3),
                        bs4ValueBoxOutput("ri_psy_pct", width=3)),
                      fluidRow(
                        bs4Card(title="Grade / cohort risk summary",width=12,collapsible=TRUE,
                                DTOutput("ri_summary_table"))),
                      fluidRow(
                        bs4Card(title="Suicidal ideation by grade",width=7,collapsible=TRUE,
                                plotlyOutput("ri_sui_grade",height="280px")),
                        bs4Card(title="Suicidal ideation by gender",width=5,collapsible=TRUE,
                                plotlyOutput("ri_sui_gender",height="280px"))),
                      fluidRow(
                        bs4Card(title="Suicidal ideation across stages \u2014 All file",width=12,collapsible=TRUE,
                                plotlyOutput("ri_sui_stage",height="260px"))),
                      fluidRow(
                        bs4Card(title="Psychosis flags by grade (old)",width=6,collapsible=TRUE,
                                plotlyOutput("ri_psy_grade",height="280px")),
                        bs4Card(title="All subscale flags \u2014 old file",width=6,collapsible=TRUE,
                                plotlyOutput("ri_subscales",height="280px"))))
             )
  ),

  # WELLBEING ─────────────────────────────────────────
  bs4TabItem(tabName="mh_wellbeing",
             fluidRow(
               column(3,bs4Card(title="Filters",width=12,collapsible=FALSE,
                                selectInput("wb_gender","Gender",choices=c("All","Female","Male")),
                                selectInput("wb_grade","Grade/cohort",choices=c("All",sort(unique(new$Grade[!is.na(new$Grade)])))),
                                selectInput("wb_stage","Stage",choices=c("All","EY (Entry Year)","S4","S5","S6")),
                                hr(),
                                p(style="font-size:11px;color:var(--text-muted);",
                                  "Grit 0\u201345",br(),
                                  "Gratitude 0\u201318",br(),
                                  "SWLS 0\u201315",br(),
                                  "Meaning 0\u201330"))),
               column(9,
                      fluidRow(
                        bs4ValueBoxOutput("wb_grit_avg", width=3),
                        bs4ValueBoxOutput("wb_grat_avg", width=3),
                        bs4ValueBoxOutput("wb_swls_avg", width=3),
                        bs4ValueBoxOutput("wb_mean_avg", width=3)),
                      fluidRow(
                        bs4Card(title="Grade / cohort wellbeing summary",width=12,collapsible=TRUE,
                                DTOutput("wb_summary_table"))),
                      fluidRow(
                        bs4Card(title="Wellbeing across stages",width=12,collapsible=TRUE,
                                plotlyOutput("wb_stage_plot",height="280px"))),
                      fluidRow(
                        bs4Card(title="Scores by gender",width=5,collapsible=TRUE,
                                plotlyOutput("wb_gender_plot",height="280px")),
                        bs4Card(title="Scores by grade",width=7,collapsible=TRUE,
                                plotlyOutput("wb_grade_plot",height="280px"))),
                      fluidRow(
                        bs4Card(title="Score distributions",width=6,collapsible=TRUE,
                                plotlyOutput("wb_dist_plot",height="270px")),
                        bs4Card(title="Wellbeing vs depression",width=6,collapsible=TRUE,
                                plotlyOutput("wb_corr_plot",height="270px"))))
             )
  )

)
