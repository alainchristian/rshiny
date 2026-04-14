# ════════════════════════════════════════════════════════
# DASHBOARD OUTPUTS
# ════════════════════════════════════════════════════════

# ── OVERVIEW ─────────────────────────────────────────────
output$overview_table <- renderDT({
  tibble(Dataset=c("Old questionnaire","All questionnaire"),Records=c(nrow(old),nrow(new)),
         Females=c(sum(old$Gender=="Female",na.rm=T),sum(new$Gender=="Female",na.rm=T)),
         Males=c(sum(old$Gender=="Male",na.rm=T),sum(new$Gender=="Male",na.rm=T)),
         `Year range`=c(paste(min(old$Year,na.rm=T),"\u2013",max(old$Year,na.rm=T)),paste(min(new$Year,na.rm=T),"\u2013",max(new$Year,na.rm=T))),
         `Depression scale`=c("2-item (max 8)","PHQ-8 (max 24)"),`Anxiety scale`=c("3-item (max 12)","GAD-7 (max 21)")) %>%
    datatable(class="nowrap",options=list(dom="t",paging=FALSE,scrollX=TRUE),rownames=FALSE)
})

output$overview_year_plot <- renderPlotly({
  df_old <- old %>% count(Year, Source) %>% filter(!is.na(Year))
  df_new <- new %>% count(Year, Source) %>% filter(!is.na(Year))
  plot_ly(type="bar") %>%
    add_trace(data=df_old, x=~Year, y=~n, name="Old questionnaire",
              marker=list(color="rgba(11,77,59,.20)", line=list(color="#0B4D3B",width=1.5)),
              textfont=list(size=11,color="#64748B")) %>%
    add_trace(data=df_new, x=~Year, y=~n, name="All questionnaire",
              marker=list(color="rgba(16,185,129,.20)", line=list(color="#10B981",width=1.5)),
              textfont=list(size=11,color="#64748B")) %>%
    plotly_layout(barmode="stack",
                  xaxis=list(title="Year", gridcolor="#F1F5F9"),
                  yaxis=list(title="Assessments", gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15), margin=list(t=10))
})

output$overview_cohort_table <- renderDT({
  new %>%
    filter(!is.na(Grade)) %>%
    group_by(`Grade/Cohort`=Grade) %>%
    summarise(
      N              = n(),
      `Avg PHQ-8`    = round(mean(Depression, na.rm=TRUE), 1),
      `Avg GAD-7`    = round(mean(Anxiety,    na.rm=TRUE), 1),
      `Dep % of max` = paste0(round(mean(Dep_pct, na.rm=TRUE), 1), "%"),
      `Anx % of max` = paste0(round(mean(Anx_pct, na.rm=TRUE), 1), "%"),
      `Mod+ dep`     = paste0(round(mean(Depression >= 10, na.rm=TRUE) * 100, 1), "%"),
      .groups="drop"
    ) %>%
    arrange(desc(`Avg PHQ-8`)) %>%
    datatable(class="nowrap",options=list(dom="tp", pageLength=8, scrollX=TRUE), rownames=FALSE)
})

output$overview_gender_table <- renderDT({
  bind_rows(
    old %>% filter(!is.na(Gender)) %>%
      group_by(Gender) %>%
      summarise(N=n(), `Avg dep`=round(mean(Depression,na.rm=T),1),
                `Avg anx`=round(mean(Anxiety,na.rm=T),1), .groups="drop") %>%
      mutate(Dataset="Old questionnaire"),
    new %>% filter(!is.na(Gender)) %>%
      group_by(Gender) %>%
      summarise(N=n(), `Avg dep`=round(mean(Depression,na.rm=T),1),
                `Avg anx`=round(mean(Anxiety,na.rm=T),1), .groups="drop") %>%
      mutate(Dataset="All questionnaire")
  ) %>%
    select(Dataset, Gender, N, `Avg dep`, `Avg anx`) %>%
    datatable(class="nowrap",options=list(dom="t", paging=FALSE, scrollX=TRUE), rownames=FALSE)
})

output$overview_stage_sev_table <- renderDT({
  new %>%
    filter(!is.na(Stage)) %>%
    mutate(Severity = sev_dep(Depression),
           Stage    = factor(Stage, levels=c("EY (Entry Year)","S4","S5","S6"))) %>%
    count(Stage, Severity) %>%
    group_by(Stage) %>%
    mutate(Label = paste0(n, " (", round(n/sum(n)*100,1), "%)")) %>%
    ungroup() %>%
    select(-n) %>%
    pivot_wider(names_from=Severity, values_from=Label, values_fill="0 (0%)") %>%
    datatable(class="nowrap",options=list(dom="t", paging=FALSE, scrollX=TRUE), rownames=FALSE)
})

# ── DEPRESSION & ANXIETY ──────────────────────────────────
da_data <- reactive({
  df<-if(input$da_source=="new") new else old
  if(input$da_gender!="All") df<-df%>%filter(Gender==input$da_gender)
  if(!is.null(input$da_grade)&&!"All"%in%input$da_grade) df<-df%>%filter(Grade%in%input$da_grade)
  df
})
output$da_grade_ui <- renderUI({
  grades<-if(input$da_source=="new") sort(unique(new$Grade[!is.na(new$Grade)])) else sort(unique(old$Grade[!is.na(old$Grade)]))
  selectInput("da_grade","Grade/cohort",choices=c("All",grades),selected="All",multiple=TRUE)
})
output$da_n       <- renderbs4ValueBox(bs4ValueBox(nrow(da_data()),"Records selected",color="primary",icon=icon("users"),width=3))
output$da_dep_avg <- renderbs4ValueBox(bs4ValueBox(round(mean(da_data()$Depression,na.rm=T),2),"Avg depression",color="danger",icon=icon("heart-pulse"),width=3))
output$da_anx_avg <- renderbs4ValueBox(bs4ValueBox(round(mean(da_data()$Anxiety,na.rm=T),2),"Avg anxiety",color="warning",icon=icon("brain"),width=3))
output$da_dep_pct <- renderbs4ValueBox(bs4ValueBox(paste0(round(mean(da_data()$Dep_pct,na.rm=T),1),"%"),"Depression % of max",color="info",icon=icon("percent"),width=3))

output$da_summary_table <- renderDT({
  da_data() %>%
    filter(!is.na(Grade)) %>%
    group_by(`Grade/Cohort` = Grade) %>%
    summarise(
      N            = n(),
      `Avg dep`    = round(mean(Depression, na.rm=TRUE), 1),
      `Avg anx`    = round(mean(Anxiety,    na.rm=TRUE), 1),
      `Dep % max`  = paste0(round(mean(Dep_pct, na.rm=TRUE), 1), "%"),
      `Anx % max`  = paste0(round(mean(Anx_pct, na.rm=TRUE), 1), "%"),
      `Mod+ dep`   = paste0(round(mean(Depression >= 10, na.rm=TRUE) * 100, 1), "%"),
      `Mod+ anx`   = paste0(round(mean(Anxiety    >= 10, na.rm=TRUE) * 100, 1), "%"),
      .groups = "drop"
    ) %>%
    arrange(desc(`Avg dep`)) %>%
    datatable(class="nowrap", options=list(dom="tp", pageLength=8, scrollX=TRUE), rownames=FALSE)
})

output$da_gender_plot <- renderPlotly({
  da_data()%>%filter(!is.na(Gender))%>%group_by(Gender)%>%
    summarise(Depression=mean(Depression,na.rm=T),Anxiety=mean(Anxiety,na.rm=T),.groups="drop")%>%
    pivot_longer(c(Depression,Anxiety),names_to="Scale",values_to="Score")%>%
    plot_ly(x=~Gender,y=~Score,color=~Scale,colors=c("#C75B39","#0369A1"),type="bar",
            text=~round(Score,2), textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(barmode="group",
                  xaxis=list(gridcolor="#F1F5F9"),
                  yaxis=list(title="Avg score",gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10))
})

output$da_grade_plot <- renderPlotly({
  da_data()%>%filter(!is.na(Grade))%>%group_by(Grade)%>%
    summarise(Depression=mean(Depression,na.rm=T),Anxiety=mean(Anxiety,na.rm=T),.groups="drop")%>%
    plot_ly(y=~reorder(Grade,Depression),x=~Depression,type="bar",name="Depression",orientation="h",
            marker=list(color="rgba(199,91,57,.18)",line=list(color="#C75B39",width=1.5)),
            text=~round(Depression,1), textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    add_trace(x=~Anxiety,name="Anxiety",orientation="h",
              marker=list(color="rgba(3,105,161,.18)",line=list(color="#0369A1",width=1.5)),
              text=~round(Anxiety,1), textposition="outside",
              textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(barmode="group",
                  xaxis=list(title="Avg score",gridcolor="#F1F5F9"),
                  yaxis=list(title="",gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10,l=90))
})

output$da_sev_plot <- renderPlotly({
  df<-da_data()
  if(input$da_source=="new"){
    df<-df%>%mutate(Sev=sev_dep(Depression))%>%count(Sev)%>%mutate(Pct=round(n/sum(n)*100,1))
    clrs<-c("#10B981","#F59E0B","#C75B39","#B91C1C","#7F1D1D")
  }else{df<-df%>%mutate(Sev=ifelse(Depression>0,"Elevated","None"))%>%count(Sev)%>%mutate(Pct=round(n/sum(n)*100,1));clrs<-c("#C75B39","#10B981")}
  plot_ly(df,labels=~Sev,values=~n,type="pie",
          marker=list(colors=clrs,line=list(color="#ffffff",width=2)),
          texttemplate="%{label}<br>%{percent}",
          hole=0.45)%>%
    plotly_layout(showlegend=FALSE,margin=list(t=10,b=10))
})

output$da_hist_plot <- renderPlotly({
  df<-da_data()
  plot_ly(opacity=0.8)%>%
    add_histogram(x=df$Depression,name="Depression",
                  marker=list(color="rgba(199,91,57,.20)",line=list(color="#C75B39",width=1)),
                  nbinsx=20)%>%
    add_histogram(x=df$Anxiety,name="Anxiety",
                  marker=list(color="rgba(3,105,161,.20)",line=list(color="#0369A1",width=1)),
                  nbinsx=20)%>%
    plotly_layout(barmode="overlay",
                  xaxis=list(title="Score",gridcolor="#F1F5F9"),
                  yaxis=list(title="Count",gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10))
})

output$da_items_plot <- renderPlotly({
  df<-da_data(); src<-input$da_source
  meta<-item_labels%>%filter(scale==ifelse(src=="new","PHQ_new","PHQ_old"))
  avgs<-sapply(meta$item_col,function(c) mean(df[[c]],na.rm=TRUE))
  plot_ly(x=avgs,y=meta$label,type="bar",orientation="h",
          marker=list(color="rgba(199,91,57,.18)",line=list(color="#C75B39",width=1.5)),
          text=round(avgs,2),textposition="outside",
          textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(xaxis=list(title="Mean item score",gridcolor="#F1F5F9"),
                  yaxis=list(title="",gridcolor="#F1F5F9"),
                  margin=list(l=130,t=10))
})

# ── COHORT PROGRESSION ────────────────────────────────────
cp_data    <- reactive({ df<-new%>%filter(Grade==input$cp_grade); if(input$cp_gender!="All") df<-df%>%filter(Gender==input$cp_gender); df })
cp_summary <- reactive({ cp_data()%>%filter(!is.na(Stage))%>%group_by(Stage)%>%summarise(n=n(),Dep_pct=mean(Dep_pct,na.rm=T),Anx_pct=mean(Anx_pct,na.rm=T),Depression=mean(Depression,na.rm=T),Anxiety=mean(Anxiety,na.rm=T),.groups="drop")%>%mutate(Stage=factor(Stage,levels=c("EY (Entry Year)","S4","S5","S6")))%>%arrange(Stage) })
n_stage    <- function(s){ v<-cp_summary()%>%filter(Stage==s)%>%pull(n); if(length(v)==0) 0 else v }
output$cp_n_ey <- renderbs4ValueBox(bs4ValueBox(n_stage("EY (Entry Year)"),"n at EY",color="success",icon=icon("flag"),width=3))
output$cp_n_s4 <- renderbs4ValueBox(bs4ValueBox(n_stage("S4"),"n at S4",color="primary",icon=icon("1"),width=3))
output$cp_n_s5 <- renderbs4ValueBox(bs4ValueBox(n_stage("S5"),"n at S5",color="warning",icon=icon("2"),width=3))
output$cp_n_s6 <- renderbs4ValueBox(bs4ValueBox(n_stage("S6"),"n at S6",color="danger",icon=icon("3"),width=3))

output$cp_summary_table <- renderDT({
  cp_data() %>%
    filter(!is.na(Stage)) %>%
    mutate(Stage = factor(Stage, levels=c("EY (Entry Year)","S4","S5","S6"))) %>%
    group_by(Stage) %>%
    summarise(
      N           = n(),
      `Avg dep`   = round(mean(Depression, na.rm=TRUE), 1),
      `Avg anx`   = round(mean(Anxiety,    na.rm=TRUE), 1),
      `Dep % max` = paste0(round(mean(Dep_pct, na.rm=TRUE), 1), "%"),
      `Anx % max` = paste0(round(mean(Anx_pct, na.rm=TRUE), 1), "%"),
      `Mod+ dep`  = paste0(round(mean(Depression >= 10, na.rm=TRUE) * 100, 1), "%"),
      `Mod+ anx`  = paste0(round(mean(Anxiety    >= 10, na.rm=TRUE) * 100, 1), "%"),
      .groups = "drop"
    ) %>%
    arrange(Stage) %>%
    datatable(class="nowrap", options=list(dom="t", paging=FALSE, scrollX=TRUE), rownames=FALSE)
})

output$cp_line_plot <- renderPlotly({
  df<-cp_summary();met<-input$cp_metric
  plot_ly(df,x=~Stage,y=~get(met),type="scatter",mode="lines+markers",
          line=list(color="#0B4D3B",width=3,shape="spline"),
          marker=list(color="#0B4D3B",size=10,line=list(color="#fff",width=2)))%>%
    plotly_layout(xaxis=list(title="Stage",gridcolor="#F1F5F9"),
                  yaxis=list(title=ifelse(grepl("pct",met),"% of max","Score"),gridcolor="#F1F5F9"),
                  margin=list(t=10))
})

output$cp_gender_plot <- renderPlotly({
  met<-input$cp_metric
  new%>%filter(Grade==input$cp_grade,!is.na(Stage),!is.na(Gender))%>%group_by(Stage,Gender)%>%
    summarise(Score=mean(get(met),na.rm=T),.groups="drop")%>%
    mutate(Stage=factor(Stage,levels=c("EY (Entry Year)","S4","S5","S6")))%>%
    plot_ly(x=~Stage,y=~Score,color=~Gender,colors=c("#C75B39","#0369A1"),type="bar",
            text=~round(Score,1), textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(barmode="group",
                  yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10))
})

output$cp_cross_plot <- renderPlotly({
  met<-input$cp_metric
  new%>%filter(Stage%in%c("S4","S5","S6"),!is.na(Grade))%>%group_by(Grade,Stage)%>%
    summarise(Score=mean(get(met),na.rm=T),n=n(),.groups="drop")%>%filter(n>=10)%>%
    mutate(Stage=factor(Stage,levels=c("S4","S5","S6")))%>%
    plot_ly(x=~Grade,y=~Score,color=~Stage,colors=c("#534AB7","#D4A843","#0E6B52"),type="bar",
            text=~round(Score,1), textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(barmode="group",
                  xaxis=list(tickangle=-30,gridcolor="#F1F5F9"),
                  yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.2),margin=list(t=10,b=60))
})

output$cp_items_plot <- renderPlotly({
  meta<-item_labels%>%filter(scale=="PHQ_new")
  new%>%filter(Grade==input$cp_grade,!is.na(Stage))%>%group_by(Stage)%>%
    summarise(across(all_of(meta$item_col),~mean(.x,na.rm=TRUE)),.groups="drop")%>%
    pivot_longer(-Stage,names_to="item_col",values_to="Score")%>%left_join(meta,by="item_col")%>%
    mutate(Stage=factor(Stage,levels=c("EY (Entry Year)","S4","S5","S6")))%>%
    plot_ly(x=~label,y=~Score,color=~Stage,colors=c("#10B981","#534AB7","#D4A843","#C75B39"),type="bar",
            text=~round(Score,2), textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(barmode="group",
                  xaxis=list(tickangle=-30,gridcolor="#F1F5F9"),
                  yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.2),margin=list(t=10,b=80))
})

# ── EY BASELINE ───────────────────────────────────────────
bl_data <- reactive({ df<-new%>%filter(Stage=="EY (Entry Year)"); if(input$bl_gender!="All") df<-df%>%filter(Gender==input$bl_gender); df })
bl_sum  <- reactive({ bl_data()%>%filter(!is.na(Grade))%>%group_by(Grade)%>%summarise(n=n(),Dep_pct=mean(Dep_pct,na.rm=T),Anx_pct=mean(Anx_pct,na.rm=T),Depression=mean(Depression,na.rm=T),Anxiety=mean(Anxiety,na.rm=T),.groups="drop")%>%arrange(desc(Dep_pct)) })
output$bl_cohorts <- renderbs4ValueBox(bs4ValueBox(nrow(bl_sum()),"EY cohorts",color="primary",icon=icon("layer-group"),width=3))
output$bl_highest <- renderbs4ValueBox({ d<-bl_sum()%>%slice_max(Dep_pct,n=1); bs4ValueBox(paste0(d$Grade," (",round(d$Dep_pct,1),"%)"), "Highest EY depression",color="danger",icon=icon("arrow-up"),width=3) })
output$bl_lowest  <- renderbs4ValueBox({ d<-bl_sum()%>%slice_min(Dep_pct,n=1); bs4ValueBox(paste0(d$Grade," (",round(d$Dep_pct,1),"%)"), "Lowest EY depression",color="success",icon=icon("arrow-down"),width=3) })
output$bl_total_n <- renderbs4ValueBox(bs4ValueBox(nrow(bl_data()),"Total EY records",color="info",icon=icon("users"),width=3))

output$bl_bar_plot <- renderPlotly({
  met<-input$bl_metric
  plot_ly(bl_sum(),x=~reorder(Grade,-get(met)),y=~get(met),type="bar",
          marker=list(color="rgba(199,91,57,.18)",line=list(color="#C75B39",width=1.5)),
          text=~round(get(met),1),textposition="outside",
          textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(xaxis=list(title="Cohort",gridcolor="#F1F5F9"),
                  yaxis=list(title=ifelse(grepl("pct",met),"% of max","Score"),gridcolor="#F1F5F9"),
                  margin=list(t=20,b=40))
})

output$bl_scatter_plot <- renderPlotly({
  plot_ly(bl_data(),x=~Depression,y=~Anxiety,color=~Grade,type="scatter",mode="markers",
          opacity=0.65,marker=list(size=7,line=list(color="rgba(255,255,255,0.6)",width=1)))%>%
    plotly_layout(xaxis=list(gridcolor="#F1F5F9"),yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10))
})

output$bl_gender_plot <- renderPlotly({
  met<-input$bl_metric
  bl_data()%>%filter(!is.na(Gender),!is.na(Grade))%>%group_by(Grade,Gender)%>%
    summarise(Score=mean(get(met),na.rm=T),.groups="drop")%>%
    plot_ly(x=~Grade,y=~Score,color=~Gender,colors=c("#C75B39","#0369A1"),type="bar",
            text=~round(Score,1), textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(barmode="group",
                  xaxis=list(tickangle=-30,gridcolor="#F1F5F9"),
                  yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.2),margin=list(t=10,b=60))
})

output$bl_ey_s4_plot <- renderPlotly({
  new%>%filter(Grade=="Grade2028",Stage%in%c("EY (Entry Year)","S4"))%>%group_by(Stage)%>%
    summarise(Depression=mean(Depression,na.rm=T),Anxiety=mean(Anxiety,na.rm=T),.groups="drop")%>%
    pivot_longer(c(Depression,Anxiety),names_to="Scale",values_to="Score")%>%
    plot_ly(x=~Stage,y=~Score,color=~Scale,colors=c("#C75B39","#0369A1"),type="bar",
            text=~round(Score,1), textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(barmode="group",
                  yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10))
})

output$bl_table <- renderDT({
  bl_sum()%>%mutate(across(c(Dep_pct,Anx_pct,Depression,Anxiety),~round(.x,2)))%>%
    rename(`Grade/cohort`=Grade,N=n,`Dep % of max`=Dep_pct,`Anx % of max`=Anx_pct,`Avg dep`=Depression,`Avg anx`=Anxiety)%>%
    datatable(class="nowrap",options=list(dom="tp",pageLength=10,scrollX=TRUE),rownames=FALSE)
})

# ── CLINICAL RISK ─────────────────────────────────────────
output$ri_grade_ui <- renderUI({ choices<-if(input$ri_source=="new") c("All",sort(unique(new$Grade[!is.na(new$Grade)]))) else c("All",sort(unique(old$Grade[!is.na(old$Grade)]))); selectInput("ri_grade","Grade",choices=choices) })
output$ri_cat_ui   <- renderUI({ choices<-if(input$ri_source=="new") c("All","EY (Entry Year)","S4","S5","S6") else c("All","initial_assessment","re-assessment"); selectInput("ri_cat","Category",choices=choices) })
ri_data <- reactive({
  df<-if(input$ri_source=="new") new else old
  if(input$ri_gender!="All") df<-df%>%filter(Gender==input$ri_gender)
  if(!is.null(input$ri_grade)&&input$ri_grade!="All") df<-df%>%filter(Grade==input$ri_grade)
  if(!is.null(input$ri_cat)&&input$ri_cat!="All"){ if(input$ri_source=="new") df<-df%>%filter(Stage==input$ri_cat) else df<-df%>%filter(Category==input$ri_cat) }
  df
})
sui_col  <- reactive({ if(input$ri_source=="new") "dep_item_8" else "Suicidal_Ideation" })
sui_flag <- reactive({ df<-ri_data();col<-sui_col();if(input$ri_source=="new") as.integer(df[[col]]>0) else df[[col]] })
psy_flag <- reactive({ if(input$ri_source=="old") ri_data()[["Psychosis"]] else rep(NA_real_,nrow(ri_data())) })
output$ri_sui_n   <- renderbs4ValueBox(bs4ValueBox(sum(sui_flag(),na.rm=T),"Suicidal ideation (any)",color="danger",icon=icon("triangle-exclamation"),width=3))
output$ri_sui_pct <- renderbs4ValueBox(bs4ValueBox(paste0(round(mean(sui_flag(),na.rm=T)*100,1),"%"),"% with ideation",color="warning",icon=icon("percent"),width=3))
output$ri_psy_n   <- renderbs4ValueBox(bs4ValueBox(ifelse(input$ri_source=="old",sum(psy_flag(),na.rm=T),"N/A"),"Psychosis flags",color="danger",icon=icon("brain"),width=3))
output$ri_psy_pct <- renderbs4ValueBox(bs4ValueBox(ifelse(input$ri_source=="old",paste0(round(mean(psy_flag(),na.rm=T)*100,1),"%"),"N/A"),"% psychosis",color="warning",icon=icon("percent"),width=3))

output$ri_summary_table <- renderDT({
  col <- sui_col()
  ri_data() %>%
    filter(!is.na(Grade)) %>%
    group_by(`Grade/Cohort` = Grade) %>%
    summarise(
      N              = n(),
      `Sui ideation` = paste0(sum(get(col) > 0, na.rm=TRUE),
                              " (", round(mean(get(col) > 0, na.rm=TRUE) * 100, 1), "%)"),
      `Avg dep`      = round(mean(Depression, na.rm=TRUE), 1),
      `Avg anx`      = round(mean(Anxiety,    na.rm=TRUE), 1),
      `Dep % max`    = paste0(round(mean(Dep_pct, na.rm=TRUE), 1), "%"),
      `Mod+ dep`     = paste0(round(mean(Depression >= 10, na.rm=TRUE) * 100, 1), "%"),
      .groups = "drop"
    ) %>%
    arrange(desc(`Avg dep`)) %>%
    datatable(class="nowrap", options=list(dom="tp", pageLength=8, scrollX=TRUE), rownames=FALSE)
})

output$ri_sui_grade <- renderPlotly({
  col<-sui_col()
  ri_data()%>%filter(!is.na(Grade))%>%group_by(Grade)%>%
    summarise(Pct=round(mean(get(col)>0,na.rm=T)*100,1),n=n(),.groups="drop")%>%filter(n>=5)%>%arrange(desc(Pct))%>%
    plot_ly(x=~reorder(Grade,-Pct),y=~Pct,type="bar",
            marker=list(color="rgba(199,91,57,.18)",line=list(color="#C75B39",width=1.5)),
            text=~paste0(Pct,"%"),textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(xaxis=list(title="Grade",tickangle=-30,gridcolor="#F1F5F9"),
                  yaxis=list(title="% with ideation",gridcolor="#F1F5F9"),
                  margin=list(t=20,b=60))
})

output$ri_sui_gender <- renderPlotly({
  col<-sui_col()
  ri_data()%>%filter(!is.na(Gender))%>%group_by(Gender)%>%
    summarise(Pct=round(mean(get(col)>0,na.rm=T)*100,1),.groups="drop")%>%
    plot_ly(x=~Gender,y=~Pct,type="bar",
            marker=list(color=c("rgba(199,91,57,.18)","rgba(3,105,161,.18)"),
                        line=list(color=c("#C75B39","#0369A1"),width=1.5)),
            text=~paste0(Pct,"%"),textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(yaxis=list(title="% with ideation",gridcolor="#F1F5F9"),margin=list(t=20))
})

output$ri_psy_grade <- renderPlotly({
  if(input$ri_source!="old") return(plot_ly(type="scatter",mode="markers")%>%plotly_layout(title="Old questionnaire only"))
  ri_data()%>%filter(!is.na(Grade))%>%group_by(Grade)%>%
    summarise(Pct=round(mean(Psychosis,na.rm=T)*100,1),n=n(),.groups="drop")%>%filter(n>=5)%>%arrange(desc(Pct))%>%
    plot_ly(x=~reorder(Grade,-Pct),y=~Pct,type="bar",
            marker=list(color="rgba(3,105,161,.18)",line=list(color="#0369A1",width=1.5)),
            text=~paste0(Pct,"%"),textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(xaxis=list(title="Grade",tickangle=-30,gridcolor="#F1F5F9"),
                  yaxis=list(title="% flagged",gridcolor="#F1F5F9"),
                  margin=list(t=20,b=60))
})

output$ri_subscales <- renderPlotly({
  if(input$ri_source!="old") return(plot_ly(type="scatter",mode="markers")%>%plotly_layout(title="Old questionnaire only"))
  sn<-c("Depression","Anger","Mania","Anxiety","Somatic_Symptoms","Suicidal_Ideation","Psychosis","Sleep_Problems","Memory","Repetitive_Thoughts_and_Behaviors","Dissociation","Personality_Functioning","Substance_Use")
  sn<-sn[sn%in%names(ri_data())]
  ri_data()%>%summarise(across(all_of(sn),~round(mean(.x,na.rm=T)*100,1)))%>%
    pivot_longer(everything(),names_to="Subscale",values_to="Pct")%>%arrange(desc(Pct))%>%
    plot_ly(y=~reorder(Subscale,Pct),x=~Pct,type="bar",orientation="h",
            marker=list(color="rgba(83,74,183,.18)",line=list(color="#534AB7",width=1.5)),
            text=~paste0(Pct,"%"),textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(xaxis=list(title="% flagged",gridcolor="#F1F5F9"),
                  yaxis=list(title="",gridcolor="#F1F5F9"),
                  margin=list(t=10,l=170,r=50))
})

output$ri_sui_stage <- renderPlotly({
  if(input$ri_source!="new") return(plot_ly(type="scatter",mode="markers")%>%plotly_layout(title="All questionnaire only"))
  new%>%filter(!is.na(Stage))%>%group_by(Stage)%>%
    summarise(Pct=round(mean(dep_item_8>0,na.rm=T)*100,1),n=n(),.groups="drop")%>%
    mutate(Stage=factor(Stage,levels=c("EY (Entry Year)","S4","S5","S6")))%>%
    plot_ly(x=~Stage,y=~Pct,type="bar",
            marker=list(color="rgba(199,91,57,.18)",line=list(color="#C75B39",width=1.5)),
            text=~paste0(Pct,"%"),textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(xaxis=list(title="Stage",gridcolor="#F1F5F9"),
                  yaxis=list(title="% with suicidal ideation",gridcolor="#F1F5F9"),
                  margin=list(t=10))
})

# ── WELLBEING ─────────────────────────────────────────────
wb_data <- reactive({
  df<-new
  if(input$wb_gender!="All") df<-df%>%filter(Gender==input$wb_gender)
  if(input$wb_grade !="All") df<-df%>%filter(Grade ==input$wb_grade)
  if(input$wb_stage !="All") df<-df%>%filter(Stage ==input$wb_stage)
  df
})
output$wb_grit_avg <- renderbs4ValueBox(bs4ValueBox(round(mean(wb_data()$Grit,     na.rm=T),1),"Avg grit (max 45)",    color="primary",icon=icon("mountain"),  width=3))
output$wb_grat_avg <- renderbs4ValueBox(bs4ValueBox(round(mean(wb_data()$Gratitude,na.rm=T),1),"Avg gratitude (max 18)",color="success",icon=icon("heart"),    width=3))
output$wb_swls_avg <- renderbs4ValueBox(bs4ValueBox(round(mean(wb_data()$SWLS,     na.rm=T),1),"Avg SWLS (max 15)",    color="info",   icon=icon("face-smile"),width=3))
output$wb_mean_avg <- renderbs4ValueBox(bs4ValueBox(round(mean(wb_data()$Meaning,  na.rm=T),1),"Avg meaning (max 30)", color="warning",icon=icon("star"),      width=3))

output$wb_summary_table <- renderDT({
  wb_data() %>%
    filter(!is.na(Grade)) %>%
    group_by(`Grade/Cohort` = Grade) %>%
    summarise(
      N              = n(),
      `Avg Grit`     = round(mean(Grit,      na.rm=TRUE), 1),
      `Avg Gratitude`= round(mean(Gratitude, na.rm=TRUE), 1),
      `Avg SWLS`     = round(mean(SWLS,      na.rm=TRUE), 1),
      `Avg Meaning`  = round(mean(Meaning,   na.rm=TRUE), 1),
      `Avg dep`      = round(mean(Depression,na.rm=TRUE), 1),
      `Avg anx`      = round(mean(Anxiety,   na.rm=TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(`Avg Grit`)) %>%
    datatable(class="nowrap", options=list(dom="tp", pageLength=8, scrollX=TRUE), rownames=FALSE)
})

output$wb_gender_plot <- renderPlotly({
  wb_data()%>%filter(!is.na(Gender))%>%group_by(Gender)%>%
    summarise(Grit=mean(Grit,na.rm=T),Gratitude=mean(Gratitude,na.rm=T),SWLS=mean(SWLS,na.rm=T),Meaning=mean(Meaning,na.rm=T),.groups="drop")%>%
    pivot_longer(-Gender,names_to="Scale",values_to="Score")%>%
    plot_ly(x=~Scale,y=~Score,color=~Gender,colors=c("#C75B39","#0369A1"),type="bar",
            text=~round(Score,1), textposition="outside",
            textfont=list(size=11,color="#64748B"))%>%
    plotly_layout(barmode="group",
                  yaxis=list(title="Avg score",gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10))
})

output$wb_grade_plot <- renderPlotly({
  wb_data()%>%filter(!is.na(Grade))%>%group_by(Grade)%>%
    summarise(Grit=mean(Grit,na.rm=T),Gratitude=mean(Gratitude,na.rm=T),SWLS=mean(SWLS,na.rm=T),Meaning=mean(Meaning,na.rm=T),.groups="drop")%>%
    plot_ly(x=~Grade,y=~Grit,type="bar",name="Grit",
            marker=list(color="rgba(83,74,183,.18)",line=list(color="#534AB7",width=1.5)))%>%
    add_trace(y=~Gratitude,name="Gratitude",
              marker=list(color="rgba(14,107,82,.18)",line=list(color="#0E6B52",width=1.5)))%>%
    add_trace(y=~SWLS,name="SWLS",
              marker=list(color="rgba(212,168,67,.18)",line=list(color="#D4A843",width=1.5)))%>%
    add_trace(y=~Meaning,name="Meaning",
              marker=list(color="rgba(199,91,57,.18)",line=list(color="#C75B39",width=1.5)))%>%
    plotly_layout(barmode="group",
                  xaxis=list(tickangle=-30,gridcolor="#F1F5F9"),
                  yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.2),margin=list(t=10,b=60))
})

output$wb_dist_plot <- renderPlotly({
  df<-wb_data()
  plot_ly(type="box",quartilemethod="linear")%>%
    add_trace(y=df$Grit,name="Grit",
              marker=list(color="#534AB7"),line=list(color="#534AB7"),fillcolor="rgba(83,74,183,.12)")%>%
    add_trace(y=df$Gratitude,name="Gratitude",
              marker=list(color="#0E6B52"),line=list(color="#0E6B52"),fillcolor="rgba(14,107,82,.12)")%>%
    add_trace(y=df$SWLS,name="SWLS",
              marker=list(color="#D4A843"),line=list(color="#D4A843"),fillcolor="rgba(212,168,67,.12)")%>%
    add_trace(y=df$Meaning,name="Meaning",
              marker=list(color="#C75B39"),line=list(color="#C75B39"),fillcolor="rgba(199,91,57,.12)")%>%
    plotly_layout(showlegend=FALSE,
                  yaxis=list(gridcolor="#F1F5F9"),
                  margin=list(t=10))
})

output$wb_corr_plot <- renderPlotly({
  wb_data()%>%filter(!is.na(SWLS))%>%
    plot_ly(x=~Depression,y=~SWLS,color=~Gender,colors=c("#C75B39","#0369A1","#94A3B8"),
            type="scatter",mode="markers",opacity=0.55,
            marker=list(size=6,line=list(color="rgba(255,255,255,0.5)",width=1)))%>%
    plotly_layout(xaxis=list(gridcolor="#F1F5F9"),yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10))
})

output$wb_stage_plot <- renderPlotly({
  new%>%filter(!is.na(Stage))%>%group_by(Stage)%>%
    summarise(Grit=mean(Grit,na.rm=T),Gratitude=mean(Gratitude,na.rm=T),SWLS=mean(SWLS,na.rm=T),Meaning=mean(Meaning,na.rm=T),.groups="drop")%>%
    mutate(Stage=factor(Stage,levels=c("EY (Entry Year)","S4","S5","S6")))%>%arrange(Stage)%>%
    plot_ly(x=~Stage)%>%
    add_trace(y=~Grit,type="scatter",mode="lines+markers",name="Grit",
              line=list(color="#534AB7",width=2.5,shape="spline"),
              marker=list(color="#534AB7",size=9,line=list(color="#fff",width=2)))%>%
    add_trace(y=~Gratitude,type="scatter",mode="lines+markers",name="Gratitude",
              line=list(color="#0E6B52",width=2.5,shape="spline"),
              marker=list(color="#0E6B52",size=9,line=list(color="#fff",width=2)))%>%
    add_trace(y=~SWLS,type="scatter",mode="lines+markers",name="SWLS",
              line=list(color="#D4A843",width=2.5,shape="spline"),
              marker=list(color="#D4A843",size=9,line=list(color="#fff",width=2)))%>%
    add_trace(y=~Meaning,type="scatter",mode="lines+markers",name="Meaning",
              line=list(color="#C75B39",width=2.5,shape="spline"),
              marker=list(color="#C75B39",size=9,line=list(color="#fff",width=2)))%>%
    plotly_layout(xaxis=list(gridcolor="#F1F5F9"),yaxis=list(gridcolor="#F1F5F9"),
                  legend=list(orientation="h",y=-0.15),margin=list(t=10))
})
