# ── Password eye toggle ──────────────────────────────────
runjs("document.addEventListener('click',function(e){
  if(e.target.closest('#pw_eye')){
    var f=document.getElementById('login_pass');
    f.type=f.type==='password'?'text':'password';
  }
});")

# ── LOGIN ────────────────────────────────────────────────
observeEvent(input$login_btn, {
  req(input$login_user, input$login_pass)
  users <- users_rv()
  u     <- trimws(tolower(input$login_user))
  match <- users %>% filter(tolower(username)==u, active=="TRUE")
  if (nrow(match)==1 && check_pw(input$login_pass, match$password_hash)) {
    auth(list(logged_in=TRUE, username=match$username,
              role=match$role, full_name=match$full_name))
    runjs("document.getElementById('login-overlay').style.display='none';
           var app = document.getElementById('main-app');
           app.style.display='block';
           document.getElementById('login_error').style.display='none';
           Shiny.bindAll(app);
           setTimeout(function(){ $(window).trigger('resize'); }, 50);")
    first_tab <- DEPARTMENTS[[1]]$config$menu_items[[1]]$tabName
    updateTabItems(session, "sidebar", selected = first_tab)
  } else {
    runjs("document.getElementById('login_error').style.display='block';")
    updateTextInput(session,"login_pass",value="")
  }
})

# ── LOGOUT ───────────────────────────────────────────────
observeEvent(input$logout_btn, {
  auth(list(logged_in=FALSE,username=NULL,role=NULL,full_name=NULL))
  runjs("document.getElementById('login-overlay').style.display='flex';
         document.getElementById('main-app').style.display='none';")
  updateTextInput(session,"login_user",value="")
  updateTextInput(session,"login_pass",value="")
})

# ── NAVBAR BADGE ─────────────────────────────────────────
output$navbar_user <- renderUI({
  a <- auth(); req(a$logged_in)
  pill_class <- if(a$role=="admin") "role-pill" else "role-pill viewer"
  div(class="user-badge", icon("user-circle"), a$full_name,
      span(class=pill_class, a$role))
})

# ── DYNAMIC SIDEBAR ───────────────────────────────────────
output$sidebar_menu <- renderUI({
  a <- auth()

  # One collapsible parent item per department; first dept starts expanded
  dept_items <- lapply(seq_along(DEPARTMENTS), function(i) {
    dept      <- DEPARTMENTS[[i]]
    sub_items <- lapply(dept$config$menu_items, function(item) {
      bs4SidebarMenuSubItem(item$label,
                            tabName = item$tabName,
                            icon    = icon(item$icon))
    })
    do.call(bs4SidebarMenuItem, c(
      list(text          = dept$config$name,
           icon          = icon(dept$config$icon),
           startExpanded = (i == 1L)),
      sub_items
    ))
  })

  sys_items <- if (isTRUE(a$role == "admin")) {
    list(
      bs4SidebarMenuItem("User Management", tabName = "admin_users", icon = icon("users-gear")),
      bs4SidebarMenuItem("My Account",      tabName = "my_account",  icon = icon("circle-user"))
    )
  } else if (isTRUE(a$logged_in)) {
    list(bs4SidebarMenuItem("My Account", tabName = "my_account", icon = icon("circle-user")))
  } else {
    list()
  }

  menu <- tryCatch(
    do.call(bs4SidebarMenu, c(list(id = "sidebar"), dept_items, sys_items)),
    error = function(e) NULL
  )

  first_tab <- DEPARTMENTS[[1]]$config$menu_items[[1]]$tabName
  tagList(menu, tags$script(HTML(sprintf(
    "setTimeout(function(){
      /* Strip any inline display/height/overflow AdminLTE stamped after init */
      $('.nav-sidebar .nav-item.has-treeview > ul.nav-treeview').each(function(){
        var open = $(this).closest('.nav-item').hasClass('menu-open');
        $(this).css({ display: open ? 'block' : 'none', height: '', overflow: '' });
      });
      /* Activate the first department tab */
      var link = $('a[data-value=\"%s\"]').first();
      if (link.length) link.tab('show');
    }, 300);", first_tab
  ))))
})
