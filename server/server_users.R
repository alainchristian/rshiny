# ── USER MANAGEMENT STAT CARDS ───────────────────────────
um_stats <- reactive({
  req(auth()$role=="admin")
  u <- users_rv()
  list(total  = nrow(u),
       active = sum(u$active=="TRUE", na.rm=TRUE),
       admins = sum(u$role=="admin",  na.rm=TRUE))
})
output$um_total  <- renderUI({ req(auth()$role=="admin"); as.character(um_stats()$total)  })
output$um_active <- renderUI({ req(auth()$role=="admin"); as.character(um_stats()$active) })
output$um_admins <- renderUI({ req(auth()$role=="admin"); as.character(um_stats()$admins) })

# ── USERS TABLE ──────────────────────────────────────────
output$users_table <- renderDT({
  req(auth()$role=="admin")
  cur_user <- auth()$username
  users_rv() %>%
    mutate(
      avatar    = paste0('<div class="um-avatar">',
                         toupper(substr(full_name, 1, 1)), '</div>'),
      name_cell = paste0(
        '<div style="line-height:1.35;">',
        '<div style="font-weight:600;font-size:13px;">', full_name, '</div>',
        '<div style="font-size:11.5px;color:#94A3B8;">@', username, '</div></div>'),
      role_disp = ifelse(role == "admin",
        '<span style="background:#D1FAE5;color:#065F46;font-size:10.5px;font-weight:700;padding:3px 10px;border-radius:20px;text-transform:uppercase;letter-spacing:.4px;">Admin</span>',
        '<span style="background:#F1F5F9;color:#475569;font-size:10.5px;font-weight:700;padding:3px 10px;border-radius:20px;text-transform:uppercase;letter-spacing:.4px;">Viewer</span>'),
      status    = ifelse(active == "TRUE",
        '<span style="background:#D1FAE5;color:#059669;font-size:11px;font-weight:600;padding:2px 9px;border-radius:12px;">&#9679; Active</span>',
        '<span style="background:#FEE2E2;color:#DC2626;font-size:11px;font-weight:600;padding:2px 9px;border-radius:12px;">&#9675; Inactive</span>'),
      actions   = paste0(
        '<button class="um-action-btn um-edit-btn" data-username="', username, '">',
        '<i class="fas fa-pen-to-square"></i> Edit</button>',
        ifelse(username == cur_user, '',
          paste0(
            '<button class="um-action-btn ',
            ifelse(active == "TRUE", "um-toggle-deactivate", "um-toggle-activate"),
            ' um-toggle-btn" data-username="', username, '">',
            ifelse(active == "TRUE",
              '<i class="fas fa-ban"></i> Deactivate',
              '<i class="fas fa-circle-check"></i> Activate'),
            '</button>'))
      )
    ) %>%
    select(avatar, name_cell, email, role_disp, status, created_at, actions) %>%
    rename(`&nbsp;` = avatar, Member = name_cell, Email = email,
           Role = role_disp, Status = status, Joined = created_at, Actions = actions) %>%
    datatable(
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        pageLength = 10,
        dom        = "ftp",
        order      = list(list(2, "asc")),
        columnDefs = list(
          list(orderable = FALSE, targets = c(0, 6)),
          list(width = "40px",   targets = 0),
          list(width = "180px",  targets = 1),
          list(width = "160px",  targets = 6)
        )
      )
    )
})

# ── USER MANAGEMENT MODALS ────────────────────────────────
modal_mode      <- reactiveVal("add")
modal_edit_user <- reactiveVal(NULL)

user_modal <- function(mode = "add", user_data = NULL) {
  is_edit <- mode == "edit"
  modalDialog(
    title     = if (is_edit) tagList(icon("user-pen"),  " Edit Member")
                else          tagList(icon("user-plus"), " New Member"),
    size      = "m",
    easyClose = TRUE,
    footer    = tagList(
      modalButton("Cancel"),
      actionButton("btn_save_modal",
        if (is_edit) "Save Changes" else "Add Member",
        class = if (is_edit) "btn-primary" else "btn-success",
        icon  = icon(if (is_edit) "floppy-disk" else "user-plus"))
    ),

    # Username: read-only display in edit, text input in add
    if (is_edit) {
      div(style = "margin-bottom:12px;",
        tags$label("Username",
          style = "font-size:12px;font-weight:700;color:var(--text-secondary);display:block;margin-bottom:4px;"),
        div(style = paste0("background:var(--border-light);border-radius:8px;",
                           "padding:8px 12px;font-size:13px;font-weight:600;color:var(--text);"),
          if (!is.null(user_data)) user_data$username else "")
      )
    } else {
      textInput("modal_username", "Username",
        value = "", placeholder = "e.g. jdoe", width = "100%")
    },

    textInput("modal_fullname", "Full name",
      value = if (!is.null(user_data)) user_data$full_name else "",
      placeholder = "First Last", width = "100%"),
    textInput("modal_email", "Email",
      value = if (!is.null(user_data)) user_data$email else "",
      placeholder = "user@example.com", width = "100%"),
    selectInput("modal_role", "Role",
      choices  = c("Viewer" = "viewer", "Admin" = "admin"),
      selected = if (!is.null(user_data)) user_data$role else "viewer",
      width    = "100%"),

    hr(style = "margin:14px 0;"),
    if (is_edit) {
      p(style = "font-size:12px;color:var(--text-muted);margin-bottom:8px;",
        icon("circle-info"),
        " Leave password fields blank to keep the current password.")
    },
    passwordInput("modal_password",
      label       = if (is_edit) "New password (optional)" else "Password",
      value       = "",
      placeholder = if (is_edit) "Leave blank to keep current" else "Min. 6 characters",
      width       = "100%"),
    if (!is_edit) {
      passwordInput("modal_password2", "Confirm password",
        value = "", placeholder = "Repeat password", width = "100%")
    },
    div(id    = "modal_err",
        style = "color:#DC2626;font-size:12.5px;min-height:18px;margin-top:6px;")
  )
}

# Open add modal
observeEvent(input$btn_open_add, {
  req(auth()$role == "admin")
  modal_mode("add"); modal_edit_user(NULL)
  showModal(user_modal("add"))
})

# Open edit modal (triggered by DT Edit button)
observeEvent(input$edit_user_click, {
  req(auth()$role == "admin")
  un <- input$edit_user_click$username
  ud <- users_rv() %>% filter(username == un)
  if (nrow(ud) == 0) return()
  modal_mode("edit"); modal_edit_user(un)
  showModal(user_modal("edit", as.list(ud[1, ])))
})

# Inline toggle (triggered by DT Toggle button)
observeEvent(input$toggle_user_click, {
  req(auth()$role == "admin")
  un    <- input$toggle_user_click$username
  users <- users_rv()
  if (!any(users$username == un)) return()
  if (un == auth()$username) {
    showNotification("Cannot deactivate your own account.", type = "warning")
    return()
  }
  updated <- users %>%
    mutate(active = ifelse(username == un,
                           ifelse(active == "TRUE", "FALSE", "TRUE"),
                           active))
  save_users(updated); users_rv(updated)
})

# Save modal — handles both add and edit
observeEvent(input$btn_save_modal, {
  req(auth()$role == "admin")
  users <- users_rv()
  mode  <- modal_mode()
  fn    <- trimws(input$modal_fullname)
  em    <- trimws(input$modal_email)
  rl    <- input$modal_role
  pw    <- input$modal_password
  err   <- NULL

  if (mode == "add") {
    un  <- trimws(input$modal_username)
    pw2 <- input$modal_password2
    if (nchar(un) < 3)
      err <- "Username must be at least 3 characters."
    else if (nchar(fn) < 2)
      err <- "Full name is required."
    else if (nchar(pw) < 6)
      err <- "Password must be at least 6 characters."
    else if (!identical(pw, pw2))
      err <- "Passwords do not match."
    else if (any(tolower(users$username) == tolower(un)))
      err <- paste0("Username '", un, "' already exists.")

    if (!is.null(err)) {
      runjs(sprintf(
        "document.getElementById('modal_err').innerText='%s';",
        gsub("'", "\\\\'", err)))
      return()
    }
    updated <- bind_rows(users,
      tibble(username = un, password_hash = hash_pw(pw), role = rl,
             full_name = fn, email = em,
             created_at = as.character(Sys.Date()), active = "TRUE"))

  } else {
    un <- modal_edit_user()
    if (nchar(fn) < 2) err <- "Full name is required."
    if (!is.null(err)) {
      runjs(sprintf(
        "document.getElementById('modal_err').innerText='%s';",
        gsub("'", "\\\\'", err)))
      return()
    }
    updated <- users %>% mutate(
      full_name     = ifelse(username == un, fn, full_name),
      email         = ifelse(username == un, em, email),
      role          = ifelse(username == un, rl, role),
      password_hash = ifelse(username == un & nchar(pw) >= 6,
                             hash_pw(pw), password_hash)
    )
  }

  save_users(updated); users_rv(updated)
  removeModal()
})

# ── MY ACCOUNT INFO ──────────────────────────────────────
output$account_info <- renderUI({
  a<-auth(); req(a$logged_in)
  u<-users_rv()%>%filter(username==a$username)
  tags$table(class="table table-sm",
             tags$tr(tags$td(strong("Username")),   tags$td(a$username)),
             tags$tr(tags$td(strong("Full name")),  tags$td(a$full_name)),
             tags$tr(tags$td(strong("Email")),      tags$td(if(nrow(u)>0) u$email else "\u2014")),
             tags$tr(tags$td(strong("Role")),       tags$td(a$role)),
             tags$tr(tags$td(strong("Since")),      tags$td(if(nrow(u)>0) u$created_at else "\u2014")))
})

# ── CHANGE PASSWORD ──────────────────────────────────────
observeEvent(input$btn_change_pw, {
  a<-auth(); req(a$logged_in)
  users<-users_rv()
  cur_hash<-users%>%filter(username==a$username)%>%pull(password_hash)
  err<-NULL
  if(!check_pw(input$cur_password,cur_hash)) err<-"Current password is incorrect."
  else if(nchar(input$new_pw1)<6)            err<-"New password must be at least 6 characters."
  else if(!identical(input$new_pw1,input$new_pw2)) err<-"New passwords do not match."
  if(!is.null(err)){
    runjs(sprintf("document.getElementById('change_pw_msg').innerHTML='<span style=\"color:#DC2626;\">%s</span>';",err)); return()}
  updated<-users%>%mutate(password_hash=ifelse(username==a$username,hash_pw(input$new_pw1),password_hash))
  save_users(updated); users_rv(updated)
  runjs("document.getElementById('change_pw_msg').innerHTML='<span style=\"color:#059669;\">Password updated successfully.</span>';")
  updateTextInput(session,"cur_password",value=""); updateTextInput(session,"new_pw1",value=""); updateTextInput(session,"new_pw2",value="")
})
