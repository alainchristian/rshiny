server <- function(input, output, session) {
  auth     <- reactiveVal(list(logged_in=FALSE, username=NULL, role=NULL, full_name=NULL))
  users_rv <- reactiveVal(load_users())

  source("server/server_auth.R",  local = environment())
  source("server/server_users.R", local = environment())

  for (dept in DEPARTMENTS) {
    source(file.path(dept$dir, "server.R"), local = environment())
  }
}
