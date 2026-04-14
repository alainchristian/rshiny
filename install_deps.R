# Run this script ONCE in RStudio to install all required packages as
# pre-built binaries (no Rtools / compilation needed).
# After it finishes, run shiny::runApp() as normal.

pkgs <- c(
  "tseries",      # missing dep for doBy
  "rbibutils",    # needs update (>= 2.4 required)
  "doBy",
  "Rdpack",
  "reformulas",
  "pbkrtest",
  "car",
  "rstatix",
  "ggpubr"
)

install.packages(pkgs, type = "binary", dependencies = TRUE)

message("\nDone! You can now run shiny::runApp()")
