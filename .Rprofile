# The .First function is called after everything else in .Rprofile is executed
.First <- function() {
  options(repos = c(CRAN = "https://cloud.r-project.org/"),
          prompt = "R> ",
          continue="+> ",
          digits = 4,
          scipen = 999,
          stringsAsFactors = FALSE,
          show.signif.stars = FALSE,
          datatable.print.nrows = 20,
          datatable.print.topn = 3)

  # Print a welcome message
  message("\nWelcome back! (", getwd(), ")\n")
}

.Last <- function(){
  message("\nGoodbye! (", date(), ")\n")
}

# Set max the number of threads that data.table should use.
Sys.setenv(R_DATATABLE_NUM_PROCS_PERCENT = 100)

# Exit R console on "exit" command.
makeActiveBinding("exit", function(x) q("no"), .GlobalEnv)

# Load the devtools and colorout packages if in interactive mode.
#if (interactive()) {
#  library(devtools)
#  library(usethis)
#}
