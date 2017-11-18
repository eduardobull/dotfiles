# The .First function is called after everything else in .Rprofile is executed
.First <- function() {
  # Print a welcome message
  message("\nWelcome back ", Sys.getenv("USER"),"! (", getwd(), ")\n")
}

# Load the devtools and colorout packages if in interactive mode
if (interactive()) {
  library(colorout)
  library(data.table)
  library(stringr)
  library(lubridate)
}

options(repos = c(CRAN = "https://cloud.r-project.org/"))
options(prompt = "R> ")
options(digits = 4)
options(stringsAsFactors = FALSE)
options(show.signif.stars = FALSE)

error <- quote(dump.frames("${R_HOME_USER}/testdump", TRUE))
