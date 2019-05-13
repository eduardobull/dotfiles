# The .First function is called after everything else in .Rprofile is executed
.First <- function() {
  options(repos = c(CRAN = "https://cloud.r-project.org/"),
          prompt = "R> ",
          digits = 4,
          stringsAsFactors = FALSE,
          show.signif.stars = FALSE)

  # Print a welcome message
  message("\nWelcome back ", Sys.getenv("USER"),"! (", getwd(), ")\n")
}

# Load the devtools and colorout packages if in interactive mode
if (interactive()) {
  library(devtools)
  library(usethis)
}
