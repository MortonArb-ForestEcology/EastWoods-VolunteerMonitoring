# Purpose: Render 6_report.Rmd to reports/. Run from the project root.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Notes: Two path details make this a script rather than a one-liner.
#        1) knit_root_dir and intermediates_dir are both set to the project root. The report
#           references figures/ and data/processed/ relative to the root; pandoc resolves resource
#           paths relative to the intermediate markdown file, so that file has to sit at the root
#           too, or none of the figures embed.
#        2) pandoc is not on the PATH on this machine, but RStudio ships one. If RSTUDIO_PANDOC is
#           unset, fall back to the bundled copy.

if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && !rmarkdown::pandoc_available()) {
  cand <- Sys.glob(c(
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/*/pandoc",
    "/Applications/RStudio.app/Contents/Resources/app/bin/pandoc/pandoc",
    "/usr/local/bin/pandoc", "/opt/homebrew/bin/pandoc"))
  cand <- cand[file.exists(cand)]
  if (length(cand) == 0) {
    stop("pandoc not found. Install pandoc, or open this project in RStudio and knit there.")
  }
  Sys.setenv(RSTUDIO_PANDOC = dirname(cand[1]))
  message("Using pandoc at ", cand[1])
}

root <- normalizePath(".")

# path.rep resolves to the shared Drive folder; see 0_helper_functions.R
source("scripts/0_helper_functions.R")

out <- rmarkdown::render(
  input = file.path("scripts", "6_report.Rmd"),
  output_file = "EastWoods_VolunteerMonitoring_Summary.html",
  output_dir = normalizePath(path.rep),
  knit_root_dir = root,
  intermediates_dir = root,   # pandoc resolves resource paths from here
  envir = new.env(),
  quiet = TRUE)

cat("Report written to", out, "\n")
cat("size:", round(file.size(out) / 1e6, 2), "MB\n")
