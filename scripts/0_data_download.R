# Purpose: Pull all raw tabs from the two project Google Sheets and cache them locally as CSVs
#          so that every downstream script is reproducible offline and we have a dated snapshot
#          of the data as volunteers continue entering it.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Description: Reads the Tree Survey and Oak Seedling sheets via googlesheets4. All tabs are read
#              as character columns; parsing and QA/QC happen in 1_qaqc_clean.R so that nothing is
#              silently coerced or dropped at the download step.

library(googlesheets4)

# ---------------------------------------------------------------------------------------------- #
# Setup
# ---------------------------------------------------------------------------------------------- #
# Sourced for path.raw, which resolves to the shared Drive folder (see 0_helper_functions.R)
source("scripts/0_helper_functions.R")

# Uses the cached OAuth token so this runs non-interactively
gs4_auth(email = "crollinson@mortonarb.org")

id.trees <- "161i75Il4W3u8oJ6yMpY9fMfudHCNlamcD89SLj0u-hY"
id.seedlings <- "1Q8JuUhpimc8POr1Ez9ERwrLmoGfaT0Ez5_w9u0QfUU4"

stamp <- format(Sys.Date(), "%Y-%m-%d")

# Helper: read one tab as all-character and write it to the cache with a date stamp.
# `skip` handles the seedling tabs that carry an extra title row above the real header.
cache.tab <- function(sheet.id, tab, out.name, skip = 0) {
  dat <- googlesheets4::read_sheet(sheet.id, sheet = tab, col_types = "c", skip = skip)
  dat <- as.data.frame(dat)

  f.out <- file.path(path.raw, paste0(out.name, "_", stamp, ".csv"))
  write.csv(dat, f.out, row.names = FALSE)

  cat(sprintf("  %-28s -> %-34s  %4d rows x %2d cols\n", tab, basename(f.out), nrow(dat), ncol(dat)))
  invisible(dat)
}

# ---------------------------------------------------------------------------------------------- #
# Tree survey sheet: all 5 tabs
# ---------------------------------------------------------------------------------------------- #
cat("Tree Survey sheet:\n")
cache.tab(id.trees, "Tree Survey", "tree_survey")
cache.tab(id.trees, "Plot List", "plot_list")
cache.tab(id.trees, "Tree/Plot Metadata", "tree_plot_metadata")
cache.tab(id.trees, "Trees - Species Code Metadata", "species_code_metadata")
cache.tab(id.trees, "README", "tree_readme")

# ---------------------------------------------------------------------------------------------- #
# Seedling sheet: the 3 real data tabs only
# ---------------------------------------------------------------------------------------------- #
# The "Data Entry" and "Printable" tabs are legacy print templates (species legends laid out for
# printing, not records) and are deliberately not downloaded.
# The 2024 and 2025 tabs have a title row above the header, hence skip=1. Both also carry a species
# legend in columns 13-19 that gets trimmed in 1_qaqc_clean.R.
cat("\nSeedling sheet:\n")
cache.tab(id.seedlings, "Reformatted 2023 Data Entry", "seedlings_2023", skip = 0)
cache.tab(id.seedlings, "2024 Data Entry", "seedlings_2024", skip = 1)
cache.tab(id.seedlings, "2025 Data Entry", "seedlings_2025", skip = 1)

cat("\nCached", length(list.files(path.raw, pattern = paste0(stamp, ".csv$"))), "tabs to", path.raw, "\n")
