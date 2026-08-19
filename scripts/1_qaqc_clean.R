# Purpose: QA/QC and cleaning for the volunteer tree survey and oak seedling data. Every correction
#          is logged to <out>/data_processed/qaqc_flags.csv so nothing is silently changed or dropped.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Inputs:  <out>/data_raw_snapshots/*.csv (from 0_data_download.R)
# Outputs: <out>/data_processed/tree_survey_clean.csv  -- one row per live/dead stem, analysis-ready
#          <out>/data_processed/plot_status.csv        -- survey status of every plot-year
#          <out>/data_processed/seedlings_clean.csv    -- one row per plot-year-species
#          <out>/data_processed/qaqc_flags.csv         -- every flag raised, for team review
# Notes:   Extends the eye-test checks in Volunteer_survery_qaqc.R (species/canopy/vigor scans, DBH
#          range) into explicit, logged corrections. That script read a `Survey Date` column; the
#          sheet column is now `Date`.

source("scripts/0_helper_functions.R")

# ---------------------------------------------------------------------------------------------- #
# Flag log
# ---------------------------------------------------------------------------------------------- #
flags <- data.frame()
add.flag <- function(dataset, issue, action, plot = NA, tag = NA, date = NA, detail = NA) {
  n <- max(length(plot), length(tag), length(date), length(detail), 1)
  rep.n <- function(x) if (length(x) == n) x else rep(x, length.out = n)
  flags <<- rbind(flags, data.frame(
    dataset = dataset, issue = issue,
    plot_id = rep.n(plot), tree_tag = rep.n(tag), date = rep.n(date),
    detail = rep.n(detail), action = action, stringsAsFactors = FALSE))
}

# ============================================================================================== #
# TREE SURVEY
# ============================================================================================== #
tree <- read.cached("tree_survey")
names(tree)[names(tree) == "DBH (cm)"] <- "DBH"
names(tree)[names(tree) == "Plot ID"] <- "PlotID"
names(tree)[names(tree) == "Tree Tag"] <- "TreeTag"
names(tree)[names(tree) == "Species Code"] <- "SppCode"
names(tree)[names(tree) == "Canopy Position"] <- "Canopy"
names(tree)[names(tree) == "Vigor Rating"] <- "Vigor"
names(tree)[names(tree) == "Crew Lead"] <- "CrewLead"
names(tree)[names(tree) == "Field Notes"] <- "FieldNotes"
names(tree)[names(tree) == "Research Notes"] <- "ResearchNotes"

n.raw <- nrow(tree)

# --- Empty spreadsheet rows (no date at all): pre-formatted rows never used -------------------- #
no.date <- is.na(tree$Date)
add.flag("tree", "blank row with no survey date", "dropped",
         detail = paste(sum(no.date), "rows had no Date and no data"))
tree <- tree[!no.date, ]

# --- Date typo: 2011 -> 2022 ------------------------------------------------------------------- #
# CC-109's two 2011-08-15 rows sit interleaved with 2022-08-15 rows from the same crew and the same
# descending tag sequence (2469, 2468 ... 2459). A year typo, not a real 2011 visit.
bad.yr <- which(substr(tree$Date, 1, 4) == "2011")
if (length(bad.yr) > 0) {
  add.flag("tree", "survey date year typo (2011)", "corrected to 2022",
           plot = tree$PlotID[bad.yr], tag = tree$TreeTag[bad.yr], date = tree$Date[bad.yr],
           detail = "interleaved with 2022-08-15 rows, same crew & tag sequence")
  tree$Date[bad.yr] <- sub("^2011", "2022", tree$Date[bad.yr])
}

tree$Date <- as.Date(tree$Date)
tree$Year <- as.integer(format(tree$Date, "%Y"))

# --- Numeric coercion -------------------------------------------------------------------------- #
# Vigor is stored inconsistently as "1" and "1.00"; canopy codes also leaked into the vigor column.
vigor.is.canopy <- tree$Vigor %in% c("D", "C", "I", "O", "S")
if (any(vigor.is.canopy)) {
  add.flag("tree", "canopy position code entered in Vigor Rating column", "vigor set to NA; needs re-entry",
           plot = tree$PlotID[vigor.is.canopy], tag = tree$TreeTag[vigor.is.canopy],
           date = as.character(tree$Date[vigor.is.canopy]), detail = tree$Vigor[vigor.is.canopy])
  tree$Vigor[vigor.is.canopy] <- NA
}
tree$Vigor <- suppressWarnings(as.numeric(tree$Vigor))
bad.vig <- !is.na(tree$Vigor) & !tree$Vigor %in% 1:3
if (any(bad.vig)) {
  add.flag("tree", "vigor rating outside 1-3", "set to NA",
           plot = tree$PlotID[bad.vig], tag = tree$TreeTag[bad.vig], detail = tree$Vigor[bad.vig])
  tree$Vigor[bad.vig] <- NA
}

tree$DBH <- suppressWarnings(as.numeric(tree$DBH))

# --- Canopy position cleanup ------------------------------------------------------------------- #
canopy.map <- c("SNAG" = "S", "OI" = "I", "1" = NA)
to.fix <- tree$Canopy %in% names(canopy.map)
if (any(to.fix)) {
  add.flag("tree", "non-standard canopy position code", "recoded (SNAG->S, OI->I, 1->NA)",
           plot = tree$PlotID[to.fix], tag = tree$TreeTag[to.fix], detail = tree$Canopy[to.fix])
  tree$Canopy[to.fix] <- canopy.map[tree$Canopy[to.fix]]
}
tree$IsSnag <- !is.na(tree$Canopy) & tree$Canopy == "S"

# Snags carrying a vigor rating: vigor is a live-tree measure, so this is dropped for snags.
snag.vig <- tree$IsSnag & !is.na(tree$Vigor)
if (any(snag.vig)) {
  add.flag("tree", "snag assigned a live-tree vigor rating", "vigor set to NA for snags",
           plot = tree$PlotID[snag.vig], tag = tree$TreeTag[snag.vig], detail = tree$Vigor[snag.vig])
  tree$Vigor[snag.vig] <- NA
}

# --- Species code cleanup ---------------------------------------------------------------------- #
# Explicit map for codes we can resolve with confidence. Codes we cannot resolve are flagged for
# review rather than guessed at.
spp.map <- c("?" = "UNK", "U" = "UNK", "UNKNOWN" = "UNK",
             "TIQM" = "TIAM",            # adjacent-key typo for Tilia americana
             "QUVE or QUPA" = "QUERCUS_UNK",  # oak, species unresolved in the field
             "BUCKEYE" = "AESP")         # Aesculus spp., recorded by common name
sp.fix <- !is.na(tree$SppCode) & tree$SppCode %in% names(spp.map)
if (any(sp.fix)) {
  add.flag("tree", "species code typo / non-code entry", "remapped via explicit lookup",
           plot = tree$PlotID[sp.fix], tag = tree$TreeTag[sp.fix],
           detail = paste(tree$SppCode[sp.fix], "->", spp.map[tree$SppCode[sp.fix]]))
  tree$SppCode[sp.fix] <- spp.map[tree$SppCode[sp.fix]]
}

# Codes that are not obviously resolvable: flagged, kept as-is, and excluded from oak/non-oak claims
spp.review <- c("PESE", "CESA")   # PESE may be a PRSE typo; CESA is ambiguous. 1 stem each.
sp.rev <- !is.na(tree$SppCode) & tree$SppCode %in% spp.review
if (any(sp.rev)) {
  add.flag("tree", "unrecognized species code", "kept as recorded; needs team review",
           plot = tree$PlotID[sp.rev], tag = tree$TreeTag[sp.rev], detail = tree$SppCode[sp.rev])
}

tree$IsOak <- is.oak(tree$SppCode)

# --- DBH below protocol cutoff ----------------------------------------------------------------- #
small <- !is.na(tree$DBH) & tree$DBH < DBH.CUTOFF.CM
if (any(small)) {
  add.flag("tree", paste0("DBH below the ", DBH.CUTOFF.CM, " cm protocol cutoff"), "retained but flagged",
           plot = tree$PlotID[small], tag = tree$TreeTag[small], detail = tree$DBH[small])
}
tree$BelowCutoff <- small

# ---------------------------------------------------------------------------------------------- #
# Plot survey status -- must be resolved before any denominator is computed
# ---------------------------------------------------------------------------------------------- #
# Rows with a date but no stem measurements are one of three very different things, and the field
# notes distinguish them:
#   (a) the plot could not be located          -> plot was NOT surveyed, drop from all denominators
#   (b) the plot was found but held no trees   -> a real observation of basal area = 0
#   (c) an individual tagged tree was missing, or the row is unused pre-printed tag filler
#                                              -> drop the row, keep the plot
tree$HasStemData <- !(is.na(tree$SppCode) & is.na(tree$DBH) & is.na(tree$Canopy) & is.na(tree$Vigor))

notes.all <- paste(ifelse(is.na(tree$FieldNotes), "", tree$FieldNotes),
                   ifelse(is.na(tree$ResearchNotes), "", tree$ResearchNotes))

plot.status <- do.call(rbind, lapply(split(seq_len(nrow(tree)), tree$PlotID), function(ii) {
  n.stems <- sum(tree$HasStemData[ii])
  nt <- paste(notes.all[ii], collapse = " ")
  status <- if (n.stems > 0) {
    "surveyed"
  } else if (grepl("not found|could not find|unable to locate", nt, ignore.case = TRUE)) {
    "plot_not_found"
  } else if (grepl("no tree", nt, ignore.case = TRUE)) {
    "surveyed_no_trees"
  } else {
    "empty_no_explanation"
  }
  data.frame(PlotID = unique(tree$PlotID[ii]), Year = min(tree$Year[ii]),
             n_stem_rows = n.stems, status = status,
             note = substr(trimws(nt), 1, 120), stringsAsFactors = FALSE)
}))
rownames(plot.status) <- NULL

add.flag("tree", "plot could not be located in the field", "plot excluded from all summaries",
         plot = plot.status$PlotID[plot.status$status == "plot_not_found"],
         detail = plot.status$note[plot.status$status == "plot_not_found"])
add.flag("tree", "plot surveyed, no trees >= cutoff present", "retained as a true basal area of 0",
         plot = plot.status$PlotID[plot.status$status == "surveyed_no_trees"],
         detail = plot.status$note[plot.status$status == "surveyed_no_trees"])
if (any(plot.status$status == "empty_no_explanation")) {
  add.flag("tree", "plot has no stem data and no explanatory note", "excluded; ambiguous",
           plot = plot.status$PlotID[plot.status$status == "empty_no_explanation"])
}

# Individual missing trees / unused filler rows inside otherwise-surveyed plots
drop.row <- !tree$HasStemData & tree$PlotID %in% plot.status$PlotID[plot.status$status == "surveyed"]
if (any(drop.row)) {
  add.flag("tree", "tagged tree not relocated, or unused pre-printed tag row", "row dropped; plot retained",
           plot = tree$PlotID[drop.row], tag = tree$TreeTag[drop.row],
           detail = substr(notes.all[drop.row], 1, 120))
}
tree <- tree[tree$HasStemData, ]

# --- Plot identity conflict: A-133 ------------------------------------------------------------- #
# A-133 has stems recorded in two different years, which should not happen under the rotating design.
# The Plot List note on AZ-133 reads "stake says A-133; might've done wrong plot", so one of these
# visits was very likely a different plot. Both are kept and flagged: this needs a field decision.
dup.plot.yr <- unique(tree[, c("PlotID", "Year")])
multi.yr <- names(which(table(dup.plot.yr$PlotID) > 1))
if (length(multi.yr) > 0) {
  for (p in multi.yr) {
    yrs <- sort(unique(tree$Year[tree$PlotID == p]))
    add.flag("tree", "plot surveyed in more than one year", "both retained; needs field decision",
             plot = p, detail = paste("years:", paste(yrs, collapse = ", "),
                                      "- see Plot List note on AZ-133 ('stake says A-133')"))
  }
}

# --- Duplicate Plot x Tag ---------------------------------------------------------------------- #
# Two causes remain now that filler rows are gone:
#   exact duplicates      -> double data entry, dedupe
#   same tag, different tree -> tag transcription error, keep both stems and flag
key <- paste(tree$PlotID, tree$TreeTag)
meas <- paste(tree$SppCode, tree$DBH, tree$Canopy, tree$Vigor)
exact <- duplicated(paste(key, meas)) & !is.na(tree$TreeTag)
if (any(exact)) {
  add.flag("tree", "exact duplicate stem record", "deduplicated",
           plot = tree$PlotID[exact], tag = tree$TreeTag[exact], detail = meas[exact])
  tree <- tree[!exact, ]
  key <- paste(tree$PlotID, tree$TreeTag)
}
tag.conflict <- !is.na(tree$TreeTag) & key %in% key[duplicated(key)]
if (any(tag.conflict)) {
  add.flag("tree", "same tree tag on two different stems", "both stems retained; tag needs correction",
           plot = tree$PlotID[tag.conflict], tag = tree$TreeTag[tag.conflict],
           detail = paste(tree$SppCode[tag.conflict], tree$DBH[tag.conflict], "cm"))
}

# --- Stems recorded outside the plot radius (cross-check vs the metadata tab) ------------------- #
md <- read.cached("tree_plot_metadata")
names(md)[names(md) == "Plot ID"] <- "PlotID"
names(md)[names(md) == "Tree Tag"] <- "TreeTag"
md$Distance <- suppressWarnings(as.numeric(md$Distance))
far <- md[!is.na(md$Distance) & md$Distance > PLOT.RADIUS.M, ]
far <- far[paste(far$PlotID, far$TreeTag) %in% paste(tree$PlotID, tree$TreeTag), ]
if (nrow(far) > 0) {
  add.flag("tree", paste0("stem mapped beyond the ", PLOT.RADIUS.M, " m plot radius"),
           "retained but flagged", plot = far$PlotID, tag = far$TreeTag,
           detail = paste(round(far$Distance, 2), "m from plot center"))
}

# --- Join plot location & monitoring unit ------------------------------------------------------ #
plots <- read.cached("plot_list")
names(plots)[names(plots) == "Plot ID"] <- "PlotID"
names(plots)[names(plots) == "Monitoring Unit"] <- "MgmtUnit"
names(plots)[names(plots) == "Survey Year Group"] <- "YearGroup"
plots <- plots[!is.na(plots$PlotID), ]
plots$Longitude <- as.numeric(plots$Longitude)
plots$Latitude <- as.numeric(plots$Latitude)
plots <- plots[, c("PlotID", "MgmtUnit", "YearGroup", "Longitude", "Latitude")]

tree <- merge(tree, plots, by = "PlotID", all.x = TRUE)
if (any(is.na(tree$MgmtUnit))) {
  miss <- unique(tree$PlotID[is.na(tree$MgmtUnit)])
  add.flag("tree", "surveyed plot missing from the master Plot List", "no unit/coordinates available",
           plot = miss)
}

plot.status <- merge(plot.status, plots, by = "PlotID", all.x = TRUE)

# --- Write ------------------------------------------------------------------------------------- #
tree <- tree[, c("PlotID", "MgmtUnit", "YearGroup", "Longitude", "Latitude", "Date", "Year",
                 "CrewLead", "TreeTag", "SppCode", "IsOak", "DBH", "Canopy", "IsSnag", "Vigor",
                 "BelowCutoff", "FieldNotes", "ResearchNotes")]
tree <- tree[order(tree$Year, tree$PlotID, tree$TreeTag), ]
write.csv(tree, file.path(path.proc, "tree_survey_clean.csv"), row.names = FALSE)
write.csv(plot.status, file.path(path.proc, "plot_status.csv"), row.names = FALSE)

cat("\n=== TREE SURVEY ===\n")
cat("raw rows:", n.raw, " -> clean stems:", nrow(tree), "\n")
cat("plots by status:\n"); print(table(plot.status$status))
cat("stems by year:\n"); print(table(tree$Year))
cat("oak stems:", sum(tree$IsOak), " snags:", sum(tree$IsSnag), "\n")
cat("stems missing DBH:", sum(is.na(tree$DBH)), " missing vigor (live):", sum(is.na(tree$Vigor) & !tree$IsSnag), "\n")

# ============================================================================================== #
# SEEDLINGS
# ============================================================================================== #
seed.cols <- c("Date", "Enterer", "Priority", "MgmtUnitRaw", "PlotID",
               "LonRaw", "LatRaw", "SppCode", "C1", "C2", "C3", "Notes")

read.seed <- function(name, year) {
  d <- read.cached(name)[, 1:12]          # cols 13-19 are a printable species legend, not data
  names(d) <- seed.cols
  d$SurveyYear <- year
  d
}
seed <- rbind(read.seed("seedlings_2023", 2023),
              read.seed("seedlings_2024", 2024),
              read.seed("seedlings_2025", 2025))
n.seed.raw <- nrow(seed)

# --- Header rows that leaked into the data range ----------------------------------------------- #
hdr <- (!is.na(seed$Date) & seed$Date == "Survey Date") |
       (!is.na(seed$MgmtUnitRaw) & seed$MgmtUnitRaw == "Monitoring Unit") |
       (!is.na(seed$SppCode) & seed$SppCode == "Species Code") |
       (!is.na(seed$C1) & seed$C1 %in% c("Class 1", "Qty."))
add.flag("seedling", "header row inside the data range", "dropped",
         detail = paste(sum(hdr), "rows"))
seed <- seed[!hdr, ]

# --- Plot ID formatting ------------------------------------------------------------------------ #
# One plot is entered without the hyphen used everywhere else
no.hyph <- !is.na(seed$PlotID) & !grepl("-", seed$PlotID)
if (any(no.hyph)) {
  fixed <- sub("^([A-Za-z]+)(\\d+)$", "\\1-\\2", seed$PlotID[no.hyph])
  ok <- fixed %in% plots$PlotID
  add.flag("seedling", "plot ID missing hyphen", ifelse(ok, "corrected", "could not match Plot List"),
           plot = seed$PlotID[no.hyph], detail = paste(seed$PlotID[no.hyph], "->", fixed))
  seed$PlotID[no.hyph][ok] <- fixed[ok]
}

# --- Dates ------------------------------------------------------------------------------------- #
# 34 rows in the 2023 tab carry a doubled year, "10/16/20/23" -- a mangled "10/16/2023". The same
# visit also appears correctly as "10/16/2023" and "2023-10-16" elsewhere in the tab, which confirms
# the intended date. Three of these rows end /24 and /26, but they sit mid-run among /23 rows for the
# same plot and enterer, so they are stray keystrokes rather than different dates: the tab's survey
# year is authoritative and the trailing digits are not trusted.
mangled <- !is.na(seed$Date) & grepl("^\\d{1,2}/\\d{1,2}/\\d{2}/\\d{2}$", seed$Date)
if (any(mangled)) {
  parts <- strsplit(seed$Date[mangled], "/")
  mo <- as.integer(sapply(parts, `[`, 1))
  dy <- as.integer(sapply(parts, `[`, 2))
  yr.implied <- as.integer(paste0(sapply(parts, `[`, 3), sapply(parts, `[`, 4)))
  fixed <- sprintf("%d-%02d-%02d", seed$SurveyYear[mangled], mo, dy)
  add.flag("seedling", "malformed survey date (doubled year)",
           ifelse(yr.implied == seed$SurveyYear[mangled], "parsed",
                  "parsed using the tab's survey year; trailing digits disagreed"),
           plot = seed$PlotID[mangled], detail = paste(seed$Date[mangled], "->", fixed))
  seed$Date[mangled] <- fixed
}
seed$Date <- as.Date(seed$Date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y"))

# --- Species cleanup --------------------------------------------------------------------------- #
seed$SppCode <- trimws(seed$SppCode)
seed.map <- c("RIMIR" = "RIMI",                              # trailing-character typo
              "Dogwood" = "COSP", "UNK - Dogwood" = "COSP",  # common name; Cornus at genus
              "Empty" = "EMPTY")
s.fix <- !is.na(seed$SppCode) & seed$SppCode %in% names(seed.map)
if (any(s.fix)) {
  add.flag("seedling", "species code typo / common name", "remapped",
           plot = seed$PlotID[s.fix], detail = paste(seed$SppCode[s.fix], "->", seed.map[seed$SppCode[s.fix]]))
  seed$SppCode[s.fix] <- seed.map[seed$SppCode[s.fix]]
}

# --- Class counts ------------------------------------------------------------------------------ #
for (cc in c("C1", "C2", "C3")) seed[[cc]] <- suppressWarnings(as.numeric(seed[[cc]]))

# --- Survey status per plot-year --------------------------------------------------------------- #
# EMPTY    = plot visited, no woody regeneration found -> a true zero, belongs in the denominator
# NOT FOUND = plot could not be located                -> not surveyed, must be excluded
# no species code at all = pre-filled plot list row, never visited
seed$Surveyed <- !is.na(seed$Date) & !is.na(seed$SppCode) & seed$SppCode != "NOT FOUND"
seed$IsEmpty <- !is.na(seed$SppCode) & seed$SppCode == "EMPTY"

nf <- !is.na(seed$SppCode) & seed$SppCode == "NOT FOUND"
if (any(nf)) {
  add.flag("seedling", "plot could not be located", "excluded from year denominators",
           plot = seed$PlotID[nf], date = as.character(seed$Date[nf]),
           detail = paste("survey year", seed$SurveyYear[nf]))
}
add.flag("seedling", "plot visited with no regeneration found (EMPTY)", "retained as a true zero",
         detail = paste(sum(seed$IsEmpty), "plot-years"))

seed <- seed[seed$Surveyed, ]

# --- Location & unit come from the master Plot List, never from this sheet --------------------- #
# The seedling sheet's own unit names are free text and inconsistent across years ("Central Wood.",
# "EW-Central", "E. W. Central"; "East woods   1" vs "East Woods 1"), and 2025 has no coordinates.
add.flag("seedling", "inconsistent free-text monitoring unit / missing coordinates",
         "unit and coordinates joined from the master Plot List by Plot ID",
         detail = paste(length(unique(seed$MgmtUnitRaw)), "distinct unit spellings across 3 years"))
seed <- merge(seed, plots, by = "PlotID", all.x = TRUE)
if (any(is.na(seed$MgmtUnit))) {
  miss <- unique(seed$PlotID[is.na(seed$MgmtUnit)])
  add.flag("seedling", "seedling plot missing from the master Plot List", "no unit/coordinates available",
           plot = miss)
}

seed$IsOak <- is.oak(seed$SppCode)
seed$IsInvasive <- !is.na(seed$SppCode) & seed$SppCode %in% INVASIVE.CODES

# The three years were not sampled at the same point in the season, which affects how detectable
# small seedlings are and is a second reason to read year-to-year differences cautiously.
doy <- tapply(seed$Date, seed$SurveyYear, function(x) paste(format(range(as.Date(x)), "%b %d"), collapse = "-"))
add.flag("seedling", "survey window differs among years", "noted as a caveat on through-time comparisons",
         detail = paste(names(doy), doy, collapse = "; "))

# 2023 classes 2-3 are almost entirely blank compared with later years -- flagged loudly because it
# determines which comparisons through time are defensible.
c23 <- seed[seed$SurveyYear == 2023 & !seed$IsEmpty, ]
add.flag("seedling", "Class 2/3 largely unrecorded in 2023",
         "primary through-time comparison uses Class 1 only",
         detail = sprintf("%d of %d 2023 species records have any Class 2/3 entry vs near-complete in 2024-25",
                          sum(!is.na(c23$C2) | !is.na(c23$C3)), nrow(c23)))

# Blank class cells on a real species record mean none in that size class
for (cc in c("C1", "C2", "C3")) seed[[cc]][is.na(seed[[cc]])] <- 0

seed <- seed[, c("PlotID", "MgmtUnit", "YearGroup", "Longitude", "Latitude", "Date", "SurveyYear",
                 "Enterer", "SppCode", "IsOak", "IsInvasive", "IsEmpty", "C1", "C2", "C3", "Notes")]
seed <- seed[order(seed$SurveyYear, seed$PlotID, seed$SppCode), ]
write.csv(seed, file.path(path.proc, "seedlings_clean.csv"), row.names = FALSE)

cat("\n=== SEEDLINGS ===\n")
cat("raw rows:", n.seed.raw, " -> clean records:", nrow(seed), "\n")
cat("plots surveyed per year:\n")
print(tapply(seed$PlotID, seed$SurveyYear, function(x) length(unique(x))))
cat("oak records per year:\n"); print(tapply(seed$IsOak, seed$SurveyYear, sum))

# ---------------------------------------------------------------------------------------------- #
# Write the flag log
# ---------------------------------------------------------------------------------------------- #
flags <- flags[order(flags$dataset, flags$issue), ]
write.csv(flags, file.path(path.proc, "qaqc_flags.csv"), row.names = FALSE)

cat("\n=== QA/QC FLAGS ===\n")
print(as.data.frame(table(dataset = flags$dataset, issue = flags$issue))[
  as.data.frame(table(dataset = flags$dataset, issue = flags$issue))$Freq > 0, ])
cat("\nTotal flags logged:", nrow(flags), "-> <out>/data_processed/qaqc_flags.csv\n")
