# Purpose: Build a crosswalk between the 2018 IES 6-letter species codes (QUEALB, TILAME) and the
#          4-letter codes volunteers use (QUAL, TIAM), so the 2018 survey can be compared with the
#          current data. Also produces a full species lookup, since the sheet's own species metadata
#          tab has only 10 codes.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Inputs:  data/East Woods Spring vegetation data.final.csv, data/raw_gsheet/*.csv
# Outputs: data/processed/species_crosswalk.csv, data/processed/veg2018_clean.csv

source("scripts/0_helper_functions.R")

# ---------------------------------------------------------------------------------------------- #
# 2018 IES survey
# ---------------------------------------------------------------------------------------------- #
v18 <- read.csv(file.path("data", "East Woods Spring vegetation data.final.csv"),
                na.strings = "", check.names = TRUE)[, 1:10]
names(v18) <- c("Date", "Sampler", "PlotID", "Code6", "SppName", "DBH", "Canopy",
                "DecayClass", "Vigor", "Notes")
v18 <- v18[!is.na(v18$PlotID), ]

# Trailing whitespace: "QUERUB " reads as a 7-character code, which broke the 6-letter rule and
# silently dropped a red oak stem the first time this ran.
v18$Code6 <- trimws(v18$Code6)
v18$SppName <- trimws(v18$SppName)

# 2018 plot IDs carry no hyphen (A133); the volunteer sheet uses A-133. Match on the stripped form.
v18$PlotJoin <- gsub("-", "", trimws(v18$PlotID))
v18$DBH <- suppressWarnings(as.numeric(v18$DBH))
v18$Vigor <- suppressWarnings(as.numeric(v18$Vigor))
v18$Canopy <- toupper(trimws(v18$Canopy))

# Vigor is the same 1-3 scale as today, but the 2018 file also holds 0 and 5, which are outside it.
bad.v <- !is.na(v18$Vigor) & !v18$Vigor %in% 1:3
cat("2018 vigor values outside 1-3 set to NA:", sum(bad.v), "\n")
v18$Vigor[bad.v] <- NA

v18$IsSnag <- !is.na(v18$Canopy) & v18$Canopy == "S"
# Snag vigor is not a live-tree measure, same treatment as the current data
v18$Vigor[v18$IsSnag] <- NA

# ---------------------------------------------------------------------------------------------- #
# The code rule
# ---------------------------------------------------------------------------------------------- #
# Both schemes are built from the same parts: 2018 uses 3 letters of genus + 3 of species, the
# volunteer sheet uses 2 + 2. So the first 2 of each half of the 6-letter code gives the 4-letter
# code: QUEALB -> QU + AL -> QUAL; ACESAU -> AC + SA -> ACSA.
code6.to.code4 <- function(x) {
  ifelse(is.na(x) | nchar(x) != 6, NA_character_,
         paste0(substr(x, 1, 2), substr(x, 4, 5)))
}

codes18 <- sort(unique(v18$Code6))
xw <- data.frame(Code6 = codes18, stringsAsFactors = FALSE)
xw$SppName <- v18$SppName[match(xw$Code6, v18$Code6)]
xw$n_stems_2018 <- as.integer(table(v18$Code6)[xw$Code6])
xw$Code4 <- code6.to.code4(xw$Code6)

# --- Non-species entries: these are records of absence or of a failed identification ------------ #
# "No trees" is the 2018 equivalent of our plots surveyed with nothing above the cutoff.
non.species <- c("No trees", "Unidentified", "Unknown", "unknown", "No Trees", "To be collected")
xw$Code4[xw$Code6 %in% non.species] <- NA
xw$note <- ifelse(xw$Code6 %in% non.species, "not a species record", NA_character_)
xw$note[grepl("sp\\.$|spp\\.$", xw$Code6)] <- "genus-level record"

# Genus-only entries such as "Fraxinus sp." do not fit the 6-letter rule; map them to the genus
# codes the volunteers use for the same thing.
genus.map <- c("Fraxinus sp." = "FRSP", "Ulmus sp." = "ULSP", "Carya sp." = "CASP",
               "Quercus sp." = "QUERCUS_UNK", "Crataegus sp." = "CRSP", "Salix sp." = "SASP",
               "Populus sp." = "POSP", "Prunus sp." = "PRSP", "Acer sp." = "ACSP",
               "Cornus sp." = "COSP", "Lonicera sp." = "LOSP", "Viburnum sp." = "VISP",
               "Rhamnus sp." = "RHSP", "Ribes sp." = "RISP", "Aesculus sp." = "AESP",
               "Picea sp." = "PISP", "Pinus sp." = "PNSP")
g.hit <- xw$Code6 %in% names(genus.map)
xw$Code4[g.hit] <- genus.map[xw$Code6[g.hit]]

xw$IsOak <- is.oak(xw$Code4)

# ---------------------------------------------------------------------------------------------- #
# Validation
# ---------------------------------------------------------------------------------------------- #
# 1. Every 2018 code that the rule could not resolve
unresolved <- xw[is.na(xw$Code4), ]
cat("\n2018 codes not mapped to a 4-letter code (", nrow(unresolved), "):\n", sep = "")
print(unresolved[, c("Code6", "SppName", "n_stems_2018", "note")], row.names = FALSE)

# 2. Confirm every oak is carried across. This is the mapping the vigor comparison depends on, so a
#    silent loss here would quietly shrink the oak sample.
oaks18 <- xw[grepl("^QUE", xw$Code6) | grepl("^Quercus", xw$SppName), ]
cat("\n2018 oak codes and their mapping:\n")
print(oaks18[, c("Code6", "SppName", "Code4", "n_stems_2018", "IsOak")], row.names = FALSE)
stopifnot(all(oaks18$IsOak))
cat("All", nrow(oaks18), "2018 oak codes map to an oak code in the current scheme.\n")

# 3. Codes that collapse: two distinct 2018 species sharing one volunteer code. This is a limit of
#    the 4-letter scheme itself, not of the mapping, so it is recorded rather than "fixed".
res <- xw[!is.na(xw$Code4), ]
coll.codes <- unique(res$Code4[duplicated(res$Code4)])
if (length(coll.codes) > 0) {
  cat("\nVolunteer codes that cannot distinguish two 2018 species:\n")
  coll <- res[res$Code4 %in% coll.codes, c("Code6", "SppName", "Code4", "n_stems_2018")]
  print(coll[order(coll$Code4, -coll$n_stems_2018), ], row.names = FALSE)
  cat("These are pooled under the shared code. Oak analyses are unaffected;\n",
      "the species composition figure pools them and the label says so.\n", sep = "")
}
xw$code4_ambiguous <- xw$Code4 %in% coll.codes

# 4. Cross-check the rule against the sheet's own (sparse) species metadata tab
meta <- read.cached("species_code_metadata")
meta$SppName <- paste(meta$Genus, meta$Species)
chk <- merge(meta[, c("Code", "SppName")], xw[, c("Code4", "SppName")],
             by.x = "Code", by.y = "Code4", suffixes = c(".meta", ".2018"))
cat("\nAgreement with the sheet's species metadata tab (", nrow(chk), " codes matched):\n", sep = "")
# Genus match is the meaningful test; the two sources abbreviate species epithets differently
chk$genus.agree <- sub(" .*", "", chk$SppName.meta) == sub(" .*", "", chk$SppName.2018)
print(chk[, c("Code", "SppName.meta", "SppName.2018", "genus.agree")], row.names = FALSE)
if (!all(chk$genus.agree)) warning("Genus disagreement between the metadata tab and the 2018 names")

# 5. Codes used by volunteers that the 2018 data never saw, and vice versa
tree <- read.csv(file.path(path.proc, "tree_survey_clean.csv"), stringsAsFactors = FALSE)
vol.codes <- sort(unique(tree$SppCode))
cat("\nVolunteer codes with no 2018 counterpart:",
    paste(setdiff(vol.codes, xw$Code4), collapse = ", "), "\n")

# ---------------------------------------------------------------------------------------------- #
# Species lookup for labelling figures
# ---------------------------------------------------------------------------------------------- #
# Ambiguous codes get a label naming both species, so no figure implies more precision than exists.
lookup <- xw[!is.na(xw$Code4) & is.na(xw$note), c("Code4", "SppName", "n_stems_2018")]
lookup <- lookup[order(lookup$Code4, -lookup$n_stems_2018), ]
for (cc in coll.codes) {
  ii <- which(lookup$Code4 == cc)
  gen <- sub(" .*", "", lookup$SppName[ii[1]])
  epi <- sub("^\\S+ ", "", lookup$SppName[ii])
  lookup$SppName[ii[1]] <- paste0(gen, " ", paste(epi, collapse = "/"))
}
lookup <- lookup[!duplicated(lookup$Code4), c("Code4", "SppName")]
extra <- data.frame(
  Code4 = c("QUERCUS_UNK", "UNK", "AESP", "PESE", "CESA"),
  SppName = c("Quercus spp. (unresolved)", "Unidentified", "Aesculus spp.",
              "unrecognized code (review)", "unrecognized code (review)"),
  stringsAsFactors = FALSE)
lookup <- rbind(lookup, extra[!extra$Code4 %in% lookup$Code4, ])
missing.lab <- setdiff(vol.codes, lookup$Code4)
if (length(missing.lab) > 0) {
  lookup <- rbind(lookup, data.frame(Code4 = missing.lab, SppName = missing.lab))
}

write.csv(xw, file.path(path.proc, "species_crosswalk.csv"), row.names = FALSE)
write.csv(lookup, file.path(path.proc, "species_lookup.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------------------------- #
# 2018 data, cleaned and carrying the current-scheme codes
# ---------------------------------------------------------------------------------------------- #
v18$Code4 <- xw$Code4[match(v18$Code6, xw$Code6)]
v18$IsOak <- is.oak(v18$Code4)
v18 <- v18[, c("PlotID", "PlotJoin", "Date", "Code6", "Code4", "SppName", "IsOak",
               "DBH", "Canopy", "IsSnag", "Vigor")]
write.csv(v18, file.path(path.proc, "veg2018_clean.csv"), row.names = FALSE)

cat("\n=== 2018 SURVEY ===\n")
cat("stems:", nrow(v18), " plots:", length(unique(v18$PlotJoin)), "\n")
cat("oak stems:", sum(v18$IsOak), " with a vigor rating:", sum(v18$IsOak & !is.na(v18$Vigor)), "\n")
cat("vigor distribution (live stems):\n"); print(table(v18$Vigor, useNA = "ifany"))
