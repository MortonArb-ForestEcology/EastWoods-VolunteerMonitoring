# Purpose: Build the plot-level summary table -- basal area, oak dominance, structure, and oak vigor
#          for every surveyed plot -- and the descriptive statistics that go with it.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Inputs:  data/processed/tree_survey_clean.csv, plot_status.csv
# Outputs: data/processed/plot_summary.csv, data/processed/summary_stats_plots.csv

source("scripts/0_helper_functions.R")

tree <- read.csv(file.path(path.proc, "tree_survey_clean.csv"), stringsAsFactors = FALSE)
status <- read.csv(file.path(path.proc, "plot_status.csv"), stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------------------------- #
# Per plot-year metrics
# ---------------------------------------------------------------------------------------------- #
# A-133 has stems in two different years (a flagged plot-identity problem), so the unit of
# summary is the plot-year, not the plot.
tree$PlotYear <- paste(tree$PlotID, tree$Year, sep = "_")

summarize.plot <- function(d) {
  live <- d[!d$IsSnag, ]
  snag <- d[d$IsSnag, ]
  live.oak <- live[live$IsOak, ]

  # Basal area can only be computed from stems that have a DBH; n_dbh_missing records how many
  # stems the plot's basal area is therefore missing.
  ba.live <- ba.plot.m2ha(live$DBH)
  ba.snag <- ba.plot.m2ha(snag$DBH)
  ba.oak <- ba.plot.m2ha(live.oak$DBH)

  # Vigor: live oaks only. prop_poor is P(vigor >= 2), the cleaner response on a 3-level ordinal.
  vig <- live.oak$Vigor[!is.na(live.oak$Vigor)]

  data.frame(
    PlotID = d$PlotID[1], Year = d$Year[1], Date = min(d$Date),
    MgmtUnit = d$MgmtUnit[1], YearGroup = d$YearGroup[1],
    Longitude = d$Longitude[1], Latitude = d$Latitude[1],
    CrewLead = paste(sort(unique(d$CrewLead)), collapse = "/"),

    stems_live = nrow(live), stems_snag = nrow(snag), stems_oak = nrow(live.oak),
    n_dbh_missing = sum(is.na(live$DBH)),

    BA_live = ba.live, BA_snag = ba.snag, BA_total = ba.live + ba.snag, BA_oak = ba.oak,
    prop_BA_oak = if (ba.live > 0) ba.oak / ba.live else NA_real_,
    prop_stem_oak = if (nrow(live) > 0) nrow(live.oak) / nrow(live) else NA_real_,
    prop_BA_snag = if ((ba.live + ba.snag) > 0) ba.snag / (ba.live + ba.snag) else NA_real_,

    QMD = if (sum(!is.na(live$DBH)) > 0) qmd.cm(live$DBH) else NA_real_,
    DBH_max = suppressWarnings(max(live$DBH, na.rm = TRUE)),
    richness = length(unique(live$SppCode[!live$SppCode %in% c("UNK", "PESE", "CESA")])),

    n_oak_vigor = length(vig),
    vigor_mean = if (length(vig) > 0) mean(vig) else NA_real_,
    prop_poor = if (length(vig) > 0) mean(vig >= 2) else NA_real_,
    stringsAsFactors = FALSE)
}

plot.sum <- do.call(rbind, lapply(split(tree, tree$PlotYear), summarize.plot))
plot.sum$DBH_max[!is.finite(plot.sum$DBH_max)] <- NA

# ---------------------------------------------------------------------------------------------- #
# Plots surveyed with no trees above the cutoff: a real basal area of zero, not a missing value
# ---------------------------------------------------------------------------------------------- #
zero.plots <- status[status$status == "surveyed_no_trees", ]
if (nrow(zero.plots) > 0) {
  z <- data.frame(
    PlotID = zero.plots$PlotID, Year = zero.plots$Year, Date = NA_character_,
    MgmtUnit = zero.plots$MgmtUnit, YearGroup = zero.plots$YearGroup,
    Longitude = zero.plots$Longitude, Latitude = zero.plots$Latitude, CrewLead = NA_character_,
    stems_live = 0L, stems_snag = 0L, stems_oak = 0L, n_dbh_missing = 0L,
    BA_live = 0, BA_snag = 0, BA_total = 0, BA_oak = 0,
    # No live basal area means oak share is undefined, not zero -- there is nothing to take a share of
    prop_BA_oak = NA_real_, prop_stem_oak = NA_real_, prop_BA_snag = NA_real_,
    QMD = NA_real_, DBH_max = NA_real_, richness = 0L,
    n_oak_vigor = 0L, vigor_mean = NA_real_, prop_poor = NA_real_,
    stringsAsFactors = FALSE)
  plot.sum <- rbind(plot.sum, z[, names(plot.sum)])
  cat("Added", nrow(z), "plot(s) surveyed with no trees above the cutoff as basal area = 0:",
      paste(z$PlotID, collapse = ", "), "\n")
}

plot.sum <- plot.sum[order(plot.sum$Year, plot.sum$PlotID), ]
rownames(plot.sum) <- NULL
write.csv(plot.sum, file.path(path.proc, "plot_summary.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------------------------- #
# Checks
# ---------------------------------------------------------------------------------------------- #
cat("\n=== CHECKS ===\n")
n.expected <- sum(status$status %in% c("surveyed", "surveyed_no_trees")) +
  sum(table(tree$PlotID) > 0 & tapply(tree$Year, tree$PlotID, function(x) length(unique(x)))[names(table(tree$PlotID))] > 1)
cat("plot-years summarized:", nrow(plot.sum), "\n")
cat("plots surveyed (status file):", sum(status$status %in% c("surveyed", "surveyed_no_trees")), "\n")
cat("plots excluded as not found:", sum(status$status == "plot_not_found"), "\n")
cat("stems accounted for:", sum(plot.sum$stems_live + plot.sum$stems_snag), "of", nrow(tree), "clean stems\n")
stopifnot(sum(plot.sum$stems_live + plot.sum$stems_snag) == nrow(tree))
cat("plot-years missing coordinates:", sum(is.na(plot.sum$Longitude)), "\n")
cat("plot-years missing monitoring unit:", sum(is.na(plot.sum$MgmtUnit)), "\n")
cat("plot-years with any stem missing DBH:", sum(plot.sum$n_dbh_missing > 0), "\n")

# Hand-check one plot against the formula, so the pipeline's basal area is verifiable by eye
chk.plot <- "R-128"
d.chk <- tree[tree$PlotID == chk.plot & !tree$IsSnag, ]
ba.chk <- sum(pi * (d.chk$DBH / 200)^2, na.rm = TRUE) / PLOT.AREA.HA
cat("\nhand-check", chk.plot, ": recomputed BA_live =", round(ba.chk, 3),
    "| table =", round(plot.sum$BA_live[plot.sum$PlotID == chk.plot], 3), "\n")
stopifnot(abs(ba.chk - plot.sum$BA_live[plot.sum$PlotID == chk.plot]) < 1e-8)

# ---------------------------------------------------------------------------------------------- #
# Descriptive statistics
# ---------------------------------------------------------------------------------------------- #
q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
desc <- function(x, label, unit = "") {
  x <- x[!is.na(x)]
  data.frame(metric = label, unit = unit, n = length(x),
             mean = mean(x), sd = sd(x), median = median(x),
             q25 = q(x, .25), q75 = q(x, .75), min = min(x), max = max(x),
             stringsAsFactors = FALSE)
}

stats <- rbind(
  desc(plot.sum$BA_live, "Live basal area", "m2/ha"),
  desc(plot.sum$BA_snag, "Snag basal area", "m2/ha"),
  desc(plot.sum$BA_total, "Total basal area (live + snag)", "m2/ha"),
  desc(plot.sum$BA_oak, "Oak basal area", "m2/ha"),
  desc(plot.sum$prop_BA_oak, "Oak share of live basal area", "proportion"),
  desc(plot.sum$prop_stem_oak, "Oak share of live stems", "proportion"),
  desc(plot.sum$prop_BA_snag, "Snag share of total basal area", "proportion"),
  desc(plot.sum$stems_live / PLOT.AREA.HA, "Live stem density", "stems/ha"),
  desc(plot.sum$stems_snag / PLOT.AREA.HA, "Snag density", "stems/ha"),
  desc(plot.sum$QMD, "Quadratic mean diameter", "cm"),
  desc(plot.sum$DBH_max, "Largest live stem", "cm"),
  desc(plot.sum$richness, "Species richness", "species/plot"),
  desc(plot.sum$vigor_mean, "Mean oak vigor (1 best - 3 worst)", "rating"),
  desc(plot.sum$prop_poor, "Oak stems with vigor >= 2", "proportion"))

# Landscape-level oak share: the basal-area-weighted value. This differs from the plot mean above,
# and the two answer different questions -- "what share of the forest is oak" vs "what does a
# typical plot look like".
land <- data.frame(
  metric = c("Oak share of live basal area (landscape, BA-weighted)",
             "Oak share of live stems (landscape, pooled)"),
  unit = "proportion", n = nrow(plot.sum),
  mean = c(sum(plot.sum$BA_oak) / sum(plot.sum$BA_live),
           sum(plot.sum$stems_oak) / sum(plot.sum$stems_live)),
  sd = NA, median = NA, q25 = NA, q75 = NA, min = NA, max = NA, stringsAsFactors = FALSE)
stats <- rbind(stats, land)

write.csv(stats, file.path(path.proc, "summary_stats_plots.csv"), row.names = FALSE)

cat("\n=== PLOT-LEVEL SUMMARY STATISTICS ===\n")
print(within(stats, {
  mean <- round(mean, 3); sd <- round(sd, 3); median <- round(median, 3)
  q25 <- round(q25, 3); q75 <- round(q75, 3); min <- round(min, 3); max <- round(max, 3)
}), row.names = FALSE)

cat("\n=== BY MONITORING UNIT ===\n")
by.unit <- do.call(rbind, lapply(split(plot.sum, plot.sum$MgmtUnit), function(d) data.frame(
  MgmtUnit = d$MgmtUnit[1], n_plots = nrow(d),
  BA_live = round(mean(d$BA_live), 1), BA_live_sd = round(sd(d$BA_live), 1),
  prop_BA_oak = round(mean(d$prop_BA_oak, na.rm = TRUE), 3),
  prop_stem_oak = round(mean(d$prop_stem_oak, na.rm = TRUE), 3),
  n_oak_stems = sum(d$stems_oak),
  prop_poor = round(weighted.mean(d$prop_poor, d$n_oak_vigor, na.rm = TRUE), 3),
  stringsAsFactors = FALSE)))
by.unit <- by.unit[order(-by.unit$prop_BA_oak), ]
print(by.unit, row.names = FALSE)
write.csv(by.unit, file.path(path.proc, "summary_stats_by_unit.csv"), row.names = FALSE)

cat("\n=== BY SURVEY YEAR ===\n")
by.yr <- do.call(rbind, lapply(split(plot.sum, plot.sum$Year), function(d) data.frame(
  Year = d$Year[1], n_plots = nrow(d), stems = sum(d$stems_live),
  BA_live = round(mean(d$BA_live), 1),
  prop_BA_oak = round(mean(d$prop_BA_oak, na.rm = TRUE), 3),
  prop_poor = round(weighted.mean(d$prop_poor, d$n_oak_vigor, na.rm = TRUE), 3),
  stringsAsFactors = FALSE)))
print(by.yr, row.names = FALSE)

# ---------------------------------------------------------------------------------------------- #
# Species-level composition, for the composition figure and table
# ---------------------------------------------------------------------------------------------- #
lookup <- read.csv(file.path(path.proc, "species_lookup.csv"), stringsAsFactors = FALSE)
live <- tree[!tree$IsSnag, ]

# Species basal area is per hectare of the whole sample, so the divisor is the total area surveyed
# across all plot-years -- not one plot's area.
area.total.ha <- nrow(plot.sum) * PLOT.AREA.HA

spp <- do.call(rbind, lapply(split(live, live$SppCode), function(d) data.frame(
  SppCode = d$SppCode[1], IsOak = d$IsOak[1], n_stems = nrow(d),
  BA_m2ha = sum(ba.stem.m2(d$DBH), na.rm = TRUE) / area.total.ha,
  n_plots = length(unique(paste(d$PlotID, d$Year))),
  DBH_mean = round(mean(d$DBH, na.rm = TRUE), 1),
  stringsAsFactors = FALSE)))
spp$SppName <- lookup$SppName[match(spp$SppCode, lookup$Code4)]
spp$pct_BA <- round(100 * spp$BA_m2ha / sum(spp$BA_m2ha), 2)
spp$pct_stems <- round(100 * spp$n_stems / sum(spp$n_stems), 2)
spp <- spp[order(-spp$BA_m2ha), c("SppCode", "SppName", "IsOak", "n_stems", "pct_stems",
                                  "BA_m2ha", "pct_BA", "n_plots", "DBH_mean")]
write.csv(spp, file.path(path.proc, "summary_stats_species.csv"), row.names = FALSE)

cat("\n=== SPECIES COMPOSITION (live stems, all years pooled) ===\n")
print(head(within(spp, BA_m2ha <- round(BA_m2ha, 2)), 15), row.names = FALSE)
