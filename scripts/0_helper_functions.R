# Purpose: Shared constants, plotting theme, and palettes sourced by every analysis script so that
#          all figures in the report read as one consistent set.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)

library(ggplot2)

# ---------------------------------------------------------------------------------------------- #
# Plot design constants (from the Tree Survey sheet README)
# ---------------------------------------------------------------------------------------------- #
PLOT.AREA.HA <- 0.025   # hectares
PLOT.RADIUS.M <- 8.92   # meters
DBH.CUTOFF.CM <- 10     # cm; stems below this should not have been recorded

# Oak identification is rule-based rather than a hand-maintained list: any code beginning "QU" is a
# Quercus in this flora (no other genus here starts with Qu), plus QUERCUS_UNK for stems recorded as
# oak but not resolved to species (e.g. the "QUVE or QUPA" entry). A fixed list silently dropped
# Quercus ellipsoidalis (QUEL, 17 stems in 2018) the first time this ran.
is.oak <- function(code) !is.na(code) & (grepl("^QU", code) | code == "QUERCUS_UNK")

# Oak codes actually present across the datasets, for reference / labelling
OAK.CODES <- c("QUAL", "QURU", "QUMA", "QUBI", "QUAM", "QUVE", "QUPA", "QUEL", "QUERCUS_UNK")

# Non-native / invasive woody species that show up in the seedling layer. Tracked separately in the
# seedling composition figure because the invasive share is directly management-relevant.
INVASIVE.CODES <- c("LOSP", "RHCA", "ROMU", "EUSP", "ACPL", "RHTY")

# ---------------------------------------------------------------------------------------------- #
# Basal area
# ---------------------------------------------------------------------------------------------- #
# Basal area of a single stem in m^2, from DBH in cm.
# NOTE: the older QAQC script used DBH^2 * 0.005454, which is the imperial ft^2/acre constant
# applied to centimeters. That is proportional to true BA so relative comparisons held, but the
# absolute values were not m^2/ha. This is the metric form.
ba.stem.m2 <- function(dbh.cm) pi * (dbh.cm / 200)^2

# Stand basal area in m^2/ha from a vector of stem DBHs in a single plot
ba.plot.m2ha <- function(dbh.cm, area.ha = PLOT.AREA.HA) {
  sum(ba.stem.m2(dbh.cm), na.rm = TRUE) / area.ha
}

# Quadratic mean diameter (cm) -- the DBH of the tree of average basal area
qmd.cm <- function(dbh.cm) sqrt(mean(dbh.cm^2, na.rm = TRUE))

# ---------------------------------------------------------------------------------------------- #
# Theme & palettes
# ---------------------------------------------------------------------------------------------- #
# Colors below are the validated reference palette from the data-viz design system, used at its
# documented values and in its documented slot order rather than hand-picked. The slot ORDER is the
# colorblind-safety mechanism, not decoration: the 8 categorical slots clear every gate for adjacent
# marks (stacked bars, grouped bars, lines), and the first THREE slots additionally clear all-pairs,
# which is what scatter plots and maps need. So: never exceed 3 series on a scatter/map, and fold the
# tail into "Other" past ~6 on a stacked bar.

# Chart chrome & ink (light surface)
INK <- c(surface = "#fcfcfb", primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
         grid = "#e1e0d9", axis = "#c3c2b7")

theme_ew <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.background = element_rect(fill = INK[["surface"]], color = NA),
      panel.background = element_rect(fill = INK[["surface"]], color = NA),
      panel.border = element_rect(color = INK[["axis"]], fill = NA, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = INK[["grid"]], linewidth = 0.3),
      strip.background = element_rect(fill = "#f0efec", color = NA),
      strip.text = element_text(face = "bold", size = rel(0.9), color = INK[["secondary"]],
                                margin = margin(4, 4, 4, 4)),
      plot.title = element_text(face = "bold", hjust = 0, size = rel(1.15),
                                color = INK[["primary"]], margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0, color = INK[["secondary"]], size = rel(0.9),
                                   margin = margin(b = 8)),
      plot.caption = element_text(hjust = 0, color = INK[["muted"]], size = rel(0.75),
                                  margin = margin(t = 8)),
      legend.key = element_blank(),
      legend.background = element_blank(),
      # Text wears text ink, never the series color
      legend.text = element_text(color = INK[["secondary"]]),
      legend.title = element_text(color = INK[["secondary"]], size = rel(0.9)),
      axis.text = element_text(color = INK[["muted"]]),
      axis.title = element_text(size = rel(0.95), color = INK[["secondary"]]),
      axis.ticks = element_line(color = INK[["axis"]], linewidth = 0.3),
      plot.margin = margin(10, 14, 10, 10)
    )
}

# Maps: equal aspect, no axis furniture -- lat/lon gridlines carry no meaning at this extent
theme_ew_map <- function(base_size = 12) {
  theme_ew(base_size) %+replace%
    theme(
      panel.grid.major = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
}

# Categorical slots, in the order that passes the CVD gates. Assign in order, never cycled.
PAL.CAT <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
             "#e87ba4", "#008300", "#4a3aa7", "#e34948")

# Sequential: one hue (blue), light -> dark = low -> high magnitude.
SEQ.BLUE <- c("100" = "#cde2fb", "250" = "#86b6ef", "400" = "#3987e5", "450" = "#2a78d6",
              "550" = "#1c5cab", "650" = "#104281", "700" = "#0d366b")

# Vigor 1-3 is an ordered severity scale, so it takes an ORDINAL sequential ramp (darker = worse),
# not three separate hues -- a green/amber/red ramp would be a rainbow, and the amber/red would also
# impersonate reserved status colors. Ordinal ramps start no lighter than step 250 on a light
# surface so the lightest class still separates from the background.
PAL.VIGOR <- c("1" = SEQ.BLUE[["250"]], "2" = SEQ.BLUE[["450"]], "3" = SEQ.BLUE[["650"]])

# Seedling composition groups. Oak, Other native and Invasive take the first three slots (the
# all-pairs-validated set); sugar maple takes slot 4, and Unknown is deliberately muted ink rather
# than a 5th series hue. In the stack order below, slot 4 (yellow) never lands beside slot 2
# (orange), which is the one adjacent pair the documented palette warns about.
PAL.SEEDGRP <- c("Oak" = PAL.CAT[1], "Sugar maple (ACSA)" = PAL.CAT[4],
                 "Other native" = PAL.CAT[3], "Invasive" = PAL.CAT[2],
                 "Unknown" = INK[["muted"]])

# Before -> after: one hue, two shades (dumbbell ends)
PAL.ERA <- c("2018" = SEQ.BLUE[["250"]], "Resurvey" = SEQ.BLUE[["550"]])

# Sequential (magnitude) and diverging (change, centered at zero) scales
scale_fill_ew_seq <- function(...) {
  scale_fill_gradientn(colours = unname(SEQ.BLUE[c("100", "250", "400", "550", "700")]), ...)
}
scale_color_ew_seq <- function(...) {
  scale_color_gradientn(colours = unname(SEQ.BLUE[c("100", "250", "400", "550", "700")]), ...)
}
# Diverging: blue <-> red poles with a neutral GRAY midpoint (never a hue at the middle)
DIV.COLS <- c("#0d366b", "#2a78d6", "#f0efec", "#e34948", "#8f1f1f")
scale_color_ew_div <- function(...) {
  scale_color_gradientn(colours = DIV.COLS, ...)
}

# Save a figure at a consistent size/resolution
save.fig <- function(plot, name, width = 9, height = 6, dpi = 200) {
  ggsave(file.path(path.fig, paste0(name, ".png")), plot, width = width, height = height,
         dpi = dpi, bg = INK[["surface"]])
  invisible(file.path(path.fig, paste0(name, ".png")))
}

# ---------------------------------------------------------------------------------------------- #
# Output paths
# ---------------------------------------------------------------------------------------------- #
path.raw <- file.path("data", "raw_gsheet")
path.proc <- file.path("data", "processed")
path.fig <- "figures"

for (p in c(path.proc, path.fig)) dir.create(p, showWarnings = FALSE, recursive = TRUE)

# Read the most recent cached snapshot of a given tab
read.cached <- function(name, path = path.raw) {
  ff <- list.files(path, pattern = paste0("^", name, "_\\d{4}-\\d{2}-\\d{2}\\.csv$"), full.names = TRUE)
  if (length(ff) == 0) stop("No cached file for '", name, "' in ", path, ". Run 0_data_download.R first.")
  f <- sort(ff, decreasing = TRUE)[1]  # most recent date stamp
  read.csv(f, colClasses = "character", check.names = FALSE, na.strings = c("", "NA"))
}
