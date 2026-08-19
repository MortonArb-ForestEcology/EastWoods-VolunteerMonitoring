# Purpose: Figures for basal area, stand structure, and oak dominance.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Inputs:  <out>/data_processed/plot_summary.csv, tree_survey_clean.csv, summary_stats_species.csv
# Outputs: <out>/figures/fig01..fig08*.png

source("scripts/0_helper_functions.R")
library(scales)

ps <- read.csv(file.path(path.proc, "plot_summary.csv"), stringsAsFactors = FALSE)
tree <- read.csv(file.path(path.proc, "tree_survey_clean.csv"), stringsAsFactors = FALSE)
spp <- read.csv(file.path(path.proc, "summary_stats_species.csv"), stringsAsFactors = FALSE)
lookup <- read.csv(file.path(path.proc, "species_lookup.csv"), stringsAsFactors = FALSE)

n.plots <- nrow(ps)
# Captions are hand-wrapped: ggplot does not wrap them, and an unwrapped caption runs off the canvas
cap.base <- paste0(n.plots, " plots (0.025 ha each), surveyed 2022/2025/2026.\n",
                   "Basal area from live stems >= 10 cm DBH.")

# Units ordered by median live basal area, so the reader can scan a ranking
ps$MgmtUnitBA <- reorder(ps$MgmtUnit, ps$BA_live, median, na.rm = TRUE)
ps$MgmtUnitOak <- reorder(ps$MgmtUnit, ps$prop_BA_oak, function(x) mean(x, na.rm = TRUE))

# ---------------------------------------------------------------------------------------------- #
# 1. Distribution of plot basal area
# ---------------------------------------------------------------------------------------------- #
# One series, so no legend -- the title names it. Snags were originally a second facet, but 159 of
# 164 plots have essentially no snag basal area, so that panel was a single spike at zero: the two
# numbers in the subtitle carry it better than a chart of one bar.
med.live <- median(ps$BA_live)
snag.share <- sum(ps$BA_snag) / sum(ps$BA_total)
snag.none <- mean(ps$BA_snag == 0)

f1 <- ggplot(ps, aes(x = BA_live)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = PAL.CAT[1],
                 color = INK[["surface"]], linewidth = 0.5) +
  geom_vline(xintercept = med.live, color = INK[["primary"]], linetype = "22", linewidth = 0.5) +
  annotate("text", x = med.live + 1.5, y = Inf, label = paste0("median ", round(med.live, 1)),
           hjust = 0, vjust = 1.8, size = 3.4, color = INK[["secondary"]]) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Live basal area spans nearly the full range a closed forest can hold",
       subtitle = sprintf(paste("Median %.1f m2/ha across %d plots. Standing dead wood adds only %.1f%%",
                                "of total basal area,\nand is absent entirely from %.0f%% of plots."),
                          med.live, n.plots, 100 * snag.share, 100 * snag.none),
       x = expression("Live basal area (m"^2*" ha"^-1*")"), y = "Plots",
       caption = cap.base) +
  theme_ew()
save.fig(f1, "fig01_basalarea_distribution", width = 8, height = 5)

# ---------------------------------------------------------------------------------------------- #
# 2. Map of live basal area
# ---------------------------------------------------------------------------------------------- #
# Sequential single hue: magnitude. Size doubles the encoding so the map survives grayscale/CVD.
f2 <- ggplot(ps, aes(x = Longitude, y = Latitude)) +
  coord_equal() +
  geom_point(aes(fill = BA_live, size = BA_live), shape = 21,
             color = INK[["surface"]], stroke = 0.6) +
  scale_fill_ew_seq(name = expression("Live BA (m"^2*" ha"^-1*")")) +
  scale_size_continuous(range = c(1.6, 6), guide = "none") +
  labs(title = "Basal area is high across the East Woods, with no strong spatial gradient",
       subtitle = "Each point is one survey plot; darker and larger means more basal area",
       caption = cap.base) +
  theme_ew_map()
save.fig(f2, "fig02_basalarea_map", width = 8, height = 6.5)

# ---------------------------------------------------------------------------------------------- #
# 3. Basal area by monitoring unit
# ---------------------------------------------------------------------------------------------- #
# Boxplot + points: with 2-33 plots per unit, showing the raw points keeps thin units honest.
# The plot count goes into the axis label rather than floating inside the panel.
n.by.unit <- table(ps$MgmtUnitBA)
lab.unit <- paste0(names(n.by.unit), "  (n=", as.integer(n.by.unit), ")")
names(lab.unit) <- names(n.by.unit)

f3 <- ggplot(ps, aes(x = BA_live, y = MgmtUnitBA)) +
  geom_boxplot(outlier.shape = NA, fill = "#f0efec", color = INK[["axis"]], linewidth = 0.4,
               width = 0.65) +
  geom_jitter(height = 0.14, width = 0, size = 1.5, shape = 21, fill = PAL.CAT[1],
              color = INK[["surface"]], stroke = 0.35, alpha = 0.9) +
  scale_y_discrete(labels = lab.unit) +
  scale_x_continuous(breaks = seq(0, 100, 20), expand = expansion(mult = c(0.02, 0.03))) +
  labs(title = "Basal area differs among management units, but the spread within them is larger",
       subtitle = "Units ordered by median live basal area; each point is one plot",
       x = expression("Live basal area (m"^2*" ha"^-1*")"), y = NULL,
       caption = paste0(cap.base, "\nUnits with few plots are poorly estimated -- read them with care.")) +
  theme_ew()
save.fig(f3, "fig03_basalarea_by_unit", width = 9, height = 7)

# ---------------------------------------------------------------------------------------------- #
# 4. Species share of basal area
# ---------------------------------------------------------------------------------------------- #
# Fold the tail into "Other" rather than adding hues past the token ceiling
top.n <- 5
spp <- spp[order(-spp$BA_m2ha), ]
keep <- spp$SppCode[seq_len(min(top.n, nrow(spp)))]
tree$SppGroup <- ifelse(tree$SppCode %in% keep,
                        lookup$SppName[match(tree$SppCode, lookup$Code4)], "All other species")
grp.levels <- c(lookup$SppName[match(keep, lookup$Code4)], "All other species")
tree$SppGroup <- factor(tree$SppGroup, levels = grp.levels)

live <- tree[!tree$IsSnag, ]
live$BA <- ba.stem.m2(live$DBH)

comp <- aggregate(BA ~ MgmtUnit + SppGroup, data = live, FUN = sum, na.rm = TRUE, drop = FALSE)
comp$BA[is.na(comp$BA)] <- 0
comp$share <- comp$BA / ave(comp$BA, comp$MgmtUnit, FUN = sum)
unit.oak <- tapply(ps$prop_BA_oak, ps$MgmtUnit, mean, na.rm = TRUE)
comp$MgmtUnit <- factor(comp$MgmtUnit, levels = names(sort(unit.oak)))

# Direct-label the oak share, which is the number the figure exists to convey
oak.lab <- data.frame(MgmtUnit = names(unit.oak), share = unit.oak)
oak.lab$MgmtUnit <- factor(oak.lab$MgmtUnit, levels = levels(comp$MgmtUnit))

# Total oak share is direct-labelled at the right; the label column is created by extending the
# x scale past 100% rather than drawing outside the panel, so nothing is clipped.
f4 <- ggplot(comp, aes(x = share, y = MgmtUnit, fill = SppGroup)) +
  geom_col(width = 0.72, color = INK[["surface"]], linewidth = 0.5) +
  geom_text(data = oak.lab, aes(x = 1.04, y = MgmtUnit, label = percent(share, accuracy = 1)),
            inherit.aes = FALSE, hjust = 0, size = 3.1, color = INK[["secondary"]]) +
  scale_fill_manual(values = setNames(c(PAL.CAT[seq_along(keep)], INK[["muted"]]), grp.levels),
                    name = NULL) +
  scale_x_continuous(labels = function(x) ifelse(x > 1, "", percent(x, accuracy = 1)),
                     limits = c(0, 1.22), breaks = c(seq(0, 1, 0.25), 1.08),
                     sec.axis = dup_axis(breaks = 1.08, labels = "total oak", name = NULL),
                     expand = c(0, 0)) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE)) +
  labs(title = "White oak carries most of the basal area in the most oak-rich units",
       subtitle = "Share of live basal area by species; units ordered by total oak share",
       x = "Share of live basal area", y = NULL,
       caption = paste0(cap.base,
                        "\nACSA pools Acer saccharum and A. saccharinum: the 4-letter code cannot separate them.")) +
  theme_ew() +
  theme(legend.position = "top",
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_text(color = INK[["secondary"]], size = rel(0.85), hjust = 0.35))
save.fig(f4, "fig04_species_composition", width = 10, height = 7.5)

# ---------------------------------------------------------------------------------------------- #
# 5. Stem size distribution, oak vs other
# ---------------------------------------------------------------------------------------------- #
live$Group <- ifelse(live$IsOak, "Oak", "All other species")
live$Group <- factor(live$Group, levels = c("Oak", "All other species"))
med.dbh <- tapply(live$DBH, live$Group, median, na.rm = TRUE)

f5 <- ggplot(live[!is.na(live$DBH), ], aes(x = DBH, fill = Group)) +
  geom_histogram(binwidth = 5, boundary = 10, color = INK[["surface"]], linewidth = 0.35) +
  geom_vline(data = data.frame(Group = names(med.dbh), m = as.numeric(med.dbh)),
             aes(xintercept = m), color = INK[["primary"]], linetype = "22", linewidth = 0.5) +
  geom_text(data = data.frame(Group = names(med.dbh), m = as.numeric(med.dbh)),
            aes(x = m, y = Inf, label = paste0("  median ", round(m, 1), " cm")),
            hjust = 0, vjust = 1.8, size = 3.3, color = INK[["secondary"]], inherit.aes = FALSE) +
  facet_wrap(~Group, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Oak" = PAL.CAT[1], "All other species" = PAL.CAT[2]),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Oaks are the large stems; everything else fills the small size classes",
       subtitle = "This is why oak share of basal area is roughly double oak share of stems",
       x = "DBH (cm)", y = "Stems",
       caption = paste0(cap.base, " Vertical lines are medians.")) +
  theme_ew()
save.fig(f5, "fig05_dbh_distribution", width = 8, height = 6)

# ---------------------------------------------------------------------------------------------- #
# 6. Oak share: basal area vs stems
# ---------------------------------------------------------------------------------------------- #
oak.long <- rbind(
  data.frame(PlotID = ps$PlotID, measure = "Share of basal area", value = ps$prop_BA_oak),
  data.frame(PlotID = ps$PlotID, measure = "Share of stems", value = ps$prop_stem_oak))
oak.long <- oak.long[!is.na(oak.long$value), ]
oak.long$measure <- factor(oak.long$measure, levels = c("Share of basal area", "Share of stems"))
mn <- tapply(oak.long$value, oak.long$measure, mean)

f6 <- ggplot(oak.long, aes(x = value, fill = measure)) +
  geom_histogram(binwidth = 0.1, boundary = 0, color = INK[["surface"]], linewidth = 0.4) +
  geom_vline(data = data.frame(measure = names(mn), m = as.numeric(mn)),
             aes(xintercept = m), color = INK[["primary"]], linetype = "22", linewidth = 0.5) +
  geom_text(data = data.frame(measure = names(mn), m = as.numeric(mn)),
            aes(x = m, y = Inf, label = paste0("  mean ", percent(m, accuracy = 1))),
            hjust = 0, vjust = 1.8, size = 3.3, color = INK[["secondary"]], inherit.aes = FALSE) +
  facet_wrap(~measure, ncol = 1) +
  scale_fill_manual(values = c("Share of basal area" = PAL.CAT[1], "Share of stems" = PAL.CAT[2]),
                    guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "How oak-dominated a plot looks depends on how you measure it",
       subtitle = "The same plots, scored by basal area and by stem count",
       x = "Oak share of plot", y = "Plots",
       caption = paste0(cap.base, " Plots with no live trees are excluded (share undefined).")) +
  theme_ew()
save.fig(f6, "fig06_oak_share_distribution", width = 8, height = 6)

# ---------------------------------------------------------------------------------------------- #
# 7. Map of oak share of basal area
# ---------------------------------------------------------------------------------------------- #
# A single encoding only. An earlier version also mapped size to absolute oak basal area, but two
# different oak measures on one map fought each other and shrank the low-share plots to invisibility.
# Points get a surface-colored ring so overlapping marks stay separable.
f7 <- ggplot(ps[!is.na(ps$prop_BA_oak), ], aes(x = Longitude, y = Latitude)) +
  coord_equal() +
  geom_point(aes(fill = prop_BA_oak), shape = 21, size = 3.4,
             color = INK[["surface"]], stroke = 0.7) +
  scale_fill_ew_seq(name = "Oak share of\nbasal area", labels = percent_format(accuracy = 1),
                    limits = c(0, 1)) +
  labs(title = "Oak dominance is patchy rather than zonal",
       subtitle = "Darker plots have a greater share of their basal area in oak",
       caption = cap.base) +
  theme_ew_map()
save.fig(f7, "fig07_oak_share_map", width = 8, height = 6.5)

# ---------------------------------------------------------------------------------------------- #
# 8. Oak share of basal area vs oak share of stems
# ---------------------------------------------------------------------------------------------- #
sc <- ps[!is.na(ps$prop_BA_oak) & !is.na(ps$prop_stem_oak), ]
f8 <- ggplot(sc, aes(x = prop_stem_oak, y = prop_BA_oak)) +
  geom_abline(slope = 1, intercept = 0, color = INK[["axis"]], linetype = "22", linewidth = 0.5) +
  annotate("text", x = 0.86, y = 0.80, label = "1:1", size = 3.2, color = INK[["muted"]],
           angle = 38) +
  geom_point(size = 2.2, shape = 21, fill = PAL.CAT[1], color = INK[["surface"]], stroke = 0.5,
             alpha = 0.85) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Nearly every plot sits above the 1:1 line",
       subtitle = paste0("Oak share of basal area exceeds oak share of stems in ",
                         round(100 * mean(sc$prop_BA_oak > sc$prop_stem_oak)), "% of plots"),
       x = "Oak share of live stems", y = "Oak share of live basal area",
       caption = paste0(cap.base,
                        "\nPoints above the line mean the plot's oaks are larger than its other trees.")) +
  theme_ew()
save.fig(f8, "fig08_oak_ba_vs_stems", width = 7, height = 6.5)

cat("Wrote 8 basal area / composition figures to", path.fig, "\n")
