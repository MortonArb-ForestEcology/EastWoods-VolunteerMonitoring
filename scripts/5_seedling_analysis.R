# Purpose: Oak seedling abundance and oak share of the regeneration layer across 2023, 2024, 2025.
#          Because a different subset of plots was visited each year, year-to-year differences are
#          reported both at the landscape scale and on the plots that genuinely repeat.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Inputs:  <out>/data_processed/seedlings_clean.csv
# Outputs: <out>/figures/fig14..fig18*.png, <out>/data_processed/seedling_plot_summary.csv,
#          <out>/data_processed/summary_stats_seedlings.csv, model_summaries_seedlings.csv

source("scripts/0_helper_functions.R")
suppressPackageStartupMessages({library(scales); library(MASS)})

seed <- read.csv(file.path(path.proc, "seedlings_clean.csv"), stringsAsFactors = FALSE)

models <- data.frame()
add.model <- function(name, term, estimate, se = NA, stat = NA, p = NA, note = "") {
  models <<- rbind(models, data.frame(model = name, term = term, estimate = estimate, se = se,
                                      statistic = stat, p_value = p, note = note,
                                      stringsAsFactors = FALSE))
}

# ---------------------------------------------------------------------------------------------- #
# Per plot-year totals
# ---------------------------------------------------------------------------------------------- #
# Class 1 (true seedlings, <1.37 m) is the only size class recorded consistently in all three years,
# so it carries the primary comparison. All-classes totals are computed alongside it.
seed$total_c1 <- seed$C1
seed$total_all <- seed$C1 + seed$C2 + seed$C3
seed$PlotYear <- paste(seed$PlotID, seed$SurveyYear, sep = "_")

sm <- do.call(rbind, lapply(split(seed, seed$PlotYear), function(d) data.frame(
  PlotID = d$PlotID[1], SurveyYear = d$SurveyYear[1], MgmtUnit = d$MgmtUnit[1],
  Longitude = d$Longitude[1], Latitude = d$Latitude[1], Date = min(d$Date),
  # An EMPTY record means the plot was searched and nothing was found: a real zero
  searched_empty = all(d$IsEmpty),
  oak_c1 = sum(d$C1[d$IsOak]), total_c1 = sum(d$C1),
  oak_all = sum(d$total_all[d$IsOak]), total_all = sum(d$total_all),
  inv_c1 = sum(d$C1[d$IsInvasive]),
  n_spp = length(unique(d$SppCode[!d$IsEmpty])),
  stringsAsFactors = FALSE)))
sm$prop_oak_c1 <- ifelse(sm$total_c1 > 0, sm$oak_c1 / sm$total_c1, NA)
sm$prop_oak_all <- ifelse(sm$total_all > 0, sm$oak_all / sm$total_all, NA)
sm$has_oak <- sm$oak_all > 0
rownames(sm) <- NULL
write.csv(sm, file.path(path.proc, "seedling_plot_summary.csv"), row.names = FALSE)

cat("=== SEEDLING SURVEY EFFORT ===\n")
eff <- do.call(rbind, lapply(split(sm, sm$SurveyYear), function(d) data.frame(
  Year = d$SurveyYear[1], plots = nrow(d),
  window = paste(format(range(as.Date(d$Date)), "%b %d"), collapse = " - "),
  plots_empty = sum(d$searched_empty), stringsAsFactors = FALSE)))
print(eff, row.names = FALSE)

# ---------------------------------------------------------------------------------------------- #
# The design problem, stated up front
# ---------------------------------------------------------------------------------------------- #
pl <- split(sm$PlotID, sm$SurveyYear)
cat("\n=== PLOT OVERLAP BETWEEN YEARS ===\n")
ov <- expand.grid(a = names(pl), b = names(pl), stringsAsFactors = FALSE)
ov <- ov[ov$a < ov$b, ]
ov$shared <- mapply(function(a, b) length(intersect(pl[[a]], pl[[b]])), ov$a, ov$b)
print(ov, row.names = FALSE)
cat("plots surveyed in all three years:", length(Reduce(intersect, pl)), "\n")
cat("NOTE: with no plots common to all years -- and none at all shared between 2024 and 2025 --\n",
    "      a raw year-to-year comparison confounds year with location.\n", sep = "")

# ---------------------------------------------------------------------------------------------- #
# Landscape-scale summaries
# ---------------------------------------------------------------------------------------------- #
ci.mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(mean(x), NA, NA))
  se <- sd(x) / sqrt(length(x))
  c(mean(x), mean(x) - 1.96 * se, mean(x) + 1.96 * se)
}

yr <- do.call(rbind, lapply(split(sm, sm$SurveyYear), function(d) {
  cm <- ci.mean(d$oak_c1)
  bt <- binom.test(sum(d$oak_c1), sum(d$total_c1))
  data.frame(
    Year = d$SurveyYear[1], plots = nrow(d),
    oak_c1_total = sum(d$oak_c1), all_c1_total = sum(d$total_c1),
    oak_all_total = sum(d$oak_all), all_all_total = sum(d$total_all),
    oak_per_plot = cm[1], oak_per_plot_lo = cm[2], oak_per_plot_hi = cm[3],
    prop_oak_pooled = sum(d$oak_c1) / sum(d$total_c1),
    prop_oak_lo = bt$conf.int[1], prop_oak_hi = bt$conf.int[2],
    pct_plots_with_oak = mean(d$has_oak),
    stringsAsFactors = FALSE)
}))
cat("\n=== OAK SEEDLINGS BY YEAR (Class 1) ===\n")
print(within(yr, {oak_per_plot <- round(oak_per_plot, 2); oak_per_plot_lo <- round(oak_per_plot_lo, 2)
                  oak_per_plot_hi <- round(oak_per_plot_hi, 2)
                  prop_oak_pooled <- round(prop_oak_pooled, 4)
                  prop_oak_lo <- round(prop_oak_lo, 4); prop_oak_hi <- round(prop_oak_hi, 4)
                  pct_plots_with_oak <- round(pct_plots_with_oak, 3)}), row.names = FALSE)
write.csv(yr, file.path(path.proc, "summary_stats_seedlings.csv"), row.names = FALSE)

for (i in seq_len(nrow(yr))) {
  add.model("Oak seedlings by year (descriptive)", paste0(yr$Year[i], ": total oak Class 1"),
            yr$oak_c1_total[i], note = paste0(yr$plots[i], " plots; ", yr$all_c1_total[i],
                                              " seedlings of all species"))
  add.model("Oak seedlings by year (descriptive)", paste0(yr$Year[i], ": oak share of Class 1"),
            yr$prop_oak_pooled[i],
            note = paste0("95% CI ", round(yr$prop_oak_lo[i], 4), "-", round(yr$prop_oak_hi[i], 4)))
  add.model("Oak seedlings by year (descriptive)", paste0(yr$Year[i], ": plots with any oak"),
            yr$pct_plots_with_oak[i], note = paste0(sum(sm$has_oak[sm$SurveyYear == yr$Year[i]]),
                                                    " of ", yr$plots[i], " plots"))
}

# ---------------------------------------------------------------------------------------------- #
# Models
# ---------------------------------------------------------------------------------------------- #
sm$YearF <- factor(sm$SurveyYear)
sm$MgmtUnitF <- factor(sm$MgmtUnit)

# Counts are heavily overdispersed (many zeros, a few large plots), so negative binomial not Poisson.
# Monitoring unit is included because the plots differ among years -- without it, "year" would absorb
# the difference between which parts of the woods were visited.
m.cnt <- tryCatch(
  MASS::glm.nb(oak_c1 ~ YearF + MgmtUnitF, data = sm),
  error = function(e) {cat("\nglm.nb with unit failed (", conditionMessage(e),
                          "), refitting year only\n"); MASS::glm.nb(oak_c1 ~ YearF, data = sm)})
cat("\n=== Oak Class-1 count ~ year (+ unit), negative binomial ===\n")
cnt.cf <- summary(m.cnt)$coefficients
print(cnt.cf[grep("YearF|Intercept", rownames(cnt.cf)), ])
cat("theta =", round(m.cnt$theta, 3), " (strong overdispersion; Poisson would be wrong)\n")
for (rn in grep("YearF", rownames(cnt.cf), value = TRUE)) {
  add.model("Oak Class-1 count ~ year + unit (neg. binomial)", rn, cnt.cf[rn, 1],
            se = cnt.cf[rn, 2], stat = cnt.cf[rn, 3], p = cnt.cf[rn, 4],
            note = "log count ratio vs 2023; monitoring unit held constant")
}

# Oak share of the seedling layer, weighted by how many seedlings back each plot's proportion
sm.p <- sm[!is.na(sm$prop_oak_c1), ]
m.prop <- glm(prop_oak_c1 ~ YearF, data = sm.p, family = quasibinomial, weights = total_c1)
prop.cf <- summary(m.prop)$coefficients
cat("\n=== Oak share of Class 1 ~ year, quasibinomial ===\n"); print(prop.cf)
for (i in seq_len(nrow(prop.cf))) {
  add.model("Oak share of Class 1 ~ year (quasibinomial)", rownames(prop.cf)[i], prop.cf[i, 1],
            se = prop.cf[i, 2], stat = prop.cf[i, 3], p = prop.cf[i, 4],
            note = "log-odds vs 2023; weighted by total seedlings per plot")
}

# Occupancy: robust to the count inflation and the possible 2023 mast year
occ <- table(sm$SurveyYear, sm$has_oak)
occ.test <- suppressWarnings(chisq.test(occ))
cat("\n=== Plots with at least one oak seedling ===\n")
print(cbind(occ, prop = round(occ[, "TRUE"] / rowSums(occ), 3)))
cat("chi-squared =", round(occ.test$statistic, 3), " p =", signif(occ.test$p.value, 3), "\n")
add.model("Oak occupancy ~ year (chi-squared)", "plots with >=1 oak seedling", NA,
          stat = occ.test$statistic, p = occ.test$p.value,
          note = "occupancy is insensitive to count inflation and mast-year effects")

# ---------------------------------------------------------------------------------------------- #
# Paired subsets: the only within-plot comparisons available
# ---------------------------------------------------------------------------------------------- #
cat("\n=== PAIRED PLOTS ===\n")
paired.res <- data.frame()
for (k in seq_len(nrow(ov))) {
  a <- ov$a[k]; b <- ov$b[k]
  shared <- intersect(pl[[a]], pl[[b]])
  if (length(shared) < 5) {
    cat(a, "vs", b, ":", length(shared), "shared plots -- too few to test\n"); next
  }
  da <- sm[sm$SurveyYear == a & sm$PlotID %in% shared, ]
  db <- sm[sm$SurveyYear == b & sm$PlotID %in% shared, ]
  da <- da[order(da$PlotID), ]; db <- db[order(db$PlotID), ]
  wt <- wilcox.test(db$oak_c1, da$oak_c1, paired = TRUE)
  cat(sprintf("%s -> %s: n=%d plots, mean oak Class 1 %.2f -> %.2f, paired Wilcoxon p = %.3f\n",
              a, b, length(shared), mean(da$oak_c1), mean(db$oak_c1), wt$p.value))
  paired.res <- rbind(paired.res, data.frame(
    from = a, to = b, n_plots = length(shared),
    oak_from = mean(da$oak_c1), oak_to = mean(db$oak_c1),
    occ_from = mean(da$has_oak), occ_to = mean(db$has_oak),
    p = wt$p.value, stringsAsFactors = FALSE))
  add.model("Paired plots (Wilcoxon signed-rank)", paste0("oak Class 1: ", a, " -> ", b),
            mean(db$oak_c1) - mean(da$oak_c1), p = wt$p.value,
            note = paste0("n=", length(shared), " plots surveyed in both years"))
}
write.csv(paired.res, file.path(path.proc, "seedling_paired.csv"), row.names = FALSE)
write.csv(models, file.path(path.proc, "model_summaries_seedlings.csv"), row.names = FALSE)

# ============================================================================================== #
# FIGURES
# ============================================================================================== #
cap.seed <- paste("Oak seedling inventory, 2023-2025. Counts are per 0.025 ha plot: the protocol",
                  "does not state a\nsearch area for the regeneration layer, so these are not",
                  "densities per hectare.")
cap.rot <- paste("A different subset of plots was visited each year (no plot appears in all three,",
                 "and 2024 and 2025\nshare none), so year-to-year differences partly reflect which",
                 "parts of the woods were visited.")
# The survey window moved by two months between 2023 and 2024. First-year oak seedlings emerge in
# spring and can be browsed or drop their leaves by autumn, so a late-September survey may simply
# see fewer of them than a July one. This is a live alternative explanation for the 2023 low count.
cap.season <- paste("Survey windows also differ: 2023 was late Sep-Oct, 2024 was Jul-Aug, 2025 was",
                    "Aug-Oct. First-year\nseedlings are easier to miss late in the season, which",
                    "could contribute to the low 2023 oak count.")

# --- 14. Oak seedling abundance by year -------------------------------------------------------- #
# Two panels because the landscape total and the per-plot mean answer different questions, and the
# total alone would imply a precision the rotating design does not support.
tot <- data.frame(Year = factor(yr$Year), value = yr$oak_c1_total, panel = "Total oak seedlings counted")
per <- data.frame(Year = factor(yr$Year), value = yr$oak_per_plot, lo = yr$oak_per_plot_lo,
                  hi = yr$oak_per_plot_hi, panel = "Mean per plot (95% CI)")

f14a <- ggplot(tot, aes(x = Year, y = value)) +
  geom_col(width = 0.6, fill = PAL.CAT[1]) +
  geom_text(aes(label = value), vjust = -0.6, size = 3.6, color = INK[["secondary"]]) +
  geom_text(aes(y = 0, label = paste0(yr$plots, " plots")), vjust = 1.8, size = 3,
            color = INK[["muted"]]) +
  scale_y_continuous(expand = expansion(mult = c(0.06, 0.14))) +
  labs(title = "Total oak seedlings counted", x = NULL, y = "Oak seedlings (Class 1)") +
  theme_ew() + theme(plot.title = element_text(size = rel(1)))

f14b <- ggplot(per, aes(x = Year, y = value)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1, color = INK[["axis"]], linewidth = 0.6) +
  geom_point(size = 3.6, shape = 21, fill = PAL.CAT[1], color = INK[["surface"]], stroke = 0.7) +
  geom_text(aes(label = sprintf("%.2f", value)), hjust = -0.45, size = 3.4,
            color = INK[["secondary"]]) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.15))) +
  labs(title = "Mean per plot, with 95% interval", x = NULL, y = "Oak seedlings per plot") +
  theme_ew() + theme(plot.title = element_text(size = rel(1)))

f14 <- cowplot::plot_grid(f14a, f14b, nrow = 1, align = "h")
f14 <- cowplot::ggdraw() +
  cowplot::draw_label("Oak seedling numbers jumped more than tenfold after 2023",
                      x = 0.01, y = 0.965, hjust = 0, fontface = "bold", size = 14,
                      colour = INK[["primary"]]) +
  cowplot::draw_label(paste("The per-plot panel is the honest version of the same number:",
                            "a handful of plots carry most of the count"),
                      x = 0.01, y = 0.915, hjust = 0, size = 10.5, colour = INK[["secondary"]]) +
  cowplot::draw_label(paste(cap.seed, cap.rot, cap.season, sep = "\n"),
                      x = 0.01, y = 0.075, hjust = 0,
                      size = 7.5, colour = INK[["muted"]], lineheight = 1.25) +
  cowplot::draw_plot(f14, y = 0.16, height = 0.71)
ggsave(file.path(path.fig, "fig14_oak_seedling_abundance.png"), f14, width = 9, height = 6,
       dpi = 200, bg = INK[["surface"]])

# --- 15. Oak share of the seedling layer ------------------------------------------------------- #
sm$YearLab <- paste0(sm$SurveyYear, "\n", table(sm$SurveyYear)[as.character(sm$SurveyYear)], " plots")
pooled <- data.frame(YearLab = unique(sm$YearLab[order(sm$SurveyYear)]),
                     value = yr$prop_oak_pooled[order(yr$Year)])

f15 <- ggplot(sm[!is.na(sm$prop_oak_c1), ], aes(x = YearLab, y = prop_oak_c1)) +
  geom_boxplot(outlier.shape = NA, fill = "#f0efec", color = INK[["axis"]], linewidth = 0.4,
               width = 0.5) +
  geom_jitter(width = 0.14, height = 0, size = 1.8, shape = 21, fill = PAL.CAT[1],
              color = INK[["surface"]], stroke = 0.4, alpha = 0.85) +
  geom_point(data = pooled, aes(x = YearLab, y = value), shape = 23, size = 3.4,
             fill = PAL.CAT[2], color = INK[["surface"]], stroke = 0.7) +
  geom_text(data = pooled, aes(x = YearLab, y = value, label = percent(value, accuracy = 0.1)),
            hjust = -0.35, size = 3.2, color = INK[["secondary"]]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Oak went from a trace of the seedling layer in 2023 to nearly half of it",
       subtitle = paste("Each blue point is one plot; the orange diamond is the pooled share",
                        "across all seedlings counted that year"),
       x = NULL, y = "Oak share of Class 1 seedlings",
       caption = paste(cap.seed, cap.rot, sep = "\n")) +
  theme_ew()
save.fig(f15, "fig15_oak_seedling_share", width = 8, height = 5.5)

# --- 16. Composition of the regeneration layer ------------------------------------------------- #
# Sugar maple is split out rather than buried in "other native": it alone accounted for 988 of the
# 1,280 seedlings counted in 2023, so the swing in oak's SHARE is as much about maple falling as
# about oak rising. Pooling it would hide the mechanism.
seed$grp <- ifelse(seed$IsOak, "Oak",
                   ifelse(seed$SppCode == "ACSA", "Sugar maple (ACSA)",
                          ifelse(seed$IsInvasive, "Invasive",
                                 ifelse(seed$SppCode %in% c("UNK", "COSP"), "Unknown",
                                        "Other native"))))
seed$grp <- factor(seed$grp, levels = c("Oak", "Sugar maple (ACSA)", "Other native",
                                        "Invasive", "Unknown"))
cmp <- aggregate(C1 ~ SurveyYear + grp, data = seed[!seed$IsEmpty, ], FUN = sum, drop = FALSE)
cmp$C1[is.na(cmp$C1)] <- 0
cmp$share <- cmp$C1 / ave(cmp$C1, cmp$SurveyYear, FUN = sum)
cmp$YearLab <- paste0(cmp$SurveyYear, "\nn = ", ave(cmp$C1, cmp$SurveyYear, FUN = sum))

f16 <- ggplot(cmp, aes(x = YearLab, y = share, fill = grp)) +
  geom_col(width = 0.62, color = INK[["surface"]], linewidth = 0.6) +
  geom_text(aes(label = ifelse(share > 0.04, percent(share, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), size = 3.2, color = INK[["surface"]],
            fontface = "bold") +
  scale_fill_manual(values = PAL.SEEDGRP, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(title = "The 2023 seedling layer was almost all sugar maple; oak matched it afterwards",
       subtitle = paste("Composition of Class 1 seedlings. Oak's rising share reflects both more",
                        "oak and far less maple,\nnot oak alone -- the total count fell from 1,280",
                        "to 783 to 340."),
       x = NULL, y = "Share of Class 1 seedlings",
       caption = paste0(cap.seed, "\nInvasive group: Lonicera, Rhamnus, Rosa multiflora, Euonymus, ",
                        "Rhus typhina.\n", cap.season)) +
  theme_ew() + theme(legend.position = "top")
save.fig(f16, "fig16_seedling_composition", width = 8.5, height = 6)

# --- 17. Paired-plot change -------------------------------------------------------------------- #
if (nrow(paired.res) > 0) {
  pr <- paired.res
  pr$label <- paste0(pr$from, " to ", pr$to, "\n", pr$n_plots, " shared plots")
  pr.long <- rbind(
    data.frame(label = pr$label, era = pr$from, value = pr$oak_from),
    data.frame(label = pr$label, era = pr$to, value = pr$oak_to))

  f17 <- ggplot(pr.long, aes(x = value, y = label)) +
    geom_segment(data = pr, aes(x = oak_from, xend = oak_to, y = label, yend = label),
                 inherit.aes = FALSE, color = INK[["axis"]], linewidth = 1.1, lineend = "round") +
    geom_point(aes(fill = era), shape = 21, size = 3.8, color = INK[["surface"]], stroke = 0.7) +
    geom_text(data = pr, aes(x = pmax(oak_from, oak_to), y = label,
                             label = paste0("  p = ", signif(p, 2))),
              inherit.aes = FALSE, hjust = 0, size = 3.2, color = INK[["secondary"]]) +
    scale_fill_manual(values = setNames(PAL.CAT[1:3], sort(unique(pr.long$era))), name = NULL) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.03, 0.22))) +
    labs(title = "The increase also shows up on the plots that genuinely repeat",
         subtitle = paste("Mean oak Class-1 seedlings per plot, restricted to plots surveyed in",
                          "both years of each pair.\nSo the rise is not merely an artifact of",
                          "visiting different ground each year."),
         x = "Mean oak seedlings per plot", y = NULL,
         caption = paste("These are the only within-plot comparisons the data support.",
                         "2024 and 2025 share no plots,\nso that pair cannot be tested.",
                         "Paired Wilcoxon signed-rank tests.\n", cap.season)) +
    theme_ew() + theme(legend.position = "top")
  save.fig(f17, "fig17_seedling_paired_change", width = 9, height = 4.4)
}

# --- 18. Map of oak seedlings by year ---------------------------------------------------------- #
sm$oak_cat <- cut(sm$oak_all, breaks = c(-Inf, 0, 2, 10, Inf),
                  labels = c("none", "1-2", "3-10", "11+"))
f18 <- ggplot(sm, aes(x = Longitude, y = Latitude)) +
  coord_equal() +
  geom_point(aes(fill = oak_cat, size = oak_cat), shape = 21,
             color = INK[["surface"]], stroke = 0.5) +
  facet_wrap(~SurveyYear, nrow = 1) +
  scale_fill_manual(values = c("none" = "#f0efec", "1-2" = SEQ.BLUE[["250"]],
                               "3-10" = SEQ.BLUE[["450"]], "11+" = SEQ.BLUE[["700"]]),
                    name = "Oak seedlings\n& saplings") +
  scale_size_manual(values = c("none" = 1.4, "1-2" = 2.4, "3-10" = 3.2, "11+" = 4.2),
                    name = "Oak seedlings\n& saplings") +
  labs(title = "Each year covers a different part of the woods",
       subtitle = paste("Which is why the year-to-year comparison above needs the paired-plot",
                        "check, not just the totals"),
       caption = paste(cap.seed, cap.rot, sep = "\n")) +
  theme_ew_map()
save.fig(f18, "fig18_oak_seedling_map", width = 11, height = 4.8)

cat("\nWrote seedling figures 14-18 and", nrow(models), "model rows\n")
