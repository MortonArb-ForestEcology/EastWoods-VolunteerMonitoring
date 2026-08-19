# Purpose: Oak vigor across space, and change in oak vigor between the 2018 IES survey and the
#          current volunteer resurvey. Also quantifies the observer effect, which is large enough
#          here to govern how the temporal result can be read.
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Inputs:  data/processed/tree_survey_clean.csv, veg2018_clean.csv, plot_summary.csv
# Outputs: figures/fig09..fig13*.png, data/processed/model_summaries.csv,
#          data/processed/vigor_paired_2018.csv

source("scripts/0_helper_functions.R")
suppressPackageStartupMessages({library(lme4); library(emmeans); library(scales); library(MASS)})

tree <- read.csv(file.path(path.proc, "tree_survey_clean.csv"), stringsAsFactors = FALSE)
v18 <- read.csv(file.path(path.proc, "veg2018_clean.csv"), stringsAsFactors = FALSE)
ps <- read.csv(file.path(path.proc, "plot_summary.csv"), stringsAsFactors = FALSE)

models <- data.frame()
add.model <- function(name, term, estimate, se = NA, stat = NA, p = NA, note = "") {
  models <<- rbind(models, data.frame(model = name, term = term, estimate = estimate,
                                      se = se, statistic = stat, p_value = p, note = note,
                                      stringsAsFactors = FALSE))
}

# Live oaks carrying a vigor rating -- the analysis population throughout
oak <- tree[tree$IsOak & !tree$IsSnag & !is.na(tree$Vigor), ]
oak$poor <- as.integer(oak$Vigor >= 2)
oak$VigorF <- factor(oak$Vigor, levels = 1:3)
cat("live oak stems with a vigor rating:", nrow(oak), "in", length(unique(oak$PlotID)), "plots\n")

cap.vig <- paste0(nrow(oak), " live oak stems in ", length(unique(oak$PlotID)),
                  " plots. Vigor: 1 = <5% dieback, 2 = >=20% undamaged foliage,\n",
                  "3 = <20% leaves undamaged. Snags carry no vigor rating and are excluded.")

# ============================================================================================== #
# The observer problem, established first because it constrains every later interpretation
# ============================================================================================== #
# JG and TO surveyed only 2022; GA surveyed 2025 and 2026. No crew spans the 2022/2025 boundary, so
# the 2022-vs-later contrast is fully confounded with observer and no model can separate them. The
# 2025-vs-2026 contrast IS estimable, because the same crew rated both.
crew.yr <- table(oak$CrewLead, oak$Year)
cat("\nCrew lead x survey year (live oak stems):\n"); print(crew.yr)
yr.crews <- apply(crew.yr > 0, 2, function(x) paste(sort(rownames(crew.yr)[x]), collapse = "+"))
shared <- sapply(seq_along(yr.crews), function(i)
  any(sapply(strsplit(yr.crews[-i], "\\+"), function(z) any(z %in% strsplit(yr.crews[i], "\\+")[[1]]))))
cat("crews per year:", paste(names(yr.crews), yr.crews, sep = "=", collapse = ", "), "\n")
cat("years whose crew also rated another year (so year is separable from observer):",
    paste(names(yr.crews)[shared], collapse = ", "), "\n")
cat("years with no crew overlap (year and observer inseparable):",
    paste(names(yr.crews)[!shared], collapse = ", "), "\n")

crew.rate <- do.call(rbind, lapply(split(oak, oak$CrewLead), function(d) {
  bt <- binom.test(sum(d$poor), nrow(d))
  data.frame(CrewLead = d$CrewLead[1], Year = paste(sort(unique(d$Year)), collapse = "/"),
             n_stems = nrow(d), n_plots = length(unique(d$PlotID)),
             prop_poor = mean(d$poor), lo = bt$conf.int[1], hi = bt$conf.int[2],
             stringsAsFactors = FALSE)
}))
cat("\nProportion of oak stems rated poor (vigor >= 2), by crew lead:\n")
print(within(crew.rate, {prop_poor <- round(prop_poor, 3); lo <- round(lo, 3); hi <- round(hi, 3)}),
      row.names = FALSE)

# Test whether crews differ more than chance
crew.tab <- table(oak$CrewLead, oak$poor)
crew.test <- suppressWarnings(chisq.test(crew.tab))
add.model("Observer effect", "crew lead vs P(vigor >= 2)", NA, stat = crew.test$statistic,
          p = crew.test$p.value,
          note = paste0("chi-squared, df=", crew.test$parameter,
                        "; crew is perfectly nested in survey year, so this cannot be ",
                        "separated from a real year effect"))
for (i in seq_len(nrow(crew.rate))) {
  add.model("Observer effect", paste0("crew ", crew.rate$CrewLead[i], " (", crew.rate$Year[i], ")"),
            crew.rate$prop_poor[i], note = paste0("n=", crew.rate$n_stems[i], " stems; 95% CI ",
                                                  round(crew.rate$lo[i], 3), "-", round(crew.rate$hi[i], 3)))
}

# ============================================================================================== #
# Vigor across space
# ============================================================================================== #
oak$MgmtUnit <- factor(oak$MgmtUnit)
oak$CrewLead <- factor(oak$CrewLead)

# Monitoring unit enters as a random effect rather than 21 fixed levels: unit sample sizes run from
# 1 to 53 oak stems, and partial pooling gives the thin units shrunken estimates instead of wild
# ones. Crew lead stays fixed so unit differences are estimated within observer.
m.space <- glmer(poor ~ scale(DBH) + CrewLead + (1 | MgmtUnit) + (1 | PlotID),
                 data = oak, family = binomial,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
cat("\n=== Spatial vigor model ===\n"); print(summary(m.space)$coefficients)

vc <- as.data.frame(VarCorr(m.space))
cat("\nvariance components:\n"); print(vc[, c("grp", "vcov", "sdcor")])

# A monitoring-unit variance of zero is a result, not a failure: once plot-to-plot variation and the
# crew difference are accounted for, there is no between-unit signal left to estimate. The unit
# differences visible in the raw proportions are not distinguishable from noise.
unit.var <- vc$vcov[vc$grp == "MgmtUnit"]
if (length(unit.var) && unit.var < 1e-8) {
  cat("\nNOTE: the monitoring-unit variance is estimated at zero (singular fit).\n",
      "     Between-unit differences in oak vigor are not separable from plot-level variation.\n",
      sep = "")
  add.model("Vigor ~ space (glmer, binomial)", "between-unit variance", 0,
            note = paste("estimated at the boundary: no detectable between-unit variation in oak",
                         "vigor once plot and crew are accounted for"))
}
for (i in seq_len(nrow(vc))) {
  add.model("Vigor ~ space (glmer, binomial)", paste0("var(", vc$grp[i], ")"), vc$vcov[i],
            note = paste0("SD = ", round(vc$sdcor[i], 3)))
}
cf <- summary(m.space)$coefficients
for (i in seq_len(nrow(cf))) {
  add.model("Vigor ~ space (glmer, binomial)", rownames(cf)[i], cf[i, 1], se = cf[i, 2],
            stat = cf[i, 3], p = cf[i, 4], note = "log-odds of vigor >= 2")
}

# Ordinal sensitivity check on the full 1-3 scale
m.ord <- MASS::polr(VigorF ~ scale(DBH) + CrewLead, data = oak, Hess = TRUE)
ord.cf <- coef(summary(m.ord))
cat("\n=== Ordinal (polr) sensitivity check ===\n"); print(ord.cf)
for (i in seq_len(nrow(ord.cf))) {
  add.model("Vigor ~ DBH + crew (ordinal polr)", rownames(ord.cf)[i], ord.cf[i, 1],
            se = ord.cf[i, 2], stat = ord.cf[i, 3],
            note = "proportional-odds; no plot random effect")
}

# Unit-level estimates (shrunken), for the figure and the table
re.unit <- ranef(m.space)$MgmtUnit
unit.est <- data.frame(MgmtUnit = rownames(re.unit), re = re.unit[, 1], stringsAsFactors = FALSE)
unit.raw <- do.call(rbind, lapply(split(oak, oak$MgmtUnit), function(d) data.frame(
  MgmtUnit = as.character(d$MgmtUnit[1]), n_stems = nrow(d), n_plots = length(unique(d$PlotID)),
  prop_poor = mean(d$poor), stringsAsFactors = FALSE)))
unit.est <- merge(unit.raw, unit.est, by = "MgmtUnit")
unit.est <- unit.est[order(-unit.est$prop_poor), ]
cat("\nOak vigor by unit (raw proportion poor, and model random effect):\n")
print(within(unit.est, {prop_poor <- round(prop_poor, 3); re <- round(re, 3)}), row.names = FALSE)
write.csv(unit.est, file.path(path.proc, "vigor_by_unit.csv"), row.names = FALSE)

# ============================================================================================== #
# Change since 2018
# ============================================================================================== #
tree$PlotJoin <- gsub("-", "", tree$PlotID)
v18$poor <- as.integer(v18$Vigor >= 2)
old <- v18[v18$IsOak & !v18$IsSnag & !is.na(v18$Vigor), ]
oak$PlotJoin <- gsub("-", "", oak$PlotID)

paired.plots <- intersect(unique(oak$PlotJoin), unique(old$PlotJoin))
cat("\n=== 2018 vs resurvey ===\n")
cat("plots with rated oaks in both eras:", length(paired.plots), "\n")

# Plot-level means in each era. 2018 has no tree tags, so this compares the plot's oak population
# then with its oak population now -- not the same individual stems tracked through time.
pair <- data.frame(PlotJoin = paired.plots, stringsAsFactors = FALSE)
agg <- function(d, key) {
  data.frame(PlotJoin = names(tapply(d$Vigor, d[[key]], mean)),
             vigor = as.numeric(tapply(d$Vigor, d[[key]], mean)),
             poor = as.numeric(tapply(d$poor, d[[key]], mean)),
             n = as.numeric(tapply(d$poor, d[[key]], length)), stringsAsFactors = FALSE)
}
a18 <- agg(old[old$PlotJoin %in% paired.plots, ], "PlotJoin")
anow <- agg(oak[oak$PlotJoin %in% paired.plots, ], "PlotJoin")
names(a18)[-1] <- paste0(names(a18)[-1], "_2018")
names(anow)[-1] <- paste0(names(anow)[-1], "_now")
pair <- merge(merge(pair, a18, by = "PlotJoin"), anow, by = "PlotJoin")

meta <- unique(oak[, c("PlotJoin", "PlotID", "MgmtUnit", "Year", "CrewLead", "Longitude", "Latitude")])
meta <- meta[!duplicated(meta$PlotJoin), ]
pair <- merge(pair, meta, by = "PlotJoin")
pair$d_vigor <- pair$vigor_now - pair$vigor_2018
pair$d_poor <- pair$poor_now - pair$poor_2018
write.csv(pair, file.path(path.proc, "vigor_paired_2018.csv"), row.names = FALSE)

w.vig <- wilcox.test(pair$vigor_now, pair$vigor_2018, paired = TRUE)
w.poor <- wilcox.test(pair$poor_now, pair$poor_2018, paired = TRUE)
cat(sprintf("mean plot oak vigor: 2018 = %.3f, resurvey = %.3f (change %+.3f)\n",
            mean(pair$vigor_2018), mean(pair$vigor_now), mean(pair$d_vigor)))
cat(sprintf("plot proportion poor: 2018 = %.3f, resurvey = %.3f (change %+.3f)\n",
            mean(pair$poor_2018), mean(pair$poor_now), mean(pair$d_poor)))
cat("paired Wilcoxon on mean vigor: V =", w.vig$statistic, " p =", signif(w.vig$p.value, 3), "\n")
cat("paired Wilcoxon on prop poor : V =", w.poor$statistic, " p =", signif(w.poor$p.value, 3), "\n")

add.model("2018 vs resurvey (paired Wilcoxon)", "plot mean oak vigor", mean(pair$d_vigor),
          stat = w.vig$statistic, p = w.vig$p.value,
          note = paste0("n=", nrow(pair), " paired plots; negative = improvement"))
add.model("2018 vs resurvey (paired Wilcoxon)", "plot proportion vigor >= 2", mean(pair$d_poor),
          stat = w.poor$statistic, p = w.poor$p.value,
          note = paste0("n=", nrow(pair), " paired plots; negative = improvement"))

# Stem-level model with a plot random effect, so plot identity is held constant across eras
stem <- rbind(
  data.frame(PlotJoin = old$PlotJoin[old$PlotJoin %in% paired.plots], era = "2018",
             poor = old$poor[old$PlotJoin %in% paired.plots],
             DBH = old$DBH[old$PlotJoin %in% paired.plots], stringsAsFactors = FALSE),
  data.frame(PlotJoin = oak$PlotJoin[oak$PlotJoin %in% paired.plots], era = "Resurvey",
             poor = oak$poor[oak$PlotJoin %in% paired.plots],
             DBH = oak$DBH[oak$PlotJoin %in% paired.plots], stringsAsFactors = FALSE))
stem$era <- factor(stem$era, levels = c("2018", "Resurvey"))
m.era <- glmer(poor ~ era + (1 | PlotJoin), data = stem, family = binomial,
               control = glmerControl(optimizer = "bobyqa"))
era.cf <- summary(m.era)$coefficients
cat("\n=== Stem-level era model ===\n"); print(era.cf)
for (i in seq_len(nrow(era.cf))) {
  add.model("2018 vs resurvey (glmer, binomial)", rownames(era.cf)[i], era.cf[i, 1],
            se = era.cf[i, 2], stat = era.cf[i, 3], p = era.cf[i, 4],
            note = "log-odds of vigor >= 2; plot random effect")
}

# Same comparison split by which crew did the resurvey. If the direction of change held across
# crews that would be weak evidence for a real signal; if it tracks the crew, it is observer drift.
cat("\nChange in plot proportion poor, split by resurvey crew:\n")
by.crew <- do.call(rbind, lapply(split(pair, pair$CrewLead), function(d) {
  wt <- tryCatch(wilcox.test(d$poor_now, d$poor_2018, paired = TRUE), error = function(e) NULL)
  data.frame(CrewLead = d$CrewLead[1], Year = paste(sort(unique(d$Year)), collapse = "/"),
             n_plots = nrow(d), poor_2018 = mean(d$poor_2018), poor_now = mean(d$poor_now),
             change = mean(d$d_poor),
             p = if (is.null(wt)) NA else wt$p.value, stringsAsFactors = FALSE)
}))
print(within(by.crew, {poor_2018 <- round(poor_2018, 3); poor_now <- round(poor_now, 3)
                       change <- round(change, 3); p <- signif(p, 3)}), row.names = FALSE)
for (i in seq_len(nrow(by.crew))) {
  add.model("2018 vs resurvey, by resurvey crew", paste0("crew ", by.crew$CrewLead[i]),
            by.crew$change[i], p = by.crew$p[i],
            note = paste0("n=", by.crew$n_plots[i], " plots; 2018=", round(by.crew$poor_2018[i], 3),
                          " -> now=", round(by.crew$poor_now[i], 3)))
}

write.csv(models, file.path(path.proc, "model_summaries.csv"), row.names = FALSE)

# ============================================================================================== #
# FIGURES
# ============================================================================================== #

# --- 9. Vigor class composition by monitoring unit ---------------------------------------------- #
vg <- as.data.frame(table(MgmtUnit = oak$MgmtUnit, Vigor = oak$Vigor))
vg$share <- vg$Freq / ave(vg$Freq, vg$MgmtUnit, FUN = sum)
unit.order <- unit.est$MgmtUnit[order(unit.est$prop_poor)]
vg$MgmtUnit <- factor(vg$MgmtUnit, levels = unit.order)
n.unit <- table(oak$MgmtUnit)[unit.order]
levels(vg$MgmtUnit) <- paste0(unit.order, "  (n=", as.integer(n.unit), ")")

f9 <- ggplot(vg, aes(x = share, y = MgmtUnit, fill = Vigor)) +
  geom_col(width = 0.72, color = INK[["surface"]], linewidth = 0.5) +
  scale_fill_manual(values = PAL.VIGOR, name = "Vigor",
                    labels = c("1  best", "2", "3  worst")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  labs(title = "Most oaks sit in the healthiest vigor class in every management unit",
       subtitle = paste("Share of live oak stems by vigor class, units ordered by proportion rated",
                        "2 or 3.\nThe model puts between-unit variance at zero: this ranking is",
                        "not distinguishable from noise."),
       x = "Share of live oak stems", y = NULL,
       caption = paste0(cap.vig, "\nMany units rest on fewer than 20 oak stems.")) +
  theme_ew() + theme(legend.position = "top")
save.fig(f9, "fig09_vigor_by_unit", width = 9, height = 7)

# --- 10. Map of proportion of oaks in poor vigor ------------------------------------------------ #
pv <- ps[!is.na(ps$prop_poor) & ps$n_oak_vigor > 0, ]
f10 <- ggplot(pv, aes(x = Longitude, y = Latitude)) +
  coord_equal() +
  geom_point(data = ps, aes(x = Longitude, y = Latitude), inherit.aes = FALSE,
             shape = 21, size = 1.5, fill = "#f0efec", color = INK[["axis"]], stroke = 0.3) +
  geom_point(aes(fill = prop_poor, size = n_oak_vigor), shape = 21,
             color = INK[["surface"]], stroke = 0.7) +
  scale_fill_ew_seq(name = "Oaks rated\nvigor 2 or 3", labels = percent_format(accuracy = 1),
                    limits = c(0, 1)) +
  scale_size_continuous(range = c(2, 6.5), name = "Oak stems\nrated", breaks = c(1, 5, 10, 20)) +
  labs(title = "Reduced-vigor oaks are scattered, with no obvious spatial cluster",
       subtitle = paste("Small grey points are plots with no rated oaks.",
                        "Point size shows how many oaks back each estimate"),
       caption = paste0(cap.vig, "\nPlots resting on 1-2 oak stems give very uncertain proportions.")) +
  theme_ew_map()
save.fig(f10, "fig10_vigor_map", width = 8.5, height = 6.5)

# --- 11. Vigor vs stem size -------------------------------------------------------------------- #
brk <- c(10, 20, 30, 40, 50, 60, 80, Inf)
lab <- c("10-20", "20-30", "30-40", "40-50", "50-60", "60-80", "80+")
oak$DBHclass <- cut(oak$DBH, breaks = brk, labels = lab, right = FALSE)
sz <- do.call(rbind, lapply(split(oak[!is.na(oak$DBHclass), ], oak$DBHclass[!is.na(oak$DBHclass)]),
                            function(d) {
  if (nrow(d) == 0) return(NULL)
  bt <- binom.test(sum(d$poor), nrow(d))
  data.frame(DBHclass = d$DBHclass[1], n = nrow(d), prop_poor = mean(d$poor),
             lo = bt$conf.int[1], hi = bt$conf.int[2], stringsAsFactors = FALSE)
}))

f11 <- ggplot(sz, aes(x = DBHclass, y = prop_poor)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, color = INK[["axis"]], linewidth = 0.6) +
  geom_point(size = 3.2, shape = 21, fill = PAL.CAT[1], color = INK[["surface"]], stroke = 0.7) +
  geom_text(aes(y = hi, label = paste0("n=", n)), vjust = -0.9, size = 3,
            color = INK[["muted"]]) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(mult = c(0.02, 0.16))) +
  labs(title = "Reduced vigor is not concentrated in the largest oaks",
       subtitle = "Proportion of oak stems rated vigor 2 or 3, by size class, with 95% intervals",
       x = "DBH class (cm)", y = "Oaks rated vigor 2 or 3", caption = cap.vig) +
  theme_ew()
save.fig(f11, "fig11_vigor_by_dbh", width = 8, height = 5.5)

# --- 12. Observer effect ----------------------------------------------------------------------- #
crew.rate$label <- paste0(crew.rate$CrewLead, "\n", crew.rate$Year)
f12 <- ggplot(crew.rate, aes(x = reorder(label, prop_poor), y = prop_poor)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1, color = INK[["axis"]], linewidth = 0.6) +
  geom_point(size = 4, shape = 21, fill = PAL.CAT[2], color = INK[["surface"]], stroke = 0.7) +
  geom_text(aes(label = paste0(percent(prop_poor, accuracy = 0.1), "  (n=", n_stems, ")")),
            hjust = -0.28, size = 3.3, color = INK[["secondary"]]) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(mult = c(0.02, 0.3))) +
  coord_flip() +
  labs(title = "Crews differ severalfold in how often they rate an oak as reduced vigor",
       subtitle = paste("Each crew surveyed a different year, so observer and year cannot be",
                        "separated.\nThis spread is as large as the 2018-to-now change below."),
       x = "Crew lead / survey year", y = "Oak stems rated vigor 2 or 3",
       caption = paste0(cap.vig, "\nBars are 95% binomial intervals.")) +
  theme_ew()
save.fig(f12, "fig12_observer_effect", width = 8, height = 4.5)

# --- 13. 2018 vs resurvey ---------------------------------------------------------------------- #
# Dumbbell by unit rather than by plot: 104 plot-level dumbbells would be an unreadable hairball,
# and the unit is the scale management acts on.
du <- do.call(rbind, lapply(split(pair, pair$MgmtUnit), function(d) data.frame(
  MgmtUnit = d$MgmtUnit[1], n_plots = nrow(d),
  poor_2018 = mean(d$poor_2018), poor_now = mean(d$poor_now), stringsAsFactors = FALSE)))
du <- du[du$n_plots >= 2, ]
du$MgmtUnit <- reorder(factor(du$MgmtUnit), du$poor_2018)
du.long <- rbind(
  data.frame(MgmtUnit = du$MgmtUnit, era = "2018", value = du$poor_2018, n_plots = du$n_plots),
  data.frame(MgmtUnit = du$MgmtUnit, era = "Resurvey", value = du$poor_now, n_plots = du$n_plots))
du.long$era <- factor(du.long$era, levels = c("2018", "Resurvey"))

f13a <- ggplot(du.long, aes(x = value, y = MgmtUnit)) +
  geom_segment(data = du, aes(x = poor_2018, xend = poor_now, y = MgmtUnit, yend = MgmtUnit),
               inherit.aes = FALSE, color = INK[["axis"]], linewidth = 1.1,
               lineend = "round") +
  geom_point(aes(fill = era), shape = 21, size = 3.4, color = INK[["surface"]], stroke = 0.7) +
  scale_fill_manual(values = PAL.ERA, name = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.04, 0.06))) +
  labs(title = "Oaks were rated reduced-vigor less often now than in 2018",
       subtitle = paste0("Mean plot-level share of oaks rated vigor 2 or 3, on the ", nrow(pair),
                         " plots rated in both eras (units with\n>= 2 paired plots). Most units",
                         " moved down, but several moved up -- and see the crew figure below."),
       x = "Oaks rated vigor 2 or 3", y = NULL,
       caption = paste("2018 IES survey vs the 2022/2025/2026 volunteer resurvey.",
                       "2018 recorded no tree tags, so this compares\neach plot's oak population",
                       "between eras, not the same individual stems. Different observers rated",
                       "the\ntwo eras, and crews differ severalfold on this measure (see previous",
                       "figure).")) +
  theme_ew() + theme(legend.position = "top")
save.fig(f13a, "fig13a_vigor_2018_dumbbell", width = 9, height = 6.5)

# Change map, diverging around zero
lim <- max(abs(pair$d_poor), na.rm = TRUE)
f13b <- ggplot(pair, aes(x = Longitude, y = Latitude)) +
  coord_equal() +
  geom_point(aes(color = d_poor), size = 3.6) +
  scale_color_ew_div(name = "Change in share\nof oaks rated 2-3",
                     limits = c(-lim, lim), labels = percent_format(accuracy = 1)) +
  labs(title = "The apparent improvement is spread across the woods, not localized",
       subtitle = "Change from 2018 to resurvey; blue = fewer reduced-vigor oaks now, red = more",
       caption = paste("Per-plot change on", nrow(pair), "paired plots.",
                       "A diverging scale centered at zero: grey means no change.\nObserver",
                       "differences between eras are not controlled -- see the caveat in the",
                       "report.")) +
  theme_ew_map()
save.fig(f13b, "fig13b_vigor_change_map", width = 8.5, height = 6.5)

# --- 13c. The same change, split by which crew did the resurvey ---------------------------------- #
# This is the figure that decides how the previous two should be read: if the 2018-to-now
# "improvement" were ecological, it would appear whichever crew did the resurvey.
bc <- by.crew
bc$label <- paste0(bc$CrewLead, " (", bc$Year, ")\n", bc$n_plots, " plots")
bc.long <- rbind(
  data.frame(label = bc$label, era = "2018", value = bc$poor_2018),
  data.frame(label = bc$label, era = "Resurvey", value = bc$poor_now))
bc.long$era <- factor(bc.long$era, levels = c("2018", "Resurvey"))
bc$label <- factor(bc$label, levels = bc$label[order(bc$change)])
bc.long$label <- factor(bc.long$label, levels = levels(bc$label))
bc$sig <- ifelse(is.na(bc$p), "", ifelse(bc$p < 0.05, "change significant",
                                         "change not significant"))

f13c <- ggplot(bc.long, aes(x = value, y = label)) +
  geom_segment(data = bc, aes(x = poor_2018, xend = poor_now, y = label, yend = label),
               inherit.aes = FALSE, color = INK[["axis"]], linewidth = 1.1, lineend = "round") +
  geom_point(aes(fill = era), shape = 21, size = 3.8, color = INK[["surface"]], stroke = 0.7) +
  geom_text(data = bc, aes(x = pmax(poor_2018, poor_now), y = label,
                           label = paste0("  ", sprintf("%+.0f", 100 * change), " pts, ", sig)),
            inherit.aes = FALSE, hjust = 0, size = 3.2, color = INK[["secondary"]]) +
  scale_fill_manual(values = PAL.ERA, name = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.62),
                     expand = expansion(mult = c(0.03, 0.02))) +
  labs(title = "The apparent improvement tracks the crew, not the woods",
       subtitle = paste("Almost all of it comes from the 2022 crews, who rated under 2% of oaks as",
                        "reduced vigor.\nThe crew rating most stringently (GA) shows no significant",
                        "change on its 69 plots."),
       x = "Oaks rated vigor 2 or 3", y = NULL,
       caption = paste("Paired plots only, split by the crew that did the resurvey. Change tested",
                       "with a paired Wilcoxon\nsigned-rank test per crew. This is why the",
                       "2018-to-now comparison cannot be read as ecological change.")) +
  theme_ew() + theme(legend.position = "top")
save.fig(f13c, "fig13c_vigor_change_by_crew", width = 9, height = 4.8)

cat("\nWrote vigor figures 9-13 and", nrow(models), "model rows to model_summaries.csv\n")
