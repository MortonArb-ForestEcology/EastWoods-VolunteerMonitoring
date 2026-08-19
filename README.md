# EastWoods-VolunteerMonitoring
 Data Associated with plot selection & analysis for volunteer-based monitoring of The Morton Arboretum's East Woods

## Key Contacts:
Christy Rollinson, Senior Scientist, Forest Ecology (crollinson@mortonarb.org)
Meghan Midgley, Senior Director of CTS & Research Scientist, Soil Ecology (mmidgley@mortonarb.org)
Spencer Campbell, Senior Manager of Natural Areas (2025-present) (scampbell@mortonarb.org)
Jasmine Dwyer, Collections Volunteers Coordinator (2023-present) (jdwyer@mortonarb.org)

Former
Kurt Driesilker, Head of Natural Resources and Collections Horticulture (2022 - 2025) (kdriesilker@mortonarb.org)


## Background:
Building off of the two past surveys (2007, 2018) performed by IES as part of a (Bell Labs??) grant at the IMLS plots scattered throughout the East Woods.  The two surveys provided detailed data across the East Woods, but the time period in between has made it hard to detect management-linked patterns of change.  In regular discussions in 2021 and 2022, a team of Arboretum Natural Resources staff (Driesilker, Campbell) and scientists (Rollinson, Midgley) agreed to shift to a rotating monitoring scheme loosely based off of the US Forest Service Forest Inventory Analysis (FIA) plan wherein a different set of plots are monitored each year so that every plot is revisited every 5 or so years.  The plots to be monitored each year will be selected using a stratified random sampling to ensure that we will receive data from all major management units each year.

## Analysis pipeline

Summarizes basal area, oak dominance, oak vigor, and oak regeneration. Run in order from the
project root; script 0 needs a Google account with access to the two source sheets.

```
Rscript scripts/0_data_download.R      # cache both Google Sheets to data/raw_gsheet/ (dated snapshot)
Rscript scripts/1_qaqc_clean.R         # clean both datasets; log every correction to qaqc_flags.csv
Rscript scripts/2_species_crosswalk.R  # link 2018 six-letter codes to the volunteer four-letter codes
Rscript scripts/3_plot_summaries.R     # plot-level metrics + summary statistics
Rscript scripts/3b_figures_basalarea.R # figures 1-8  (basal area, composition, oak share)
Rscript scripts/4_vigor_analysis.R     # figures 9-13 (oak vigor in space and vs 2018) + models
Rscript scripts/5_seedling_analysis.R  # figures 14-18 (oak regeneration) + models
Rscript scripts/7_render_report.R      # knit reports/EastWoods_VolunteerMonitoring_Summary.html
```

`scripts/0_helper_functions.R` holds the plot-design constants (0.025 ha, 8.92 m radius, 10 cm DBH
cutoff), the basal area functions, the shared figure theme and palette, and the output paths.
Sourced by the others.

### Where the outputs go

Everything is written to the team's shared Google Drive folder so the whole group has access:

**[East Woods Inventory - Volunteers / Analysis_Output](https://drive.google.com/drive/folders/1Ab8fnpEu5riQCKB1XaKLoITaizHPVOmv)**

```
Analysis_Output/
  data_raw_snapshots/   dated CSV snapshots of every Google Sheet tab
  data_processed/       cleaned data, summary tables, model output, QA/QC log
  figures/              20 figures
  reports/              EastWoods_VolunteerMonitoring_Summary.html
```

This needs Google Drive for Desktop running and synced. The folder is located by its **Drive folder
ID**, not by its path, so renaming or moving it will not silently send output somewhere else — the
scripts read the ID from the `com.google.drivefs.item-id` extended attribute and check it matches.

- If Drive is not available the scripts fall back to writing into the project directory and print a
  warning saying the outputs are not shared.
- To write somewhere else, set `EASTWOODS_OUT`, e.g.
  `export EASTWOODS_OUT=~/Desktop/ew_test` before running.
- Generated outputs are gitignored; the repo holds the code and the 2018 source CSV only.

Note that Google Drive will offer to *download* the HTML report rather than rendering it in the
browser. To read it in place, open it with the Drive HTML preview, or download and open locally.

**Read the report's data-quality appendix before quoting numbers.** Three things in particular:
oak vigor is not comparable across survey years because crew and year are confounded and crews
differ severalfold in how often they rate a tree as reduced vigor; both datasets are rotating panels
so year is confounded with location; and the seedling protocol does not define a search area, so
seedling counts are per plot and cannot be converted to densities.

Note: `Volunteer_survery_qaqc.R` is the original 2022 QA/QC script. It reads a `Survey Date` column
that the sheet no longer has, and computes basal area with an imperial constant applied to
centimeters. `1_qaqc_clean.R` supersedes it; keep it for reference only.
