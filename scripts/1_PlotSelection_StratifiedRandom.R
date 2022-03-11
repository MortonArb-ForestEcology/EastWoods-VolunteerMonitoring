# Purpose: Script to do the study design for volunteer-based monitoring of The Morton Arboretum's East Woods
# Author: Christy Rollinson, Forest Ecologist (crollinson@mortonarb.org)
# Date: 2022-03-11
# Description: Stratified random sampling plot selection design to ensure that we get data from all major management units each year with 5-year return interval.

library(ggplot2)

# File paths -- using what we've already done for ease right now
#  Ideally will get redone to show what original GIS files are being used, but I can't keep dragging my feet on this
ew.google <- "/Volumes/GoogleDrive/My Drive/East Woods" # Our Master East Woods folder (mac file path)
path.2018 <- file.path(ew.google, "East Woods Inventory 2018/Analyses_Rollinson") # Where everything Christy did for the 2018 IMLS poitn survey lives
path.vols <- file.path(ew.google, "East Woods Inventory - Volunteers")

# Read in the file that has all the characteristics I'd extracted in the past
dat.gis <- read.csv(file.path(path.2018, "data_processed", "point_info_GIS.csv"))
dat.gis$PlotID2 <- dat.gis$PlotID
dat.gis$PlotID <- as.factor(gsub("-", "", dat.gis$PlotID))
dat.gis$MgmtUnit <- ifelse(is.na(dat.gis$wooded), "Non-Wooded", 
                           ifelse(dat.gis$wooded=="Hidden Lake", "Hidden Lake", 
                                  ifelse(dat.gis$unit=="South 40 South", "Annual Burn", 
                                         ifelse(!is.na(dat.gis$unit), "Mixed Management", "No Management"))))
dat.gis$MgmtUnit[is.na(dat.gis$MgmtUnit)] <- "No Management"
dat.gis$MgmtUnit <- as.factor(dat.gis$MgmtUnit)
head(dat.gis)
summary(dat.gis)

# Subset to just the wooded units
unique(dat.gis$MgmtUnit)
dat.woods <- droplevels(dat.gis[dat.gis$wooded %in% c("East Woods", "Hidden Lake"),])
summary(dat.woods)

# Looking at what kind of units for stratification we already have
summary(as.factor(dat.woods$unit)) # This will probably be a useful thing to stratify by
ggplot(data=dat.woods) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=unit))

# -------------------
# Doing some recoding of existing units to have this work with our planned scheme
# -------------------
dat.woods$unit.monitoring <- dat.woods$unit

# Giving things without a label a name
dat.woods$unit.monitoring[dat.woods$wooded=="Hidden Lake"] <- "Hidden Lake"
dat.woods$unit.monitoring[is.na(dat.woods$unit.monitoring)] <- "Other" # These look to all be in the same place

# If a unit has less than 5 plots (1 per year), we should combine it with something else
dat.woods$unit.monitoring[grep("Big Rock", dat.woods$unit.monitoring)] <- "Big Rock" # This makes sense based on names
dat.woods$unit.monitoring[grep("Crataegus", dat.woods$unit.monitoring)] <- "Heritage Trail South" # This makes sense based on location
dat.woods$unit.monitoring[dat.woods$unit.monitoring=="East Woods 7"] <- "East Woods 6"

# Dropping 2 that sounds like they'd be wet messes
dat.woods <- dat.woods[!dat.woods$unit.monitoring %in% c("Bur Reed Marsh Drainage", "Crowley Marsh"),]

# THis is how I plot certain things to see what makes sense to combine
# ggplot(data=dat.woods[dat.woods$unit.monitoring %in% c("Crataegus South", "Crataegus North", "Heritage Trail South"),]) +
#   coord_equal() +
#   geom_point(aes(x=lon, y=lat, color=unit.monitoring))
nrow(dat.woods)

summary(as.factor(dat.woods$unit.monitoring)) 
ggplot(data=dat.woods) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=unit.monitoring)) #+
  # geom_point(data=dat.woods[dat.woods$unit.monitoring %in% c("East Woods 6", "East Woods 7"),],
             # aes(x=lon, y=lat),color="black")
# -------------------


# -------------------
# Now to randomly assign plots from each management unit to a year
# -------------------
n.return = 5 # This is where we can adjust our return interval easily
dat.woods$obs.year <- NA
set.seed(3111051)
for(UNIT in unique(dat.woods$unit.monitoring)){
  unit.all <- which(dat.woods$unit.monitoring==UNIT) 
  n.unit = length(unit.all)
  for(i in 1:n.return){ # Randomly get at least one plot per year
    rows.unit <- which(dat.woods$unit.monitoring==UNIT & is.na(dat.woods$obs.year))
    rows.yr <- sample(rows.unit, min(length(unit.all)/n.return, length(rows.unit)), replace=F)
    
    dat.woods$obs.year[rows.yr] <- i
    # ggplot(data=dat.woods[dat.woods$unit.monitoring==UNIT, ]) +
      # geom_point(aes(x=lon, y=lat))
  }
  # If we have leftover plots, randomly assign them to a year
  leftover <- which(is.na(dat.woods$obs.year[unit.all]))
  if(length(leftover)>0){
    dat.woods$obs.year[unit.all[leftover]] <- sample(1:n.return, length(leftover))
  }
}
head(dat.woods)
summary(dat.woods)
summary(as.factor(dat.woods$obs.year))
summary(as.factor(dat.woods$obs.year[dat.woods$unit.monitoring=="Hidden Lake"]))


pdf(file.path(path.vols, "MonitoringLists_byYear_Units.pdf"), height=8, width=11)
ggplot(data=dat.woods) +
  facet_wrap(~obs.year) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=unit.monitoring)) +
  theme(legend.position="top")
dev.off()
# -------------------
