#----------------------------------------------------------------------------------------------------------------------#
# Script by: Lucien Fitzpatrick
# Project: East Woods Inventory 2022
# Purpose: This script provides a general QAQC and a comparison QAQC for our 2022 east woods inventory
# Inputs: 2018 East woods inventory data
#         2022 Volunteer east woods inventory survey
# Outputs: 
# Notes: Most of these are eye tests that you will need to manually interpret and fix
#----------------------------------------------------------------------------------------------------------------------#
#Reading in our volunteer gathered data
dat.volsurv <- googlesheets4::read_sheet("161i75Il4W3u8oJ6yMpY9fMfudHCNlamcD89SLj0u-hY", sheet = "Tree Survey")

#Converting plot id names to match
dat.volsurv$`Plot ID` <- gsub("-","",dat.volsurv$`Plot ID`)

#Removing vempty values from a pre-filled data sheet
dat.volsurv <- dat.volsurv[!is.na(dat.volsurv$`Survey Date`),]
dat.volsurv$`Species Code` <- ifelse(is.na(dat.volsurv$`Species Code`),"None", dat.volsurv$`Species Code`)

#Calculating Basal area
dat.volsurv$BA <- dat.volsurv$`DBH (cm)`^2 * 0.005454 

#------------------------------------------#
#Quick QAQC checks on our columns
#------------------------------------------#
#Checking date range
range(dat.volsurv$`Survey Date`)

#Checking nonsense Tree tag's
range(dat.volsurv$`Tree Tag`)

#Checking DBH range. Anything below 10cm shouldn't have been recorded and watch for common sense maximums
range(as.numeric(dat.volsurv$`DBH (cm)`), na.rm = T)

#Making sure all asnwers are accurate species codes. Checking for typos
unique(dat.volsurv$`Species Code`)

#Making sure all answers are valid options
unique(dat.volsurv$`Canopy Position`)

#Making sure all answers are valid options
unique(dat.volsurv$`Vigor Rating`)

#Reading in the 2018 survey data
midtree.data <- read.csv("../data/Old EW Survey Data/East Woods Spring vegetation data.final.csv", na.strings="")

#Selecting on the plots we have data for
midtree.data <- midtree.data[midtree.data$Plot.ID.Number %in% unique(dat.volsurv$`Plot ID`), 1:10]
midtree.data <- midtree.data[!is.na(midtree.data$Plot.ID.Number), ]

midtree.data$BA <- as.numeric(midtree.data$DBH.cm)^2 * 0.005454 

dat.comb <- data.frame()
dat.diff <- data.frame()
for(PLOT in unique(dat.volsurv$`Plot ID`)){
  
  dat.temp <- dat.volsurv[dat.volsurv$`Plot ID` == PLOT,]
  dat.mid <- midtree.data[midtree.data$Plot.ID.Number == PLOT,]
  
  tbl.temp <- as.data.frame(table(dat.temp$`Species Code`))
  colnames(tbl.temp) <- c("vol.species", "vol.num")
  tbl.temp$vol.sum <- sum(tbl.temp$vol.num)
  
  #Checking we do have old survey data and creating a filler if not so the loop doesn't break
  if(nrow(dat.mid) == 0){
    tbl.mid <- data.frame(NA, NA, NA)
    colnames(tbl.mid) <- c("orig.species", "orig.num", "orig.sum")
  } else{
  tbl.mid <- as.data.frame(table(dat.mid$Species))
  colnames(tbl.mid) <- c("orig.species", "orig.num")
  tbl.mid$orig.sum <- sum(tbl.mid$orig.num)
  }

  
  dat.sum <- data.frame(unique(tbl.mid$orig.sum), unique(tbl.temp$vol.sum))
  colnames(dat.sum) <- c("orig.sum", "vol.sum")
  dat.sum$plot <- PLOT
  
  #Summing Basal area
  dat.sum$orig.BA <- sum(dat.mid$BA, na.rm = T)
  dat.sum$vol.BA <- sum(dat.temp$BA, na.rm = T)
  
  #Min and Max tree size
  dat.sum$orig.MAX <- max(as.numeric(dat.mid$DBH.cm), na.rm = T)
  dat.sum$orig.MIN <- min(as.numeric(dat.mid$DBH.cm), na.rm = T)
  
  dat.sum$vol.MAX <- max(as.numeric(dat.temp$`DBH (cm)`), na.rm = T)
  dat.sum$vol.MIN <- min(as.numeric(dat.temp$`DBH (cm)`), na.rm = T)
  
  dat.both <- merge(tbl.mid, tbl.temp, by.x = "orig.species", by.y = "vol.species", all.x = T, all.y = T)
  dat.both$plot <- PLOT
  dat.comb <- rbind(dat.comb, dat.both)
  dat.diff <- rbind(dat.diff, dat.sum)
}

#Difference in stem density
dat.diff$stem.diff <- dat.diff$vol.sum - dat.diff$orig.sum
#Difference in Basal area
dat.diff$BA.diff <- dat.diff$vol.BA - dat.diff$orig.BA
#Difference in min tree size
dat.diff$MIN.diff <- dat.diff$vol.MIN - dat.diff$orig.MIN
#Difference in max tree size
dat.diff$MAX.diff <- dat.diff$vol.MAX- dat.diff$orig.MAX

#--------------------#
#Histograms to flag extremes
#--------------------#

hist(dat.diff$stem.diff)

hist(dat.diff$BA.diff)

hist(dat.diff$MIN.diff)

hist(dat.diff$MAX.diff)

