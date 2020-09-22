###########################################################################
# Program: Create a map using zip code data
# Date: 09.04.2020
# Author: XZ
############################################################################

# See the website below to map tutorial
# https://arilamstein.com/creating-zip-code-choropleths-choroplethrzip/

# Get the relevant packages for the map
# install.packages("devtools")
# library(devtools)
# install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethrZip)

# Load packages
library(dplyr)
library(readxl) 
library(ggplot2)
library(ggpubr)
rm(list=ls())

# Set path
Dir       <-
DirInput  <-
DirOutput <-

###########################################################################
# Import data
###########################################################################

# Import federal housing data
filename <- paste(DirInput, "census_housing_california.csv", sep="/")
masterData <- read.csv(filename )%>%
  # Make GEOID a character
  mutate(GEOID = as.character(GEOID)) 

###########################################################################
# Create a list of interested LA zip codes
###########################################################################

# Found the below zip codes in the website below:
# https://www.usmapguide.com/california/los-angeles-zip-code-map/#:~:text=The%20Verdugos%20(Los%20Angeles)%20Zip,%2C%2091208%2C%2091210%2C%2091214.
LARegions <- as.character(c(90601, 90602, 90603, 90605, 90631, 91006, 91007, 91008, 91010, 91016, 91024, 
              91030, 91108, 91706, 91722, 91723, 91724, 91731, 91732, 91733, 91741, 
              91744, 91745, 91746, 91748, 91754, 91765, 91770, 91773, 91775, 91776, 
              91780, 91789, 91790, 91791, 91792, 91801, 91803, 90031, 90032, 90041, 
              90042, 90065, 91204, 91205, 90004, 90005, 90006, 90012, 90013, 90014, 
              90015, 90017, 90019, 90021, 90026, 90027, 90028, 90035, 90036, 90038, 
              90039, 90046, 90048, 90057, 90068, 90069, 90071, 90022, 90023, 90033, 
              90063, 90024, 90025, 90034, 90049, 90056, 90064, 90066, 90067, 90073, 
              90077, 90094, 90210, 90212, 90230, 90232, 90272, 90291, 90292, 90401, 
              90402, 90403, 90404, 90405, 90001, 90002, 90003, 90007, 90008, 90011, 
              90016, 90018, 90037, 90043, 90044, 90047, 90059, 90061, 90062, 90089, 
              90220, 90305, 90040, 90058, 90201, 90240, 90241, 90242, 90255, 90262, 
              90270, 90280, 90604, 90606, 90638, 90640, 90650, 90660, 90670, 90703, 
              90706, 90723, 90045, 90245, 90249, 90250, 90254, 90260, 90266, 90274, 
              90275, 90277, 90278, 90293, 90301, 90302, 90303, 90304, 90501, 90503, 
              90504, 90505, 90506, 90717, 90221, 90502, 90710, 90712, 90713, 90715, 
              90716, 90731, 90732, 90744, 90745, 90746, 90755, 90802, 90803, 90804, 
              90805, 90806, 90807, 90808, 90810, 90813, 90814, 90815, 90822, 90831))

###########################################################################
# Filter data
###########################################################################

# Input interested variable
var = c("federallyBackedLoans")

# The data.frame that you provide to zip_choropleth must have one column named 
# "region" and one column named "value". Your entries for "region" must exactly 
# match how regions are named in the map which choroplethr uses. 

masterDataMap <- masterData %>%
  # Filter to LA FIPS code
  filter(GEOID %in% LARegions) %>%
  # Only keep interested variables
  select(GEOID, var)

# Rename variables to work with package
names(masterDataMap) <- c("region", "value")

###########################################################################
# Create map
###########################################################################

# Use the choroplethrZip package
map <- zip_choropleth(masterDataMap,
               # Filter to LA codes
               zip_zoom = LARegions,
               legend   = "Count") + 
  # Change the title theme
  theme(plot.title = element_text(size = 12, face = "bold", hjust=0.5, margin = margin(b = 20, r=0, l=0, t=20)),
        legend.title = element_text( size=10, face="bold")) +
  # Add a title
  ggtitle(paste0(var,"\n", "Greater Los Angeles"))

mapClean <- map %>%
  # Add the Notes
  annotate_figure(bottom = text_grob("Notes:", vjust=0.5, color = "black", size = 10, just="left", x=0.05, face="bold")) %>%
  # Add the specific notes
  annotate_figure(bottom = text_grob(paste0("1. The legend can represent dollars, count, or percentages.\n",
                                            "2. The zip codes are manually selected, and might not represent the governement definition of Greater Los Angeles.\n","\n"),
                                               vjust=0.5, color = "black", size = 10, just="left", x=.05))

###########################################################################
# Save map
###########################################################################

# Open the PDF file
pdf(file =  paste(DirOutput, var, " Map.pdf", sep=""),
    width = 8.5,
    height = 11)

# Print map
print(map)

# Close out PDf
dev.off()

##################################EOF######################################
