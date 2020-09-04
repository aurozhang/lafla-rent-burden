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
# Filter data
###########################################################################

# Call dataset from choroplethrZip
data(zip.regions)

# Filter to LA Zip codes from choroplethrZip
LARegions <- (zip.regions %>% 
  filter(county.fips.numeric == 6037) %>%
  distinct(region))[[1]]

# The data.frame that you provide to zip_choropleth must have one column named 
# "region" and one column named "value". Your entries for "region" must exactly 
# match how regions are named in the map which choroplethr uses. 

# Input interested variable
var = c("federallyBackedLoans")

masterDataMap <- masterData %>%
  # Filter to LA FIPS code
  filter(GEOID %in% LARegions) %>%
  # Only keep interested variables
  select(GEOID, var)

# Rename variables to work with package
names(masterDataPre) <- c("region", "value")

rm(zip.regions)
###########################################################################
# Create map
###########################################################################

map <- zip_choropleth(masterDataMap,
               # Filter to LA codes
               county_zoom = 6037,
               title    = paste(var, "in Los Angeles"),
               legend   = "Count")

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