# rent burdened households in LA

library(shiny)
library(janitor)
library(readxl)
library(dplyr)
library(stringr)

path <- "/Users/aurorazhang/Projects/Academic Projects/LAFLA Rent Burden Project/lafla-rent-burden/Data/Merged LAFLA Data (Full Joins).xlsx"

df <- read_excel(path)



str_detect(names(df),"B25070")

var_list <- function(base, number){
  list <- c()
  for(i in seq(number)){
    if (i < 10){
      ending <- paste0("_00", i)
    } else {
      ending <- paste0("_0", i)
    }
    list <- append(list, paste0(base, ending))
  }
  return(list)
}


gross_rb <- select(df, (contains("GEOID") | contains("B25070") | contains("B25003") | contains("B25064")))
names(gross_rb) <- c("GEOID", "Total Renter-Occupied", "<10%", "15%", "20%", "25%", "30%", "35%",
                     "40%", "45%", ">50%", "NC", "Total", "Owner Occ", "Renter Occ", "Median Rent")

gross_rb <- mutate(gross_rb, moderate_rb = `30%` + `35%` + `40%` + `45%` + `>50%`, extreme_rb = `40%` + `45%` + `>50%`) %>%
  mutate(percent_mod_rb = moderate_rb*100/`Total Renter-Occupied`, percent_ex_rb = extreme_rb*100/`Total Renter-Occupied`)
                   