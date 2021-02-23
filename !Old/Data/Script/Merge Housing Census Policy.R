
## Merge LAFLA Datasets

library(data.table)
library(feather)
library(janitor)
library(lubridate)
library(pdftools)
library(plotly)
library(readxl)
library(stringr)
library(tidyverse)
library(writexl)
library(xts)
library(zoo)

options(scipen=999, digits = 2)
rm(list=ls())

census_housing   <- read.csv("//ace/data/case3/LAFLA_Rent_Burden_972140/Data/Data Cleaning/Merging/Input/census_housing_california.csv")
la_employment    <- read_xlsx("//ace/data/case3/LAFLA_Rent_Burden_972140/Data/Data Cleaning/Merging/Input/LA County Employment Data.xlsx")
la_supervisorial <- read_xlsx("//ace/data/case3/LAFLA_Rent_Burden_972140/Data/Data Cleaning/Merging/Input/LA Supervisorial District Data.xlsx")
la_court_locs    <- read_xlsx("//ace/data/case3/LAFLA_Rent_Burden_972140/Data/Data Cleaning/Merging/Input/LA Courthouse Data.xlsx")

la_employment    <- la_employment    %>% mutate(GEOID = as.integer(zip)) %>% filter(year == 2018)
la_supervisorial <- la_supervisorial %>% mutate(GEOID = as.integer(`Zip Code`))
la_court_locs    <- la_court_locs    %>% mutate(GEOID = as.integer(`Zip Code`))

merged_dataset   <- full_join(la_supervisorial, la_employment , by = c("GEOID"))
merged_dataset   <- full_join(la_court_locs   , merged_dataset, by = c("GEOID"))
merged_dataset   <- full_join(census_housing  , merged_dataset, by = c("GEOID"))

zip_crosswalk    <- read_xlsx("//ace/data/case3/LAFLA_Rent_Burden_972140/Data/Data Cleaning/Merging/Input/08_11_2020_crosswalk.xlsx") %>% filter(state_abs == "CA")
zip_crosswalk    <- zip_crosswalk %>% filter(!is.na(zip_code)) %>% mutate(GEOID = as.integer(zip_code))
zip_crosswalk    <- zip_crosswalk %>% filter(county == 37) %>% select(GEOID) %>% unique()
merged_dataset   <- merged_dataset %>% filter(GEOID %in% zip_crosswalk$GEOID)


write_xlsx(merged_dataset,
           "//ace/data/case3/LAFLA_Rent_Burden_972140/Data/Data Cleaning/Merging/Merged LAFLA Data (Full Joins).xlsx")
