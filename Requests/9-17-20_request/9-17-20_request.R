# rent burdened households in LA

library(shiny)
library(janitor)
library(readxl)
library(dplyr)
library(stringr)

wd <- "/Users/aurorazhang/Projects/Academic Projects/LAFLA Rent Burden Project/lafla-rent-burden/"

setwd(wd)

data_path <- paste0(wd,"Data/Merged LAFLA Data (Full Joins).xlsx")

df <- read_excel(data_path)


# find percentage of rent burden and percentage of renters
gross_rb <- select(df, (contains("GEOID") | contains("B25070") | contains("B25003") | contains("B25064")))
names(gross_rb) <- c("GEOID", "total_renter_occ1", "<10%", "15%", "20%", "25%", "30%", "35%",
                     "40%", "45%", ">50%", "nc", "total_count", "count_owner_occ", "count_renter_occ", "median_rent")

gross_rb <- mutate(gross_rb, moderate_rb = `30%` + `35%` + `40%` + `45%` + `>50%`, extreme_rb = `40%` + `45%` + `>50%`) %>%
  mutate(percent_mod_rb = moderate_rb*100/count_renter_occ, percent_ex_rb = extreme_rb*100/count_renter_occ) %>%
  mutate(percent_owner_occ = count_owner_occ*100/total_count, percent_renter_occ = count_renter_occ*100/total_count) %>%
  select(GEOID, total_count, count_owner_occ, percent_owner_occ, count_renter_occ, percent_renter_occ, median_rent, moderate_rb, extreme_rb,
         percent_mod_rb, percent_ex_rb)


# find out how much the median hh income is above or below the la county AMI
county_ami <- c(39450, 45050, 50700, 56300, 60850, 65350, 69850)

ami_calc <- select(df, (contains("GEOID") | contains("B19019") | contains("B11016")))

ami_calc <- mutate(ami_calc, `1p_perc` = `B11016_010`/`B11016_001`, 
                   `2p_perc` = (`B11016_003`+`B11016_011`)/`B11016_001`,
                   `3p_perc` = (`B11016_004`+`B11016_012`)/`B11016_001`,
                   `4p_perc` = (`B11016_005`+`B11016_013`)/`B11016_001`,
                   `5p_perc` = (`B11016_006`+`B11016_014`)/`B11016_001`,
                   `6p_perc` = (`B11016_007`+`B11016_015`)/`B11016_001`,
                   `7+p_perc` = (`B11016_008`+`B11016_016`)/`B11016_001`) %>%
  mutate(`1p_under_ami` = `B19019_002` - county_ami[1]) %>%
  mutate(`2p_under_ami` = `B19019_003` - county_ami[2]) %>%
  mutate(`3p_under_ami` = `B19019_004` - county_ami[3]) %>%
  mutate(`4p_under_ami` = `B19019_005` - county_ami[4]) %>%
  mutate(`5p_under_ami` = `B19019_006` - county_ami[5]) %>%
  mutate(`6p_under_ami` = `B19019_007` - county_ami[6]) %>%
  mutate(`7+p_under_ami` = `B19019_008` - county_ami[7]) %>%
  mutate(ami_weighted_sum = `1p_under_ami`*`1p_perc` + `2p_under_ami`*`2p_perc` +`3p_under_ami`*`3p_perc`
         + `4p_under_ami`*`4p_perc` + `5p_under_ami`*`5p_perc` + `6p_under_ami`*`6p_perc` + 
           `7+p_under_ami`*`7+p_perc`) %>%
  mutate(`median_ami_diff` = `B19019_001` - 77300) %>%
  select(GEOID, ami_weighted_sum, median_ami_diff)


rb_ami <- full_join(gross_rb, ami_calc, by="GEOID") %>%
  mutate(zcta5 = GEOID) %>%
  select(-GEOID)
  

  
# get list of relevant zip codes
zip_list <- read_excel(paste0(wd,"Requests/9-17-20_request/zip-codes.xlsx"),
                       sheet = "LA Zips by Court")
zip_list <- mutate(zip_list, GEOID = `Zip`)
# get only la city zip codes
la_city_zips <- pull(zip_list, `Zip`)

# crosswalk between zip code and ZCTA             
crosswalk_path <- paste0(wd,"Data/Individual Datasets/08_11_2020_crosswalk.xlsx")
crosswalk_df <- read_excel(crosswalk_path)

# crosswalk between zip code and ZCTA, filtered down to la city 
la_crosswalk_df <- filter(crosswalk_df, zip_code %in% la_city_zips) %>%
  distinct(zcta5, zip_code) %>%
  mutate(Zip = as.double(zip_code))

# la zip list, cleaned.
# note that GEOID here refers to ZCTA
la_zip_list <- left_join(zip_list, la_crosswalk_df, on = `Zip`) %>%
  select(Zip, Place, Court, zcta5)



rb_ami_final <- left_join(la_zip_list, rb_ami, on="zcta5")

write.csv(rb_ami_final, "output.csv", row.names=FALSE)
