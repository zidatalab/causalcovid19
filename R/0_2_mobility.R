library(sf)
library(tidyverse)
library(geojsonsf)
library(geojsonio)

mydatapath <- "./data/mobility/"

# google data
gmobillink <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
mytemp <- tempfile()
download.file(gmobillink, mytemp, quiet=TRUE)
google_german_mobility <- read_csv(mytemp) %>%
  filter(country_region_code=="DE") %>%
  dplyr::select(-country_region_code, -country_region, -sub_region_2)

en_rs_map_bl <- c("08", "09", "11", "12",
                  "04", "02", "06", "03",
                  "13", "05", "07", "10",
                  "14", "15","01", "16")
# names(en_de_map_bl) <- c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg",
#                          "Bremen", "Hamburg", "Hessen", "Niedersachsen",
#                          "Mecklenburg-Vorpommern", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland",
#                          "Sachsen", "Sachsen-Anhalt","Schleswig-Holstein", "Thüringen")
names(en_rs_map_bl) <- c("Baden-Württemberg", "Bavaria", "Berlin", "Brandenburg",
                         "Bremen", "Hamburg", "Hesse", "Lower Saxony",
                         "Mecklenburg-Vorpommern", "North Rhine-Westphalia", "Rhineland-Palatinate", "Saarland",
                         "Saxony", "Saxony-Anhalt","Schleswig-Holstein", "Thuringia")
google_german_mobility <- google_german_mobility %>%
  mutate(grocery_and_pharmacy_percent_change_from_baseline=replace_na(grocery_and_pharmacy_percent_change_from_baseline, 0)) %>%
  mutate(parks_percent_change_from_baseline=replace_na(parks_percent_change_from_baseline, 0)) %>%
  mutate(RS_first2=en_rs_map_bl[sub_region_1]) %>%
  dplyr::select(-sub_region_1) %>%
  # drop_na() %>%
  pivot_longer(cols=ends_with("baseline"), names_to = "areatype", values_to = "perc_change")
write_csv(google_german_mobility, paste0(mydatapath, "google_mobilitychange_percentages_per_bundesland_and_areatype.csv"))
