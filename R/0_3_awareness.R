library(tidyverse)
library(lubridate)

mydatapath <- "./data/awareness/"

bundeslaender_maps <- tibble(
  BL_ID=1:16,
  Bundesland=c(
    "Schleswig-Holstein",
    "Hamburg",
    "Niedersachsen",
    "Bremen",
    "Nordrhein-Westfalen",
    "Hessen",
    "Rheinland-Pfalz",
    "Baden-Württemberg",
    "Bayern",
    "Saarland",
    "Berlin",
    "Brandenburg",
    "Mecklenburg-Vorpommern",
    "Sachsen",
    "Sachsen-Anhalt",
    "Thüringen"
  ),
  Kuerzel=c(
    "SH",
    "HH",
    "NI",
    "HB",
    "NW",
    "HE",
    "RP",
    "BW",
    "BY",
    "SL",
    "BE",
    "BB",
    "MV",
    "SN",
    "ST",
    "TH"
  )
)

trends_alle_bl <- tibble()

for (kuerzel in bundeslaender_maps$Kuerzel) {
  this_bl <- read_csv(paste0("data/awareness/trends_", kuerzel, ".csv"), 
                      col_names = c("woche", "searches"),
                      col_types = cols(woche=col_date(format = "%Y-%m-%d"), 
                                       searches=col_double()), 
                      skip = 2) %>%
    mutate(searches=ifelse(is.na(searches), 0, searches),
           BL_ID=bundeslaender_maps%>%filter(Kuerzel==kuerzel)%>%pull(BL_ID),
           Kuerzel=kuerzel,
           Bundesland=bundeslaender_maps%>%filter(Kuerzel==kuerzel)%>%pull(Bundesland)) %>%
    drop_na()
  trends_alle_bl <- bind_rows(trends_alle_bl,
                              this_bl)
}

relativ_gesamt_2020_03_15 <- read_csv(paste0(mydatapath, 
                                             "relativ_gesamt_2020-03-15.csv"))

relativ_bl_2020_03_15 <- left_join(trends_alle_bl %>% 
                                     filter(woche=="2020-03-15"), 
                                   relativ_gesamt_2020_03_15,
                                   by="Bundesland") %>%
  mutate(baseline20200315=searches*Relativtrend/100) %>%
  rename(trend20200315=searches) %>%
  dplyr::select(BL_ID, baseline20200315, trend20200315)

relativ_bl <- left_join(trends_alle_bl, 
                        relativ_bl_2020_03_15, 
                        by="BL_ID") %>%
  mutate(relativtrend=baseline20200315/trend20200315*searches) %>%
  rename(date=woche) %>%
  dplyr::select(date, BL_ID, Bundesland, relativtrend) %>%
  mutate(year=year(date),
         kw=isoweek(date))

all_days <- tibble(date=as_date(as_date("2020-01-01"):max(relativ_bl$date))) %>%
  mutate(year=year(date),
         kw=isoweek(date))

searches_data <- all_days %>%
  left_join(relativ_bl, by=c("year", "kw")) %>%
  dplyr::select(date=date.x, BL_ID, Bundesland, relativtrend)

write_csv(searches_data, paste0(mydatapath, "awareness.csv"))
