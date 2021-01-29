library(tidyverse)

mydatapath <- "./data/awareness/"

trends_alle_bl <- read_delim(paste0(mydatapath, "trends_alle_bl.csv"), 
                             ";", 
                             escape_double = FALSE, 
                             col_types = cols(BL_ID = col_integer(), 
                                              Coronatrend = col_double(), 
                                              Datum = col_date(format = "%Y-%m-%d")), 
                             locale = locale(encoding = "UTF-8"),
                             trim_ws = TRUE)
relativ_gesamt_2020_03_13 <- read_csv(paste0(mydatapath, 
                                             "relativ_gesamt_2020-03-13.csv"))

relativ_bl_2020_03_13 <- left_join(trends_alle_bl %>% 
                                     filter(Datum=="2020-03-13"), 
                                   relativ_gesamt_2020_03_13,
                                   by="Bundesland") %>%
  mutate(baseline20200313=Coronatrend*Relativtrend/100) %>%
  rename(trend20200313=Coronatrend) %>%
  dplyr::select(BL_ID, baseline20200313, trend20200313)

relativ_bl <- left_join(trends_alle_bl, 
                        relativ_bl_2020_03_13, 
                        by="BL_ID") %>%
  mutate(relativtrend=baseline20200313/trend20200313*Coronatrend) %>%
  rename(date=Datum) %>%
  dplyr::select(date, BL_ID, Bundesland, relativtrend)

write_csv(relativ_bl, paste0(mydatapath, "awareness.csv"))
