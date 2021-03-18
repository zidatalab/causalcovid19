mylags <- 1:20

myaic <- rep(0, 20)
myglms <- vector("list", 20)

library(lubridate)
library(tidyverse)
library(DBI)
library(broom)
library(MASS)

mydatapath <- "secondwave_analysis/data/" 

google_mobility <- read_csv(paste0(mydatapath, "mobility/google_mobilitychange_percentages_per_bundesland_and_areatype.csv")) %>%
  mutate(id=as.integer(RS_first2)) %>%
  dplyr::select(-RS_first2) %>%
  pivot_wider(names_from = areatype, values_from = perc_change) %>%
  rename(`Mobility (retail and recreation)`=retail_and_recreation_percent_change_from_baseline,
         `Mobility (grocery and pharmacy)`=grocery_and_pharmacy_percent_change_from_baseline,
         `Mobility (parks)`=parks_percent_change_from_baseline,
         `Mobility (transit stations)`=transit_stations_percent_change_from_baseline,
         `Mobility (workplaces)`=workplaces_percent_change_from_baseline,
         `Mobility (residential)`=residential_percent_change_from_baseline)

weather <- read_csv(paste0(mydatapath, "weather/mittleres_kreiswetter.csv")) %>%
  mutate(date=ymd(MESS_DATUM), id=as.integer(RS)*1000) %>%
  dplyr::select(-MESS_DATUM, -RS) %>%
  mutate(Wind=replace_na(Wind, mean(Wind, na.rm=TRUE)),
         Feuchtigkeit=replace_na(Feuchtigkeit, mean(Feuchtigkeit, na.rm=TRUE))) %>%
  rename(`Weather (rainfall)`=Niederschlag,
         `Weather (wind)`=Wind,
         `Weather (temperature)`=Temperatur,
         `Weather (humidity)`=Feuchtigkeit)

awareness <- read_csv(paste0(mydatapath, "awareness/awareness.csv")) %>%
  rename(`Searches corona`=relativtrend) %>%
  dplyr::select(-Bundesland)

pflege_destatis <- read_delim(paste0(mydatapath, "sociodemographic/pflege_destatis.csv"), 
                              ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = ".", encoding = "WINDOWS-1252"), 
                              trim_ws = TRUE, skip = 6, na = "-") %>%
  `colnames<-`(c("stand", "id", "kreisname", "pflege_ambulant", "personal_pflege_ambulant", "pflege_stationaer", "plaetze_pflege_stationaer", "dauerplaetze_pflege_stationaer", "personal_pflege_stationaer")) %>%
  mutate(id=as.integer(id)*1000) %>%
  rename(`Nursing homes`=pflege_stationaer) %>%
  dplyr::select(id, `Nursing homes`)

inkar <- read_csv(paste0(mydatapath, "sociodemographic/inkar.csv")) %>%
  mutate(id=as.integer(Kennziffer)*1000) %>%
  dplyr::select(-Kennziffer, -Raumeinheit, -Aggregat) %>%
  mutate(`Anteil Schutzsuchender an Bevölkerung`=replace_na(`Anteil Schutzsuchender an Bevölkerung`, mean(`Anteil Schutzsuchender an Bevölkerung`, na.rm = TRUE))) %>%
  rename(`Age (pop. 65 and older)`=`Einwohner 65 Jahre und älter`,
         `Population density`=Einwohnerdichte,
         `Sex`=Frauenanteil,
         `Foreign residents`=Ausländeranteil,
         `Foreign residents (refugees)`=`Anteil Schutzsuchender an Bevölkerung`,
         `Socio-economic status`=`Haushalte mit niedrigem Einkommen`,
         `Right-wing populist party votes`=`Stimmenanteile AfD`,
         Turnout=Wahlbeteiligung)

brd_timeseries <- read_csv(paste0(mydatapath, "rki_cases/brd_timeseries.csv")) %>% mutate(date=as.Date(date))

kreisids <- unique(inkar$id)

germanholidays <- as.Date(c("2020-04-10", #karfreitag
                            "2020-04-13", #ostermontag
                            "2020-05-01", # tag der arbeit
                            "2020-05-21", # himmelfahrt
                            "2020-06-01", # pfingstmontag
                            "2020-10-03", # day of unity
                            "2020-12-24", "2020-12-25", "2020-12-26", # christmas
                            "2020-12-31", # new years eve
                            "2021-01-01" # new year
)) 
nobigevents <- as.Date("2020-03-08")
noschool <- as.Date("2020-03-16")
contactrestrict <- as.Date("2020-03-22")
somerelaxations <- as.Date("2020-05-06")
lockdownlight <- as.Date("2020-11-02")
lockdownharder <- as.Date("2020-12-13")
measures_zeroone <- tibble(date=as_date(as_date("2020-01-01"):as_date("2021-01-29"))) %>%
  mutate(`Ban of mass gatherings`=ifelse(date<nobigevents, 0, 1),
         `School/Kita closures`=case_when(
           date<noschool ~ 0,
           date>=noschool & date<somerelaxations ~ 1,
           date>=somerelaxations & date<lockdownharder ~ 0.5,
           date>=lockdownharder ~ 1
         ),
         `Contact restrictions`=case_when(
           date<contactrestrict ~ 0,
           date>=contactrestrict & date<somerelaxations ~ 1,
           date>=somerelaxations & date<lockdownlight ~ 0.5,
           date>=lockdownlight ~ 1
         ),
         Holiday=ifelse(date %in% germanholidays, 1, 0),
         `Mandatory face masks`=0,
         id=kreisids[1])
measures_zeroone_all <- measures_zeroone
for (kid in kreisids[-1]) {
  measures_zeroone_all <- bind_rows(measures_zeroone_all, measures_zeroone %>% mutate(id=kid))
}
measures_zeroone_all <- measures_zeroone_all %>%
  mutate(Holiday=ifelse(floor(id/1000/1000)==11 & date==as.Date("2020-05-08"), 1, Holiday),
         `School/Kita closures`=ifelse(date>=as.Date("2020-04-27"), 0.5, `School/Kita closures`),
         `School/Kita closures`=ifelse(floor(id/1000/1000)==15 & date>=as.Date("2020-04-23"), 0.5, `School/Kita closures`),
         `School/Kita closures`=ifelse(floor(id/1000/1000)==14 & date>=as.Date("2020-04-20"), 0.5, `School/Kita closures`),
         `School/Kita closures`=ifelse(floor(id/1000/1000)==10 & date>=as.Date("2020-04-27"), 0, `School/Kita closures`),
         `School/Kita closures`=ifelse(floor(id/1000/1000)==10 & date>=as.Date("2020-05-04"), 0.5, `School/Kita closures`),
         `Mandatory face masks`=ifelse(date>=as.Date("2020-04-27"), 1, 0),
         `Mandatory face masks`=ifelse(floor(id/1000/1000)==1 & date>=as.Date("2020-04-27"), 0, `Mandatory face masks`), # schleswigholstein etwas später
         `Mandatory face masks`=ifelse(floor(id/1000/1000)==1 & date>=as.Date("2020-04-29"), 1, `Mandatory face masks`),
         `Mandatory face masks`=ifelse(id==3101000 & date>=as.Date("2020-04-25"), 1, `Mandatory face masks`), # braunschweig
         `Mandatory face masks`=ifelse(floor(id/1000/1000)==16 & date>=as.Date("2020-04-24"), 1, `Mandatory face masks`), # thüringen
         `Mandatory face masks`=ifelse(floor(id/1000/1000)==15 & date>=as.Date("2020-04-22"), 1, `Mandatory face masks`), # sachsenanhalt
         `Mandatory face masks`=ifelse(id==3103000 & date>=as.Date("2020-04-20"), 1, `Mandatory face masks`), # wolfsburg
         `Mandatory face masks`=ifelse(id==6435000  & date>=as.Date("2020-04-20"), 1, `Mandatory face masks`), # main-kinzig
         `Mandatory face masks`=ifelse(floor(id/1000/1000)==14 & date>=as.Date("2020-04-20"), 1, `Mandatory face masks`), # sachsen
         `Mandatory face masks`=ifelse(id==8325049 & date>=as.Date("2020-04-17"), 1, `Mandatory face masks`), # rottweil
         `Mandatory face masks`=ifelse(id==16062041  & date>=as.Date("2020-04-14"), 1, `Mandatory face masks`), # nordhausen
         `Mandatory face masks`=ifelse(id==16053000  & date>=as.Date("2020-04-06"), 1, `Mandatory face masks`) # jena
  ) %>% 
  mutate( # schulferien sommer
    `School/Kita closures`=ifelse(floor(id/1000/1000)==1 & date>=as.Date("2020-06-29") & date<=as.Date("2020-08-08"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==2 & date>=as.Date("2020-06-25") & date<=as.Date("2020-08-05"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==3 & date>=as.Date("2020-07-16") & date<=as.Date("2020-08-26"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==4 & date>=as.Date("2020-07-16") & date<=as.Date("2020-08-26"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==5 & date>=as.Date("2020-06-29") & date<=as.Date("2020-08-11"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==6 & date>=as.Date("2020-07-06") & date<=as.Date("2020-08-14"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==7 & date>=as.Date("2020-07-06") & date<=as.Date("2020-08-14"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==8 & date>=as.Date("2020-07-30") & date<=as.Date("2020-09-12"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==9 & date>=as.Date("2020-07-27") & date<=as.Date("2020-09-07"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==10 & date>=as.Date("2020-07-06") & date<=as.Date("2020-08-14"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==11 & date>=as.Date("2020-06-25") & date<=as.Date("2020-08-07"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==12 & date>=as.Date("2020-06-25") & date<=as.Date("2020-08-08"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==13 & date>=as.Date("2020-06-22") & date<=as.Date("2020-08-01"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==14 & date>=as.Date("2020-07-20") & date<=as.Date("2020-08-28"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==15 & date>=as.Date("2020-07-16") & date<=as.Date("2020-08-26"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==16 & date>=as.Date("2020-07-20") & date<=as.Date("2020-08-29"), 1, `School/Kita closures`)
  ) %>%
  mutate( # schulferien herbst
    `School/Kita closures`=ifelse(floor(id/1000/1000)==1 & date>=as.Date("2020-10-05") & date<=as.Date("2020-10-17"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==2 & date>=as.Date("2020-10-05") & date<=as.Date("2020-10-16"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==3 & date>=as.Date("2020-10-12") & date<=as.Date("2020-10-23"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==4 & date>=as.Date("2020-10-12") & date<=as.Date("2020-10-24"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==5 & date>=as.Date("2020-10-12") & date<=as.Date("2020-10-24"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==6 & date>=as.Date("2020-10-05") & date<=as.Date("2020-10-17"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==7 & date>=as.Date("2020-10-12") & date<=as.Date("2020-10-23"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==8 & date>=as.Date("2020-10-26") & date<=as.Date("2020-10-31"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==9 & date>=as.Date("2020-10-31") & date<=as.Date("2020-11-06"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==10 & date>=as.Date("2020-10-12") & date<=as.Date("2020-10-23"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==11 & date>=as.Date("2020-10-12") & date<=as.Date("2020-10-24"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==12 & date>=as.Date("2020-10-12") & date<=as.Date("2020-10-24"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==13 & date>=as.Date("2020-10-05") & date<=as.Date("2020-10-10"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==14 & date>=as.Date("2020-10-19") & date<=as.Date("2020-10-31"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==15 & date>=as.Date("2020-10-19") & date<=as.Date("2020-10-24"), 1, `School/Kita closures`),
    `School/Kita closures`=ifelse(floor(id/1000/1000)==16 & date>=as.Date("2020-10-17") & date<=as.Date("2020-10-30"), 1, `School/Kita closures`)
  ) %>%
  mutate(`School/Kita closures`=ifelse(date>=as.Date("2020-08-01") & date<=lockdownharder & `School/Kita closures`==0.5, 0, `School/Kita closures`)) %>%
  mutate(Interventions=`Ban of mass gatherings`+`School/Kita closures`+`Contact restrictions`+`Mandatory face masks`) %>%
  dplyr::select(-c(`Ban of mass gatherings`, `School/Kita closures`, `Contact restrictions`, `Mandatory face masks`))

dateset <- as.Date(Reduce(intersect, list(awareness$date, google_mobility$date, weather$date, brd_timeseries$date)),
                   origin="1970-01-01")

brd_timeseries <- brd_timeseries %>%
  mutate(id=replace(id, id==11, 11000000)) %>%
  filter(id<=11000000 | id>=12000000) %>%
  group_by(id) %>%
  mutate(`Reported new cases COVID-19`=cases-lag(cases),
         `Reported new cases COVID-19`=ifelse(`Reported new cases COVID-19`<0, 0, `Reported new cases COVID-19`),
         `Active cases`=cases-lag(cases, 14),
         `Active cases`=ifelse(`Active cases`<0, 0, `Active cases`)) %>%
  ungroup() %>%
  filter(date >= min(dateset)) %>%
  mutate(daycount=as.integer(date-min(dateset)),
         `Weekday (report)`=as.character(paste0(wday(date, week_start=5), wday(date, week_start=1, label=TRUE))),
         `Holiday (report)`=ifelse(date %in% germanholidays, 1, 0),
         bl_id=floor(id/1000/1000)) %>%
  filter(id>16)

google_mobility <- google_mobility %>% filter(date %in% dateset)
weather <- weather %>% filter(date %in% dateset)
awareness <- awareness %>% filter(date %in% dateset)

google_mobility <- google_mobility %>%
  mutate(`Mobility (mean)` = rowMeans(google_mobility %>% dplyr::select(contains("Mobility")), na.rm = TRUE))

enddate <- as.Date("2021-01-25") # as.Date("2020-12-16") # max(brd_timeseries$date) # 

startdate <- enddate - days(100)# as.Date("2020-07-16") # +max(mylags)

for (mylag in mylags) {
  lagweather <- mylag
  lagmobility <- mylag
  lagawareness <- mylag
  lagrestrict <- mylag
  
  brd_burden <- brd_timeseries %>% mutate(date=date+mylag, `COVID-19 burden`=`Reported new cases COVID-19`) %>% dplyr::select(id, date, `COVID-19 burden`)
  
  modeldata_raw <- left_join(brd_timeseries, google_mobility %>%
                               mutate(date=date+lagmobility,
                                      `Weekday (exposure)`=as.character(paste0(wday(date, week_start=1), wday(date, week_start=1, label=TRUE)))),
                             by=c('date'='date', 'bl_id'='id')) %>%
    left_join(., weather %>% mutate(date=date+lagweather), by=c('date'='date', 'id'='id')) %>%
    left_join(., awareness %>% mutate(date=date+lagawareness), by=c('date'='date', 'bl_id'='BL_ID')) %>%
    left_join(., measures_zeroone_all %>% mutate(date=date+lagrestrict), by=c('date'='date', 'id'='id')) %>%
    left_join(., inkar, by=c("id"="id")) %>%
    left_join(., pflege_destatis, by=c("id"="id")) %>%
    left_join(., brd_burden, by=c('date'='date', 'id'='id')) %>%
    filter(id>16 & date>=startdate & date<=enddate) %>%
    rename(# `School and kindergarten closures`=`School/Kita closures`,
      `Holiday (exposure)`=Holiday,
      `Foreign citizens`=`Foreign residents`,
      `Foreign citizens (refugees)`=`Foreign residents (refugees)`,
      `Rainfall`=`Weather (rainfall)`,
      `Humidity`=`Weather (humidity)`,
      `Gender`=`Sex`,
      `Temperature`=`Weather (temperature)`,
      `Wind`=`Weather (wind)`) %>%
    filter(date<=enddate) %>%
    mutate(dummy = 1) %>% # column with single value
    pivot_wider(
      names_from = "Weekday (exposure)", # column to spread
      names_prefix = "Weekday ",
      values_from = dummy,
      values_fill = 0
    ) %>%
    dplyr::select(-"Weekday (report)", -"Weekday 4Do", -contains("iso"), -contains("census"))
  
  modeldata_X <- modeldata_raw %>%
    dplyr::select(-c(id, bl_id, date, cases, deaths, recovered, `Reported new cases COVID-19`, `Active cases`, daycount, `Mobility (mean)`))
  
  modeldata_X_cont <- modeldata_X %>% dplyr::select(`Mobility (retail and recreation)`, `Mobility (grocery and pharmacy)`,
                                                    `Mobility (parks)`, `Mobility (transit stations)`,
                                                    `Mobility (workplaces)`, `Mobility (residential)`,
                                                    Rainfall, Temperature, Humidity, Wind,
                                                    `Searches corona`,
                                                    `Socio-economic status`, `Age (pop. 65 and older)`,
                                                    `Age (pop. younger 18)`, `Foreign citizens`,
                                                    `Foreign citizens (refugees)`, Turnout,
                                                    `Right-wing populist party votes`, `Population density`,
                                                    Gender, `Nursing homes`,
                                                    `COVID-19 burden`)
  scale_params <- tibble(variable=colnames(modeldata_X_cont),
                         mymean=colMeans(modeldata_X_cont),
                         mysd=apply(modeldata_X_cont, 2, sd))
  modeldata_X_cont_scaled <- sapply(seq(dim(modeldata_X_cont)[2]),
                                    function(i) scale(modeldata_X_cont[, i], scale_params[i, 2], 2*scale_params[i, 3])) # gelman
  colnames(modeldata_X_cont_scaled) <- colnames(modeldata_X_cont)
  modeldata_X_cont_scaled <- as_tibble(modeldata_X_cont_scaled)
  modeldata_X_bin <- modeldata_X %>% dplyr::select(-colnames(modeldata_X_cont))
  
  modeldata_scaled <- bind_cols(modeldata_X_bin,
                                modeldata_X_cont_scaled)
  
  pca_mobility <- prcomp(modeldata_scaled %>% dplyr::select(contains("Mobility")))
  cumsum(pca_mobility$sdev)/sum(pca_mobility$sdev)
  modeldata_scaled_pca_mobility <- modeldata_scaled %>% 
    dplyr::select(-contains("Mobility")) %>%
    bind_cols("Mobility (PC1)"=pca_mobility$x[,1],
              "Mobility (PC2)"=pca_mobility$x[,2],
              "Mobility (PC3)"=pca_mobility$x[,3],
              "Mobility (PC4)"=pca_mobility$x[,4],
              "Mobility (PC5)"=pca_mobility$x[,5],
              "Mobility (PC6)"=pca_mobility$x[,6])
  
  modeldata <- modeldata_scaled_pca_mobility %>%
    bind_cols(modeldata_raw %>% dplyr::select(`Reported new cases COVID-19`, `Active cases`))
  
  myformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))-`Active cases`+."
  myglm <- glm.nb(as.formula(myformula),
                  data=modeldata) 
  myglms[[mylag]] <- myglm
  
  myaic[mylag] <- AIC(myglm)

}

myaic
