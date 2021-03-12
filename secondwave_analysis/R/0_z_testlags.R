mylags <- 1:20

library(lubridate)
library(tidyverse)
library(DBI)
library(broom)
library(MASS)

mydatapath <- "./data/" 

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
                            "2020-06-01")) # pfingstmontag
nobigevents <- as.Date("2020-03-10")
noschool <- as.Date("2020-03-16")
contactrestrict <- as.Date("2020-03-22")
measures_zeroone <- tibble(date=as.Date("2020-01-01")+0:360) %>%
  mutate(`Ban of mass gatherings`=ifelse(date<nobigevents, 0, 1),
         `School/Kita closures`=ifelse(date<noschool, 0, 1),
         `Contact restrictions`=ifelse(date<contactrestrict, 0, 1),
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
  )

dateset <- as.Date(Reduce(intersect, list(awareness$date, google_mobility$date, weather$date, brd_timeseries$date)),
                   origin="1970-01-01")

for (mylag in mylags) {
  
  cat("\n mylag =", mylag, "\n")
  
  startdate <- min(dateset)+mylag
  enddate <- max(brd_timeseries$date)
  
  lagweather <- mylag
  lagmobility <- mylag
  lagawareness <- mylag
  lagrestrict <- mylag
  
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
  brd_burden <- brd_timeseries %>% mutate(date=date+mylag, `COVID-19 burden`=`Reported new cases COVID-19`) %>% dplyr::select(id, date, `COVID-19 burden`)
  google_mobility <- google_mobility %>% filter(date %in% dateset)
  # rki_mobility <- rki_mobility %>% filter(date %in% dateset)
  weather <- weather %>% filter(date %in% dateset)
  awareness <- awareness %>% filter(date %in% dateset)
  
  google_mobility <- google_mobility %>%
    mutate(`Mobility (mean)` = rowMeans(google_mobility %>% dplyr::select(contains("Mobility")), na.rm = TRUE))
  
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
    filter(id>16 & date>=startdate & date<=enddate)# %>% drop_na()
  modeldata_X <- modeldata_raw %>%
    dplyr::select(-c(id, bl_id, date, cases, deaths, recovered, `Reported new cases COVID-19`, `Active cases`, daycount)) %>%
    mutate_if(is.numeric, scale, center=TRUE, scale=FALSE)
  modeldata <- bind_cols(modeldata_X, modeldata_raw %>%
                           dplyr::select(c(id, bl_id, date, cases, deaths, recovered, `Reported new cases COVID-19`, `Active cases`, daycount)))
  
  library(lubridate)
  library(tidyverse)
  library(DBI)
  library(broom)
  library(MASS)
  library(dagitty)
  library(ggdag)
  library(ggthemes)
  library(pcalg)
  library(pscl)
  
  # load dag
  dagfile <- "./data/bigdag4covid19.txt"
  dagtext <- readChar(dagfile, file.info(dagfile)$size)
  dag <- dagitty::dagitty(dagtext)
  
  unobserved <- c("Access to tests", 
                  "Tracing capacity",
                  "Health care utilization",
                  "Herd immunity",
                  "Onset of COVID-19",
                  "Trust in society",
                  "Susceptibility")
  
  # Load Data
  modeldata <- modeldata %>%
    dplyr::select(-id, -bl_id, -cases, -deaths, -recovered, -daycount,
                  -`Mobility (mean)`,
                  -contains("iso"), -contains("census")
    ) %>% 
    rename(`School and kindergarten closures`=`School/Kita closures`,
           `Holiday (exposure)`=Holiday,
           `Foreign citizens`=`Foreign residents`,
           `Foreign citizens (refugees)`=`Foreign residents (refugees)`,
           `Rainfall`=`Weather (rainfall)`,
           `Humidity`=`Weather (humidity)`,
           `Gender`=`Sex`,
           `Temperature`=`Weather (temperature)`,
           `Wind`=`Weather (wind)`) %>%
    filter(date<="2020-07-08") %>%
    dplyr::select(-date)
  
  # convert dagitty to adjacency matrix
  dagitty_to_adjmatrix <- function(daggity_obj) {
    edg <- dagitty:::edges(daggity_obj)
    node_names <- dagitty:::names.dagitty(daggity_obj)
    ans_mat <- matrix(
      data = 0, nrow = length(node_names),
      ncol = length(node_names),
      dimnames = list(node_names, node_names)
    )
    
    ans_mat[as.matrix(edg[c("v", "w")])] <- 1
    return(ans_mat)
  }
  
  # function to assemble variable names
  my_vars <- function(modeldata, mynames) {
    myvars <- character()
    allvars <- colnames(modeldata)
    idx <- 1
    for (mn in mynames) {
      thesevars <- allvars[grep(mn, allvars, fixed=TRUE)]
      for (tvidx in seq(thesevars)) {
        myvars[idx] <- thesevars[tvidx]
        idx <- idx+1
      }
    }
    return(myvars)
  }
  
  # function to get optimal adjustment set without unobserved (check for validness)
  get_optadjset <- function(amat, exposure, outcome, unobserved) {
    x.pos <- which(colnames(amat)==exposure)
    y.pos <- which(colnames(amat)==outcome)
    optadjset <- colnames(amat)[optAdjSet(amat, x.pos = x.pos, y.pos = y.pos)]
    optadjset_red <- setdiff(optadjset, unobserved)
    optadjset_red_idx <- sapply(optadjset_red, function(z) which(colnames(amat)==z))
    is_valid <- gac(amat, x.pos, y.pos, optadjset_red_idx, type="dag")
    if (is_valid$gac) {
      return(list(adjset=optadjset_red, is_reduced=(length(optadjset_red)<length(optadjset))))
    } else {
      return(NULL)
    }
  }
  
  # function to get minimal and optimal adjustment sets without unobserved (check for validness)
  get_adjsets <- function(amat, exposure, outcome, unobserved) {
    x.pos <- which(colnames(amat)==exposure)
    y.pos <- which(colnames(amat)==outcome)
    minadjsets <- adjustmentSets(dag, exposure=exposure, outcome=outcome)
    valid_adjsets <- vector("list", length(minadjsets))
    for (mas in seq(minadjsets)) {
      minadjset_idx <- sapply(minadjsets[[mas]], function(z) which(colnames(amat)==z))
      is_valid <- gac(amat, x.pos, y.pos, minadjset_idx, type="dag")
      if (is_valid$gac) {
        valid_adjsets[[mas]] <- minadjsets[[mas]]
      } else {
        valid_adjsets[[mas]] <- NULL
      }
    }
    optadjset <- get_optadjset(amat, exposure, outcome, unobserved)
    adjsets <- valid_adjsets
    names(adjsets) <- paste0("MinAdjSet", seq(valid_adjsets))
    if (optadjset[[2]]) { # is a reduced set
      adjsets[["RedOptAdjSet"]] <- optadjset[[1]]
    } else { # is actually real optimal adjustment set
      adjsets[["OptAdjSet"]] <- optadjset[[1]]
    }
    return(adjsets)
  }
  
  # function for neg-binomial model based on modeldata and adjustment set from dag
  my_negbin <- function(modeldata, exposure=NULL, adjsets=NULL) {
    mynullformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))" 
    if (is.null(exposure) & is.null(adjsets)) { # adjustment set not given
      myformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))-`Active cases`+."
      nf <- 1
    } else if (is.null(exposure)+is.null(adjsets)==1) { # exactly one is different from NULL: not allowed
      myformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))-`Active cases`+."
      cat("Adjustment set AND exposure must be given. Calculate complete model.\n")
      exposure <- NULL
      adjsets <- NULL
      nf <- 1
    } else { # adjsets and exposure given
      nas <- length(adjsets)
      expovars <- my_vars(modeldata, exposure)
      nexp <- length(expovars)
      myformula <- vector("character", nas)
      for (as in seq(nas)) {
        if (is_empty(adjsets[[as]])) {
          myformula[as] <- paste("`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))+",#-`Active cases`
                                 sprintf("`%s`", paste(expovars, collapse = "`+`")),
                                 sep = "")
        } else {
          myadjset <- my_vars(modeldata, adjsets[[as]])
          myformula[as] <- paste("`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))+",#-`Active cases`
                                 sprintf("`%s`", paste(c(myadjset, expovars), collapse = "`+`")),
                                 sep = "")
        }
      }
      nf <- length(myformula)
    }
    coefficients <- vector("list", nf)
    causaleffects <- vector("list", nf)
    pseudor2s <- rep(0, nf)
    mynullglm <- glm.nb(as.formula(mynullformula),
                        data=modeldata)
    for (idxf in seq(nf)) {
      myglm <- glm.nb(as.formula(myformula[idxf]),
                      data=modeldata)
      # myglm_zi <- zeroinfl(as.formula(paste0(myformula[idxf], "| 1+offset(log(`Active cases`+1))+
      #                                     `Weekday (report)`+`Holiday (report)`")), data = modeldata)
      # pseudo RSquared
      pseudor2s[idxf] <- 1-sum((myglm$fitted.values-myglm$y)^2)/sum((mynullglm$fitted.values-mynullglm$y)^2)# 1-summary(myglm)$deviance/summary(mynullglm)$deviance
      cat("   pseudo R2:", pseudor2s[idxf], "\n")
      coefficients[[idxf]] <- tidy(myglm, exponentiate=TRUE)
      if (!is.null(exposure)) {
        effects <- tail(coefficients[[idxf]]$estimate, nexp)
        pvals <- tail(coefficients[[idxf]]$p.value, nexp)
        causaleffects[[idxf]] <- tibble(variables=expovars, estimates=effects)
      } else {
        causaleffects[[idxf]] <- coefficients[[idxf]]
      }
    }
    return(list(pseudor2s=pseudor2s, causaleffects=causaleffects))
  }
  
  # all-in-one function
  my_causal <- function(dag, modeldata, exposure, unobserved) {
    if (is.null(exposure)) {
      adjsets <- NULL
    } else {
      amat <- t(dagitty_to_adjmatrix(dag))
      adjsets <- get_adjsets(amat, exposure, outcome="Reported new cases COVID-19", unobserved)
    }
    res <- my_negbin(modeldata, exposure=exposure, adjsets=adjsets)
    res[["adjustmentsets"]] <- adjsets
    return(res)
  }
  
  # all variables
  cat("all:\n")
  res <- my_causal(dag, modeldata, exposure=NULL, unobserved)
  # mobility
  cat("mobility:\n")
  res <- my_causal(dag, modeldata, exposure="Mobility", unobserved=unobserved)
  # awareness
  cat("awareness c19 burden:\n")
  res <- my_causal(dag, modeldata, exposure="COVID-19 burden", unobserved=unobserved)
  cat("awareness searches corona:\n")
  res <- my_causal(dag, modeldata, exposure="Searches corona", unobserved=unobserved)

  # weather
  for (myweather in c("Temperature", "Rainfall", "Humidity", "Wind")) {
    cat(myweather,":\n")
    res <- my_causal(dag, modeldata, exposure=myweather, unobserved=unobserved)
  }
}

