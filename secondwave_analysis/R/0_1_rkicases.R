library(tidyverse)
library(lubridate)

csvurl <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
mytemp <- tempfile()
options(timeout=300)
download.file(csvurl, mytemp, quiet=TRUE)
myresult <- read_csv(mytemp)
localrki <- myresult %>%
  mutate(Meldedatum= as.character(as_date(Meldedatum)),
         Refdatum= as.character(as_date(Refdatum)),
         Datenstand=as.character(as.Date(Datenstand, format="%d.%m.%Y")))
  
  rkitimeframe <- localrki %>% summarise(mindate=min(date(Meldedatum)),maxdate=max(date(Meldedatum)))
  rkidays <- date(rkitimeframe$maxdate)-date(rkitimeframe$mindate)
  rkidates = date(now())-seq(1,rkidays+1)
  
  Kreise <- localrki %>%  mutate(date=date(Meldedatum)) %>%
    rename(id=IdLandkreis,cases=AnzahlFall,deaths=AnzahlTodesfall,recovered=AnzahlGenesen) %>%
    dplyr::select(id,date,cases,deaths,recovered) %>% group_by(id,date) %>%
    summarise(cases=sum(cases,na.rm=T),deaths=sum(deaths,na.rm=T),recovered=sum(recovered,na.rm=T)) %>%
    ungroup() %>% mutate(id=as.integer(id)*1000) %>%
    filter(!is.na(id)) %>% arrange(id,date)
  
  Kreise <- expand.grid("id"=unique(Kreise$id),"date"=rkidates) %>% as_tibble(.) %>%
    arrange(id,date) %>%
    left_join(Kreise,by=c("id","date")) %>%
    mutate(cases=ifelse(is.na(cases),0,cases),
           deaths=ifelse(is.na(deaths),0,deaths),
           recovered=ifelse(is.na(recovered),0,recovered)) %>%
    mutate(date=as.character(date))
  
  Laender <- localrki %>% mutate(date=date(Meldedatum)) %>%
    rename(id=IdBundesland,cases=AnzahlFall,deaths=AnzahlTodesfall,recovered=AnzahlGenesen) %>%
    dplyr::select(id,date,cases,deaths,recovered) %>% group_by(id,date) %>%
    summarise(cases=sum(cases,na.rm=T),deaths=sum(deaths,na.rm=T),recovered=sum(recovered,na.rm=T)) %>%
    filter(id>0) %>% arrange(id,date) %>%
    mutate(date=as.character(date))
  
  Laender <- expand.grid("id"=unique(Laender$id),"date"=rkidates) %>% as_tibble(.) %>%
    arrange(id,date) %>% mutate(date=as.character(date)) %>%
    left_join(Laender,by=c("id","date")) %>%
    mutate(cases=ifelse(is.na(cases),0,cases),
           deaths=ifelse(is.na(deaths),0,deaths),
           recovered=ifelse(is.na(recovered),0,recovered))
  
  Bund <-localrki %>% mutate(date=date(Meldedatum)) %>%
    rename(cases=AnzahlFall,deaths=AnzahlTodesfall,recovered=AnzahlGenesen) %>%
    dplyr::select(date,cases,deaths,recovered) %>% group_by(date) %>%
    summarise(id=0,cases=sum(cases,na.rm=T),deaths=sum(deaths,na.rm=T),recovered=sum(recovered,na.rm=T)) %>%
    arrange(id,date) %>% 
    dplyr::select(id,date,cases,deaths,recovered) %>% mutate(date=as.character(date))
  
  Bund <- expand.grid("id"=unique(Bund$id),"date"=rkidates) %>% as_tibble(.) %>%
    arrange(id,date) %>% mutate(date=as.character(date)) %>%
    left_join(Bund,by=c("id","date")) %>%
    mutate(cases=ifelse(is.na(cases),0,cases),
           deaths=ifelse(is.na(deaths),0,deaths),
           recovered=ifelse(is.na(recovered),0,recovered))
  
  brddata.db <- bind_rows(Bund,Laender,Kreise) %>% group_by(id) %>% arrange(id,date) %>%
    mutate(cases=cumsum(cases),deaths=cumsum(deaths),recovered=cumsum(recovered))
  
write_csv(brddata.db, "./data/rki_cases/brd_timeseries.csv")
