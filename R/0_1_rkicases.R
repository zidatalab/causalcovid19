library(tidyverse)
library(lubridate)

  # Config Params
  baseurl = "https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?"
  paramsforids = "where=1%3D1&outFields=*&returnGeometry=false&returnDistinctValues=true&returnIdsOnly=true&outSR=4326&f=json"
  otherparamsforurl = "&outFields=*&outSR=4326&f=json"
  
  # Get objects
  mytemp = tempfile()
  myjson = paste0(baseurl,paramsforids)
  download.file(myjson, mytemp, quiet=TRUE)
  objectids <- jsonlite::fromJSON(mytemp, flatten = TRUE)
  myfieldname <- objectids$objectIdFieldName
  myids <- sort(objectids$objectIds)
  batchsize <- 4000
  myidsindexlist <- seq(1,length(myids),batchsize)
  
  # Query Objects
  resultdf <- tibble()
  
  # Query all batches except last one
  for (myid in seq(1,length(myidsindexlist),1)){
    if (myid < length(myidsindexlist)) {
      mytemp = tempfile()
      myjson = paste0(baseurl,"where=","(",myfieldname,">=",myids[myidsindexlist[myid]],")","AND","(",myfieldname,"<",myids[myidsindexlist[myid+1]],")",otherparamsforurl)
      download.file(myjson, mytemp, quiet=TRUE)
      myresult <- jsonlite::fromJSON(mytemp, flatten = TRUE)
    }
    myresult <- as.data.frame(myresult$features)
    resultdf <- bind_rows(resultdf,myresult)
  }
  
  # Query last batch
  mytemp = tempfile()
  myjson = paste0(baseurl,"where=",myfieldname,">=",myids[myidsindexlist[length(myidsindexlist)]],otherparamsforurl)
  download.file(myjson, mytemp, quiet=TRUE)
  myresult <- jsonlite::fromJSON(mytemp, flatten = TRUE)
  myresult <- as.data.frame(myresult$features)
  resultdf <- bind_rows(resultdf,myresult)
  
  # Remove name prefix
  newnames <- str_remove(colnames(resultdf),"attributes.")
  names(resultdf) <- newnames
  resultdf <- resultdf %>%
    mutate(Meldedatum= as.character(date(as_datetime(Meldedatum/1000))),
           Refdatum= as.character(date(as_datetime(Refdatum/1000))),
           Datenstand=as.character(as.Date(Datenstand, format="%d.%m.%Y")))
  
  localrki <- resultdf
  
  rkitimeframe <- localrki %>% summarise(mindate=min(date(Meldedatum)),maxdate=max(date(Meldedatum)))
  rkidays <- date(rkitimeframe$maxdate)-date(rkitimeframe$mindate)
  rkidates = date(now())-seq(1,rkidays+1)
  
  Kreise <- localrki %>%  mutate(date=date(Meldedatum)) %>%
    rename(id=IdLandkreis,cases=AnzahlFall,deaths=AnzahlTodesfall,recovered=AnzahlGenesen) %>%
    select(id,date,cases,deaths,recovered) %>% group_by(id,date) %>%
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
    select(id,date,cases,deaths,recovered) %>% group_by(id,date) %>%
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
    select(date,cases,deaths,recovered) %>% group_by(date) %>%
    summarise(id=0,cases=sum(cases,na.rm=T),deaths=sum(deaths,na.rm=T),recovered=sum(recovered,na.rm=T)) %>%
    arrange(id,date) %>% select(id,date,cases,deaths,recovered) %>% mutate(date=as.character(date))
  
  Bund <- expand.grid("id"=unique(Bund$id),"date"=rkidates) %>% as_tibble(.) %>%
    arrange(id,date) %>% mutate(date=as.character(date)) %>%
    left_join(Bund,by=c("id","date")) %>%
    mutate(cases=ifelse(is.na(cases),0,cases),
           deaths=ifelse(is.na(deaths),0,deaths),
           recovered=ifelse(is.na(recovered),0,recovered))
  
  brddata.db <- bind_rows(Bund,Laender,Kreise) %>% group_by(id) %>% arrange(id,date) %>%
    mutate(cases=cumsum(cases),deaths=cumsum(deaths),recovered=cumsum(recovered))
  
write_csv(brddata.db, "./data/rki_cases/brd_timeseries.csv")
