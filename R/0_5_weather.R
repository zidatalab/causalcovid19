# the download of weather files takes a while! ~ 1 h

library(readr)
library(tidyverse)

mydatapath <- "./data/weather/"

opendata_dwd_path <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/"

download.file(paste0(opendata_dwd_path, "KL_Tageswerte_Beschreibung_Stationen.txt"),
              paste0(mydatapath, "weatherstations.txt"))

header <- read_table2(file = paste0(mydatapath, "weatherstations.txt"),
                   col_names = FALSE,
                   n_max = 1)
weatherdata <- read_table(file = paste0(mydatapath, "weatherstations.txt"),
                    col_names = FALSE,
                    locale = locale(encoding = "WINDOWS-1252"),
                    skip = 2)
colnames(weatherdata) <- header[1, ]

extractedfiles <- c()

best_bisdatum <- last(names(sort(table(weatherdata$bis_datum))))

weatherdata <- weatherdata %>%
  filter(bis_datum==as.numeric(best_bisdatum) & von_datum <= 20200101 & Stationshoehe<1000)

for (stationid in weatherdata$Stations_id) {
  tryCatch({
    download.file(paste0(opendata_dwd_path, "tageswerte_KL_", stationid, "_akt.zip"),
                  paste0(mydatapath, "temp.zip"),
                  quiet=TRUE)
    cat("downloaded data for", stationid, "successfully\n")
    listoffilesinzip <- unzip(paste0(mydatapath, "temp.zip"),
                              list=TRUE)
    extractedfile <- unzip(paste0(mydatapath, "temp.zip"),
                           files=last(listoffilesinzip$Name),
                           exdir=paste0(mydatapath, "temp"))
    extractedfiles <- c(extractedfiles, extractedfile)
  }, error = function(e) {
    cat(stationid, "did not work\n")
  }, warning = function(w) {
    cat(stationid, "did not work\n")
  })
  
}

write_lines(extractedfiles,
          paste0(mydatapath, "workingstations.txt"))

allweatherdata <- tibble()

for (fn in extractedfiles) {
  allweatherdata <- rbind(allweatherdata,
                          read_delim(fn, ";",
                                     escape_double = FALSE,
                                     locale = locale(encoding = "WINDOWS-1252"), 
                                     trim_ws = TRUE))
  unlink(fn)
}

allweatherdata_red <- allweatherdata %>%
  filter(MESS_DATUM>=20200000)

write_csv(allweatherdata_red,
          paste0(mydatapath, "allweatherdata_red.csv"))

weatherdata <- weatherdata %>% mutate(stations_id_num=as.numeric(Stations_id))

weatherdata_compl <- left_join(allweatherdata_red, weatherdata, by=c("STATIONS_ID"="stations_id_num"))

novalues <- weatherdata_compl %>% group_by(STATIONS_ID) %>% summarise(nafm=sum(FM==-999)/n(),
                                                                      natmk=sum(TMK==-999)/n(),
                                                                      naupm=sum(UPM==-999)/n(),
                                                                      narsk=sum(RSK==-999)/n()) %>%
  mutate(sumnovals=nafm+natmk+narsk+naupm)
# --> often either wind or temp/rainf/humid at the stations...
stations_fm <- novalues %>% filter(nafm<1) %>% pull(STATIONS_ID)
stations_rest <- novalues %>% filter(natmk<1) %>% pull(STATIONS_ID)
write_csv(weatherdata_compl,
          paste0(mydatapath, "allweatherdata.csv"))

library("RCurl") # fetch data
library("tidyverse") # data manipulation
library("sf") # simple features fp
library("rmapshaper") # simplify shapefiles

mydatapath <- "./data/weather/"

### Download SHP for Germany on "Kreis" level
# tempdata <- tempdir()
# download.file("https://daten.gdz.bkg.bund.de/produkte/vg/vg1000_ebenen_0101/aktuell/vg1000_01-01.utm32s.shape.ebenen.zip",paste0(tempdata,"vg1000.zip"), method="libcurl", mode = "wb")
# unzip(paste0(tempdata,"vg1000.zip"), exdir=tempdata, junkpaths = TRUE,
#       files=
#         c("vg1000_2019-01-01.utm32s.shape.ebenen/vg1000_ebenen/VG1000_KRS.cpg",
#           "vg1000_2019-01-01.utm32s.shape.ebenen/vg1000_ebenen/VG1000_KRS.dbf",
#           "vg1000_2019-01-01.utm32s.shape.ebenen/vg1000_ebenen/VG1000_KRS.prj",
#           "vg1000_2019-01-01.utm32s.shape.ebenen/vg1000_ebenen/VG1000_KRS.shp",
#           "vg1000_2019-01-01.utm32s.shape.ebenen/vg1000_ebenen/VG1000_KRS.shx"))
# KRS  <- st_read(paste0(tempdata,"/VG1000_KRS.shp"))
# file.remove(paste0(tempdata,c("/VG1000_KRS.shp","/VG1000_KRS.dbf","/VG1000_KRS.prj","/VG1000_KRS.shx","/VG1000_KRS.cpg")))
## quelle angeben!!!
# write_sf(KRS, paste0(mydatapath, "../shp/kreise.shp"))
KRS <- read_sf(paste0(mydatapath, "../shp/kreise.shp"))

# wetterstationen
wetterdata <- read_csv(paste0(mydatapath, "allweatherdata.csv")) %>%
  replace(.==-999, NA)
stationen_sf_fm <- st_as_sf(wetterdata %>%
                           dplyr::select(Stationshoehe, STATIONS_ID, geoBreite, geoLaenge) %>%
                           distinct() %>% filter(STATIONS_ID %in% stations_fm),
                         coords = c("geoLaenge", "geoBreite"),
                         crs=4326)
stationen_sf_rest <- st_as_sf(wetterdata %>%
                              dplyr::select(Stationshoehe, STATIONS_ID, geoBreite, geoLaenge) %>%
                              distinct() %>% filter(STATIONS_ID %in% stations_rest),
                            coords = c("geoLaenge", "geoBreite"),
                            crs=4326)
stationen_sf_fm <- stationen_sf_fm %>%
  st_transform(25832) 
stationen_sf_rest <- stationen_sf_rest %>%
  st_transform(25832) 

# plots

ggplot(KRS, aes(fill=SN_L))+geom_sf() # 

REG <- KRS %>%
  group_by(RS) %>%
  count() %>%
  ungroup()

# plot(st_geometry(REG))
# plot(st_geometry(st_centroid(REG)), pch = 3, col = 'green', add = TRUE)
# plot(st_geometry(stationen_sf), pch = 3, col = 'red', add = TRUE)

ggplot(REG) + geom_sf(aes(fill=n))

ggplot(REG) + geom_sf() + 
  geom_sf(data=st_geometry(st_centroid(REG)), size=1, shape=20, color="darkred") +
  geom_sf(data=st_geometry(stationen_sf), size=1, shape=10, color="blue")

kreiswetter_wind <- tibble()
kreiswetter_wind_na <- tibble()
kreiswetter_rest <- tibble()
kreiswetter_rest_na <- tibble()

for (k in REG$RS) {
  kmp <- st_geometry(st_centroid(REG %>% filter(RS==k))) # get midpoint of kreis
  # find three nearest stations:
  ## wind:
  station1_idx <- st_nearest_feature(kmp, stationen_sf_fm)
  station1_id <- stationen_sf_fm$STATIONS_ID[station1_idx]
  station2_idx <- st_nearest_feature(kmp, stationen_sf_fm[-station1_idx, ])
  station2_id <- stationen_sf_fm[-station1_idx, ]$STATIONS_ID[station2_idx]
  station3_idx <- st_nearest_feature(kmp, stationen_sf_fm[-station1_idx, ][-station2_idx, ])
  station3_id <- stationen_sf_fm[-station1_idx, ][-station2_idx, ]$STATIONS_ID[station3_idx]
  # test for na values
  weather_wind_na <- wetterdata %>%
    filter(STATIONS_ID %in% c(station1_id, station2_id, station3_id)) %>%
    group_by(MESS_DATUM) %>% 
    summarise(Wind=sum(is.na(FM))) %>%
    mutate(RS=k)
  kreiswetter_wind_na <- rbind(kreiswetter_wind_na, weather_wind_na)
  # filter wetterdata for these station and aggregate for mean-weather by day
  weather3_wind <- wetterdata %>%
    filter(STATIONS_ID %in% c(station1_id, station2_id, station3_id)) %>%
    group_by(MESS_DATUM) %>% 
    summarise(Wind=mean(FM, na.rm=TRUE)) %>%
    mutate(RS=k)
  kreiswetter_wind <- rbind(kreiswetter_wind, weather3_wind)
  
  ## rest (temp, rainf, humid, pressure):
  station1_idx <- st_nearest_feature(kmp, stationen_sf_rest)
  station1_id <- stationen_sf_rest$STATIONS_ID[station1_idx]
  station2_idx <- st_nearest_feature(kmp, stationen_sf_rest[-station1_idx, ])
  station2_id <- stationen_sf_rest[-station1_idx, ]$STATIONS_ID[station2_idx]
  station3_idx <- st_nearest_feature(kmp, stationen_sf_rest[-station1_idx, ][-station2_idx, ])
  station3_id <- stationen_sf_rest[-station1_idx, ][-station2_idx, ]$STATIONS_ID[station3_idx]
  # test for na values
  weather_rest_na <- wetterdata %>%
    filter(STATIONS_ID %in% c(station1_id, station2_id, station3_id)) %>%
    group_by(MESS_DATUM) %>% 
    summarise(Niederschlag=sum(is.na(RSK)), Temperatur=sum(is.na(TMK)), Feuchtigkeit=sum(is.na(UPM)), Luftdruck=sum(is.na(PM))) %>%
    mutate(RS=k)
  kreiswetter_rest_na <- rbind(kreiswetter_rest_na, weather_rest_na)
  # filter wetterdata for these station and aggregate for mean-weather by day
  weather3_rest <- wetterdata %>%
    filter(STATIONS_ID %in% c(station1_id, station2_id, station3_id)) %>%
    group_by(MESS_DATUM) %>% 
    summarise(Niederschlag=mean(RSK, na.rm=TRUE), Temperatur=mean(TMK, na.rm=TRUE), Feuchtigkeit=mean(UPM, na.rm=TRUE), Luftdruck=mean(PM, na.rm=TRUE)) %>%
    mutate(RS=k)
  kreiswetter_rest <- rbind(kreiswetter_rest, weather3_rest)
}

kreiswetter <- left_join(kreiswetter_rest, kreiswetter_wind, by=c("MESS_DATUM"="MESS_DATUM", "RS"="RS"))

write_csv(kreiswetter, paste0(mydatapath, "mittleres_kreiswetter.csv"))

kreise_shape_wetter <- left_join(kreiswetter, REG, by="RS")

ggplot(st_as_sf(kreise_shape_wetter %>% filter(MESS_DATUM==20210124)), aes(fill=Temperatur)) + geom_sf()
