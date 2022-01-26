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
source("R/z_auxiliary_functions_for_2.R")

# save tables to:
tablepath <- "./manuscript/v2_medrxiv/tables/"

# load dag
dagfile <- "./data/bigdag4covid19.txt"
dagtext <- readChar(dagfile, file.info(dagfile)$size)
dag_origin <- dagitty::dagitty(dagtext)

unobserved <- c("Access to tests", 
                "Tracing capacity",
                "Health care utilization",
                "Herd immunity",
                "Onset of COVID-19",
                "Trust in society",
                "Susceptibility")

# Load Data
modeldata <- read_csv("./data/Modeldata.csv") %>%
  dplyr::select(-id, -bl_id, -cases, -deaths, -recovered, -daycount,
                -`Mobility (mean)`,
                # -`Relaxation of measures`,
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

adjsets_list <- vector("list", 3)
names(adjsets_list) <- c("original", "equivalence", "mag")

exposures <- c("Mobility", "Searches corona", "COVID-19 burden", "Temperature", "Rainfall", "Humidity", "Wind")
outcome <- "Reported new cases COVID-19"

dag_equiv <- equivalenceClass(dag_origin)
dag_mag <- toMAG(dag_origin)

# original adjustment sets
for (expo in exposures) {
  adjsets_list$original[[expo]] <- c(adjsets_list$original[[expo]], get_adjsets(dag_origin, expo, outcome, unobserved))
  adjsets_list$equivalence[[expo]] <- c(adjsets_list$equivalence[[expo]], get_adjsets(dag_equiv, expo, outcome, unobserved))
  adjsets_list$mag[[expo]] <- c(adjsets_list$mag[[expo]], get_adjsets(dag_mag, expo, outcome, unobserved))
}
