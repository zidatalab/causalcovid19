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

unobserved <- c("Access to tests", 
                "Tracing capacity",
                "Health care utilization",
                "Herd immunity",
                "Onset of COVID-19",
                "Trust in society",
                "Susceptibility")

# Load Data
modeldata <- read_csv("data/Modeldata.csv") %>%
  dplyr::select(-id, -bl_id, -cases, -deaths, -recovered, -daycount,
                -`Mobility (mean)`,
                -`Relaxation of measures`,
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

modeldata_cont <- modeldata %>%
  dplyr::select(`Mobility (retail and recreation)`, `Mobility (grocery and pharmacy)`,
         `Mobility (parks)`, `Mobility (transit stations)`,
         `Mobility (workplaces)`, `Mobility (residential)`,
         Rainfall, Temperature, Humidity, Wind,
         `Searches corona`,
         `Socio-economic status`, `Age (pop. 65 and older)`,
         `Foreign citizens`, `Foreign citizens (refugees)`,
         Turnout, `Right-wing populist party votes`,
         `Population density`, Gender,
         `Age (pop. younger 18)`, `Nursing homes`, 
         `COVID-19 burden`, `Active cases`)
myY <- log1p(modeldata$`Reported new cases COVID-19`/(1+modeldata$`Active cases`))
myYagainstcont <- bind_cols(modeldata_cont, Outcome=myY) %>%
  pivot_longer(cols=`Mobility (retail and recreation)`:`Active cases`)
ggplot(myYagainstcont, aes(x=value, y=Outcome)) +
  geom_point() +
  facet_wrap(~name, nrow = 6, scales="free_x") +
  geom_smooth()
