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
source("secondwave_analysis/R/z_auxiliary_functions_for_2.R")

# save tables to:
tablepath <- "manuscript/v4_revision/tables/secondwave_analysis/"

# load dag
dagfile <- "data/bigdag4covid19.txt"
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
modeldata <- read_csv("secondwave_analysis/data/Modeldata_scaled.csv") %>%
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
         `Indoor humidity`=`Weather (indoor humidity)`,
         `Gender`=`Sex`,
         `Temperature`=`Weather (temperature)`,
         `Wind`=`Weather (wind)`) %>%
  # filter(date<="2020-12-31") %>%
  dplyr::select(-date)

# all variables
res <- my_causal(dag, modeldata, exposure=NULL, unobserved)

file.create(paste0(tablepath, "t_pseudor2s.csv"))
# mobility
res <- my_causal(dag, modeldata, exposure="Mobility", unobserved=unobserved)
write_cause(res, exposure="Mobility")

# awareness
res <- my_causal(dag, modeldata, exposure="COVID-19 burden", unobserved=unobserved)
write_cause(res, exposure="COVID-19 burden")
res <- my_causal(dag, modeldata, exposure="Searches corona", unobserved=unobserved)
write_cause(res, exposure="Searches corona")

# weather
for (myweather in c("Temperature", "Rainfall", "Humidity", "Wind", "Indoor humidity")) {
  res <- my_causal(dag, modeldata, exposure=myweather, unobserved=unobserved)
  write_cause(res, exposure=myweather)
}

# maskenpflicht
res <- my_causal(dag, modeldata, exposure="Mandatory face masks", unobserved=unobserved)

# kontaktsperre 
res <- my_causal(dag, modeldata, exposure="Contact restrictions", unobserved=unobserved)

# school/kita closure adjustment 
res <- my_causal(dag, modeldata, exposure="School and kindergarten closures", unobserved=unobserved)


# "cluster" Pflegeheime adjustment 
res <- my_causal(dag, modeldata, exposure="Nursing homes", unobserved=unobserved)

# "cluster" Age? --> schools
res <- my_causal(dag, modeldata, exposure="Age", unobserved=unobserved)

