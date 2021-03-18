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
tablepath <- "manuscript/v4_medrxiv_revision/tables/secondwave/"

# load dag
dagfile <- "secondwave_analysis/data/bigdag4covid19.txt"
dagtext <- readChar(dagfile, file.info(dagfile)$size)
dag <- dagitty::dagitty(dagtext)

unobserved <- c("Access to tests", 
                "Tracing capacity",
                "Health care utilization",
                "Exposure to SARS-COV-2",
                "Herd immunity",
                "Onset of COVID-19",
                "Trust in society",
                "Susceptibility")

# Load Data
modeldata <- read_csv("secondwave_analysis/data/Modeldata_scaled_pcamobility.csv")

# all variables
res <- my_causal(dag, modeldata, exposure=NULL, unobserved)

file.create(paste0(tablepath, "t_pseudor2s.csv"))
file.create(paste0(tablepath, "t_aics.csv"))
# mobility
res <- my_causal(dag, modeldata, exposure="Mobility", unobserved=unobserved)
write_cause(res, exposure="Mobility")

# awareness
res <- my_causal(dag, modeldata, exposure="COVID-19 burden", unobserved=unobserved)
write_cause(res, exposure="COVID-19 burden")
res <- my_causal(dag, modeldata, exposure="Searches corona", unobserved=unobserved)
write_cause(res, exposure="Searches corona")

# weather
for (myweather in c("Temperature", "Rainfall", "Humidity", "Wind")) {
  res <- my_causal(dag, modeldata, exposure=myweather, unobserved=unobserved)
  write_cause(res, exposure=myweather)
}

# other variables
othervariables <- setdiff(names(dag), c("Reported new cases COVID-19", unobserved,
                                        "Mobility", "COVID-19 burden", "Searches corona",
                                        "Temperature", "Rainfall", "Humidity", "Wind"))
for (ov in othervariables) {
  res <- my_causal(dag, modeldata, exposure=ov, unobserved=unobserved)
  write_cause(res, exposure=ov)
}
