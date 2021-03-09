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
tablepath <- "manuscript/v2_medrxiv/tables/"

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
modeldata <- read_csv("data/Modeldata_scaled_pcamobility.csv")

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
for (myweather in c("Temperature", "Rainfall", "Humidity", "Wind")) {
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


# tidy_dag <- tidy_dagitty(dag) %>% mutate(name=str_wrap(name,width = 10)) 
# ggdag(tidy_dag,text_size = 1.5,color="grey",
#       edge_type = "link",node_size = 13,label_size = 1.0) + 
#   ggdag::theme_dag_blank(base_size = 4)
# 
# tidy_dag %>% 
#   ggplot(aes(x = x, y = y, xend = xend, yend = yend))+
#   geom_dag_edges_diagonal(check_overlap = TRUE) +
#   geom_dag_point(size=10,color="lightgrey") +
#   geom_dag_text(col = "black",size=1.2) +
#   theme_solid()

# library(igraph)
# data <- dagitty_to_adjmatrix(dag)
# network <- graph_from_adjacency_matrix(data , mode='directed', diag=FALSE)
# 
# plot(network, layout=layout.sphere, main="sphere")
# plot(network, layout=layout.circle, main="circle")
# plot(network, layout=layout.random, main="random")
# plot(network, layout=layout.fruchterman.reingold,
#      vertex.size=10,
#      edge.arrow.size=0.5,
#      margin=-0.5,
#      vertex.label.cex=0.5)