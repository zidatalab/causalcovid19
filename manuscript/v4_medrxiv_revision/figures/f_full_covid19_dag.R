library(tidyverse)
library(ggdag)
library(cowplot)
library(stringr)

figurepath <- "manuscript/v4_medrxiv_revision/figures/"

# Set theme
theme_set(theme_cowplot())
mycolors <- c("#268bd2","#dc322f","#d33682","#859900","grey60","lightgrey", "#ffa500")

dagfile <- "data/bigdag4covid19_plot.txt"
dagtext <- readChar(dagfile, file.info(dagfile)$size)
dag <- dagitty::dagitty(dagtext,layout = F)
tidy_dag <- tidy_dagitty(dag) %>% mutate(name=str_wrap(name,width = 10)) 

tdc <- tidy_dag %>% mutate(curves = 0,
                           curves = ifelse(name=="Age"&to=="Onset of COVID-19",
                                           -0.2,
                                           curves),
                           curves = ifelse(name=="Age"&to=="Exposure to SARS-COV-2",
                                           +0.1,
                                           curves),
                           curves = ifelse(name=="Gender"&to=="Turnout",
                                           +0.2,
                                           curves),
                           curves = ifelse(name=="Foreign\ncitizens"&to=="Right-wing populist party votes",
                                           +0.3,
                                           curves),
                           curves = ifelse(name=="Health\ncare\nutilization*"&to=="Exposure to SARS-COV-2",
                                           -0.2,
                                           curves))

tidy_dag %>% 
  group_by(name) %>% mutate(connections=n(),
                                         farbe = case_when(str_detect(name,"Mobility")~"4",
                                                         str_detect(name,"burden")~"3",
                                                         str_detect(name,"Searches")~"3",
                                                         str_detect(name,"Temperature")~"2",
                                                         str_detect(name,"Rainfall")~"2",
                                                         str_detect(name,"Humidity")~"2",
                                                         str_detect(name,"Wind")~"2",
                                                         str_detect(name,"new cases")~"1",  
                                                         str_detect(name,"Interventions")~"7", 
                                                         str_detect(name,"Tracing")~"6",
                                                         str_detect(name,"Herd")~"6",
                                                         str_detect(name,"Access")~"6",
                                                         str_detect(name,"Susceptibility")~"6",
                                                         str_detect(name,"Exposure")~"6",
                                                         str_detect(name,"Onset")~"6",
                                                         str_detect(name,"utiliz")~"6",
                                                         str_detect(name,"Trust")~"6",
                                                         TRUE~"5")) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges_arc(curvature = tdc$data$curves) +
    geom_dag_point(aes(color=farbe), show.legend = FALSE, size=19) +
    geom_dag_text(color="black", size=2) +
    theme_dag() +
    scale_y_reverse() +
    scale_color_manual(values = mycolors)
  
  ggsave(filename = paste0(figurepath, "f_full_covid19_dag.eps"),
       dpi=600, width=25,height = 25, unit="cm")

  # ggsave(filename = paste0(figurepath, "f_full_covid19_dag.png"),
  #        dpi=600, width=25,height = 25, unit="cm")
  