library("tidyverse")
library("cowplot")

figurepath <- "./manuscript/v1_medrxiv/figures/"

Modeldata <- read_csv("data/Modeldata_raw.csv")
Sys.setlocale("LC_TIME", "C")
mybreaks <- as.Date("2020-02-15")+seq(0, 144, length.out = 5)

mycolors <- c("#268bd2","#dc322f","#d33682","#859900")

cases <- Modeldata %>% dplyr::select(date, `Reported new cases COVID-19`, id) %>%
  ggplot(aes(x=date,
             y=`Reported new cases COVID-19`)) + 
  geom_point(aes(group=id), color="lightblue", stroke=0, alpha=0.2) +
  geom_smooth(color=mycolors[1],size=2,se = F) + 
  scale_x_date(breaks=mybreaks, limits=c(min(mybreaks), max(mybreaks)), date_labels = "%d %b") +
  labs(x="Date", y="new cases") + 
  scale_y_log10() +
  theme(axis.title.x = element_blank()) +
  theme_cowplot(font_size = 8)

logrates <- Modeldata %>% dplyr::select(date, `Reported new cases COVID-19`, `Active cases`, id) %>%
  ggplot(aes(x=date,
             y=`Reported new cases COVID-19`/(`Active cases`+1))) + 
  geom_point(aes(group=id), color="lightblue", stroke=0, alpha=0.2) +
  geom_smooth(color=mycolors[1], size=2, se = FALSE) + 
  scale_x_date(breaks=mybreaks, limits=c(min(mybreaks), max(mybreaks)), date_labels = "%d %b") +
  labs(x="Date", y="new cases/active cases") + 
  scale_y_log10() +
  theme(axis.title.x = element_blank()) +
  theme_cowplot(font_size = 8)

plot_grid(cases, logrates, labels = "AUTO",label_size = 12)

ggsave(filename = paste0(figurepath, "f_timeseries_cases_and_lograte.eps"),
       dpi=600, width=15,height = 7, unit="cm", device=cairo_ps, fallback_resolution = 600)

# ggsave(filename = paste0(figurepath, "f_timeseries_cases_and_lograte.png"),
#        dpi=600, width=15,height = 7, unit="cm", device=cairo_ps, fallback_resolution = 600)
