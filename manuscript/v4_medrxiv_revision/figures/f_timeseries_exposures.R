library("tidyverse")
library("cowplot")

figurepath <- "manuscript/v4_medrxiv_revision/figures/"

Modeldata <- read_csv("data/Modeldata_raw.csv")
Sys.setlocale("LC_TIME", "C")
mybreaks <- as.Date("2020-02-20")+seq(0, 139, length.out = 5)

mycolors <- c("#268bd2","#dc322f","#d33682","#859900")
cases <- Modeldata %>% dplyr::select(date,contains("cases"), id) %>%
    ggplot(aes(x=date,
                       y=`Reported new cases COVID-19`/(`Active cases`+1)))+
  geom_point(aes(group=id), color="lightblue", stroke=0, alpha=0.2) +
  geom_smooth(color=mycolors[1],size=2,se = F) + 
  scale_x_date(breaks=mybreaks, limits=c(min(mybreaks), max(mybreaks)), date_labels = "%d %b") + labs(x="Date", y="new cases/active cases") + 
  scale_y_log10() + 
  theme(axis.title.x = element_blank()) +
  theme_cowplot(font_size = 8)

temperature <- Modeldata %>% dplyr::select(date,id, contains("Temperature")) %>%
  ggplot(aes(x= date-5,
             y= `Temperature`))+
  geom_point(aes(group=id), color="moccasin", stroke=0, alpha=0.2) +
  geom_smooth(color=mycolors[2],size=2,se = F) + 
  scale_x_date(breaks=mybreaks, limits=c(min(mybreaks), max(mybreaks)+2), date_labels = "%d %b") + labs(x="Date", y="Temperature") + 
  scale_y_continuous() + 
  theme(axis.title.x = element_blank())+
  theme_cowplot(font_size = 8)

awareness <- Modeldata %>% dplyr::select(date, id, `Searches corona`) %>%
  ggplot(aes(x=date-5,
             y= `Searches corona`)) +
  geom_point(aes(group=id), color="lightpink", stroke=0, alpha=0.2) +
  geom_smooth(color=mycolors[3],size=2,se = F) + 
  scale_x_date(breaks=mybreaks, limits=c(min(mybreaks), max(mybreaks)), date_labels = "%d %b") + labs(x="Date", y="Awareness") + 
  scale_y_continuous(limits = c(0,100))+
  labs(y="Awareness") +
  theme(axis.title.x = element_blank())+
  theme_cowplot(font_size = 8)

mobility <- Modeldata %>% dplyr::select(date, id, `Mobility (retail and recreation)`) %>% # Mobility (residential)
  ggplot(aes(x=date-5,
             y= `Mobility (retail and recreation)`)) + # Mobility (residential)
  geom_point(aes(group=id), color="darkseagreen1", stroke=0, alpha=0.2) +
  geom_smooth(color=mycolors[4],size=2,se = F) + 
  scale_x_date(breaks=mybreaks, limits=c(min(mybreaks), max(mybreaks)+2), date_labels = "%d %b") + labs(x="Date", y="Mobility (retail/recreation)") + 
  theme(axis.title.x = element_blank())+
  theme_cowplot(font_size = 8)
  
myplotgrid <- plot_grid(cases,mobility,awareness,temperature, labels = "AUTO",label_size = 12)

ggsave(filename = paste0(figurepath, "f_timeseries_exposures.eps"),
       plot=myplotgrid,
       dpi=600, width=15,height = 10, unit="cm", device=cairo_ps, fallback_resolution = 600)

# ggsave(filename = paste0(figurepath, "f_timeseries_exposures.png"),
#        dpi=600, width=15,height = 10, unit="cm", device=cairo_ps, fallback_resolution = 600)
