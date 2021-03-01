library("tidyverse")
library("cowplot")

figurepath <- "./manuscript/v1_medrxiv/figures/"

modeldata_raw <- read_csv("data/Modeldata_raw.csv")

varnames <- c("Mobility (retail and recreation)",
              "Mobility (grocery and pharmacy)",
              "Mobility (parks)",
              "Mobility (transit stations)",
              "Mobility (workplaces)",
              "Mobility (residential)",
              "Weather (temperature)", "Weather (rainfall)", "Weather (humidity)", "Weather (wind)",
              "Searches corona", "COVID-19 burden") 
varnames <- factor(varnames, levels=varnames, ordered=TRUE)
vardata <- tibble(varnames=varnames, means=0, effects=0, conflow=0, confhigh=0, rangelow=0, rangehigh=0)

ranges <- apply(modeldata_raw[, varnames], 2, quantile, probs=c(0.025, 0.975))

vardata <- vardata %>%
  mutate(means=sapply(varnames, function(v) mean(modeldata_raw %>% pull(v) )),
         rangelow=sapply(varnames, function(v) range(modeldata_raw %>% pull(v) )[1]),
         rangehigh=sapply(varnames, function(v) range(modeldata_raw %>% pull(v) )[2]))
vardata$myrangelow <- ranges[1, varnames] # c(-100, -25, -0, -100, -50, -10, -10, 0, 0, 0, 0, 0)
vardata$myrangehigh <- ranges[2, varnames] # c(+50, +25, +100, +50, +0, +20, 30, 20, 100, 15, 100, 100)
vardata[1:6, 3:5] <- cbind(read_csv(paste0(figurepath, "../tables/t_effects_Mobility_MinAdjSet1.csv"))$estimates,
                        read_csv(paste0(figurepath, "../tables/t_effects_Mobility_MinAdjSet1.csv"))$conflow,
                        read_csv(paste0(figurepath, "../tables/t_effects_Mobility_MinAdjSet1.csv"))$confhigh)
vardata[11, 3:5] <- cbind(read_csv(paste0(figurepath, "../tables/t_effects_Searches corona_RedOptAdjSet.csv"))$estimates,
                      read_csv(paste0(figurepath, "../tables/t_effects_Searches corona_RedOptAdjSet.csv"))$conflow,
                      read_csv(paste0(figurepath, "../tables/t_effects_Searches corona_RedOptAdjSet.csv"))$confhigh)
vardata[12, 3:5] <- cbind(read_csv(paste0(figurepath, "../tables/t_effects_COVID-19 burden_RedOptAdjSet.csv"))$estimates,
                          read_csv(paste0(figurepath, "../tables/t_effects_COVID-19 burden_RedOptAdjSet.csv"))$conflow,
                          read_csv(paste0(figurepath, "../tables/t_effects_COVID-19 burden_RedOptAdjSet.csv"))$confhigh)
weather <- c("Temperature", "Rainfall", "Humidity", "Wind")
for (w in seq(weather)) {
  vardata[6+w, 3:5] <- cbind(read_csv(paste0(figurepath, "../tables/t_effects_", weather[w], "_RedOptAdjSet.csv"))$estimates,
                              read_csv(paste0(figurepath, "../tables/t_effects_", weather[w], "_RedOptAdjSet.csv"))$conflow,
                              read_csv(paste0(figurepath, "../tables/t_effects_", weather[w], "_RedOptAdjSet.csv"))$confhigh)
}

myf_expvseff <- function(x, mymean, myeffect) {
  return(myeffect^(x-mymean))
}

mycolors <- c("#268bd2","#dc322f","#d33682","#859900")

plotdata <- tibble(sequence=as.vector(apply(vardata[, c("myrangelow", "myrangehigh")], 1, function(myr) seq(myr[1], myr[2], length.out = 100))),
                   exposure=rep(vardata$varnames, each=100),
                   means=rep(vardata$means, each=100),
                   effects=rep(vardata$effects, each=100),
                   conflows=rep(vardata$conflow, each=100),
                   confhighs=rep(vardata$confhigh, each=100),
                   seqeffects=myf_expvseff(sequence, means, effects),
                   seqconflows=myf_expvseff(sequence, means, conflows),
                   seqconfhighs=myf_expvseff(sequence, means, confhighs),
                   mycolor=c(rep(mycolors[2], 600), rep(mycolors[3], 400), rep(mycolors[4], 200)))


myfacetplot <- plotdata %>%
  ggplot(aes(x=sequence,
             y=seqeffects*100-100)) +
  geom_ribbon(aes(ymin = seqconflows*100-100, ymax = seqconfhighs*100-100), fill = "grey70") + 
  geom_line() +
  geom_vline(aes(xintercept=means), color="darkgrey", linetype="dashed") +
  facet_wrap(vars(exposure),scales = "free", strip.position="bottom") +
  coord_cartesian(ylim = c(-50, 50)) +
  geom_line(aes(col=mycolor), size=1) +
  scale_color_manual(values=mycolors[c(3,2,4)]) +
  labs(y="relative effect") +
  theme_cowplot(font_size = 8) +
  theme(strip.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.placement = "outside") + # , strip.background = element_rect(fill="grey90")
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

myfacetplot

# tag_facet2 <- function(p, open = "", close = "", tag_pool = LETTERS, x = -Inf, y = Inf, 
#                        hjust = -0.5, vjust = 1, fontface = 2, family = "", ...) {
#   gb <- ggplot_build(p)
#   lay <- gb$layout$layout
#   tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
#   p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, vjust = vjust, 
#                 fontface = fontface, family = family, inherit.aes = FALSE)
# }

# tag_facet2(myfacetplot)

ggsave(filename = paste0(figurepath, "f_exposure_vs_effect.eps"),
       dpi=600, width=16,height = 12, unit="cm", device=cairo_ps, fallback_resolution = 600)

# ggsave(filename = paste0(figurepath,"f_exposure_vs_effect.png"),
#        dpi=600, width=16,height = 12, unit="cm", device=cairo_ps, fallback_resolution = 600)
