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
  dplyr::select(-date) %>%
  mutate(dummy = 1) %>% # column with single value
  spread(
    key = "Weekday (exposure)", # column to spread
    value = dummy,
    fill = 0
  ) %>%
  dplyr::select(-"Weekday (report)", -"7Sun")

id_daycount <- read_csv("data/Modeldata.csv") %>%
  dplyr::select(id, daycount, date, bl_id) %>%
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

exposure=NULL
adjsets=NULL
myformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))-`Active cases`+."
myglm <- glm.nb(as.formula(myformula),
                  data=modeldata)
mynullformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))" 
mynullglm <- glm.nb(as.formula(mynullformula),
                    data=modeldata)
pseudor2 <- 1-sum((myglm$fitted.values-myglm$y)^2)/sum((mynullglm$fitted.values-mynullglm$y)^2)# 1-summary(myglm)$deviance/summary(mynullglm)$deviance
myaic <- AIC(myglm)
# coefficients <- tidy(myglm, exponentiate=TRUE, conf.int=TRUE, conf.level = 0.99)

devresids <- residuals(myglm, type="deviance")
library(statmod)
qresids <- qresid(myglm)[-1989]

# linearity # [-1989]
scatter.smooth(predict(myglm, type="link"), devresids, col='gray')

myresidagainstcont <- bind_cols(modeldata_cont, devresids=devresids) %>%
  pivot_longer(cols=`Mobility (retail and recreation)`:`Active cases`)
ggplot(myresidagainstcont, aes(x=value, y=devresids)) +
  geom_point() +
  facet_wrap(~name, nrow = 6, scales="free_x") +
  geom_smooth(se=FALSE)

# proper distribution
qqnorm(qresids)
qqline(qresids)
hist(qresids, freq=FALSE)
lines(density(qresids),
      lwd = 2,
      col = "red")
lines(density(rnorm(length(qresids))),
      lwd = 2,
      col = "blue")

# independence of observations
scatter.smooth(seq(length(devresids)), devresids, col='gray')
myresid_id_daycount <- bind_cols(devresids=devresids, id_daycount)
ggplot(myresid_id_daycount, aes(x=daycount, y=devresids)) +
  geom_point() +
  geom_smooth()
# ggplot(myresid_id_daycount, aes(x=reorder(as.factor(id), devresids, FUN = median), y=devresids)) +
#   geom_boxplot()
ggplot(myresid_id_daycount, aes(x=as.factor(id), y=devresids)) +
  geom_boxplot(aes(col=as.factor(bl_id)))

# multicollinearity
library(corrplot)
library(Hmisc)
corrplotscores <- rcorr(as.matrix(modeldata_cont), type="pearson")
ntests <- dim(corrplotscores$r)[1]^2-dim(corrplotscores$r)[1]
corrplot(corrplotscores$r, 
         type = "upper", #diag=FALSE,
         tl.col = "black", tl.srt = 45,
         p.mat = corrplotscores$P, sig.level = 0.01, insig = "blank")
library(car)
myvifs <- vif(myglm)
myvifs[myvifs>=5]

# pca on mobility
pca_mobi <- prcomp(modeldata %>% dplyr::select(contains("Mobility")) %>% dplyr::select(-contains("parks")))
cumsum(pca_mobi$sdev)/sum(pca_mobi$sdev)
modeldata_pca_mobi <- modeldata %>% 
  dplyr::select(-contains("Mobility")) %>%
  bind_cols(pcamobi1=pca_mobi$x[,1],pcamobi2=pca_mobi$x[,2], pcamobi3=pca_mobi$x[,3], mobiparks=modeldata$`Mobility (parks)`)
myglm_pca_mobi <- glm.nb(as.formula(myformula),
                data=modeldata_pca_mobi)
pseudor2_pca_mobi <- 1-sum((myglm_pca_mobi$fitted.values-myglm_pca_mobi$y)^2)/sum((mynullglm$fitted.values-mynullglm$y)^2)# 1-summary(myglm)$deviance/summary(mynullglm)$deviance
myaic_pca_mobi <- AIC(myglm_pca_mobi)
myvifs_pca_mobi <- vif(myglm_pca_mobi)
myvifs_pca_mobi[myvifs_pca_mobi>=5]