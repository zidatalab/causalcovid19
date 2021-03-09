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
modeldata <- bind_cols(read_csv("data/Modeldata_scaled_pcamobility.csv"),
                       read_csv("data/Modeldata_raw.csv") %>%
                         dplyr::select(`Reported new cases COVID-19`, `Active cases`))

id_daycount <- read_csv("data/Modeldata_raw.csv") %>%
  dplyr::select(id, daycount, date, bl_id) %>%
  dplyr::select(-date)
  

modeldata_cont <- modeldata %>%
  dplyr::select(-c("1Mo", "2Di", "3Mi", "5Fr", "6Sa", "7So",
                   "Holiday (exposure)", "Holiday (report)",
                   "Reported new cases COVID-19"))

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
mynullaic <- AIC(mynullglm)
devresids <- residuals(myglm, type="deviance")
library(statmod)
qresids <- qresid(myglm)

# linearity # [-1989]
scatter.smooth(predict(myglm, type="link"), devresids, col='gray')

myresidagainstcont <- bind_cols(modeldata_cont, devresids=devresids) %>%
  pivot_longer(cols=`Rainfall`:`Active cases`)
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