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
library(glmnet)
source("R/z_auxiliary_functions_for_2.R")

# Load Data
modeldata <- bind_cols(read_csv("data/Modeldata_scaled_pcamobility.csv"))

id_daycount <- read_csv("data/Modeldata_raw.csv") %>%
  dplyr::select(id, daycount, date, bl_id) %>%
  dplyr::select(-date)

# run full model (and null model)
myformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))+."
myglm <- glm.nb(as.formula(myformula),
                data=modeldata) # %>%dplyr::select(-`Mobility (PC4)`)
mynullformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))" 
mynullglm <- glm.nb(as.formula(mynullformula),
                    data=modeldata)
pseudor2 <- 1-sum((myglm$fitted.values-myglm$y)^2)/sum((mynullglm$fitted.values-mynullglm$y)^2)# 1-summary(myglm)$deviance/summary(mynullglm)$deviance
myaic <- AIC(myglm)
mynullaic <- AIC(mynullglm)

# regularized, cv
mytheta <- myglm$theta
myglm_fixtheta <- glm(as.formula(myformula),
              data=modeldata, 
              family=negative.binomial(mytheta))
myy <- modeldata$`Reported new cases COVID-19`
myx <- as.matrix(modeldata %>% dplyr::select(-`Reported new cases COVID-19`))

myfoldids <- (id_daycount %>% group_indices(id)) %% 10 + 1

set.seed(1502)
myglm_ridge_cv <- cv.glmnet(x=myx, y=myy, family = negative.binomial(mytheta),
                            offset=log(modeldata%>%pull(`Active cases`)+1),
                            standardize=FALSE,
                            alpha=0, nfolds=10, foldid = myfoldids) # 

mylambda <- myglm_ridge_cv$lambda.1se

write_csv(tibble(mytheta=mytheta, mylambda=mylambda), "data/myglmnetparams.csv")

myglm_ridge <- glmnet(myx, myy, family = negative.binomial(mytheta),
                      offset=log(modeldata%>%pull(`Active cases`)+1),
                      standardize=FALSE,
                      alpha=0, lambda=mylambda)

# residuals

devresids <- residuals(myglm, type="deviance")
# library(statmod)
# qresids <- qresid(myglm_ridge)

# linearity
scatter.smooth(predict(myglm, type="link"), devresids, col='gray')

modeldata_cont <- modeldata %>%
  dplyr::select(-c("Weekday 1Mo", "Weekday 2Di", "Weekday 3Mi", "Weekday 5Fr", "Weekday 6Sa", "Weekday 7So",
                   "Holiday (exposure)", "Holiday (report)",
                   "Reported new cases COVID-19"))

myresidagainstcont <- bind_cols(modeldata_cont, devresids=devresids) %>%
  pivot_longer(cols=`Rainfall`:`Active cases`)
ggplot(myresidagainstcont, aes(x=value, y=devresids)) +
  geom_point() +
  facet_wrap(~name, nrow = 6, scales="free_x") +
  geom_smooth(se=FALSE)

# proper distribution
qqnorm(qresids[-1971])
qqline(qresids[-1971])
hist(qresids[-1971], freq=FALSE)
lines(density(qresids[-1971]),
      lwd = 2,
      col = "red")
lines(density(rnorm(length(qresids[-1971]))),
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

# rescale principal components of mobility to original mobility variables
pcaloadings <- read_csv("data/pca_mobility_loadings.csv")
orig_mobi <- as.vector(exp(as.matrix(pcaloadings[, 2:7]) %*% myglm$coefficients[c(
  "`Mobility (PC1)`",
  "`Mobility (PC2)`",
  "`Mobility (PC3)`",
  "`Mobility (PC4)`",
  "`Mobility (PC5)`",
  "`Mobility (PC6)`"
)]))
names(orig_mobi) <- pcaloadings$original_variable
orig_mobi

coeffs <- tidy(myglm, exponentiate=TRUE) # , conf.int=TRUE, conf.level = 0.99
