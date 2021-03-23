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

# convert dagitty to adjacency matrix
dagitty_to_adjmatrix <- function(daggity_obj) {
  edg <- dagitty:::edges(daggity_obj)
  node_names <- dagitty:::names.dagitty(daggity_obj)
  ans_mat <- matrix(
    data = 0, nrow = length(node_names),
    ncol = length(node_names),
    dimnames = list(node_names, node_names)
  )
  ans_mat[as.matrix(edg[c("v", "w")])] <- 1
  ans_mat[as.matrix(edg[edg$e=="--", ][c("w", "v")])] <- 1
  return(ans_mat)
}

# function to assemble variable names
my_vars <- function(modeldata, mynames) {
  myvars <- character()
  allvars <- colnames(modeldata)
  idx <- 1
  for (mn in mynames) {
    thesevars <- allvars[grep(mn, allvars, fixed=TRUE)]
    for (tvidx in seq(thesevars)) {
      myvars[idx] <- thesevars[tvidx]
      idx <- idx+1
    }
  }
  return(myvars)
}

# get AIC from glmnet
aic_glmnet <- function(myglmnet) {
  tLL <- -deviance(myglmnet) # myglmnet$nulldev*myglmnet$dev.ratio # myglmnet$nulldev - deviance(myglmnet)
  k <- myglmnet$df
  n <- myglmnet$nobs
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  return(AICc)
}

# function to get optimal adjustment set without unobserved (check for validness)
get_optadjset <- function(amat, exposure, outcome, unobserved) {
  x.pos <- which(colnames(amat)==exposure)
  y.pos <- which(colnames(amat)==outcome)
  optadjset <- colnames(amat)[optAdjSet(amat, x.pos = x.pos, y.pos = y.pos)]
  optadjset_red <- setdiff(optadjset, unobserved)
  optadjset_red_idx <- sapply(optadjset_red, function(z) which(colnames(amat)==z))
  is_valid <- gac(amat, x.pos, y.pos, optadjset_red_idx, type="dag")
  if (is_valid$gac) {
    return(list(adjset=optadjset_red, is_reduced=(length(optadjset_red)<length(optadjset))))
  } else {
    return(NULL)
  }
}

# function to get minimal and optimal adjustment sets without unobserved (check for validness)
get_adjsets <- function(dag, exposure, outcome, unobserved) {
  amat <- t(dagitty_to_adjmatrix(dag))
  x.pos <- which(colnames(amat)==exposure)
  y.pos <- which(colnames(amat)==outcome)
  minadjsets <- adjustmentSets(dag, exposure=exposure, outcome=outcome)
  valid_adjsets <- vector("list", length(minadjsets))
  for (mas in seq(minadjsets)) {
    minadjset_idx <- sapply(minadjsets[[mas]], function(z) which(colnames(amat)==z))
    is_valid <- gac(amat, x.pos, y.pos, minadjset_idx, type="dag")
    if (is_valid$gac) {
      valid_adjsets[[mas]] <- minadjsets[[mas]]
    } else {
      valid_adjsets[[mas]] <- NULL
    }
  }
  adjsets <- valid_adjsets
  if (length(adjsets)>0) {
    names(adjsets) <- paste0("MinAdjSet", seq(valid_adjsets))
    optadjset <- get_optadjset(amat, exposure, outcome, unobserved)
    if (!is.null(optadjset)) {
      if (optadjset[[2]]) { # is a reduced set
        adjsets[["RedOptAdjSet"]] <- optadjset[[1]]
      } else { # is actually real optimal adjustment set
        adjsets[["OptAdjSet"]] <- optadjset[[1]]
      }
    }
  }
  return(adjsets)
}

# function for neg-binomial model based on modeldata and adjustment set from dag
my_negbin <- function(modeldata, exposure=NULL, adjsets=NULL) {
  mynullformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))" 
  if (is.null(exposure) & is.null(adjsets)) { # adjustment set not given
    myformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))-`Active cases`+."
    nf <- 1
  } else if (is.null(exposure)+is.null(adjsets)==1) { # exactly one is different from NULL: not allowed
    myformula <- "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))-`Active cases`+."
    cat("Adjustment set AND exposure must be given. Calculate complete model.\n")
    exposure <- NULL
    adjsets <- NULL
    nf <- 1
  } else { # adjsets and exposure given
    nas <- length(adjsets)
    expovars <- my_vars(modeldata, exposure)
    nexp <- length(expovars)
    myformula <- vector("character", nas)
    for (as in seq(nas)) {
      if (is_empty(adjsets[[as]])) {
        myformula[as] <- paste("`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))+",#-`Active cases`
                               sprintf("`%s`", paste(expovars, collapse = "`+`")),
                               sep = "")
      } else {
        myadjset <- my_vars(modeldata, adjsets[[as]])
        myformula[as] <- paste("`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))+",#-`Active cases`
                               sprintf("`%s`", paste(c(myadjset, expovars), collapse = "`+`")),
                               sep = "")
      }
    }
    nf <- length(myformula)
  }
  coefficients <- vector("list", nf)
  causaleffects <- vector("list", nf)
  pseudor2s <- rep(0, nf)
  aics <- rep(0, nf)
  mynullglm <- glm.nb(as.formula(mynullformula),
                      data=modeldata)
  scale_params <- read_csv("data/scale_params.csv") # original scaling first wave!
  to_rescale <- scale_params$variable
  for (idxf in seq(nf)) {
    cat("   Exposure:", exposure, "\n")
    cat("   Adjustment set:", adjsets[[idxf]], "\n")
    myglm <- glm.nb(as.formula(myformula[idxf]),
                    data=modeldata)
    # myglm_zi <- zeroinfl(as.formula(paste0(myformula[idxf], "| 1+offset(log(`Active cases`+1))+
    #                                     `Weekday (report)`+`Holiday (report)`")), data = modeldata)
    # pseudo RSquared
    pseudor2s[idxf] <- 1-sum((myglm$fitted.values-myglm$y)^2)/sum((mynullglm$fitted.values-mynullglm$y)^2)# 1-summary(myglm)$deviance/summary(mynullglm)$deviance
    aics[idxf] <- AIC(myglm)
    cat("   pseudo R2:", pseudor2s[idxf], "\n")
    cat("   AIC:", aics[idxf], "\n")
    coefficients[[idxf]] <- tidy(myglm, exponentiate=TRUE, conf.int=TRUE, conf.level = 0.99)
    for (expovar in coefficients[[idxf]]$term) {
      expovar_clean <- str_remove_all(expovar, "`")
      if (expovar_clean %in% to_rescale) {
        # coefficients[[idxf]]$estimate[coefficients[[idxf]]$term=="(Intercept)"] <- coefficients[[idxf]]$estimate[coefficients[[idxf]]$term=="(Intercept)"]*exp(-coefficients[[idxf]]$estimate[coefficients[[idxf]]$term==expovar]*scale_params$mymean[scale_params$variable==expovar_clean]/scale_params$mysd[scale_params$variable==expovar_clean])
        coefficients[[idxf]]$estimate[coefficients[[idxf]]$term==expovar] <- coefficients[[idxf]]$estimate[coefficients[[idxf]]$term==expovar] ^(1/scale_params$mysd[scale_params$variable==expovar_clean])
        coefficients[[idxf]]$conf.low[coefficients[[idxf]]$term==expovar] <- coefficients[[idxf]]$conf.low[coefficients[[idxf]]$term==expovar] ^(1/scale_params$mysd[scale_params$variable==expovar_clean])
        coefficients[[idxf]]$conf.high[coefficients[[idxf]]$term==expovar] <- coefficients[[idxf]]$conf.high[coefficients[[idxf]]$term==expovar] ^(1/scale_params$mysd[scale_params$variable==expovar_clean])
      }
    }
    if (!is.null(exposure)) {
      effects <- tail(coefficients[[idxf]]$estimate, nexp)
      pvals <- tail(coefficients[[idxf]]$p.value, nexp)
      conflows <- tail(coefficients[[idxf]]$conf.low, nexp)
      confhighs <- tail(coefficients[[idxf]]$conf.high, nexp)
      cat("   Effect on outcome:\n")
      for (idxexp in seq(nexp)) {
        cat(effects[idxexp], expovars[idxexp], "with p-value", pvals[idxexp], "\n")
      }
      causaleffects[[idxf]] <- tibble(variables=expovars, estimates=effects, conflow=conflows, confhigh=confhighs)
    } else {
      causaleffects[[idxf]] <- coefficients[[idxf]]
    }
  }
  return(list(pseudor2s=pseudor2s, aics=aics, causaleffects=causaleffects))
}

# function for neg-binomial model based on modeldata and adjustment set from dag, but with ridge regularization
my_negbin_ridge <- function(modeldata, exposure=NULL, adjsets=NULL) {
  myglmnetparams <- read_csv("data/myglmnetparams.csv")
  mytheta <- myglmnetparams %>% pull(mytheta)
  mylambda <- myglmnetparams %>% pull(mylambda)
  myy <- modeldata$`Reported new cases COVID-19`
  myx_data <- modeldata %>% dplyr::select(-`Reported new cases COVID-19`)
  mynullx <- cbind(0, as.matrix(myx_data %>% dplyr::select(`Active cases`))) # "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))" 
  if (is.null(exposure) & is.null(adjsets)) { # adjustment set not given
    myxcols <- vector("list", 1)
    myxcols[[1]] <- colnames(myx_data) # "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))-`Active cases`+."
    nf <- 1
  } else if (is.null(exposure)+is.null(adjsets)==1) { # exactly one is different from NULL: not allowed
    myxcols <- vector("list", 1)
    myxcols[[1]] <- colnames(myx_data) # "`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))-`Active cases`+."
    cat("Adjustment set AND exposure must be given. Calculate complete model.\n")
    exposure <- NULL
    adjsets <- NULL
    nf <- 1
  } else { # adjsets and exposure given
    nas <- length(adjsets)
    expovars <- my_vars(modeldata, exposure)
    nexp <- length(expovars)
    myxcols <- vector("list", nas) # myformula <- vector("character", nas)
    for (as in seq(nas)) {
      if (is_empty(adjsets[[as]])) {
        myxcols[[as]] <- c("Active cases", expovars)
        # myformula[as] <- paste("`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))+",#-`Active cases`
        #                        sprintf("`%s`", paste(expovars, collapse = "`+`")),
        #                        sep = "")
      } else {
        myadjset <- my_vars(modeldata, adjsets[[as]])
        myxcols[[as]] <- c("Active cases", myadjset, expovars)
        # myformula[as] <- paste("`Reported new cases COVID-19` ~1+offset(log(`Active cases`+1))+",#-`Active cases`
        #                        sprintf("`%s`", paste(c(myadjset, expovars), collapse = "`+`")),
        #                        sep = "")
      }
    }
    nf <- length(myxcols)
  }
  coefficients <- vector("list", nf)
  causaleffects <- vector("list", nf)
  pseudor2s <- rep(0, nf)
  aics <- rep(0, nf)
  # mynullglm <- glm.nb(as.formula(mynullformula),
  #                     data=modeldata)
  mynullglm <- glmnet(mynullx, myy, family = negative.binomial(mytheta),
                                     offset=log(modeldata%>%pull(`Active cases`)+1),
                                     standardize=FALSE,
                                     alpha=0, lambda=mylambda)
  scale_params <- read_csv("data/scale_params.csv") # original scaling first wave!
  to_rescale <- scale_params$variable
  for (idxf in seq(nf)) {
    cat("   Exposure:", exposure, "\n")
    cat("   Adjustment set:", adjsets[[idxf]], "\n")
    myx <- as.matrix(myx_data %>% dplyr::select(myxcols[[idxf]]))
    myglm <- glmnet(myx, myy, family = negative.binomial(mytheta),
                          offset=log(modeldata%>%pull(`Active cases`)+1),
                          standardize=FALSE,
                          alpha=0, lambda=mylambda)
    myglm_fitted.values <- exp(predict(myglm, myx, newoffset=log(modeldata%>%pull(`Active cases`)+1))[,1])
    mynullglm_fitted.values <- exp(predict(mynullglm, mynullx, newoffset=log(modeldata%>%pull(`Active cases`)+1))[,1])
    pseudor2s[idxf] <- 1-sum((myglm_fitted.values-myy)^2)/sum((mynullglm_fitted.values-myy)^2)# 1-summary(myglm)$deviance/summary(mynullglm)$deviance
    aics[idxf] <- aic_glmnet(myglm)
    cat("   pseudo R2:", pseudor2s[idxf], "\n")
    cat("   AIC:", aics[idxf], "\n")
    coefficients[[idxf]] <- tidy(myglm) %>% dplyr::select(term, estimate) %>% mutate(estimate=exp(estimate))
    for (expovar in coefficients[[idxf]]$term) {
      expovar_clean <- str_remove_all(expovar, "`")
      if (expovar_clean %in% to_rescale) {
        coefficients[[idxf]]$estimate[coefficients[[idxf]]$term==expovar] <- coefficients[[idxf]]$estimate[coefficients[[idxf]]$term==expovar] ^(1/scale_params$mysd[scale_params$variable==expovar_clean])
        # coefficients[[idxf]]$conf.low[coefficients[[idxf]]$term==expovar] <- coefficients[[idxf]]$conf.low[coefficients[[idxf]]$term==expovar] ^(1/scale_params$mysd[scale_params$variable==expovar_clean])
        # coefficients[[idxf]]$conf.high[coefficients[[idxf]]$term==expovar] <- coefficients[[idxf]]$conf.high[coefficients[[idxf]]$term==expovar] ^(1/scale_params$mysd[scale_params$variable==expovar_clean])
      }
    }
    if (!is.null(exposure)) {
      effects <- tail(coefficients[[idxf]]$estimate, nexp)
      if (exposure=="Mobility") {
        pcaloadings <- read_csv("data/pca_mobility_loadings.csv")
        mobi_coeffs_pca <- coefficients[[idxf]] %>% filter(term %in% c(
          "Mobility (PC1)",
          "Mobility (PC2)",
          "Mobility (PC3)",
          "Mobility (PC4)",
          "Mobility (PC5)",
          "Mobility (PC6)"
        )) %>% pull(estimate)
        orig_mobi <- sapply(1:6, function(s) prod(mobi_coeffs_pca^as.numeric(pcaloadings[s, -1])))# as.vector(exp(as.matrix(pcaloadings[, 2:7]) %*% mobi_coeffs_pca))
        names(orig_mobi) <- pcaloadings$original_variable
        effects <- rep(0, 6)
        expovars <- names(orig_mobi)
        for (mobvar in names(orig_mobi)) {
          names(effects) <- names(orig_mobi)
          effects[mobvar] <- orig_mobi[mobvar]^(1/scale_params$mysd[scale_params$variable==mobvar])
        }
      }
      # pvals <- tail(coefficients[[idxf]]$p.value, nexp)
      # conflows <- tail(coefficients[[idxf]]$conf.low, nexp)
      # confhighs <- tail(coefficients[[idxf]]$conf.high, nexp)
      cat("   Effect on outcome:\n")
      for (idxexp in seq(nexp)) {
        cat(effects[idxexp], expovars[idxexp], "\n") #, "with p-value", pvals[idxexp], "\n")
      }
      causaleffects[[idxf]] <- tibble(variables=expovars, estimates=effects) # , conflow=conflows, confhigh=confhighs
    } else {
      causaleffects[[idxf]] <- coefficients[[idxf]]
    }
  }
  return(list(pseudor2s=pseudor2s, aics=aics, causaleffects=causaleffects))
}

# all-in-one function
my_causal <- function(dag, modeldata, exposure, unobserved) {
  if (is.null(exposure)) {
    adjsets <- NULL
  } else {
    adjsets <- get_adjsets(dag, exposure, outcome="Reported new cases COVID-19", unobserved)
  }
  if (length(adjsets)>0 | is.null(exposure)) {
    res <- my_negbin_ridge(modeldata, exposure=exposure, adjsets=adjsets)
    res[["adjustmentsets"]] <- adjsets
  } else {
    res <- NULL
  }
  return(res)
}

# write final causal effects to csv
write_cause <- function(res, exposure) {
  if (!is.null(res)) {
    whichmaxr2 <- which.max(res$pseudor2s)
    whichminaic <- which.min(res$aics)
    line <- paste0(exposure, ",", max(res$pseudor2s))
    write(line, file=paste0(tablepath, "t_pseudor2s.csv"), append=TRUE)
    line <- paste0(exposure, ",", min(res$aics))
    write(line, file=paste0(tablepath, "t_aics.csv"), append=TRUE)
    write_csv(res$causaleffects[[whichminaic]],
              paste0(tablepath, "t_effects_",
                     exposure, "_", 
                     names(res$adjustmentsets[whichminaic]), ".csv"))
    write(res$adjustmentsets[[whichminaic]],
          paste0(tablepath, "t_adjust_",
                 exposure, "_", 
                 names(res$adjustmentsets[whichminaic]), ".csv"))
  } else {
    cat("\n NO ADJUSTMENT SET FOR ", exposure, " (non-identifiable) \n \n ")
  }
  return(NULL)
}