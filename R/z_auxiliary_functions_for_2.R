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
    if (optadjset[[2]]) { # is a reduced set
      adjsets[["RedOptAdjSet"]] <- optadjset[[1]]
    } else { # is actually real optimal adjustment set
      adjsets[["OptAdjSet"]] <- optadjset[[1]]
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

# all-in-one function
my_causal <- function(dag, modeldata, exposure, unobserved) {
  if (is.null(exposure)) {
    adjsets <- NULL
  } else {
    adjsets <- get_adjsets(dag, exposure, outcome="Reported new cases COVID-19", unobserved)
  }
  res <- my_negbin(modeldata, exposure=exposure, adjsets=adjsets)
  res[["adjustmentsets"]] <- adjsets
  return(res)
}

# write final causal effects to csv
write_cause <- function(res, exposure) {
  whichmaxr2 <- which.max(res$pseudor2s)
  whichminaic <- which.max(res$aics)
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
  return(NULL)
}