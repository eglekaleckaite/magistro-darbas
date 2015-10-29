rm(list = ls())
library(nlme)
library(plyr)
library(gdata)
library(lme4)
library(mitools)
library(lmerTest)
library(robustlmm)
library(foreach)
library(boot)
library(MASS)
library(Matrix)
library(gtools)
library(snow)
library(matrixcalc)
source("10code.R")

load("data.2011.RData")

varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES", "IDSTUD", "TOTWGT", "SCHWGT", "STUDWGT", "SCHWGT")
#data.2011  <- drop.levels(subset(data.2011, IDSCHOOL %in% unique(data.2011$IDSCHOOL)[1:30]))
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                       "BSMMAT05", varb)]))



st1 <- system.time(ivM2 <- mySIMPLE(dt = data.2011,
                                 fixed = "BSMMAT01 ~ 1",
                                 random1 = "~1|IDSCHOOL",
                                 weights = c("STUDWGT", "SCHWGT")))

bbMINQUE <- bootMINQUE(data = data.2011, FUN  = bfunSIMPLE, R = 10,
                       fixed = "BSMMAT01 ~ 1", random = "~1|IDSCHOOL",
                       strata = data.2011$IDSCHOOL, hierar = F)
  
bootMINQUE <- function(data = data.2011, FUN  = bfunSIMPLE, R = 100,
                       fixed = "BSMMAT01 ~ 1+SSEX", random = "~1+SSEX|IDSCHOOL",
                       strata = NULL, hierar = F) {
  print("Iteration:")
  boutSIMPLE <- boot(data, FUN, R = R, 
                     formul = fixed, 
                     parallel = "multicore", random = random,
                     strata = strata, attrib = T, hierar = hierar)
  
  #collect results
  attrib  <- boutSIMPLE$attr
  dims <- attrib$dims
  nms <- attrib$nms
  bb <- boutSIMPLE[[1]]
  tt <- bb$t
  tt0 <- bb$t0
  # collect beta
  beta  <- tt[,1:dims$coef, drop = F]
  colnames(beta) <- nms$coef
  Bcov <- cov(beta)
  Bint <- t(t(beta)-(tt0[1:dims$coef, drop = F]))
  Bint <- apply(Bint, 2, function(x) quantile(x, .975))
  Bint <- rbind((t(tt0[1:dims$coef, drop = F])-Bint), (t(tt0[1:dims$coef, drop = F])+Bint))
  rownames(Bint) <- c("2.5%", "97.5%")
  beta <- apply(beta, 2, mean)
  Bbias <- beta - tt0[1:dims$coef, drop = F]
  beta <- tt0[1:dims$coef, drop = F]-Bbias
  beta <- cbind(Coefficient = beta, St.err = sqrt(diag(Bcov)), Bias = Bbias, t(Bint))
  attr(beta, "cov") <- Bcov
  
  tt <- tt[,-1:-dims$coef, drop = F]
  tt0 <- tt0[-1:-dims$coef, drop = F]
  # collect runef
  rdim <- dims$ranef
  ranef <- tt[,1:(rdim[1]*rdim[2]), drop = F]
  ranef0 <- tt0[1:(rdim[1]*rdim[2]), drop = F]
  
  ranef <- foreach(i = 1:rdim[1]) %do% {
    wh <- (1:rdim[2]*rdim[1]- rdim[1])+i
    rr <- ranef[, wh, drop = F]
    colnames(rr) <- nms$ranef[[2]]
    rr0 <- ranef0[wh, drop = F]
    Rcov <- cov(rr)
    #Rint <- apply(rr, 2, function(x) quantile(x, c(.025, .975)) )
    Rint <- t(t(rr)-(rr0))
    Rint <- apply(Rint, 2, function(x) quantile(x, .975))
    Rint <- rbind((t(rr0)-Rint), (t(rr0)+Rint))
    rownames(Rint) <- c("2.5%", "97.5%")
    
    rr <- apply(rr, 2, mean)
    Rbias <- rr - rr0
    rr <- rr0 - Rbias
    rr <- cbind(Coefficient = rr, St.err = sqrt(diag(Rcov)), Bias = Rbias, t(Rint))
    attr(rr, "cov") <- Rcov
    return(rr)
  }
  names(ranef) <- nms$ranef[[1]]
  
  tt <- tt[,-1:-(rdim[1]*rdim[2]), drop = F]
  tt0 <- tt0[-1:-(rdim[1]*rdim[2]), drop = F]
  
  # collect sigma
  sigma  <- tt[,1:dims$sigma, drop = F]
  colnames(sigma) <- nms$sigma
  Scov <- cov(sigma)
  #Sint <- apply(sigma, 2, function(x) quantile(x, c(.025, .975)) )
  Sint <- t(t(sigma)-(tt0[1:dims$sigma, drop = F]))
  Sint <- apply(Sint, 2, function(x) quantile(x, .975))
  Sint <- rbind((t(tt0[1:dims$sigma, drop = F])-Sint), (t(tt0[1:dims$sigma, drop = F])+Sint))
  rownames(Sint) <- c("2.5%", "97.5%")
  
  sigma <- apply(sigma, 2, mean)
  Sbias <- sigma - tt0[1:dims$sigma, drop = F]
  sigma <- tt0[1:dims$sigma, drop = F] - Sbias
  sigma <- cbind(Coefficient = sigma, St.err = sqrt(diag(Scov)), Bias = Sbias, t(Sint))
  attr(sigma, "cov") <- Scov
  
  tt <- tt[,-1:-dims$sigma, drop = F]
  tt0 <- tt0[-1:-dims$sigma, drop = F]
  
  #collect TT
  TTdim <- dims$TT[1]
  TT  <- tt
  perm <- apply(permutations(TTdim,2,1:TTdim, repeats.allowed = T), 1, paste, collapse = "")
  comb <- apply(combinations(TTdim,2,1:TTdim, repeats.allowed = T), 1, paste, collapse = "")
  wh <- perm %in% comb
  TT <- TT[,wh, drop = F]
  TTcov <- cov(TT)
  #TTint <- apply(TT, 2, function(x) quantile(x, c(.025, .975)) )
  TTint <- t(t(TT)-(tt0[wh, drop = F]))
  TTint <- apply(TTint, 2, function(x) quantile(x, .975))
  TTint <- rbind((t(tt0[wh, drop = F])-TTint), (t(tt0[wh, drop = F])+TTint))
  rownames(TTint) <- c("2.5%", "97.5%")
  
  TT <- apply(TT, 2, mean)
  TTbias <- TT - tt0[wh, drop = F]
  TT <- tt0[wh, drop = F] - TTbias
  CTT <- cbind(Coefficient = TT, St.err = sqrt(diag(TTcov)), Bias = TTbias, t(TTint))
  rownames(CTT) <- as.vector(outer(nms$TT[[1]], nms$TT[[1]], function(x, y) paste(x, y, sep = '.')))[wh]
  TT <- fillT(TT)
  attr(TT, "char") <- CTT
  
  return(list(beta = beta, ranef = ranef, sigma2 = sigma, TT = TT))
}

bfunSIMPLE <- function(dat, orig, formul, random, attrib = FALSE) {
  print("Iteration:")
  ivM <- mySIMPLE(dt = dat[orig, ],
                   fixed = formul,
                   random1 = random)
  cc <- c(ivM$beta, unlist(ivM$ranef[[1]]), ivM$sigma2, unlist(ivM$TT))
  if (attrib) {
    attr(cc, "dims") <- list(coef = length(ivM$beta), ranef = dim(ivM$ranef[[1]]), sigma = 1, TT = dim(ivM$TT))
    attr(cc, "nms") <- list(coef = names(ivM$beta), ranef = dimnames(ivM$ranef[[1]]), sigma = "sigma2", TT = dimnames(ivM$TT))
  }
  return(cc)
}