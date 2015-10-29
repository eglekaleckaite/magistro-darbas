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

varb <- c("IDSTUD", "IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES", "TOTWGT", "SCHWGT", "STUDWGT")
data.2011  <- drop.levels(subset(data.2011, IDSCHOOL %in% unique(data.2011$IDSCHOOL)[1:2]))
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                       "BSMMAT05", varb)]))

# fixed <- "BSMMAT01 ~ 1+SSEX+STHMWRK+SMENG+SMCONF+SHEDRES"
# random <- "~1+SMENG|IDCLASS"
# fixed <- "BSMMAT01 ~ 1"
# random1 <- "~1|IDCLASS"
# random2 <- "~1|IDSCHOOL"
st1 <- system.time(ivM <- myMINQUE(dt = data.2011,
                                   fixed = "BSMMAT01 ~ 1+SMENG",
                                   random1 = "~1|IDCLASS", 
                                   random2 = "~1|IDSCHOOL"))
 
st1 <- system.time(ivM <- myMINQUE(dt = data.2011,
                                   fixed = "BSMMAT01 ~ 1+SMENG",
                                   random1 = "~1|IDCLASS", 
                                   random2 = NULL))

st1 <- system.time(ivM2 <- myMINQUE2(dt = data.2011,
                                     fixed = "BSMMAT01 ~ 1+SMENG+STHMWRK",
                                     random1 = "~1|IDSCHOOL"))#,
                                     #weights = c("STUDWGT", "SCHWGT")))
st1 <- system.time(ivM1 <- myMINQUE(dt = sleepstudy, fixed = "Reaction ~ Days", random1 = "~Days|Subject"))

st1 <- system.time(ivM2 <- myMINQUE(dt = as.data.frame(Orthodont), fixed = "distance ~ age+Sex", random1 = "~1|Subject"))


myMINQUE2 <- function(dt, fixed, random1 = NULL, random2 = NULL, weights = NULL) {
  N <- nrow(dt)
  # Form Y and fixed effects data frame
  ff <- model.frame(fixed, dt)
  Y <- as(as(ff[,1, drop = F], "matrix"), "Matrix")
  if (grepl("-1",fixed)) {
    X <- as(as(ff[,-1, drop = F], "matrix"), "Matrix")
  } else {
    X <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(ff),
                            dimnames = list(NULL, "(Intercept)")), 
                     ff[,-1, drop = F]), "matrix"), "Matrix")
  }
  
  if(!is.null(weights)){
    wgt1 <- 1/sqrt(dt[,weights[1]])
    wgt2 <- 1/sqrt(dt[,weights[2]])
    wgt1 <- wgt2*wgt1
  } else {
    wgt1 <- rep(1, N)
    wgt2 <- rep(1, N)
  }
  
  if (!is.null(random1)) {
    id1 <- strsplit(random1, "\\|")
    random1 <- unlist(id1)[1]
    nmid1 <- unlist(id1)[2]
    rr1 <- model.frame(random1, dt)
    id1 <- model.frame(paste("~", nmid1), dt)[,1]
    if (grepl("-1",random1)) {
      Z1 <- as(wgt2*as( rr1, "matrix"), "Matrix")
    } else {
      Z1 <- as(wgt2*as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
                                dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
                "Matrix")
    }
    q <- ncol(Z1)
    colnames(Z1) <- paste(nmid1, colnames(Z1), sep = ".")
    clnms1 <- colnames(Z1)
    Z1 <- foreach(i = unique(id1)) %do% {Z1[id1 %in% i, ,drop = F]}
    n1 <- length(unique(id1))
    l <- q*(q+1)/2
    TT1 <- formTT(q)
    QI <- llply(TT1, function(tt) bdiag(llply(Z1, function(z) z%*%tt%*%t(z))))
  } else {
    Z1 <- NULL
    clnms1 <- NULL
    n1 <- NULL
    QI <- NULL
  }
  
  l <- l+1
  Qr <- c(list(diag(wgt1)), QI)
  V0 <- solve(Reduce("+", Qr))
  C0 <- V0-V0%*%X%*%solve(t(X)%*%V0%*%X)%*%t(X)%*%V0
  CQ0 <- llply(Qr, function(qr) C0%*%qr)
  CY0 <- C0%*%Y
  WI <- laply(Qr, function(qr) as.numeric(t(CY0)%*%qr%*%CY0))
  S <- solve(fillS(CQ0))
  
  theta0 <- S%*%WI
    print(theta0)
  V <- solve(Reduce("+", mapply(function(qr, thr) thr*qr, Qr, theta0)))
  iM <- iterMINQUE(V, X, Qr, theta0, Y)
  
  
  gc()
  doIM <- (round(sum(abs(iM$theta - theta0)), 4) != 0)
  while(doIM){
    theta <- iM$theta
    print(theta)
    iM <- iterMINQUE(iM$V, X, Qr, theta, Y)
    doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
  }
    
  sigma2 <- iM$theta[1]
  names(sigma2) <- "sigma2"
  TT <- fillT(iM$theta[-1])
  dimnames(TT) <- list(clnms1, clnms1)
  beta <- as(solve(t(X)%*%iM$V%*%X)%*%t(X)%*%iM$V%*%Y, "numeric")
  names(beta) <- colnames(X)

  gc()
  
  return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, n1 = n1))
}


fillT <- function(tau) {
  ns <- round(sqrt(length(tau)*2))
  kk <- combinations(ns,2,1:ns, repeats.allowed = T)
  x <- matrix(0, ncol = ns, nrow = ns)
  for(ii in 1:nrow(kk)) {
    i <- kk[ii, 1]
    j <- kk[ii, 2]
    x[i,j] <- tau[ii]
    x[j, i] <- tau[ii]
  }
  return(x)
}

iterMINQUE <- function(V, X, Qr, theta0, Y){
  C <- V-V%*%X%*%solve(t(X)%*%V%*%X)%*%t(X)%*%V
  CQ <- llply(Qr, function(qr) sum(diag(C%*%qr)))
  CY <- C%*%Y
  WI <- laply(Qr, function(qr) as.numeric(t(CY)%*%qr%*%CY))
  
#   S <- ginv(fillS(CQ))
#   
#   theta <- S%*%WI
  
  theta <- mapply(function(th, cqi, wi){cat("thetai\n");print(th)
                                        th*wi/cqi}, theta0, CQ, WI)
  V <- solve(Reduce("+", mapply(function(qr, thr) thr*qr, Qr, theta)))
  gc()
  return(list(V = V, theta = theta))
}