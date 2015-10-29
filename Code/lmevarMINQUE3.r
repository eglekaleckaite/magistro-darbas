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
library(varComp)
source("10code.R")

load("data.2011.RData")

varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES")
data.2011  <- drop.levels(subset(data.2011, IDCLASS %in% unique(data.2011$IDCLASS)[1:5]))
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                       "BSMMAT05", varb)]))

# fixed <- "BSMMAT01 ~ 1+SSEX+STHMWRK+SMENG+SMCONF+SHEDRES"
# random <- "~1+SMENG|IDCLASS"
# fixed <- "BSMMAT01 ~ 1"
# random1 <- "~1|IDCLASS"
# random2 <- "~1|IDSCHOOL"
st1 <- system.time(ivM <- myMINQUE(dt = data.2011,
                                   fixed = "BSMMAT01 ~ 1+SMENG+STHMWRK+SMVALUE",
                                   random1 = "~1+SSEX|IDCLASS", 
                                   random2 = NULL))
 
st1 <- system.time(ivM2 <- myMINQUE2(dt = data.2011,
                                   fixed = "BSMMAT01 ~ 1",
                                   random1 = "~1|IDCLASS", 
                                   random2 = "~1|IDSCHOOL"))
st1 <- system.time(ivM1 <- myMINQUE(dt = sleepstudy, fixed = "Reaction ~ Days", random1 = "~Days|Subject"))

st1 <- system.time(ivM2 <- myMINQUE(dt = as.data.frame(Orthodont), fixed = "distance ~ age+Sex", random1 = "~1|Subject"))


myMINQUE <- function(dt, fixed, random1 = NULL, random2 = NULL) {
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
  
  if (!is.null(random1)) {
    id1 <- strsplit(random1, "\\|")
    random1 <- unlist(id1)[1]
    nmid1 <- unlist(id1)[2]
    rr1 <- model.frame(random1, dt)
    id1 <- model.frame(paste("~", nmid1), dt)[,1]
    if (grepl("-1",random1)) {
      HH1 <- as(as( rr1, "matrix"), "Matrix")
    } else {
      HH1 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
                                dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
                "Matrix")
    }
    k <- ncol(HH1)
    colnames(HH1) <- paste(nmid1, colnames(HH1), sep = ".")
    clnms1 <- colnames(HH1)
    HH1 <- bdiag(foreach(i = unique(id1)) %do% {HH1[id1 %in% i,]})
    HH1i <- foreach(i = 1:(ncol(HH1)/k)) %do% {HH1[,(i*k-1):(i*k), drop = F]}
    n1 <- length(unique(id1))
    l <- k*(k+1)/2
    TT1 <- formTT(k)
    QI <- llply(TT1, function(x) HH1 %*% kronecker(diag(n1), x) %*% t(HH1))
    
  } else {
    HH1 <- NULL
    clnms1 <- NULL
    n1 <- NULL
    QI <- NULL
  }
  
#   if (!is.null(random2)) {
#     id2 <- strsplit(random2, "\\|")
#     random2 <- unlist(id2)[1]
#     nmid2 <- unlist(id2)[2]
#     rr2 <- model.frame(random2, dt)
#     id2 <- model.frame(paste("~", nmid2), dt)[,1]
#     if (grepl("-1",random2)) {
#       HH2 <- as(as( rr2, "matrix"), "Matrix")
#     } else {
#       HH2 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr2),
#                                 dimnames = list(NULL, "(Intercept)")), rr2),
#                    "matrix"),
#                 "Matrix")
#     }
#     colnames(HH2) <- paste(nmid2, colnames(HH2), sep = ".")
#     clnms2 <- colnames(HH2)
#     HH2 <- alply(HH2, 2, function(x) Matrix(foreach(i = unique(id2), .combine = cbind) %do% {y <- x; y[!id2 %in% i] <- 0; return(y)}))    
#     n2 <- length(unique(id2))
#   } else {
#     HH2 <- NULL
#     clnms2 <- NULL
#     n2 <- NULL
#   }
  HH <- cBind(diag(N), HH1)
  HHi <- c(list(diag(N)), HH1i)
  VV <- HH%*%t(HH)
  kk <- combinations(k,2,1:k, repeats.allowed = T)
#   QI0 <- list(diag(N))
#   QI <- c(QI0, QI)
#   QI <- llply(QI,  as.matrix)
  #minque(as.numeric(Y), QI)

  
  #T0 <- Reduce("+", QI)
#   th0 <- matrix(1,ncol = length(QI))
#   T0 <-  solve(Reduce("+", mapply(function(th, qi) th*qi, as.list(th0[,1]), QI)))
  #iT <- ginv(as(T0, "matrix"))
  iT <- solve(VV)
  C0 <- iT-iT%*%X%*%solve(t(X)%*%iT%*%X)%*%t(X)%*%iT
  QI <- llply(HHi, function(x) x%*%t(x))
  CQ <- llply(QI, function(x) C0%*%x)
  S <- fillS(CQ)
  CY <- C0%*%Y
  WI <- makeWI(QI, CY)
  theta0 <- ginv(S)%*%as.matrix(WI, ncol = 1)
  Tw <- solve(Reduce("+", mapply(function(th, qi) th*qi, as.list(theta0[,1]), QI)))
  iM <- iterMINQUE(Tw, X, QI, kk, theta0[,1], Y)
  
  
  gc()
  doIM <- (round(sum(abs(iM$theta - theta0[, 1])), 4) != 0)
  while(doIM){
    theta <- iM$theta
    print(theta)
    iM <- iterMINQUE(iM$D, X, QI, kk, theta, Y)
    doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
  }
  
  iterMINQUE <- function(Tw, X, QI, kk, theta0, Y){
    TwX <- Tw%*%X
    C <- Tw-TwX%*%solve(t(X)%*%TwX)%*%t(TwX)
    CY <- C%*%Y
    WI <- makeWI(QI, CY)
    gc()
    CQ <- llply(QI, function(x) C%*%x)
    theta <- mapply(function(th, cq, wi){print(th)
                                         th*wi/sum(diag(cq))}, theta0, CQ, WI)
    D <- solve(Reduce("+", mapply(function(th, qi) th*qi, as.list(theta), QI)))
    gc()
    return(list(D = D, theta = theta, sigma = sigma))
  }
  # st1 <- system.time(minqueiv <- lmevarMINQUE(Y, X, HH, id))
  # st2 <- system.time(minqueIiv <- lmevarMINQUEI(Y, X, HH, id))
#   VC <- varComp(fixed = BSMMAT01 ~ 1+SMENG+STHMWRK+SMVALUE, data = data.2011)
#   VC <- varComp(fixed = BSMMAT01 ~ 1+SMENG+STHMWRK+SMVALUE, data = data.2011, random = )
#   
  
  ivMin <- lmevarMINQUEI(Y, X, HI, idd = id1)
  gc()
  return(c(ivMin, N = N, n1 = n1, n2 = n2))
}


lmevarMINQUEI2 <- function(Y, X, HI, idd) {
  m <- ncol(X)
  k <- length(HI)
  N <- nrow(Y)
  idl <- foreach(ii = unique(idd)) %do% which(idd %in% ii)
  
  sigmaNms <- names(HI)
  options(width = 180)
  dimHI <- llply(HI, dim)
  H <- do.call(cBind, HI)
  Th <- solve(tcrossprod(H))
  Th <- do.call(bdiag, foreach(ii = idl) %do% Th[ii,ii])
  rm(list = c("H"))
  ThX <- Th%*%X
  C <- Th-ThX%*%solve(t(X)%*%ThX)%*%t(ThX)
  rm(list = c("ThX", "Th"))
  Ckk <- foreach(ii = idl) %do% C[ii,ii]
  Yk <- foreach(ii = idl) %do% Y[ii, drop = F]
  l <- k*(k+1)/2
  QIWI <- makeQIWI2(HI, l, Ckk, Yk, k, idl)
  
  S <- ginv(fillS2(QIWI[[1]], Ckk))
  gc()
  theta0 <- S%*%QIWI[[2]]
  
  D0 <- solve(Reduce("+", mapply(function(th, qi) {th*do.call(bdiag, qi)}, theta0[,1], QIWI[[1]])))
  
  iM <- iterMINQUE2(D0, X, QIk = QIWI[[1]], kk = QIWI[[3]], theta0[,1], Yk, idl)
  
  
  gc()
  doIM <- (round(sum(abs(iM$theta - theta0[, 1])), 4) != 0)
  while(doIM){
    theta <- iM$theta
    print(theta)
    iM <- iterMINQUE2(iM$D, X, QIk = QIWI[[1]], kk = QIWI[[3]], theta, Yk, idl)
    doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
  }
  
  
  sigma <- fillSigma(iM$theta, QIWI[[3]])
  dimnames(sigma) <- list(sigmaNms, sigmaNms)
  beta <- as(solve(t(X)%*%iM$D%*%X)%*%t(X)%*%iM$D%*%Y, "numeric")
  names(beta) <- colnames(X)
  sigma2 <- sigma[1,1]
  names(sigma2) <- "sigma2"
  tau  <- sigma[2:k, 2:k, drop = F]
  gc()
  return(list(gamma = beta, sigma2 = sigma2, tauBeta = tau))
}

