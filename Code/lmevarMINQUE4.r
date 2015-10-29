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
          "SMLIKE", "SBULLED", "SHEDRES")
#data.2011  <- drop.levels(subset(data.2011, IDCLASS %in% unique(data.2011$IDCLASS)[1:2] & SSEX == 1))
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
                                   fixed = "BSMMAT01 ~ 1+SMENG+STHMWRK+SMVALUE",
                                   random1 = "~1+SSEX|IDCLASS", 
                                   random2 = NULL))

st1 <- system.time(ivM2 <- myMINQUE2(dt = data.2011,
                                   fixed = "BSMMAT01 ~ 1+SMENG+STHMWRK",
                                   random1 = "~1|IDSCHOOL"))
st1 <- system.time(ivM1 <- myMINQUE(dt = sleepstudy, fixed = "Reaction ~ Days", random1 = "~Days|Subject"))

st1 <- system.time(ivM2 <- myMINQUE(dt = as.data.frame(Orthodont), fixed = "distance ~ age+Sex", random1 = "~1|Subject"))


myMINQUE2 <- function(dt, fixed, random1 = NULL, random2 = NULL) {
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
      Z1 <- as(as( rr1, "matrix"), "Matrix")
    } else {
      Z1 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
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
    QI <- llply(TT1, function(tt) llply(Z1, function(z) z%*%tt%*%t(z)))
  } else {
    Z1 <- NULL
    clnms1 <- NULL
    n1 <- NULL
    QI <- NULL
  }
  
  l <- l+1
  Qj <- c(list(llply(QI[[1]], function(zj) diag(nrow(zj)))), QI)
  Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i,]}
  Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
  Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", llply(Qj, function(qj) qj[[i]])))
  V0 <- bdiag(Vj)
  #C0 <- V0-V0%*%X%*%solve(t(X)%*%V0%*%X)%*%t(X)%*%V0
  C0 <- diag(N)-X%*%solve(t(X)%*%X)%*%t(X)
  Cjj <- foreach(i = unique(id1)) %do% {C0[id1 %in% i, id1 %in% i]}
  #Cjj <- mapply(function(v, x) v-v%*%x%*%solve(t(x)%*%v%*%x)%*%t(x)%*%v, Vj, Xj)
  
  S <- fillS(Cjj, Qj)
    #fillS(Cjj, Qj)
  WI <- laply(Qj, function(qj) Reduce("+", foreach(j = 1:length(Cjj)) %do% sum(diag(t(Yj[[j]])%*%Cjj[[j]]%*%qj[[j]]%*%Cjj[[j]]%*%Yj[[j]]))))
  
  theta0 <- ginv(S)%*%matrix(WI, ncol = 1)
  Vj0 <- foreach(i = 1:length(unique(id1))) %do% 
    solve(Reduce("+", foreach(k = 1:length(theta0)) %do% {theta0[k]*Qj[[k]][[i]]}))
  #theta0 <- c(3000, 1200)
  iM <- iterMINQUE(Vj0, Xj, Qj, theta0, Yj, id1, X)
  
  
  gc()
  doIM <- (round(sum(abs(iM$theta - theta0)), 4) != 0)
  while(doIM){
    theta <- iM$theta
    print(theta)
    iM <- iterMINQUE(iM$Vj, Xj, Qj, theta, Yj, id1, X)
    doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
  }
    
  gc()
  return(c(iM, N = N, n1 = n1))
}

iterMINQUE <- function(Vj, Xj, Qj, theta, Yj, id1, X){
#   V0 <- bdiag(Vj)
#   C0 <- V0-V0%*%X%*%solve(t(X)%*%V0%*%X)%*%t(X)%*%V0
#   Cjj <- foreach(i = unique(id1)) %do% {C0[id1 %in% i, id1 %in% i]}
  xvx <- solve(t(X)%*%bdiag(Vj)%*%X)
  Cjj <- mapply(function(v, x) v-v%*%x%*%xvx%*%t(x)%*%v, Vj, Xj)
  #Cjj <- mapply(function(v, x) v-v%*%x%*%solve(t(x)%*%v%*%x)%*%t(x)%*%v, Vj, Xj)
  
  WI <- laply(Qj, function(qj) Reduce("+", foreach(j = 1:length(Cjj)) %do% sum(diag(t(Yj[[j]])%*%Cjj[[j]]%*%qj[[j]]%*%Cjj[[j]]%*%Yj[[j]]))))
  
  S <- fillS(Cjj, Qj)
  #fillS(Cjj, Qj)
  theta <- ginv(S)%*%matrix(WI, ncol = 1)
  
  #     CQI <- foreach(r = 1:length(WI)) %do% {Reduce("+", foreach(j = 1:length(Cjj)) %do% sum(diag(Cjj[[j]]%*%Qj[[r]][[j]])))}
  #       #foreach(r = 1:length(WI)) %do% {Reduce("+", foreach(j = 1:length(Cjj)) %do% sum(diag(Cjj[[j]]%*%Qj[[r]][[j]])))}
  #     
  #     theta <- mapply(function(th, cqi, wi){cat("thetai\n");print(th)
  #                                          th*wi/cqi}, theta, CQI, WI)
  Vj <- foreach(j = 1:length(unique(id1))) %do% 
    solve(Reduce("+", foreach(r = 1:length(theta)) %do% {theta[r]*Qj[[r]][[j]]}))
  gc()
  return(list(Vj = Vj, theta = theta))
}

fillS <- function(Cjj, Qj) {
  x <- matrix(0, ncol = length(Qj), nrow = length(Qj))
  ns <- nrow(x)
  cc <- combinations(ns,2,1:ns, repeats.allowed = T)
  foreach(ii = 1:nrow(cc)) %do% {
    k <- cc[ii, 1]
    l <- cc[ii, 2]
    s <-  Reduce("+", foreach(j = 1:length(Cjj)) %do% sum(diag(Cjj[[j]]%*%Qj[[k]][[j]]%*%Cjj[[j]]%*%Qj[[l]][[j]])))
    x[k,l] <- s
    x[l, k] <- s
    gc()
  }
  gc()
  return(x)
}

