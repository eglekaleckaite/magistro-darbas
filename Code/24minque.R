## Neveikia???

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
library(data.table)
source("10code.R")

load("data.2011.RData")

varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES")
data.2011 <- drop(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                       "BSMMAT05", varb)]))

dt <- data.2011
fixed <- "BSMMAT01 ~ 1+SSEX+STHMWRK+SMENG+SMCONF+SHEDRES"
random <- "~1+STHMWRK|IDCLASS"
ff <- model.frame(fixed, dt)
fact1 <- strsplit(random, "\\|")
random <- unlist(fact1)[1]
fact1 <- unlist(fact1)[2]
rr <- model.frame(random, dt)
fact1 <- model.frame(paste("~", fact1), dt)[,1]

Y <- as(as(ff[,1, drop = F], "matrix"), "Matrix")
Z <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(ff),
                   dimnames = list(NULL, "(Intercept)")), ff[,-1, drop = F]), "matrix"), "Matrix")
rr <- as(cbind(matrix(1, ncol = 1, nrow = nrow(rr),
                   dimnames = list(NULL, "(Intercept)")), rr), "matrix")
XI <- alply(rr, 2, function(x) Matrix(foreach(i = unique(fact1), 
                                             .combine = cbind) %do% {y <- x; y[!fact1 %in% i] <- 0; return(y)}))

X0 <- list(Matrix(diag(1, nrow = dim(dt)[1])))
XI <- c(X0, XI)
# X1 <- Matrix(foreach(i = unique(id1), .combine = cbind) %do% as.numeric(id1 %in% i))

# V0 <- tcrossprod(X0)
# V1 <- tcrossprod(X1)
VI <- llply(XI, tcrossprod)
# X <- cBind(X0, X1)
V <- Reduce("+",VI)
iV <- Matrix(solve(V))
SV <- as(ginv(as(t(Z)%*%crossprod(iV, Z), "matrix")), "Matrix")

P  <-  crossprod(iV, (diag(1, nrow(dt))-Z%*%tcrossprod(SV,Z)%*% iV))
GammaI <- laply(VI, function(mm) as.numeric(t(Y)%*% P %*% mm %*% P %*% Y))
S <- fillSMINQUE2(P,VI)

iS <- ginv(S)
sigma <- 1/S*GammaI
D <- solve(Reduce("+", mapply(function(vi,x) vi*x, VI, sigma)))

#   llply(PV, function(x, PV) llply(PV, function(y) y), PV = PV, .parallel = T)
#   mapply(function(x, y) {sum(diag(crossprod(x,y)))}, PV, PV)
beta <- ginv(as(t(Z)%*%D%*%Z, "matrix"))%*%t(Z)%*%D%*%Y
#sigma <- solve(sum(diag(P%*%Z%*%t(Z)%*%P%*%Z%*%t(Z))))%*%t(Y)%*%P%*%Z%*%t(Z)%*%P%*%Y
# s00 <- sum(diag(V0%*%P%*%V0%*%P))
# s01 <- sum(diag(V0%*%P%*%V1%*%P))
# s11 <- sum(diag(V1%*%P%*%V1%*%P))
# gamma0 <- t(Y)%*%P%*%V0%*%P%*%Y
# gamma1 <- t(Y)%*%P%*%V1%*%P%*%Y
# S <- abs(s00*s11-s01^2)
# sigma00 <- (s00*gamma1-s01*gamma0)/S
# sigma11 <- (s11*gamma0-s01*gamma1)/S