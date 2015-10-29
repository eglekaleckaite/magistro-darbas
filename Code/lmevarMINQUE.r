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
data.2011  <- drop.levels(subset(data.2011, MNPEOPLE == 1 & SSEX == 1))
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                       "BSMMAT05", varb)]))

# fixed <- "BSMMAT01 ~ 1+SSEX+STHMWRK+SMENG+SMCONF+SHEDRES"
# random <- "~1+SMENG|IDCLASS"
# fixed <- "BSMMAT01 ~ 1"
# random1 <- "~1|IDCLASS"
# random2 <- "~1|IDSCHOOL"
st1 <- system.time(ivM <- myMINQUE(dt = data.2011,
                                   fixed = "BSMMAT01 ~ 1+SMENG+STHMWRK+SMVALUE",
                                   random1 = "~1|IDCLASS", 
                                   random2 = "~1|IDSCHOOL"))
 
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
    colnames(HH1) <- paste(nmid1, colnames(HH1), sep = ".")
    clnms1 <- colnames(HH1)
    HH1 <- alply(HH1, 2, function(x) Matrix(foreach(i = unique(id1), .combine = cbind) %do% {y <- x; y[!id1 %in% i] <- 0; return(y)}))
    n1 <- length(unique(id1))
  } else {
    HH1 <- NULL
    clnms1 <- NULL
    n1 <- NULL
  }
  
  if (!is.null(random2)) {
    id2 <- strsplit(random2, "\\|")
    random2 <- unlist(id2)[1]
    nmid2 <- unlist(id2)[2]
    rr2 <- model.frame(random2, dt)
    id2 <- model.frame(paste("~", nmid2), dt)[,1]
    if (grepl("-1",random2)) {
      HH2 <- as(as( rr2, "matrix"), "Matrix")
    } else {
      HH2 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr2),
                                dimnames = list(NULL, "(Intercept)")), rr2),
                   "matrix"),
                "Matrix")
    }
    colnames(HH2) <- paste(nmid2, colnames(HH2), sep = ".")
    clnms2 <- colnames(HH2)
    HH2 <- alply(HH2, 2, function(x) Matrix(foreach(i = unique(id2), .combine = cbind) %do% {y <- x; y[!id2 %in% i] <- 0; return(y)}))    
    n2 <- length(unique(id2))
  } else {
    HH2 <- NULL
    clnms2 <- NULL
    n2 <- NULL
  }
  HI <- c(list(diag(N)), HH1, HH2)
  names(HI) <- c("sigma2", clnms1, clnms2)
  gc()
# st1 <- system.time(minqueiv <- lmevarMINQUE(Y, X, HH, id))
# st2 <- system.time(minqueIiv <- lmevarMINQUEI(Y, X, HH, id))

  ivMin <- lmevarMINQUEI2(Y, X, HI, idd = id1)
  gc()
  return(c(ivMin, N = N, n1 = n1, n2 = n2))
}