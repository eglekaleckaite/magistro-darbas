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
#data.2011  <- drop.levels(subset(data.2011, IDSCHOOL %in% unique(data.2011$IDSCHOOL)[1:10]))
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                       "BSMMAT05", varb)]))



st1 <- system.time(ivM4 <- myMINQUE2(dt = data.2011,
                                 fixed = "BSMMAT01 ~ 1",
                                 random1 = "~1|IDSCHOOL",
                                 weights = c("STUDWGT", "SCHWGT")))

bbMINQUE <- bootMINQUE(data = data.2011, FUN  = bfunMINQUE, R = 10,
                       fixed = "BSMMAT01 ~ 1+SSEX", random = "~1|IDSCHOOL",
                       strata = data.2011$IDSCHOOL, hierar = F)



