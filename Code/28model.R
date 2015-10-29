rm(list = ls())
library(nlme)
library(plyr)
library(gdata)
library(lme4)
library(mitools)
library(lmerTest)
library(robustlmm)
library(foreach)
source("10code.R")

load("data.2011.RData")

############################################################################
varb <- c("IDSTUD", "IDSCHOOL", "IDCLASS", 
          "SSEX", "STHMWRK2", "SMENG", "SMCONF", "SMVALUE", "SHEDRES",
                   "SMLIKE", "SBULLED", "TPROBL", "TAKADSEKM", "TMOKAPL", "TCONF",
          #           "TSATISF", "TBENDR", "TENGAGE", "TLYTIS", "TISSIL2", "TISSIL.1", 
          #           "TISSIL.2", "TISSIL.3", "TPATIR", "TAMZIUS2", "TAMZIUS.1", "TAMZIUS.2",
          #           "TAMZIUS.3", "KDYDIS", "MDYDIS", "MSUDET2", "MSUDET.1", "MSUDET.2",
          #           "MSUDET.3", "MDISSAFE", "MSUCCESS", "MMATSHORT", "MAINCOME", "MAINCOME.1",
          #           "MAINCOME.2", "MAINCOME.3", "MVIETA2", "MVIETA.1", "MVIETA.2", "MVIETA.3",
          #           "MVIETA.4", "MVIETA.5", "MKITKALB2", "MKITKALB.1", "MKITKALB.2", "MKITKALB.3",
          "TOTWGT", "SCHWGT", "STUDWGT", "SCHWGT")
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", varb), with = F]))
data.2011$cons <- 1
data.2011$SSEX1 <- data.2011$SSEX*1/sqrt(data.2011$SCHWGT)
############################################################################
lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDSCHOOL), data = data.2011)
(sm0 <- summary(lme.0))

lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDSCHOOL), data = data.2011, weights = 1/data.2011$STUDWGT)
(sm0 <- summary(lme.0))

lme.0 <-lmer(BSMMAT01 ~ 1+(1+SSEX|IDSCHOOL), data = data.2011, weights = sqrt(data.2011$STUDWGT)*sqrt(data.2011$SCHWGT))
(sm0 <- summary(lme.0))

lme.1 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMENG+SMCONF+
               SHEDRES+(1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm1 <- summary(lme.1))

lme.2 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMENG+SMCONF+SMVALUE+SMLIKE+SBULLED+
               SHEDRES+(1+SSEX+SMCONF|IDCLASS)+(1+SMCONF|IDSCHOOL), data = data.2011)
(sm2 <- summary(lme.2))

