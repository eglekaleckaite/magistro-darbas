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
library(data.table)
source("10code.R")

load("data.2011.RData")

########################################  
# Total MODEL
########################################
varb <- c("IDSTUD", "IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "SMCONF","SBULLED", "SHEDRES",
          "TMOKAPL", "TCONF", "TISSIL.3", "TAMZIUS.3", "KDYDIS", "MAINCOME.1",
          "MVIETA.4", "MVIETA.5", "MKITKALB.3", "TLYTIS", "MDYDIS", "TOTWGT")
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03",
                                              "BSMMAT04", "BSMMAT05", varb), with = F]))

d2011 <- list(data.2011[,c("BSMMAT01", varb), with = F],
              data.2011[,c("BSMMAT02", varb), with = F], 
              data.2011[,c("BSMMAT03", varb), with = F],
              data.2011[,c("BSMMAT04", varb), with = F],
              data.2011[,c("BSMMAT05", varb), with = F])
d2011 <- llply(d2011, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
d2011 <- imputationList(d2011)
########################################  
# NULL MODEL
######################################## 
lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm0 <- summary(lme.0))
models.0 <- with(d2011, summary(lmer(BSMMAT ~ 1+(1|IDCLASS)+(1|IDSCHOOL))))
(sm01 <- MIsummary(models.0))

mbb0 <- with(d2011, fun = function(x) MYboot(x, formula(BSMMAT ~ 1+(1|IDCLASS)+(1|IDSCHOOL))))
(sm02 <- MIsummary(mbb0))
########################################
lme.1 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SBULLED+SHEDRES+
                TMOKAPL+TCONF+TISSIL.3+TAMZIUS.3+KDYDIS+TLYTIS+
                MAINCOME.1+
                I(MVIETA.4+MVIETA.5)+MKITKALB.3+
                (1+SSEX|IDCLASS)+(1+TLYTIS|IDSCHOOL), data = data.2011)
(sm1 <- summary(lme.1))

models.1 <- with(d2011, summary(lmer(BSMMAT ~ 1+SSEX+STHMWRK2+SMCONF+SBULLED+SHEDRES+
                                  TMOKAPL+TCONF+TISSIL.3+TAMZIUS.3+KDYDIS+TLYTIS+
                                  MAINCOME.1+
                                  I(MVIETA.4+MVIETA.5)+MKITKALB.3+
                                  (1+SSEX|IDCLASS)+(1+TLYTIS|IDSCHOOL))))
(sm11 <- MIsummary(models.1))

mbb1 <- with(d2011, fun = function(x) MYboot(x, formula(BSMMAT ~ 1+SSEX+STHMWRK2+SMCONF+SBULLED+SHEDRES+
                                                          TMOKAPL+TCONF+TISSIL.3+TAMZIUS.3+KDYDIS+TLYTIS+
                                                          MAINCOME.1+
                                                          I(MVIETA.4+MVIETA.5)+MKITKALB.3+
                                                          (1+SSEX|IDCLASS)+(1+TLYTIS|IDSCHOOL))))
(sm12 <- MIsummary(mbb1))
########################################
lme.2 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
                KDYDIS+TLYTIS+
                MAINCOME.1+
                I(MVIETA.4+MVIETA.5)+
                (1+SSEX|IDCLASS)+(1+TLYTIS|IDSCHOOL), data = data.2011)
(sm2 <- summary(lme.2))

models.2 <- with(d2011, summary(lmer(BSMMAT ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
                                       KDYDIS+TLYTIS+
                                       MAINCOME.1+
                                       I(MVIETA.4+MVIETA.5)+MKITKALB.3+
                                       (1+SSEX|IDCLASS)+(1+TLYTIS|IDSCHOOL))))
(sm21 <- MIsummary(models.2))

mbb2 <- with(d2011, fun = function(x) MYboot(x, formula(BSMMAT ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
                                                          KDYDIS+TLYTIS+
                                                          MAINCOME.1+
                                                          I(MVIETA.4+MVIETA.5)+MKITKALB.3+
                                                          (1+SSEX|IDCLASS)+(1+TLYTIS|IDSCHOOL))))
(sm22 <- MIsummary(mbb2))
########################################

lme.3 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
                MDYDIS+
                MAINCOME.1+
                I(MVIETA.4+MVIETA.5)+(1+SSEX|IDSCHOOL), data = data.2011)
(sm3 <- summary(lme.3))

models.3 <- with(d2011, summary(lmer(BSMMAT ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
                                       MDYDIS+
                                       MAINCOME.1+
                                       I(MVIETA.4+MVIETA.5)+
                                       (1+SSEX|IDSCHOOL))))
(sm31 <- MIsummary(models.3))

mbb3 <- with(d2011, fun = function(x) MYboot(x, formula(BSMMAT ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
                                                          MDYDIS+
                                                          MAINCOME.1+
                                                          I(MVIETA.4+MVIETA.5)+
                                                          (1+SSEX|IDSCHOOL))))
(sm23 <- MIsummary(mbb3))


# ############################################################
# data.2011.1 <- ddply(data.2011, ~ IDSTUD, function(x) {for(i in 1:(round(x$TOTWGT)-1)) x <- rbind(x,x); return(x)})
# ########################################  
# # NULL MODEL
# ######################################## 
# lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDCLASS)+(1|IDSCHOOL), data = data.2011.1)
# (sm0 <- summary(lme.0))

