rm(list = ls())
library(nlme)
library(plyr)
library(gdata)
library(lme4)
library(mitools)
library(lmerTest)
library(robustlmm)
library(foreach)
library(data.table)
source("10code.R")

load("data.2011.RData")

############################################################################
varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES", "TPROBL", "TAKADSEKM", "TMOKAPL", "TCONF",
          "TSATISF", "TBENDR", "TENGAGE", "TLYTIS", "TISSIL2", "TISSIL.1", 
          "TISSIL.2", "TISSIL.3", "TPATIR", "TAMZIUS2", "TAMZIUS.1", "TAMZIUS.2",
          "TAMZIUS.3", "KDYDIS", "MDYDIS", "MSUDET2", "MSUDET.1", "MSUDET.2",
          "MSUDET.3", "MDISSAFE", "MSUCCESS", "MMATSHORT", "MAINCOME", "MAINCOME.1",
          "MAINCOME.2", "MAINCOME.3", "MVIETA2", "MVIETA.1", "MVIETA.2", "MVIETA.3",
          "MVIETA.4", "MVIETA.5", "MKITKALB2", "MKITKALB.1", "MKITKALB.2", "MKITKALB.3")
data.2011 <- data.2011[,c("BSMMAT01", varb), with = F]

########################################  
# NULL MODEL
######################################## 
lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm0 <- summary(lme.0))


########################################  
# STUDENT MODEL
######################################## 
lme.1 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SBULLED+SHEDRES+
               # SMLIKE+SMVALUE+SMENG+
               (1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm1 <- summary(lme.1))


########################################  
# TEACHER MODEL
########################################

lme.2 <- lmer(BSMMAT01 ~ 1+
                # TSATISF+TISSIL.2+TPROBL+TENGAGE+TAMZIUS.2+TAMZIUS.3+
                TAKADSEKM+TMOKAPL+TCONF+TBENDR+
                TLYTIS+TISSIL.3+TAMZIUS.1+KDYDIS+
                (1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm2 <- summary(lme.2))

########################################  
# SCHOOL MODEL
########################################

lme.3 <- lmer(BSMMAT01 ~ 1+
                # MSUDET.2+MDISSAFE+MSUCCESS+MKITKALB.1+MAINCOME.1+MAINCOME.3+
                # I(MVIETA.1+MVIETA.2)+MINSTRHWR+MMATSHORT+MSUDET.3+MDYDIS+MAINCOME.3+
                MAINCOME.1+
                I(MVIETA.4+MVIETA.5)+I(MKITKALB.2+MKITKALB.3)+
                (1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm3 <- summary(lme.3))


########################################  
# Total MODEL
########################################
varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "SMCONF","SBULLED", "SHEDRES",
          "TMOKAPL", "TCONF", "TISSIL.3", "TAMZIUS.3", "KDYDIS", "MAINCOME.1",
          "MVIETA.4", "MVIETA.5", "MKITKALB.3", "TLYTIS", "MDYDIS")
data.2011 <- drop.levels(na.omit(data.2011[, c("BSMMAT01", varb), with = F]))

########################################  
# NULL MODEL
######################################## 
lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm0 <- summary(lme.0))
lme.01 <-lmer(BSMMAT01 ~ 1+(1|IDCLASS), data = data.2011)
(sm01 <- summary(lme.01))

ci0 <- confint(lme.0)
anova(lme.0,lme.01)
########################################

lme.4 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SBULLED+SHEDRES+
                TMOKAPL+TCONF+TISSIL.3+TAMZIUS.3+
                MAINCOME.1+KDYDIS+
                I(MVIETA.4+MVIETA.5)+MKITKALB.3+
                (1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm4 <- summary(lme.4))
ci4 <- confint(lme.4)
anova(lme.0,lme.4)
########################################

lme.5 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SBULLED+SHEDRES+
                TMOKAPL+TCONF+TISSIL.3+TAMZIUS.3+KDYDIS+
                MAINCOME.1+
                I(MVIETA.4+MVIETA.5)+MKITKALB.3+
                (1+SSEX|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm5 <- summary(lme.5))

anova(lme.4,lme.5)
ci5 <- confint(lme.5, 1:5)

lme.6 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SBULLED+SHEDRES+
                TMOKAPL+TCONF+TISSIL.3+TAMZIUS.3+KDYDIS+TLYTIS+
                MAINCOME.1+
                I(MVIETA.4+MVIETA.5)+MKITKALB.3+
                (1+SSEX|IDCLASS)+(1+TLYTIS|IDSCHOOL), data = data.2011)
(sm6 <- summary(lme.6))

anova(lme.5,lme.6)
#ci6 <- confint(lme.6)

lme.7 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
                KDYDIS+TLYTIS+
                MAINCOME.1+
                I(MVIETA.4+MVIETA.5)+
                (1+SSEX|IDCLASS)+(1+TLYTIS|IDSCHOOL), data = data.2011)
(sm7 <- summary(lme.7))

anova(lme.7,lme.6)

lme.8 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
                MDYDIS+
                MAINCOME.1+
                I(MVIETA.4+MVIETA.5)+(1+SSEX|IDSCHOOL), data = data.2011)
(sm8 <- summary(lme.8))

anova(lme.7,lme.8)
