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

lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm0 <- summary(lme.0))

lme.1 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK+SMENG+SMCONF+
               SHEDRES+(1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm1 <- summary(lme.1))

lme.2 <- lmer(BSMMAT01 ~ 1+SSEX+STHMWRK+SMENG+SMCONF+SMVALUE+SMLIKE+SBULLED+
               SHEDRES+(1+SSEX+SMCONF|IDCLASS)+(1+SMCONF|IDSCHOOL), data = data.2011)
(sm2 <- summary(lme.2))

############################################################################
varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES", "TPROBL", "TAKADSEKM", "TMOKAPL", "TCONF",
          "TSATISF", "TBENDR", "TENGAGE", "TMTEST", "TDHMWRK", "TLYTIS", "TISSIL",
          "TPATIR", "TGUIDE", "TQUIZ", "TNOGUIDE", "TBOOKS", "KDYDIS", "MDYDIS",
          "MSUDET", "MDISSAFE", "MSUCCESS", "MMATSHORT", "MCOMP", "MAINCOME",
          "MVIETA", "MNPEOPLE", "MKITKALB", "MEDISAD", "MEAFFL")
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
models.0 <- with(d2011, summary(lmer(BSMMAT ~ 1+(1|IDCLASS)+(1|IDSCHOOL))))
 
(sm0 <- MIsummary(models.0))
## Total variance
(tv.0 <- sm0$sigma^2+sum(unlist(sm0$varcor)))
##### within classroom
(rho1 <- sm0$sigma^2/tv.0)
##### between classroom
(rho2 <- unlist(sm0$varcor)[1]/tv.0)
##### between school
(rho3 <- unlist(sm0$varcor)[2]/tv.0)
########################################  
# STUDENTS LEVEL
########################################                 
models.11 <- with(d2011, summary(lmer(BSMMAT ~ 1+SSEX+STHMWRK+SMENG+SMCONF+
                               SHEDRES+(1|IDCLASS)+(1|IDSCHOOL))))

(sm11 <- MIsummary(models.11))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm11$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- unlist(sm11$varcor)[1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- unlist(sm11$varcor)[2])/unlist(sm0$varcor)[2])
########################################                 
models.12 <- with(d2011, summary(lmer(BSMMAT ~ 1+SSEX+STHMWRK+SMENG+SMCONF+
                               SHEDRES+(1+SSEX|IDCLASS)+(1+SSEX|IDSCHOOL))))

(sm12 <- MIsummary(models.12))
#### 1 lvl R^2
(R21 <- (sm0$sigma^2 - sm12$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R22 <- (unlist(sm0$varcor)[1]- sm12$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R23 <- (unlist(sm0$varcor)[2]- sm12$varcor[[2]][1])/unlist(sm0$varcor)[2])
########################################  
# CLASS/TEACHER LEVEL
########################################  
models.21 <- with(d2011, summary(lmer(BSMMAT ~ 1+TAKADSEKM+KDYDIS+
                             (1|IDCLASS)+(1|IDSCHOOL))))

(sm21 <- MIsummary(models.21))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm21$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm21$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm21$varcor[[2]][1])/unlist(sm0$varcor)[2])
########################################  
# CLASS/TEACHER LEVEL
########################################  
models.31 <- with(d2011, summary(lmer(BSMMAT ~ 1+MSUDET+MNPEOPLE+(1|IDCLASS)+(1|IDSCHOOL))))

(sm31 <- MIsummary(models.31))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm31$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm31$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm31$varcor[[2]][1])/unlist(sm0$varcor)[2])

########################################  
# ALL LEVEL
########################################
models.41 <- with(d2011, summary(lmer(BSMMAT ~ 1+MSUDET+MNPEOPLE+SSEX+STHMWRK+SMENG+
                                SMCONF+TAKADSEKM+KDYDIS+
                                SHEDRES+(1|IDCLASS)+(1|IDSCHOOL))))

(sm41 <- MIsummary(models.41))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm41$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm41$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm41$varcor[[2]][1])/unlist(sm0$varcor)[2])

# Atskiriems regionams
#######################
models.411 <- with(d2011, summary(lmer(BSMMAT ~ 1+MSUDET+SSEX+STHMWRK+SMENG+
                                        SMCONF+TAKADSEKM+KDYDIS+
                                        SHEDRES+(1|IDCLASS)+(1|IDSCHOOL))))

(sm411 <- MIsummary(models.411))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm411$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm411$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm411$varcor[[2]][1])/unlist(sm0$varcor)[2])

# Vilnius
dd1 <- subset(data.2011, MNPEOPLE == 1)
d2011 <- list(dd1[,c("BSMMAT01", varb), with = F], 
              dd1[,c("BSMMAT02", varb), with = F], 
              dd1[,c("BSMMAT03", varb), with = F],
              dd1[,c("BSMMAT04", varb), with = F],
              dd1[,c("BSMMAT05", varb), with = F])
d2011 <- llply(d2011, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
d2011 <- imputationList(d2011)

models.412 <- with(d2011, summary(lmer(BSMMAT ~ 1+MSUDET+SSEX+STHMWRK+SMENG+
                                         SMCONF+TAKADSEKM+KDYDIS+
                                         SHEDRES+(1|IDCLASS)+(1|IDSCHOOL))))

(sm412 <- MIsummary(models.412))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm412$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm412$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm412$varcor[[2]][1])/unlist(sm0$varcor)[2])

# Didmiesciai
dd1 <- subset(data.2011, MNPEOPLE == 2)
d2011 <- list(dd1[,c("BSMMAT01", varb), with = F], 
              dd1[,c("BSMMAT02", varb), with = F], 
              dd1[,c("BSMMAT03", varb), with = F],
              dd1[,c("BSMMAT04", varb), with = F],
              dd1[,c("BSMMAT05", varb), with = F])
d2011 <- llply(d2011, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
d2011 <- imputationList(d2011)

models.412 <- with(d2011, summary(lmer(BSMMAT ~ 1+MSUDET+SSEX+STHMWRK+SMENG+
                                         SMCONF+TAKADSEKM+KDYDIS+
                                         SHEDRES+(1|IDCLASS)+(1|IDSCHOOL))))

(sm412 <- MIsummary(models.412))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm412$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm412$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm412$varcor[[2]][1])/unlist(sm0$varcor)[2])

# Miestai
dd1 <- subset(data.2011, MNPEOPLE == 3)
d2011 <- list(dd1[,c("BSMMAT01", varb), with = F],
              dd1[,c("BSMMAT02", varb), with = F], 
              dd1[,c("BSMMAT03", varb), with = F],
              dd1[,c("BSMMAT04", varb), with = F],
              dd1[,c("BSMMAT05", varb), with = F])
d2011 <- llply(d2011, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
d2011 <- imputationList(d2011)

models.412 <- with(d2011, summary(lmer(BSMMAT ~ 1+MSUDET+SSEX+STHMWRK+SMENG+
                                         SMCONF+TAKADSEKM+KDYDIS+
                                         SHEDRES+(1|IDCLASS)+(1|IDSCHOOL))))

(sm412 <- MIsummary(models.412))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm412$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm412$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm412$varcor[[2]][1])/unlist(sm0$varcor)[2])

# Miesteliai
dd1 <- subset(data.2011, MNPEOPLE == 4)
d2011 <- list(dd1[,c("BSMMAT01", varb), with = F],
              dd1[,c("BSMMAT02", varb), with = F], 
              dd1[,c("BSMMAT03", varb), with = F],
              dd1[,c("BSMMAT04", varb), with = F],
              dd1[,c("BSMMAT05", varb), with = F])
d2011 <- llply(d2011, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
d2011 <- imputationList(d2011)

models.412 <- with(d2011, summary(lmer(BSMMAT ~ 1+MSUDET+SSEX+STHMWRK+SMENG+
                                         SMCONF+TAKADSEKM+KDYDIS+
                                         SHEDRES+(1|IDCLASS)+(1|IDSCHOOL))))

(sm412 <- MIsummary(models.412))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm412$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm412$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm412$varcor[[2]][1])/unlist(sm0$varcor)[2])

# Kaimai
dd1 <- subset(data.2011, MNPEOPLE %in% 5:6)
d2011 <- list(dd1[,c("BSMMAT01", varb), with = F],
              dd1[,c("BSMMAT02", varb), with = F], 
              dd1[,c("BSMMAT03", varb), with = F],
              dd1[,c("BSMMAT04", varb), with = F],
              dd1[,c("BSMMAT05", varb), with = F])
d2011 <- llply(d2011, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
d2011 <- imputationList(d2011)

models.412 <- with(d2011, summary(lmer(BSMMAT ~ 1+MSUDET+SSEX+STHMWRK+SMENG+
                                         SMCONF+TAKADSEKM+KDYDIS+
                                         SHEDRES+(1|IDCLASS)+(1|IDSCHOOL))))

(sm412 <- MIsummary(models.412))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm412$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm412$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm412$varcor[[2]][1])/unlist(sm0$varcor)[2])

########################################
varb <- c("IDSCHOOL", "IDCLASS", "MFEDUC", "CLPREDUC", "SHPREDUC",
          "TOTPOSS1", "CLTOTPOS1", "SHTOTPOS1",
          "TOTPOSS2", "CLTOTPOS2", "SHTOTPOS2",
          "NBOOKS", "CLNBOOKS", "SHNBOOKS",
          "MTPRES", "CLMTPRES", "SHMTPRES",
          "SMENG", "CLMENG", "SHMENG",
          "SMCONF", "CLCONF", "SHCONF",
          "SMVALUE", "CLVALUE", "SHVALUE",
          "SMLIKE", "CLLIKE", "SHLIKE", 
          "SBULLED", "CLBULLED", "SHBULLED")
d2011 <- list(data.2011[,c("BSMMAT01", varb), with = F],
              data.2011[,c("BSMMAT02", varb), with = F], 
              data.2011[,c("BSMMAT03", varb), with = F],
              data.2011[,c("BSMMAT04", varb), with = F],
              data.2011[,c("BSMMAT05", varb), with = F])
d2011 <- llply(d2011, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
d2011 <- imputationList(d2011)

models.51 <- with(d2011, summary(lmer(BSMMAT ~ 1+MFEDUC+CLPREDUC+SHPREDUC+
#                               TOTPOSS1+CLTOTPOS1+SHTOTPOS1+
#                               TOTPOSS2+CLTOTPOS2+SHTOTPOS2+
#                               NBOOKS+CLNBOOKS+SHNBOOKS+
                                (1|IDCLASS)+(1|IDSCHOOL))))

(sm51 <- MIsummary(models.51))
#### 1 lvl R^2
(R11 <- (sm0$sigma^2 - sm51$sigma^2)/sm0$sigma^2)
#### 2 lvl R^2
(R12 <- ( unlist(sm0$varcor)[1]- sm51$varcor[[1]][1])/unlist(sm0$varcor)[1])
#### 3 lvl R^2
(R13 <- ( unlist(sm0$varcor)[2]- sm51$varcor[[2]][1])/unlist(sm0$varcor)[2])








#################################################################


