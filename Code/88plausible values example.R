library(mitools)

## Not run:
## CRAN doesnt like this example
data.dir <- system.file("dta",package="mitools")
files.men <- list.files(data.dir,pattern="m.\\.dta$",full=TRUE)
men <- imputationList(lapply(files.men, foreign::read.dta))
files.women <- list.files(data.dir,pattern="f.\\.dta$",full=TRUE)
women <- imputationList(lapply(files.women, foreign::read.dta))
men <- update(men, sex=1)
women <- update(women,sex=0)
all <- rbind(men,women)
all <- update(all, drinkreg=as.numeric(drkfre)>2)
all
## End(Not run)

data(smi)
models<-with(smi, glm(drinkreg~wave*sex,family=binomial()))
summary(MIcombine(models))
betas<-MIextract(models,fun=coef)
vars<-MIextract(models, fun=vcov)
summary(MIcombine(betas,vars))


require(Zelig)
require(Amelia)
require(ZeligMultilevel)

data(freetrade)
length(freetrade$country) #grouping variable

#Imputation of missing data

a.out <- amelia(freetrade, m=5, ts="year", cs="country")

library("ZeligMultilevel")
ML.model.0 <- zelig(dv~1 + tag(1|group), model="ls.mixed",
                    data=a.out$imputations)
summary(ML.model.0)

data(voteincome)
z.out1 <- zelig(income ~ education + age + female + tag(1 | state), data=voteincome, model="ls.mixed")


#########################################################################
#Mano
#########################################################################
rm(list = ls())
library(plyr)
library(mvtnorm)
library(expm)
library(simFrame)
library(foreach)
library(msm)
library(gtools)
library(MASS)
library(lme4)
library(gdata)
library(Matrix)
library(data.table)
library(foreach)
require(ZeligMultilevel)
library(Zelig)
require(Amelia)
#library(sample)
source("10code1.R")

#set.seed(2345)


load("data.2011.RData")
varb <- c("IDSCHOOL", "IDCLASS", "IDSTRATI", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES", "IDSTUD", "TOTWGT", "SCHWGT", "STUDWGT",
          "SCHWGT")
#data.2011  <- drop.levels(subset(data.2011, IDSCHOOL %in% unique(data.2011$IDSCHOOL)[1:10]))
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                              "BSMMAT05", varb), with = F]))
data.2011[, c("IDSCHOOL", "IDCLASS", "IDSTUD") := llply(.SD, as.character), .SDcols = c("IDSCHOOL", "IDCLASS", "IDSTUD")]
data.list <- foreach(ii = c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                            "BSMMAT05")) %do% {
                              dd <- drop.levels(na.omit(data.2011[,c(ii, varb), with = F]))
                              setnames(dd, ii, "BSMMAT")
                              return(dd)
                            }

data.list1 <- imputationList(data.list)

rb0 <- try(bootREML_PV(data = data.list1, R = 10,
                       form = "BSMMAT ~ 1+SSEX+SMENG+(1|IDSCHOOL)", idstrata = "IDSTRATI"))

z.out <- zelig(formula= BSMMAT ~ 1+SSEX+SMENG+tag(1|IDSCHOOL),
               data=data.list, model="ls.mixed")
summary(z.out)

lme.1 <-lmer(BSMMAT01 ~ 1+SSEX+SMENG+(1|IDSCHOOL), data = data.2011)
(sm1 <- summary(lme.1))

ivM4 <- try(bootMINQUE2(fixed = "BSMMAT ~ 1+SSEX+SMENG", random = "~1|IDSCHOOL",
                        idstrata = "IDSTRATI", R = 10, data = data.list, BFUN = bootSample))

#################################################
