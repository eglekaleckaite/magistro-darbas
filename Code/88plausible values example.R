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
#library(sample)
source("10code.R")

#set.seed(2345)


load("data.2011.RData")
varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES", "IDSTUD", "TOTWGT", "SCHWGT", "STUDWGT", "SCHWGT")
#data.2011  <- drop.levels(subset(data.2011, IDSCHOOL %in% unique(data.2011$IDSCHOOL)[1:10]))
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                              "BSMMAT05", varb), with = F]))

data.list <- foreach(ii = c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                            "BSMMAT05")) %do% {
                              dd <- drop.levels(na.omit(data.2011[,c(ii, varb), with = F]))
                              setnames(dd, ii, "BSMMAT")
                              return(dd)
                            }
data.list <- imputationList(data.list)
models<-with(data.list, lmer(BSMMAT ~ 1+SSEX+SMENG+(1|IDSCHOOL)))
mm <- MIcombine(models)


