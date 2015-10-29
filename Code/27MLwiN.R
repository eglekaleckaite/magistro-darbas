# Neveikia???

rm(list = ls())
library(nlme)
library(plyr)
library(gdata)
library(lme4)
library(mitools)
library(lmerTest)
library(robustlmm)
library(foreach)
library(R2MLwiN)
library(data.table)
source("10code.R")

load("data.2011.RData")

############################################################################
varb <- c("IDSTUD", "IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "SMENG", "SMCONF",
          "SHEDRES", "TAKADSEKM", "TMOKAPL", "TCONF", "TBENDR",
          "MDYDIS",  "MSUDET.1", "MSUDET.2",
          "MSUDET.3", "MAINCOME.1", "MAINCOME.2", "MAINCOME.3",
          "MVIETA.1", "MVIETA.2", "MVIETA.3",
          "MVIETA.4", "MVIETA.5", "STUDWGT", "SCHWGT", "IDSTRATE")
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", varb), with = F]))

data.2011$cons <- 1
########################################  
# NULL MODEL
######################################## 
lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDSCHOOL), data = data.2011)
(sm0 <- summary(lme.0))

## Define the model
formula="BSMMAT01 ~ (0|cons)+(1|cons)+(2|cons)"
levID=c("IDSCHOOL", "IDSTUD")
## Choose option(s) for inference
estoptions= list(EstM=0)

mlwin ="C:/Program Files (x86)/MLwiN trial/i386"
## Fit the model
mymodel=runMLwiN(formula, levID, D="Normal", data.2011, estoptions, MLwiNPath=mlwin,
                 workdir = "C:/Users/Egle/Desktop/minque/Magistro darbas/Code/MLwiN output/")
summary(mymodel)

############## Su svoriais:
## Define the model
formula="BSMMAT01 ~ (0|cons)+(1|cons)+(2|cons)"
levID=c("IDSCHOOL", "IDSTUD")
## Choose option(s) for inference
estoptions= list(EstM=0, weighting = list(levels = 1:2, weights = c("STUDWGT", "SCHWGT"), mode = 2))

mlwin ="C:/Program Files (x86)/MLwiN trial/i386"
## Fit the model
mymodel=runMLwiN(formula, levID, D="Normal", data.2011, estoptions, MLwiNPath=mlwin,
                 workdir = "C:/Users/Egle/Desktop/minque/Magistro darbas/Code/MLwiN output/")
summary(mymodel)


formula="BSMMAT01 ~ (0|cons)+(1|cons)+(2|cons)"
levID=c("IDSCHOOL", "IDSTUD")
## Choose option(s) for inference
estoptions= list(EstM=0, weighting = list(levels = 1:2, weights = c("STUDWGT", "SCHWGT"), mode = 2))

mlwin ="C:/Program Files (x86)/MLwiN trial/i386"
## Fit the model
mymodel=runMLwiN(formula, levID, D="Normal", data.2011, estoptions, MLwiNPath=mlwin,
                 workdir = "C:/Users/Egle/Desktop/minque/Magistro darbas/Code/MLwiN output/")
summary(mymodel)

########################################################
## Define the model
formula="BSMMAT01 ~ (0|cons)+(1|cons)+(2|cons)+(3|cons)"
levID=c("IDSCHOOL", "IDCLASS", "IDSTUD")
## Choose option(s) for inference
estoptions= list(EstM=0, weighting = list(levels = 1, weights = "TOTWGT", mode = 2))

mlwin ="C:/Program Files (x86)/MLwiN trial/i386"
## Fit the model
mymodel=runMLwiN(formula, levID, D="Normal", data.2011, estoptions, MLwiNPath=mlwin,
                 workdir = "C:/Users/Egle/Desktop/Magistro darbas/Code/MLwiN output/")
summary(mymodel)


formula="BSMMAT01 ~ (0|cons)+(1|cons)+(2|cons+SSEX)"
levID=c("IDSCHOOL","IDSTUD")
## Choose option(s) for inference
estoptions= list(EstM=0)#, weighting = list(levels = c(2,1), weights = c("SCHWGT", "STUDWGT"), mode = 1))#, FSDE = 2, RSDE = 2))

mlwin ="C:/Program Files (x86)/MLwiN trial/i386"
wrkdir <- "C:/Users/Egle/Desktop/minque/Magistro darbas/Code/MLwiN output"
## Fit the model
mymodel <- runMLwiN(formula, levID, D="Normal", data.2011, estoptions, MLwiNPath=mlwin,
                 workdir = wrkdir)
summary(mymodel)


smpl$cons <- 1
smpl <- smpl[,-8]
formula="Y1 ~ (0|cons)+(1|cons)+(2|cons)"
levID=c("IDSCHOOL","IDSTUD")
## Choose option(s) for inference
estoptions= list(EstM=0)#, weighting = list(levels = c(2,1), weights = c("w2", "w1"), mode = 1))#, FSDE = 2, RSDE = 2))

mlwin ="C:/Program Files (x86)/MLwiN trial/i386"
wrkdir <- "C:/Users/Egle/Desktop/minque/Magistro darbas/Code/MLwiN output"
## Fit the model
mymodel <- runMLwiN(formula, levID, D="Normal", smpl, estoptions, MLwiNPath=mlwin,
                    workdir = wrkdir)
summary(mymodel)




## Not run:
library(R2MLwiN)
## Modify the following paths as appropriate.
## MLwiN folder
mlwin ="C:/Program Files (x86)/MLwiN trial/"
## MLwiN sample worksheet folder
wspath=paste(mlwin,"/samples/",sep="")
## MLwiN sample worksheet: tutorial dataset
wsfile=paste(wspath,"tutorial.ws",sep="");inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
## Define the model
formula="normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)"
levID=c("school","student")
## Choose option(s) for inference
estoptions= list(EstM=0)
## Fit the model
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin)
##summary method
summary(mymodel)
##get method
mymodel["LIKE"]
## End(Not run)