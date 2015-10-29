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

varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES")
data.2011 <- drop(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                  "BSMMAT05", varb), with = F]))

d2011 <- list(data.2011[,c("BSMMAT01", varb), with = F],
              data.2011[,c("BSMMAT02", varb), with = F], 
              data.2011[,c("BSMMAT03", varb), with = F],
              data.2011[,c("BSMMAT04", varb), with = F],
              data.2011[,c("BSMMAT05", varb), with = F])
d2011 <- llply(d2011, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
d2011 <- imputationList(d2011)


models.0 <- with(d2011, summary(lmer(BSMMAT ~ 1+(1|IDCLASS)+(1|IDSCHOOL))))
(sm0 <- MIsummary(models.0))

mbb0 <- with(d2011, fun = function(x) MYboot(x, formula(BSMMAT ~ 1+(1|IDCLASS)+(1|IDSCHOOL))))
(smb <- MIsummary(mbb0))

mbb01 <- with(d2011, fun = function(x) MYboot(x, formula(BSMMAT ~ 1+(1|IDCLASS)+(1|IDSCHOOL)), nsmpl = 1000))
(smb01 <- MIsummary(mbb01))

mbb02 <- with(d2011, fun = function(x) 
  MYboot(x, formula(BSMMAT ~ 1+(1|IDCLASS)+(1|IDSCHOOL)), strata = data.2011$IDSCHOOL))
(smb02 <- MIsummary(mbb02))


# x <- data.2011$BSMMAT05
# h<-hist(x, breaks=4566, col="black", xlab="Liekanos",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=40)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# lines(xfit, yfit, col="blue", lwd=2) 
