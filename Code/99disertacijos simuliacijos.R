########## Disertacijos simuliacijos
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
source("10code1.R")
source("10code.R")

pop1 <- makePOP4(M = 1000)
pop1$IDSTRATI <- 1

mm <- lmer(Y1 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL), data = pop1)
summary(mm)

rb0 <- try(bootREML(data = pop1, R = 100,
                    form = "Y1 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL)", idstrata = "IDSTRATI"))
save(rb0, file = "Output/rb0.RData")

rb0X <- try(bootREML(data = pop1, R = 100,
                    form = "Y2 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL)", idstrata = "IDSTRATI"))
save(rb0X, file = "Output/rb0X.RData")

M0 <- try(bootMINQUE2(fixed = "Y1 ~ 1+W+X1+X1*W", random = "~1+X1|IDSCHOOL", wgt = NULL,
                        idstrata = "IDSTRATI", R = 100, data = pop1, BFUN = bootSampleREML))

save(M0, file = "Output/M0.RData")

M0X <- try(bootMINQUE2(fixed = "Y2 ~ 1+W+X1+X1*W", random = "~1+X1|IDSCHOOL", wgt = NULL,
                      idstrata = "IDSTRATI", R = 100, data = pop1, BFUN = bootSampleREML))

save(M0X, file = "Output/M0X.RData")
