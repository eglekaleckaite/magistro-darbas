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
library(boot)

#library(sample)
source("10code1.R")
source("10code.R")

pop1 <- makePOP4(M = 10000)
#pop1$IDSTRATI <- 1

smpl <- mySample(pop1, m = 100)
smpl$w2 <- 1/smpl$psch
smpl$w1 <- 1/smpl$pstud
smpl$wtot <- 1/smpl$ptot
smpl$IDSTRATI <- rep(1, nrow(smpl))

mm <- lmer(Y1 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL), data = pop1)#, weights = w1)
summary(mm)

min1 <- myMINQUE2(dt = smpl,
                  fixed = "Y2 ~ 1+W+X1+X1*W",
                  random1 = "~1+X1|IDSCHOOL",
                  weights = NULL)
                  #weights = c("w1", "w2"))
min1




pop2 <- makePOP5(M = 100)
pop2$IDSTRATI <- 1

mm2 <- lmer(Y2 ~ 1+X1+(1+X1|IDSCHOOL), data = pop2)
summary(mm2)



min2 <- myMINQUE2(dt = pop2,
                  fixed = "Y2 ~ 1+X1",
                  random1 = "~1+X1|IDSCHOOL",
                  weights = NULL)

r1 <- bootMer(mm2, nsim = 1000, FUN = mySumm2)

 r2 <- try(bootREML(data = pop2, R = 500,
                    form = "Y1 ~ 1+X1+(1+X1|IDSCHOOL)", idstrata = "IDSTRATI"))



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


################ poisson
pop2 <- makePOP6(M = 100)

mm <- lmer(Y2 ~ 1+X1+(1|IDSCHOOL), data = pop2)#, weights = w1)
summary(mm)

min1 <- myMINQUE2(dt = pop2,
                  fixed = "Y2 ~ 1+X1",
                  random1 = "~1|IDSCHOOL",
                  weights = NULL)
#weights = c("w1", "w2"))
min1

