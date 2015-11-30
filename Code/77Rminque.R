library(minque)
data(ncii)
res=lmm(Yld~1|Female*Male+Rep,data=ncii,method=c("minque"))
res[[1]]$Var
res[[1]]$FixedEffect
res[[1]]$RandomEffect


library(minque)
data(maize)
#names(maize)
res=lmm(Yld~1|Cultivar*Year+Cultivar*Location+Year*Location,data=maize)
res[[1]]$Var
res[[1]]$FixedEffect
res[[1]]$RandomEffect
res=lmm.jack(Yld~1|Cultivar*Year+Cultivar*Location+Year*Location,
             data=maize,JacNum=10,JacRep=1,ALPHA=0.05)
res[[1]]$Var
res[[1]]$PVar
res[[1]]$FixedEffect
res[[1]]$RandomEffect

sim <- makePOP4(30)
res=lmm(Y1 ~ 1+W+X1+X1*W+1|1*IDSCHOOL+X1*IDSCHOOL+1*X1*IDSCHOOL,data=sim, method = "minque")

mm <- lmer(Y1 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL), data = sim)
summary(mm)
#########################################################
#########################################################
library(CLME)
data( rat.blood )

model_mats <- model_terms_clme( mcv ~ time + temp + sex + (1|id) ,
                                data = rat.blood )
Y  <- model_mats$Y
X1 <- model_mats$X1
X2 <- model_mats$X2
U  <- model_mats$U

# No covariates or random effects
minque(Y = Y, X1 = X1 )

# Include covariates and random effects
minque(Y = Y, X1 = X1, X2 = X2, U = U )
################################################

pop1 <- makePOP4(M = 100)

mf <- model_terms_clme(Y1 ~ -1+W+X1+X1*W+(1+X1|IDSCHOOL), data = pop1)
Y  <- mf$Y
X1 <- mf$X1
X2 <- mf$X2
U  <- mf$U

CLME:::minque(Y = Y, X1 = X1, X2 = X2, U = U, mq.iter=1)



########################################################
########################################################
library(varComp)
library(nlme)
data(Oxide)
vcf0 = varComp(Thickness~Source, Oxide, ~Lot/Wafer,
               control=varComp.control(nlminb=nlminb.control(iter.max=0L)))
coef(vcf0,"var.ratio")
(st=minque(vcf0$residual.contrast, vcf0$working.cor, lower.bound = 0))
(vcf = varComp(Thickness~Source, Oxide, ~Lot/Wafer, control=varComp.control(start=st)))
coef(vcf,"var.ratio")

(vcf = varComp(Y1 ~ 1+W+X1, pop1, ~1+X1/IDSCHOOL,
               control=varComp.control(nlminb=nlminb.control(iter.max=0L))))
coef(vcf,"var.ratio")
(st=minque(y1[-2041:-2043], vcf$working.cor, lower.bound = 0))

mm <- lmer(Y1 ~ 1+W+X1+(1+X1|IDSCHOOL), data = pop1)#, weights = w1)
summary(mm)


########################################################
########################################################
library(Rcpp)
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

pop1 <- makePOP4(M = 35)

mm <- lmer(Y1 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL), data = pop1)#, weights = w1)
summary(mm)

(m1 <- aov(Y1 ~ 1+W+Error(IDSCHOOL), data = pop1))#???


m2 <- lmer(Y1 ~ 1 + (1 | IDSCHOOL), data=pop1)
summary(m2)

res=lmm(Y1~1+W+X1+X1*W|1+X1,data=pop1, method=c("minque"))


########################################################
########################################################

library(minque)
data(ncii)
res=lmm(Yld~1|Female*Male+Rep,data=ncii,method=c("reml","minque"))
res[[1]]$Var
res[[1]]$FixedEffect
res[[1]]$RandomEffect
#End
