########## bagakas simuliacijos
rm(list = ls())
library(plyr)
library(mvtnorm)
library(expm)
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
library(utils)
library(iterators)
library(doParallel)
library(doSNOW)
library(mitools)

#library(sample)
source("10code.R")


pcg <- c("plyr", "mvtnorm", "expm", "foreach", "msm", "gtools", 
         "MASS", "lme4", "gdata", "Matrix", "data.table", "utils", "iterators",
         "doParallel", "doSNOW")

N <- 1000

pop <- makePOP7(50)

mm <- lmer(Y2 ~ 1+X1+X3+X4+(1|IDSCHOOL), data = pop)
summary(mm)

min1 <- myMINQUE(dt = pop,
                  fixed = "Y2 ~ 1+X1+X3+X4",
                  random1 = "~1|IDSCHOOL",
                  weights = NULL,
                  apriori= c(1, 0))
#weights = c("w1", "w2"))
min1

min2 <- myMINQUE(dt = pop,
                 fixed = "Y2 ~ 1+X1+X3+X4",
                 random1 = "~1|IDSCHOOL",
                 weights = NULL,
                 apriori= c(1, 1))
min2

min3 <- myMINQUE(dt = pop,
                 fixed = "Y2 ~ 1+X1+X3+X4",
                 random1 = "~1|IDSCHOOL",
                 weights = NULL,
                 apriori= c(100, 5))
min3


p <- ggplot(data=as.data.frame(qqnorm(pop$eP, plot=F)), mapping=aes(x=x, y=y)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)+labs(x="Teoriniai kvantiliai", y = "Matematikos rezultatas")


pp <- ggplot(data=as.data.frame(qqnorm(pop$eN, plot=F)), mapping=aes(x=x, y=y)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)+labs(x="Teoriniai kvantiliai", y = "Matematikos rezultatas")
