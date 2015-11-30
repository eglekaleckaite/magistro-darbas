########## Disertacijos simuliacijos
rm(list = ls())
library(plyr)
library(mvtnorm)
library(expm)
#library(simFrame)
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
library(matrixcalc)

#library(sample)
source("10code.R")

pcg <- c("plyr", "mvtnorm", "expm", "foreach", "msm", "gtools", 
         "MASS", "lme4", "gdata", "Matrix", "data.table", "utils", "iterators",
         "doParallel", "doSNOW")

N <- 1000

cl <- makeCluster(6, outfile="simulPOPbagakas.txt") # number of cores. Notice 'outfile'
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = N, style = 3)

mcmc <- foreach(ii = 1:N, .combine = rbind, .packages = pcg) %dopar% {
  
  res <- simPopMy(M = 300, formul ="Y1 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL)", 
                  popF = makePOPW, sigma2 = 2000, tau00 = 100, 
                  tau01 = 50, tau11 = 100)
  if(class(res) == "try-error") return(NULL)
  
  setTxtProgressBar(pb, ii)
  
  return(res)
}

close(pb)
stopCluster(cl) 

save(mcmc, file = "Output/mcmcY1_simulMy_2000_100_50_100_35.RData")

# pop <- makePOPW(100, sigma2 = 2000, tau00 = 100, tau01 = 50, tau11 = 100)
# smpl <- samplePOP(pop, 35)
# smpl$w1 <- smpl$wstd*smpl$wcl
# smpl$w2 <- smpl$wsch
# 
# 
# mm <- lmer(Y1 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL), data = smpl)
# (sm <- summary(mm))
# 
# (min1 <- myMINQUE(dt = smpl,
#                  fixed = "Y1 ~ 1+W+X1+X1*W",
#                  random1 = "~1+X1|IDSCHOOL",
#                  weights = NULL,
#                  apriori= c(1, 0, 0, 0)))
# 
# (min2 <- myMINQUE(dt = smpl,
#                   fixed = "Y1 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = c("w1", "w2"),
#                   apriori= c(1, 0, 0, 0)))
# 
# 
# (min3 <- myMINQUE(dt = smpl,
#                   fixed = "Y1 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = NULL,
#                   apriori= c(1, 1, 1, 1)))
# 
# (min4 <- myMINQUE(dt = smpl,
#                   fixed = "Y1 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = c("w1", "w2"),
#                   apriori= c(1, 1, 1, 1)))
# 
# lm1 <- lm("Y1 ~ 1+W+X1+X1*W", data = smpl)
# sg <- summary(lm1)$sigma^2
# (min5 <- myMINQUE(dt = smpl,
#                   fixed = "Y1 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = NULL,
#                   apriori= c(sg, 1, 1, 1)))
# 
# (min6 <- myMINQUE(dt = smpl,
#                   fixed = "Y1 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = c("w1", "w2"),
#                   apriori= c(sg, 1, 1, 1)))

