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
         "doParallel", "doSNOW", "nlme")

N <- 1000

cl <- makeCluster(4, outfile="simulPOPW.txt") # number of cores. Notice 'outfile'
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = N, style = 3)

mcmc <- foreach(ii = 1:N, .combine = rbind, .packages = pcg) %dopar% {
  
  res <- simPopMyW1(M = 300, formul ="Y1 ~ 1+W+X1+(1+X1|IDSCHOOL)", 
                  popF = makePOPW3, m = 35, nj = 20)
  if(class(res) == "try-error") return(NULL)
  
  setTxtProgressBar(pb, ii)
  
  return(res)
}

close(pb)
stopCluster(cl) 

save(mcmc, file = "Output/mcmcY1_simulMy_Wf1.RData")


# g00 <- rbind(mcmcRES(mcmc[,1], 1),
#              mcmcRES(mcmc[,4], 1),
#              mcmcRES(mcmc[,7], 1))
# 
# 
# g01 <- rbind(mcmcRES(mcmc[,2], 1),
#              mcmcRES(mcmc[,5], 1),
#              mcmcRES(mcmc[,8], 1))
# 
# g10 <- rbind(mcmcRES(mcmc[,3], 1),
#              mcmcRES(mcmc[,6], 1),
#              mcmcRES(mcmc[,9], 1))
# 
# sigma2 <- rbind(mcmcRES(mcmc[,10], 1),
#                 mcmcRES(mcmc[,11], 1),
#                 mcmcRES(mcmc[,12], 1))
# 
# tau00 <- rbind(mcmcRES(mcmc[,13], 0.25),
#                mcmcRES(mcmc[,17], 0.25),
#                mcmcRES(mcmc[,21], 0.25))
# 
# tau01 <- rbind(mcmcRES(mcmc[,14], sqrt(3)/8),
#                mcmcRES(mcmc[,18], sqrt(3)/8),
#                mcmcRES(mcmc[,22], sqrt(3)/8))
# 
# tau11 <- rbind(mcmcRES(mcmc[,16], 0.75),
#                mcmcRES(mcmc[,20], 0.75),
#                mcmcRES(mcmc[,24], 0.75))

# 
# sink("output_mcmcY1_simulMy_2000_2000_1000_2000_80.txt")
# cat("g00\n")
# print(g00)
# cat("g01\n")
# print(g01)
# cat("g10\n")
# print(g10)
# cat("g11\n")
# print(g11)
# cat("sigma2\n")
# print(sigma2)
# cat("tau00\n")
# print(tau00)
# cat("tau01\n")
# print(tau01)
# cat("tau11\n")
# print(tau11)
# sink()
# 





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

