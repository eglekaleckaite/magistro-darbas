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

N <- 500

cl <- makeCluster(6, outfile="simulPOPbagakas.txt") # number of cores. Notice 'outfile'
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = N, style = 3)

mcmc <- foreach(ii = 1:N, .combine = rbind, .packages = pcg) %dopar% {
  
  res <- simPopMyFixed2(M = 35, formul ="Y2 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL)", 
                  popF = makePOPFixed, sigma2 = 2000, tau00 = 100, 
                  tau01 = 50, tau11 = 100, m = 35)
  if(class(res) == "try-error") return(NULL)
  
  setTxtProgressBar(pb, ii)
  
  return(res)
}

close(pb)
stopCluster(cl) 

save(mcmc, file = "Output/mcmcY2_simulMy_2000_100_50_100_35_fixed.RData")


# g00 <- rbind(mcmcRES(mcmc[,1], 450),
#              mcmcRES(mcmc[,5], 450),
#              mcmcRES(mcmc[,9], 450),
#              mcmcRES(mcmc[,13], 450),
#              mcmcRES(mcmc[,17], 450),
#              mcmcRES(mcmc[,21], 450),
#              mcmcRES(mcmc[,25], 450))
# 
# g01 <- rbind(mcmcRES(mcmc[,2], 10),
#              mcmcRES(mcmc[,6], 10),
#              mcmcRES(mcmc[,10], 10),
#              mcmcRES(mcmc[,14], 10),
#              mcmcRES(mcmc[,18], 10),
#              mcmcRES(mcmc[,22], 10),
#              mcmcRES(mcmc[,26], 10))
# 
# g10 <- rbind(mcmcRES(mcmc[,3], 30),
#              mcmcRES(mcmc[,7], 30),
#              mcmcRES(mcmc[,11], 30),
#              mcmcRES(mcmc[,15], 30),
#              mcmcRES(mcmc[,19], 30),
#              mcmcRES(mcmc[,23], 30),
#              mcmcRES(mcmc[,27], 30))
# 
# g11 <- rbind(mcmcRES(mcmc[,4], 5),
#              mcmcRES(mcmc[,8], 5),
#              mcmcRES(mcmc[,12], 5),
#              mcmcRES(mcmc[,16], 5),
#              mcmcRES(mcmc[,20], 5),
#              mcmcRES(mcmc[,24], 5),
#              mcmcRES(mcmc[,28], 5))
# 
# sigma2 <- rbind(mcmcRES(mcmc[,29], 2000),
#                 mcmcRES(mcmc[,30], 2000),
#                 mcmcRES(mcmc[,31], 2000),
#                 mcmcRES(mcmc[,32], 2000),
#                 mcmcRES(mcmc[,33], 2000),
#                 mcmcRES(mcmc[,34], 2000),
#                 mcmcRES(mcmc[,35], 2000))
# 
# tau00 <- rbind(mcmcRES(mcmc[,36], 100),
#                mcmcRES(mcmc[,40], 100),
#                mcmcRES(mcmc[,44], 100),
#                mcmcRES(mcmc[,48], 100),
#                mcmcRES(mcmc[,52], 100),
#                mcmcRES(mcmc[,56], 100),
#                mcmcRES(mcmc[,60], 100))
# 
# tau01 <- rbind(mcmcRES(mcmc[,37], 1000),
#                mcmcRES(mcmc[,41], 1000),
#                mcmcRES(mcmc[,45], 1000),
#                mcmcRES(mcmc[,49], 1000),
#                mcmcRES(mcmc[,53], 1000),
#                mcmcRES(mcmc[,57], 1000),
#                mcmcRES(mcmc[,61], 1000))
# 
# tau11 <- rbind(mcmcRES(mcmc[,39], 2000),
#                mcmcRES(mcmc[,43], 2000),
#                mcmcRES(mcmc[,47], 2000),
#                mcmcRES(mcmc[,51], 2000),
#                mcmcRES(mcmc[,55], 2000),
#                mcmcRES(mcmc[,59], 2000),
#                mcmcRES(mcmc[,63], 2000))
# 
# 
# sink("output_mcmcY2_simulMy_2000_2000_1000_2000_20.txt")
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
# mm <- lmer(Y2 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL), data = smpl)
# (sm <- summary(mm))
# 
# (min1 <- myMINQUE(dt = smpl,
#                  fixed = "Y2 ~ 1+W+X1+X1*W",
#                  random1 = "~1+X1|IDSCHOOL",
#                  weights = NULL,
#                  apriori= c(1, 0, 0, 0)))
# 
# (min2 <- myMINQUE(dt = smpl,
#                   fixed = "Y2 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = c("w1", "w2"),
#                   apriori= c(1, 0, 0, 0)))
# 
# 
# (min3 <- myMINQUE(dt = smpl,
#                   fixed = "Y2 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = NULL,
#                   apriori= c(1, 1, 1, 1)))
# 
# (min4 <- myMINQUE(dt = smpl,
#                   fixed = "Y2 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = c("w1", "w2"),
#                   apriori= c(1, 1, 1, 1)))
# 
# lm1 <- lm("Y2 ~ 1+W+X1+X1*W", data = smpl)
# sg <- summary(lm1)$sigma^2
# (min5 <- myMINQUE(dt = smpl,
#                   fixed = "Y2 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = NULL,
#                   apriori= c(sg, 1, 1, 1)))
# 
# (min6 <- myMINQUE(dt = smpl,
#                   fixed = "Y2 ~ 1+W+X1+X1*W",
#                   random1 = "~1+X1|IDSCHOOL",
#                   weights = c("w1", "w2"),
#                   apriori= c(sg, 1, 1, 1)))

# qsigma2 <- quantile(mcmc[,29]/2000, probs = c(0.01, 0.05, 0.1, 0.2,
#                                               0.3, 0.4, 0.5, 0.6, 0.7,
#                                               0.8, 0.9, 0.95, 0.99))
