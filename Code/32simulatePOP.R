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

#library(sample)
source("10code.R")


pcg <- c("plyr", "mvtnorm", "expm", "foreach", "msm", "gtools", 
         "MASS", "lme4", "gdata", "Matrix", "data.table", "utils", "iterators",
         "doParallel", "doSNOW")

N <- 1000

cl <- makeCluster(6, outfile="simulPOP.txt") # number of cores. Notice 'outfile'
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = N, style = 3)

mcmc <- foreach(ii = 1:N, .combine = rbind, .packages = pcg) %dopar% {
  
  res <- simPop(M = 80, formul ="Y1 ~ 1+X1+(1+X1|IDSCHOOL)", popF = makePOP5)
  if(class(res) == "try-error") return(NULL)
  
  setTxtProgressBar(pb, ii)

  return(res)
}

close(pb)
stopCluster(cl) 

save(mcmc, file = "Output/mcmcY1_POP_big.RData")


# mMCMC <- apply(mcmc, 2, mean)
# mdiff <- mMCMC-rep(c(1, 0.3, 0.5, 0.005, 0.0025, 0.0025, 0.005), 2)
# 
# load("Output/mcmcY2_POP.RData")
# 
# mMCMC2 <- apply(mcmc, 2, mean)
# mdiff2 <- mMCMC2-rep(c(1, 0.3, 0.5, 0.005, 0.0025, 0.0025, 0.005), 2)
# 
# 
# tb <- cbind(mMCMC[1:7], mdiff[1:7], mMCMC[8:14], mdiff[8:14], mMCMC2[1:7], 
#             mdiff2[1:7], mMCMC2[8:14], mdiff2[8:14])
# colnames(tb) <- rep(c("Įvertis", "Skirtumas"), 4)
# 
# library(xtable)
# 
# xtable(tb, digits = 6)
