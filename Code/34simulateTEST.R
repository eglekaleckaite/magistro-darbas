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

cl <- makeCluster(6, outfile="simulPOPbagakas.txt") # number of cores. Notice 'outfile'
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = N, style = 3)

mcmc <- foreach(ii = 1:N, .combine = rbind, .packages = pcg) %dopar% {
  
  res <- simPopB(M = 50, formul ="Y2 ~ 1+X1+X3+X4+(1|IDSCHOOL)", popF = makePOP7)
  if(class(res) == "try-error") return(NULL)
  
  setTxtProgressBar(pb, ii)
  
  return(res)
}

close(pb)
stopCluster(cl) 

save(mcmc, file = "Output/mcmcY2_POP_bagakas_REML_minque0_1_anova.RData")