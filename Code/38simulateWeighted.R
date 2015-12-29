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
library(xtable)

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

save(mcmc, file = "Output/mcmcY1_simulMy_W_2.RData")


mcmc1 <- mcmc
load("Output/mcmcY1_simulMy_Wf1.RData")




g00 <- rbind(mcmcRES(mcmc[,1], 1),
             mcmcRES(mcmc[,4], 1),
             mcmcRES(mcmc[,7], 1))


g01 <- rbind(mcmcRES(mcmc[,2], 1),
             mcmcRES(mcmc[,5], 1),
             mcmcRES(mcmc[,8], 1))

g10 <- rbind(mcmcRES(mcmc[,3], 1),
             mcmcRES(mcmc[,6], 1),
             mcmcRES(mcmc[,9], 1))

sigma2 <- rbind(mcmcRES(mcmc[,10], 1, set.zero = T),
                mcmcRES(mcmc[,11], 1, set.zero = T),
                mcmcRES(mcmc[,12], 1, set.zero = T))

tau00 <- rbind(mcmcRES(mcmc[,13], 0.25, set.zero = T),
               mcmcRES(mcmc[,17], 0.25, set.zero = T),
               mcmcRES(mcmc[,21], 0.25, set.zero = T))

tau01 <- rbind(mcmcRES(mcmc[,14], sqrt(3)/8),
               mcmcRES(mcmc[,18], sqrt(3)/8),
               mcmcRES(mcmc[,22], sqrt(3)/8))

tau11 <- rbind(mcmcRES(mcmc[,16], 0.75, set.zero = T),
               mcmcRES(mcmc[,20], 0.75, set.zero = T),
               mcmcRES(mcmc[,24], 0.75, set.zero = T))

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

## form table
tbb <- rbind(as.vector(t(g00)), as.vector(t(g01)), as.vector(t(g10)), 
             as.vector(t(sigma2)), as.vector(t(tau00)), as.vector(t(tau01)),
             as.vector(t(tau11)))
tbb <- round(tbb, 3)
tbb1 <- rbind(as.vector(t(g00)), as.vector(t(g01)), as.vector(t(g10)), 
             as.vector(t(sigma2)), as.vector(t(tau00)), as.vector(t(tau01)),
             as.vector(t(tau11)))
tbb1 <- round(tbb1, 3)

tbb2 <- cbind(tbb[,1:3], tbb1[,4:9], tbb[,4:9])

colnames(tbb2) <- c("Vidurkis", "MRBIAS", "MRSE", "Vidurkis", "MRBIAS", "MRSE",
                "Vidurkis", "MRBIAS", "MRSE", "Vidurkis", "MRBIAS", "MRSE",
                "Vidurkis", "MRBIAS", "MRSE")
tbb2 <- cbind(Tikroji=c("1","1","1","1","$\\frac{1}{4}$","$\frac{\sqrt{3}}{8}$", "$\frac{3}{4}$"), tbb2)
tbb2 <- cbind(Parametras=c("$\\gamma_{00}$","$\\gamma_{01}$","$\\gamma_{10}$","$\\sigma^2$","$\\tau_{00}$","$\\tau_{01}$", "$\\tau_{11}$"), tbb2)

xt <- xtable(tbb2)

addtorow <- list()
addtorow$pos <- list(-1)
addtorow$command <- c("& &\\multicolumn{2}{c|}{REML}&\\multicolumn{2}{c|}{MINQUE(1)}&\\multicolumn{2}{c|}{PWMINQUE(1)}&\\multicolumn{2}{c|}{MINQUE($\\theta$)}&\\multicolumn{2}{c|}{PWMINQUE($\\theta$)}\\\\")


print( xtable(tbb2, digits=3, 
              align = c("c","c","c|","c","c","c|","c","c","c|","c","c","c|","c","c","c|","c","c","c|")),
       include.rownames = FALSE, 
       add.to.row = addtorow, sanitize.colnames.function = identity, 
       sanitize.text.function = identity, #size = "footnotesize",
       floating.environment = "sidewaystable")
