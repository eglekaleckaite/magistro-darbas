rm(list = ls())
gc()
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
library(utils)
library(iterators)
library(doParallel)
library(doSNOW)
library(mitools)

#library(sample)
source("10code.R")

#set.seed(2345)


# load("data.2011.RData")
# varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
#           "SMLIKE", "SBULLED", "SHEDRES", "IDSTUD", "TOTWGT", "SCHWGT", "STUDWGT", "SCHWGT")
# #data.2011  <- drop.levels(subset(data.2011, IDSCHOOL %in% unique(data.2011$IDSCHOOL)[1:10]))
# data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
#                                               "BSMMAT05", varb, with = F)]))
# 
# pops <- makePOP3(M = 300)
# # pops$w2 <- 1/pops$psch
# # pops$w1 <- 1/pops$pstud
# # save(pops, file = "pops.RData")
# # 
# # smpls <- llply(1:500, function(x) mySample(pops))
# # save(smpls, file = "smpls.RData")
# # 
# # 
# # 
# # pops <- makePOP()
# 
pcg <- c("plyr", "mvtnorm", "expm", "simFrame", "foreach", "msm", "gtools", 
         "MASS", "lme4", "gdata", "Matrix", "data.table", "utils", "iterators",
         "doParallel", "doSNOW")

N <- 200

cl <- makeCluster(6, outfile="simul.txt") # number of cores. Notice 'outfile'
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = N, style = 3)

mcmc <- foreach(ii = 1:N, .combine = rbind, .packages = pcg) %dopar% {
  pops <- makePOP3(M = 300)
  smpl <- mySample(pops, m = 35)
  smpl$w2 <- 1/smpl$psch
  smpl$w1 <- 1/smpl$pstud
  smpl$IDSTRATI <- rep(1, nrow(smpl))
  
  #save(smpl, file = "Output/smpl1.RData")
  rb0 <- try(bootREML_PV(data = smpl, R = 100,
                  form = "Y ~ 1+W+X1+(1|IDSCHOOL)", idstrata = "IDSTRATI"))
  if(class(rb0) == "try-error") return(NULL)
  tt0 <- rb0$tt0
  sm0 <- lme4:::summary.merMod(tt0)
  c0 <- confint(tt0)
  c0[1:2,] <- c0[1:2,]^2
  ivM4 <- try(bootMINQUE2(fixed = "Y1 ~ 1+W+X1", random = "~1|IDSCHOOL", wgt = c("w1", "w2"),
                             idstrata = "IDSTRATI", R = 100, data = smpl, BFUN = bootSample))
  ivM5 <- ivM4
#     try(bootMINQUE2(fixed = "Y4 ~ 1+W+X1+X2", random = "~1|IDSCHOOL", wgt = NULL,
#                           idstrata = "IDSTRATI", R = 100, data = smpl, BFUN = bootSampleREML))
#   
  if(class(ivM4) == "try-error") return(NULL)
  
  setTxtProgressBar(pb, ii)
  
  res <- data.frame(Rb = t(fixef(tt0)), Rt = t(as.numeric(sm0$varcor[[1]])), 
                    Rs = sm0$sigma^2, Rbint1 = t(c0[3,]), Rbint2 = t(c0[4,]), 
                    Rbint3 = t(c0[5,]), Rbint4 = t(c0[6,]), 
                    Rtint = t(c0[1,]), Rsint = t(c0[2,]),
                    Gb = t(rb0$beta[,1]), Gt = t(as.numeric(rb0$TT[1])), 
                    Gs = t(as.numeric(rb0$sigma2[1])), Gbint1 = t(rb0$beta[1,4:5]),
                    Gbint2 = t(rb0$beta[2,4:5]), Gbint3 = t(rb0$beta[3,4:5]),
                    Gbint4 = t(rb0$beta[4,4:5]), Gtint = t(as.numeric(rb0$TT[,4:5])),
                    Gsint = t(as.numeric(rb0$sigma2[,4:5])),
                    Mwb = t(ivM4$beta[,1]), Mwt = t(as.numeric(ivM4$TT[1])), 
                    Mws = t(as.numeric(ivM4$sigma2[1])), Mwbint1 = t(ivM4$beta[1,4:5]),
                    Mwbint2 = t(ivM4$beta[2,4:5]), Mwbint3 = t(ivM4$beta[3,4:5]),
                    Mwbint4 = t(ivM4$beta[4,4:5]),
                    Mwtint = t(as.numeric(ivM4$TT[,4:5])),
                    Mwsint = t(as.numeric(ivM4$sigma2[,4:5])),
                    Mb = t(ivM5$beta[,1]), Mt = t(as.numeric(ivM5$TT[1])), 
                    Ms = t(as.numeric(ivM5$sigma2[1])), Mbint1 = t(ivM5$beta[1,4:5]),
                    Mbint2 = t(ivM5$beta[2,4:5]), Mbint3 = t(ivM5$beta[3,4:5]),
                    Mbint4 = t(ivM5$beta[4,4:5]),
                    Mtint = t(as.numeric(ivM5$TT[,4:5])),
                    Msint = t(as.numeric(ivM5$sigma2[,4:5]))) 
  return(res)
}

close(pb)
stopCluster(cl) 

#save(mcmc, file = "Output/mcmcY1_remlvsminque_simple.RData")
save(mcmc, file = "Output/mcmcY1_remlvsminqueweights_missingvariable.RData")
#save(mcmc, file = "Output/mcmcY4wg.RData")
rbind(mcmcRES(mcmc[,1], 500),
      mcmcRES(mcmc[,19], 500),
      mcmcRES(mcmc[,37], 500),
      mcmcRES(mcmc[,55], 500))

rbind(mcmcRES(mcmc[,2], 20),
      mcmcRES(mcmc[,20], 20),
      mcmcRES(mcmc[,38], 20),
      mcmcRES(mcmc[,56], 20))

rbind(mcmcRES(mcmc[,3], 50),
      mcmcRES(mcmc[,21], 50),
      mcmcRES(mcmc[,39], 50),
      mcmcRES(mcmc[,57], 50))

rbind(mcmcRES(mcmc[,4], -4),
      mcmcRES(mcmc[,22],-4),
      mcmcRES(mcmc[,40],-4),
      mcmcRES(mcmc[,58],-4))

rbind(mcmcRES(mcmc[,5], 1300),
      mcmcRES(mcmc[,23], 1300),
      mcmcRES(mcmc[,41], 1300),
      mcmcRES(mcmc[,59], 1300))

rbind(mcmcRES(mcmc[,6], 2000),
      mcmcRES(mcmc[,24], 2000),
      mcmcRES(mcmc[,42], 2000),
      mcmcRES(mcmc[,60], 2000))


rbind(c(mean(mcmc[,7]), mean(mcmc[,8])),
      c(mean(mcmc[,25]), mean(mcmc[,26])),
      c(mean(mcmc[,43]), mean(mcmc[,44])),
      c(mean(mcmc[,61]), mean(mcmc[,62])))

rbind(c(mean(mcmc[,9]), mean(mcmc[,10])),
      c(mean(mcmc[,27]), mean(mcmc[,28])),
      c(mean(mcmc[,45]), mean(mcmc[,46])))

rbind(c(mean(mcmc[,11]), mean(mcmc[,12])),
      c(mean(mcmc[,29]), mean(mcmc[,30])),
      c(mean(mcmc[,47]), mean(mcmc[,48])))

rbind(c(mean(mcmc[,13]), mean(mcmc[,14])),
      c(mean(mcmc[,31]), mean(mcmc[,32])),
      c(mean(mcmc[,49]), mean(mcmc[,50])))

rbind(c(mean(mcmc[,15]), mean(mcmc[,16])),
      c(mean(mcmc[,33]), mean(mcmc[,34])),
      c(mean(mcmc[,51]), mean(mcmc[,52])))

rbind(c(mean(mcmc[,17]), mean(mcmc[,18])),
      c(mean(mcmc[,35]), mean(mcmc[,36])),
      c(mean(mcmc[,53]), mean(mcmc[,54])))

# mcmcRES <- function(x, x0) {
#   m  <- mean(x)
#   RBias <- m/x0-1
#   RMSE <- sqrt(mean((x/x0-1)^2))
#   return(data.frame(Mean = m, RBias = RBias, RMSE = RMSE))
# }
# st1 <- system.time(ivM4 <- myMINQUE(dt = smpl,
#                                      fixed = "Y4 ~ 1",
#                                     random1 = "~1|IDSCHOOL",
#                                     weights = c("w1", "w2")))

# lme.0 <-lmer(BSMMAT01 ~ 1+SSEX+SMENG+(1|IDSCHOOL), data = data.2011)
# (sm0 <- summary(lme.0))

# st1 <- system.time(ivM5 <- myIGLS(dt = data.2011,
#                                      fixed = "BSMMAT01 ~ 1+SSEX+SMENG",
#                                      random1 = "~1|IDSCHOOL",
#                                      weights = c("STUDWGT", "SCHWGT")))
# st1 <- system.time(ivM3 <- myMINQUE2(dt = data.2011,
#                                      fixed = "BSMMAT01 ~ 1+SSEX+SMENG",
#                                      random1 = "~1|IDSCHOOL",
#                                      weights = c("STUDWGT", "SCHWGT")))
# 
# bbMINQUE1 <- bootMINQUE(data = smpl, FUN  = bfunMINQUE, R = 100,
#                        fixed = "Y4 ~ 1", random = "~1|IDSCHOOL",
#                        strata = smpl$IDSCHOOL, #hierar = T, 
#                        wgt = c("w1", "w2"))
# bbIGLS1 <- bootMINQUE(data = smpl, FUN  = bfunIGLS, R = 100,
#                         fixed = "Y4 ~ 1", random = "~1|IDSCHOOL",
#                         strata = smpl$IDSCHOOL, #hierar = T, 
#                         wgt = c("w1", "w2"))
# lme.0 <-lmer(Y1 ~ 1+MDYDIS+SSEX+SMCONF+(1+SSEX|IDSCHOOL), data = smpl)
# (sm0 <- summary(lme.0))
# 
# st1 <- system.time(ivM1 <- myIGLS(dt = smpl,
#                                  fixed = "Y1 ~ 1+MDYDIS+SSEX+SMCONF",
#                                  random1 = "~1+SSEX|IDSCHOOL",
#                                  weights = c("w1", "w2")))
# 
# st1 <- system.time(ivM2 <- mySIMPLE(dt = smpl,
#                                     fixed = "Y1 ~ 1+MDYDIS+SSEX+SMCONF",
#                                     random1 = "~1+SSEX|IDSCHOOL",
#                                     weights = c("w1", "w2")))
# 
# st1 <- system.time(ivM3 <- myMINQUE(dt = smpl,
#                                     fixed = "Y1 ~ 1+MDYDIS+SSEX+SMCONF",
#                                     random1 = "~1+SSEX|IDSCHOOL",
#                                     weights = c("w1", "w2")))

# 
# mySample <- function(dt, m = 75){
#   info <- dt[,c("IDSCHOOL", "IDSTUD", "nj", "psch", "pstud", "eN")]
#   idcl <- unique(info[, c("IDSCHOOL", "psch")])
#   sidcl <- sample(idcl$IDSCHOOL, m, prob = idcl$psch)
#   smplDt <- foreach(ii = sidcl, .combine = rbind) %do% {
#     stud <- subset(info, IDSCHOOL %in% ii, c("IDSTUD", "nj", "eN"))
#     n1 <- round(3/5*unique(stud$nj))
#     n2 <- round(2/5*unique(stud$nj))
#     print(c(n1, n2))
#     stud1 <- subset(stud, eN<=0)$IDSTUD
#     stud2 <- subset(stud, eN>0)$IDSTUD
#     if(n1 < length(stud1) & n2 < length(stud2)){
#       idstud1 <- sample(stud1, n1)
#       idstud2 <- sample(stud2, n2)
#       smpl1 <- subset(dt, IDSTUD %in% idstud1)
#       smpl1$pstud <- n1/length(stud1)
#       print(sum(smpl1$pstud))
#       smpl2 <- subset(dt, IDSTUD %in% idstud2)
#       smpl2$pstud <- n2/length(stud2)
#       print(sum(smpl2$pstud))
#       rbind(smpl1, smpl2)
#     } else {
#       idstud <- sample(stud$IDSTUD, unique(stud$nj))
#       smpl1 <- subset(dt, IDSTUD %in% idstud)
#       smpl1$pstud <- unique(stud$nj)/nrow(stud)
#       smpl1
#      }
#   }
#   smplDt$psch <- m*smplDt$psch
#   return(smplDt)
# }
#   
#   #cluster(pops, "IDSCHOOL", 75, method="systematic",pik=daply(pops, ~IDSCHOOL, function(x) unique(x$psch)))
# 
# makePOP3 <- function(M = 300){
#   struct <- 1:M
#   uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
#   Nj <- round(50*exp(uj))
#   #ups(190, M, prob = c(1:95, rep(1,95)), replace = TRUE)+10
#   struct <- as.data.frame(cbind(IDSCHOOL = struct, Nj = Nj))
#   # struct <- ddply(data.2011, ~IDSCHOOL, function(x) length(unique(x$IDSTUD)))
#   # struct$IDSCHOOL <- 1:nrow(struct)
#   # names(struct)[2] <- "Nj"
#   struct$psch <- struct$Nj/sum(struct$Nj)
#   
#   #Make u
# #   S1 <- matrix(c(1300,-180,-180,480),nrow=2,byrow=TRUE)
# #   mu1 <- c(1,1)
# #   S2 <- matrix(c(1300,-180,-180,480),nrow=2,byrow=TRUE)
# #   mu2 <- c(-1,-1)
#   
# #   n <- M
# #   p1 <- 0.8
# #   n1 <- rbinom(1,size=n,prob=p1)  ## how many from first distribution?
# #   n2 <- n-n1
# #   val1 <- mvrnorm(n1,mu=mu1,Sigma=S1)
# #   val2 <- mvrnorm(n2,mu=mu2,Sigma=S2)
# #   allval <- rbind(val1,val2)      ## combine
# #   allval <- allval[sample(n,n),]
# #   u0j <- allval[, 1]
# #   u1j <- allval[, 2]
# #   uN <- rmvnorm(M, mean = c(0, 0), sigma = S2)
# #   # Make alpha   
# #   struct$aN <- 500+uN[,1]
# #   struct$aX <- 500+u0j
# #   # Make beta
# #   struct$bN <- -4+uN[,2]
# #   struct$bX <- -4+u1j
# #   S1 <- sqrt(0.5)
# #   mu1 <- c(-20)
# #   S2 <- sqrt(0.5)
# #   mu2 <- c(20)
# #   
# #   n <- M
# #   p1 <- 0.8
# #   n1 <- rbinom(n,size=1,prob=p1)  ## how many from first distribution?
# #   n2 <- 1-n1
# #   val1 <- rnorm(n,mu1,S1)
# #   val2 <- rnorm(n,mu2,S2)
# #   allval <- n1*val1+n2*val2      ## combine
# #   allval <- (allval[sample(n,n)]-(p1*mu1+(1 - p1)*mu2))/sqrt(p1*(mu1-p1*mu1-(1 - p1)*mu2)^2+(1-p1)*(mu2-p1*mu1-(1 - p1)*mu2)^2+S1^2)*sqrt(1300)
#   allval <- (rpois(M, 1)-1)*sqrt(1300)
#   # Make alpha   
#   struct$aN <- 500+rnorm(M,0, sqrt(1300))
#   struct$aX <- 500+allval
# 
#   
#   
#   # Make main error
# #   S1 <- sqrt(1)
# #   mu1 <- c(-10)
# #   S2 <- sqrt(1)
# #   mu2 <- c(10)
# #   
# #   n <- sum(struct$Nj)
# #   p1 <- 0.8
# #   n1 <- rbinom(n,1,prob=p1)  ## how many from first distribution?
# #   n2 <- abs(n1-1)
# #   val1 <- rnorm(n,mu1,S1)
# #   val2 <- rnorm(n,mu2,S2)
# #   allval <- n1*val1+n2*val2      ## combine
# #   allval <- (allval[sample(n,n)]-(p1*mu1+(1 - p1))*mu2)/sqrt(p1*(mu1-p1*mu1-(1 - p1)*mu2)^2+(1-p1)*(mu2-p1*mu1-(1 - p1)*mu2)^2+S1^2)*sqrt(2000)
#   allval <- (rpois(sum(struct$Nj), 2)-2)/sqrt(2)*sqrt(2000)
#   eN <- ddply(struct, ~IDSCHOOL, function(x) matrix(rnorm(x$Nj,0, sqrt(2000)), ncol = 1))
#   names(eN)[2] <- "eN"
#   eN$eN <- rnorm(nrow(eN),0, sqrt(2000))
#   eN$eX <- allval
#   
#   
#   struct <- merge(struct, eN, by = "IDSCHOOL")
#   
#   struct <- ddply(struct, ~IDSCHOOL, function(x) {x$nj <- round(ifelse(nrow(x)<30, nrow(x), ifelse(nrow(x)>=30&nrow(x)<60, nrow(x)/2, nrow(x)/3)));x$pstud <- 1/nrow(x); return(x)})
#   struct$ptot <- struct$psch*struct$pstud
#   struct$X <- rnorm(nrow(struct), 90, 20)
#   
#   
# #   struct$Y1 <- struct$aN+struct$bN*struct$X+struct$eN
# #   struct$Y2 <- struct$aX+struct$bX*struct$X+struct$eN
# #   struct$Y3 <- struct$aN+struct$bN*struct$X+struct$eX
# #   struct$Y4 <- struct$aX+struct$bX*struct$X+struct$eX
#   
#   struct$Y1 <- struct$aN+50*struct$X+struct$eN
#   struct$Y2 <- struct$aX+50*struct$X+struct$eN
#   struct$Y3 <- struct$aN+50*struct$X+struct$eX
#   struct$Y4 <- struct$aX+50*struct$X+struct$eX
#   
# #   struct$Y1 <- struct$aN+struct$eN
# #   struct$Y2 <- struct$aX+struct$eN
# #   struct$Y3 <- struct$aN+struct$eX
# #   struct$Y4 <- struct$aX+struct$eX
#   
#   struct$IDSTUD <- 1:nrow(struct)
#   return(struct)
# }


# 
# makePOP <- function(M = 300){
#   struct <- 1:M
#   uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
#   Nj <- round(50*exp(uj))
#   #ups(190, M, prob = c(1:95, rep(1,95)), replace = TRUE)+10
#   struct <- as.data.frame(cbind(IDSCHOOL = struct, Nj = Nj))
#   # struct <- ddply(data.2011, ~IDSCHOOL, function(x) length(unique(x$IDSTUD)))
#   # struct$IDSCHOOL <- 1:nrow(struct)
#   # names(struct)[2] <- "Nj"
#   struct$psch <- struct$Nj/sum(struct$Nj)
#   
#   # Make alpha
#   struct$aN <- 1+rnorm(nrow(struct), 0, sqrt(0.2))
#   struct$aX <- 1+(rchisq(nrow(struct), df = 1)-1)/sqrt(2*5)
#   eN <- ddply(struct, ~IDSCHOOL, function(x) matrix(rnorm(x$Nj,0, sqrt(0.5)), ncol = 1))
#   names(eN)[2] <- "eN"
#   eN <- ddply(eN, ~IDSCHOOL, function(x) {x$eX <- (rchisq(nrow(x), df = 1)-1)/sqrt(4); return(x)})
# 
#   
#   struct <- merge(struct, eN, by = "IDSCHOOL")
#   
#   struct <- ddply(struct, ~IDSCHOOL, function(x) {x$nj <- round(ifelse(nrow(x)<50, nrow(x), ifelse(nrow(x)>=50&nrow(x)<100, nrow(x)/2, nrow(x)/3)));x$pstud <- 1/nrow(x); return(x)})
#   struct$ptot <- struct$psch*struct$pstud
#   
#   struct$Y1 <- struct$aN+struct$eN
#   struct$Y2 <- struct$aX+struct$eN
#   struct$Y3 <- struct$aN+struct$eX
#   struct$Y4 <- struct$aX+struct$eX
# 
#   
#   struct$IDSTUD <- 1:nrow(struct)
#   return(struct)
# }
# 
# 
# 
# # Make population
# makePOP2 <- function(M = 300){
#   struct <- 1:M
#   uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
#   Nj <- round(75*exp(uj))-10
#     #ups(190, M, prob = c(1:95, rep(1,95)), replace = TRUE)+10
#   struct <- as.data.frame(cbind(IDSCHOOL = struct, Nj = Nj))
#   # struct <- ddply(data.2011, ~IDSCHOOL, function(x) length(unique(x$IDSTUD)))
#   # struct$IDSCHOOL <- 1:nrow(struct)
#   # names(struct)[2] <- "Nj"
#   struct$psch <- 75*struct$Nj/sum(struct$Nj)
#   
#   # Make alpha
#   alpha <- struct
#   alpha$MDYDIS <- alpha$Nj*sample(4:12,1,replace=T)
#   alpha$a <- 150+0.1*alpha$MDYDIS
#   
#   #Make
#   beta <- data.frame(struct, b = -4)
#   
#   # Make random effects
#   q = sqrt(1300)
#   s = sqrt(100)
#   r = -180/q/s
#   cov.matrix <-  matrix(c(q^2, r * q * s, r * q * s, s^2), nrow = 2,
#                         byrow = TRUE)
#   random.effects.norm <-  rmvnorm(nrow(beta), mean = c(0, 0), sigma = cov.matrix)
#   
#   random.effects.chi <-  rmvnorm(nrow(beta), mean = c(0, 0), sigma = matrix(c(q,sqrt(-r*q*s), sqrt(-q*r*s), s),ncol=2))^2
#   random.effects.chi[,1] <- (random.effects.chi[,1])/1.414
#   random.effects.chi[,2] <- -(random.effects.chi[,2])/1.413
#   
#   alpha$aN <- alpha$a+random.effects.norm[,1]
#   alpha$aX <- alpha$a+random.effects.chi[,1]
#   
#   beta$bN <- beta$b+random.effects.norm[,2]
#   beta$bX <- beta$b+random.effects.chi[,2]
#   
#   struct <- cbind(merge(merge(struct, alpha), beta), random.effects.norm, random.effects.chi)
#   struct$b2 <- 30
#   
#   SMCONF <- ddply(struct, ~IDSCHOOL, function(x) {matrix(rnorm(x$Nj, mean = 10, sd = 2), ncol = 1, dimnames = list(NULL, "SMCONF"))})
#   SMCONF$SSEX <- rbinom(nrow(SMCONF), 1, .5)
#   
#   struct <- merge(struct, SMCONF)
#   
#   struct$eN <- rnorm(nrow(struct),0, sqrt(4000))
#   struct$eX <- (rnorm(nrow(struct),0, 1)^2-1)*sqrt(2000)
#   struct <- ddply(struct, ~IDSCHOOL, function(x) {x$nj <- round(ifelse(nrow(x)<50, nrow(x), ifelse(nrow(x)>=50&nrow(x)<100, nrow(x)/2, nrow(x)/3)));x$pstud <- x$nj/nrow(x); return(x)})
#   struct$ptot <- struct$psch*struct$pstud
#   
#   struct$Y1 <- struct$aN+struct$bN*struct$SSEX+30*struct$SMCONF+struct$eN
#   struct$Y2 <- struct$aX+struct$bN*struct$SSEX+30*struct$SMCONF+struct$eN
#   struct$Y3 <- struct$aN+struct$bX*struct$SSEX+30*struct$SMCONF+struct$eN
#   struct$Y4 <- struct$aX+struct$bX*struct$SSEX+30*struct$SMCONF+struct$eN
#   struct$Y5 <- struct$aN+struct$bN*struct$SSEX+30*struct$SMCONF+struct$eX
#   struct$Y6 <- struct$aX+struct$bN*struct$SSEX+30*struct$SMCONF+struct$eX
#   struct$Y7 <- struct$aN+struct$bX*struct$SSEX+30*struct$SMCONF+struct$eX
#   struct$Y8 <- struct$aX+struct$bX*struct$SSEX+30*struct$SMCONF+struct$eX
#   
#   struct$IDSTUD <- 1:nrow(struct)
#   return(struct)
# }
# 
# 
# myMINQUE2 <- function(dt, fixed, random1 = NULL, weights = NULL, apriori = NULL) {
#   #dt <- arrange(dt, IDSCHOOL)
#   N <- nrow(dt)
#   # Form Y and fixed effects data frame
#   ff <- model.matrix(as.formula(fixed), model.frame(fixed, dt))
#   Y <- as(as(model.frame(fixed, dt)[,1, drop = F], "matrix"), "Matrix")
#   if (grepl("-1",fixed)) {
#     X <- as(as(ff[,-1, drop = F], "matrix"), "Matrix")
#   } else {
#     X <- as(as(ff, "matrix"), "Matrix")
#   }
#   
#   if (!is.null(random1)) {
#     id1 <- strsplit(random1, "\\|")
#     random1 <- unlist(id1)[1]
#     nmid1 <- unlist(id1)[2]
#     rr1 <- model.frame(random1, dt)
#     id1 <- model.frame(paste("~", nmid1), dt)[,1]
#     if(!is.null(weights)){
#       wg <- dt[,c(nmid1, weights)]
#       wg <- foreach(ii = unique(id1)) %do% {x <- wg[id1 %in% ii, , drop = F];x[,2] <- x[,2]*nrow(x)/sum(x[,2]);return(x)}
#       swg <- Reduce("+", llply(wg, function(x) unique(x[,3])))
#       wg <- llply(wg, function(x) {x[,3] <- x[,3]*length(unique(id1))/swg; x$both <- x[, 2]*x[,3]; return(x)}) 
#       
#       
#       wgt1i <- llply(wg, function(x) x[,2])
#       wgt2i <- llply(wg, function(x) x[,3])
#       wgt12i <- llply(wg, function(x) x[,4])
#       
#     } else {
#       wg <- foreach(ii = unique(id1)) %do% {sum(id1 %in% ii)}
#       wgt1i <- llply(wg, function(x) rep(1, x))
#       wgt2i <- llply(wg, function(x) rep(1, x))
#       wgt12i <- llply(wg, function(x) rep(1, x))
#     }
#     if (grepl("-1",random1)) {
#       Z1 <- as(as( rr1, "matrix"), "Matrix")
#     } else {
#       Z1 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
#                                dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
#                "Matrix")
#     }
#     q <- ncol(Z1)
#     colnames(Z1) <- paste(nmid1, colnames(Z1), sep = ".")
#     clnms1 <- colnames(Z1)
#     Z1 <- foreach(i = unique(id1)) %do% {Z1[id1 %in% i, ,drop = F]}
#     Z1 <- mapply(function(z, w) {z*1/sqrt(w)}, Z1, wgt2i)
#     Z <- bdiag(Z1)
#     n1 <- length(unique(id1))
#     l <- q*(q+1)/2
#     TT1 <- formTT(q)
#     QI <- llply(TT1, function(tt) llply(Z1, function(z) z%*%tt%*%t(z)))
#   } else {
#     Z1 <- NULL
#     clnms1 <- NULL
#     n1 <- NULL
#     QI <- NULL
#   }
#   
#   #   wgt1i <- foreach(i = unique(id1)) %do% {wgt1[id1 %in% i]}
#   #   wgt2i <- foreach(i = unique(id1)) %do% {wgt2[id1 %in% i]}
#   #   wgt12i <- foreach(i = unique(id1)) %do% {wgt12[id1 %in% i]}
#   l <- l+1
#   Qj <- c(list(llply(QI[[1]], function(zj) diag(nrow(zj)))), QI)
#   Qj[[1]] <- foreach(i = 1:length(Qj[[1]])) %do% {1/wgt12i[[i]]*Qj[[1]][[i]]}
#   QI <- llply(Qj, function(x) bdiag(x))
#   Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i, ,drop = F]}
#   X <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Xj[[ii]]), "Matrix")
#   Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
#   Y <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Yj[[ii]]), "Matrix")
#   
#   if (is.null(apriori)) apriori <- rep(1, length(QI))
#   #   Z2 <- cBind(apriori[1]/sum(apriori)*diag(nrow(Y)), apriori[2]/sum(apriori)*Z)
#   #   V <- Z2%*%t(Z2)
#   
#   Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", mapply(function(qj, th) th*qj[[i]], Qj, apriori)))
#   V <- bdiag(Vj)
#     
#   iM <- iMINQUE(V, X, Y, QI, wgt2i)
#   #print(iM$theta)
#   gc()
#   doIM <- T
#   while(doIM){
#     theta <- iM$theta
#     print(theta)
#     iM <- iMINQUE2(theta, iM$V, X, Y, QI, wgt2i)
#     doIM <- (round(sum(abs(iM$theta - theta))) != 0)
#   }
# 
#   P <- ginv(as.matrix((t(X)%*%iM$V%*%X)))
#   iM$beta <- P%*%t(X)%*%iM$V%*%Y
#   gc()
#   sigma2 <- iM$theta[1]
#   names(sigma2) <- "sigma2"
#   TT <- fillT(iM$theta[-1])
#   dimnames(TT) <- list(clnms1, clnms1)
#   beta <- as.numeric(iM$beta)
#   names(beta) <- colnames(X)
#   return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, n1 = n1))
#   
# }
# 
# fillSMINQUE2 <- function(CQ, wgt2i) {
#   x <- matrix(0, ncol = length(CQ), nrow = length(CQ))
#   ns <- nrow(x)
#   cc <- combinations(ns,2,1:ns, repeats.allowed = T)
#   foreach(ii = 1:nrow(cc)) %do% {
#     k <- cc[ii, 1]
#     l <- cc[ii, 2]
#     s <- sum(diag(unlist(wgt2i)*CQ[[k]]%*%CQ[[l]]))
#     x[k,l] <- s
#     x[l, k] <- s
#     gc()
#   }
#   gc()
#   return(x)
# }
# 
# iMINQUE <- function(V, X, Y, QI, wgt2i) {
#   XVX <- ginv(as.matrix((t(X)%*%V%*%X)))
#   Cjj <- V-V%*%X%*%XVX%*%t(X)%*%V#(V*unlist(wgt2i))
#   CQ <- llply(QI, function(qi) Cjj%*%qi)
#   S <- fillSMINQUE2(CQ, wgt2i)
#   WI <- laply(CQ, function(cq) sum(diag(t(Y)%*%cq%*%Cjj%*%Y)))
#   theta0 <- ginv(S)%*%matrix(WI, ncol = 1)
#   V <- ginv(as.matrix(Reduce("+", mapply(function(qi, th){qi*th}, QI, theta0))))
#   return(list(theta = theta0, V = V, P = XVX))
# }
# iMINQUE2 <- function(theta0, V, X, Y, QI, wgt2i) {
#   XVX <- ginv(as.matrix((t(X)%*%V%*%X)))
#   Cjj <- V-V%*%X%*%XVX%*%t(X)%*%V#(V*unlist(wgt2i))
#   CQ <- llply(QI, function(qi) Cjj%*%qi)
#   WI <- laply(CQ, function(cq) sum(diag(t(Y)%*%cq%*%Cjj%*%Y)))
#   theta <- mapply(function(wi, th, cq) {th*wi/sum(diag((unlist(wgt2i)*cq)))}, WI, theta0, CQ)
#   V <- ginv(as.matrix(Reduce("+", mapply(function(qi, th){qi*th}, QI, theta0))))
#   return(list(theta = theta0, V = V, P = XVX))
# }