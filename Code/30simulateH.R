rm(list = ls())
library(plyr)
library(mvtnorm)
library(expm)
library(Rcpp)
library(simFrame)
library(foreach)
library(msm)
library(gtools)
library(MASS)
library(lme4)
library(gdata)
library(Matrix)
library(data.table)
#library(sample)
source("10code.R")

#set.seed(2345)


load("data.2011.RData")
varb <- c("IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES", "IDSTUD", "TOTWGT", "SCHWGT", "STUDWGT", "SCHWGT")
#data.2011  <- drop.levels(subset(data.2011, IDSCHOOL %in% unique(data.2011$IDSCHOOL)[1:10]))
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", 
                                              "BSMMAT05", varb), with = F]))

pops <- makePOP3(M = 300)
# pops$w2 <- 1/pops$psch
# pops$w1 <- 1/pops$pstud
# save(pops, file = "pops.RData")
# 
# smpls <- llply(1:500, function(x) mySample(pops))
# save(smpls, file = "smpls.RData")
# 
# 
# 
# pops <- makePOP()
# 
mcmc <- foreach(ii = 1:5, .combine = rbind) %do% {
  print(paste("mc:", ii))
  pops <- makePOP3(M = 400)
  smpl <- mySample(pops, m = 30)
  smpl[, w2 := 1/psch]
  smpl[, w1 := 1/pstud]
  smpl[, IDSTRATI := 1]
  
  #save(smpl, file = "Output/smpl1.RData")
  rb0 <- try(bootREML(data = smpl, R = 10,
                  form = "Y3 ~ 1+W+X1+X2+(1|IDSCHOOL)", idstrata = "IDSTRATI"))
  if(class(rb0) == "try-error") return(NULL)
  tt0 <- rb0$tt0
  sm0 <- lme4:::summary.merMod(tt0)
  c0 <- confint(tt0)
  c0[1:2,] <- c0[1:2,]^2
#   st1 <- system.time(ivM1 <- myIGLS(dt = smpl,
#                                     fixed = "Y4~ 1+X",
#                                     random1 = "~1|IDSCHOOL",
#                                     weights = c("w1", "w2")))
#   
#  st1 <- system.time(ivM3 <- myMINQUE2(dt = smpl,
#                                        fixed = "Y3 ~ 1+X",
#                                        random1 = "~1|IDSCHOOL", apriori = c(2000,1300)))#,
#                                       # weights = c("w1", "w2")))
  ivM4 <- try(bootMINQUE2(fixed = "Y2 ~ 1+W+X1+X2", random = "~1|IDSCHOOL", wgt = NULL, #c("w1", "w2"),
                             idstrata = "IDSTRATI", R = 10, data = smpl))
  if(class(ivM4) == "try-error") return(NULL)
  
#   ivM6 <- bootMINQUE2(fixed = "Y4 ~ 1+X", random = "~1|IDSCHOOL", wgt = c("w1", "w2"),
#                       idstrata = "IDSTRATI", R = 100, data = smpl, FUN = myIGLS)

#   res <- data.frame(Rb = t(coef(sm0)[,1]), Rt = t(as.numeric(sm0$varcor[[1]])), Rs = sm0$sigma^2,
#                     Gb = t(ivM1$beta), Gt = t(as.numeric(ivM1$TT)), Gs = ivM1$sigma2,
#                     Mb = t(ivM3$beta), Mt = t(as.numeric(ivM3$TT)), Ms = ivM3$sigma2)
#   res <- data.frame(Rb = t(coef(sm0)[,1]), Rt = t(as.numeric(sm0$varcor[[1]])), Rs = sm0$sigma^2, Rbint = t(cf[3,]), Rtint = t(cf[2,]), Rsint = t(cf[1,]),
#                     Gb = t(ivM6$beta[,1]), Gt = t(as.numeric(ivM6$TT[1])), Gs = t(as.numeric(ivM6$sigma2[1])), Gbint = t(ivM6$beta[,4:5]), Gt = t(as.numeric(ivM6$TT[,4:5])), Gs = t(as.numeric(ivM6$sigma2[,4:5])),
#                     Mb = t(ivM4$beta[,1]), Mt = t(as.numeric(ivM4$TT[1])), Ms = t(as.numeric(ivM4$sigma2[1])), Mbint = t(ivM4$beta[,4:5]), Mtint = t(as.numeric(ivM4$TT[,4:5])), Msint = t(as.numeric(ivM4$sigma2[,4:5])))
#   
  res <- data.table(Rb = t(fixef(tt0)), Rt = t(as.numeric(sm0$varcor[[1]])), 
                    Rs = sm0$sigma^2, Rbint1 = t(c0[3,]), Rbint2 = t(c0[4,]), 
                    Rbint3 = t(c0[5,]), Rbint4 = t(c0[6,]),
                    Rtint = t(c0[1,]), Rsint = t(c0[2,]),
                    Gb = t(rb0$beta[,1]), Gt = t(as.numeric(rb0$TT[1])), 
                    Gs = t(as.numeric(rb0$sigma2[1])), Gbint1 = t(rb0$beta[1,4:5]),
                    Gbint2 = t(rb0$beta[2,4:5]), Gbint1 = t(rb0$beta[3,4:5]),
                    Gbint2 = t(rb0$beta[4,4:5]), Gtint = t(as.numeric(rb0$TT[,4:5])),
                    Gsint = t(as.numeric(rb0$sigma2[,4:5])),
                    Mb = t(ivM4$beta[,1]), Mt = t(as.numeric(ivM4$TT[1])), 
                    Ms = t(as.numeric(ivM4$sigma2[1])), Mbint1 = t(ivM4$beta[1,4:5]),
                    Mbint2 = t(ivM4$beta[2,4:5]), Mbint3 = t(ivM4$beta[3,4:5]),
                    Mbint4 = t(ivM4$beta[4,4:5]), Mtint = t(as.numeric(ivM4$TT[,4:5])),
                    Msint = t(as.numeric(ivM4$sigma2[,4:5]))) 
  return(res)
}

#save(mcmc, file = "Output/mcmcY1_remlvsminqueno.RData")
save(mcmc, file = "Output/mcmcY3_remlvsminqueno.RData")
#save(mcmc, file = "Output/mcmcY4wg.RData")


rbind(mcmcRES(mcmc[,1], 500),
mcmcRES(mcmc[,19], 500),
mcmcRES(mcmc[,37], 500))

rbind(mcmcRES(mcmc[,2], 20),
mcmcRES(mcmc[,20], 20),
mcmcRES(mcmc[,38], 20))

rbind(mcmcRES(mcmc[,3], 50),
      mcmcRES(mcmc[,21], 50),
      mcmcRES(mcmc[,39], 50))

rbind(mcmcRES(mcmc[,4], -4),
      mcmcRES(mcmc[,22],-4),
      mcmcRES(mcmc[,40],-4))

rbind(mcmcRES(mcmc[,5], 1300),
mcmcRES(mcmc[,23], 1300),
mcmcRES(mcmc[,41], 1300))

rbind(mcmcRES(mcmc[,6], 2000),
mcmcRES(mcmc[,24], 2000),
mcmcRES(mcmc[,42], 2000))


rbind(c(mean(mcmc[,7]), mean(mcmc[,8])),
c(mean(mcmc[,25]), mean(mcmc[,26])),
c(mean(mcmc[,43]), mean(mcmc[,44])))

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
bbMINQUE1 <- bootMINQUE(data = smpl, FUN  = bfunMINQUE, R = 100,
                       fixed = "Y4 ~ 1", random = "~1|IDSCHOOL",
                       strata = smpl$IDSCHOOL, #hierar = T, 
                       wgt = c("w1", "w2"))
bbIGLS1 <- bootMINQUE(data = smpl, FUN  = bfunIGLS, R = 100,
                        fixed = "Y4 ~ 1", random = "~1|IDSCHOOL",
                        strata = smpl$IDSCHOOL, #hierar = T, 
                        wgt = c("w1", "w2"))
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


# mySample <- function(dt, m = 75){
#   info <- dt[,c("IDSCHOOL", "IDSTUD", "nj", "psch", "pstud", "eN"), with = F]
#   idcl <- unique(info[, c("IDSCHOOL", "psch"), with = F])
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
#   struct$W <- rbinom(M, 1, prob = 0.2)
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
#   struct$X1 <- rnorm(nrow(struct), 90, 20)
#   struct$X2 <- rbinom(nrow(struct), 1, prob=0.5)
#   
# #   struct$Y1 <- struct$aN+struct$bN*struct$X+struct$eN
# #   struct$Y2 <- struct$aX+struct$bX*struct$X+struct$eN
# #   struct$Y3 <- struct$aN+struct$bN*struct$X+struct$eX
# #   struct$Y4 <- struct$aX+struct$bX*struct$X+struct$eX
#   
#   struct$Y1 <- struct$aN+20*struct$W+50*struct$X1-4*struct$X2+struct$eN
#   struct$Y2 <- struct$aX+20*struct$W+50*struct$X1-4*struct$X2+struct$eN
#   struct$Y3 <- struct$aN+20*struct$W+50*struct$X1-4*struct$X2+struct$eX
#   struct$Y4 <- struct$aX+20*struct$W+50*struct$X1-4*struct$X2+struct$eX
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
# 
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
# myMINQUE2 <- function(dt, fixed, random1 = NULL, weights = NULL) {
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
#       id1 <- strsplit(random1, "\\|")
#       random1 <- unlist(id1)[1]
#       nmid1 <- unlist(id1)[2]
#       rr1 <- model.frame(random1, dt)
#       id1 <- model.frame(paste("~", nmid1), dt)[,1]
#       if(!is.null(weights)){
#           wg <- dt[,c(nmid1, weights)]
#           wg <- foreach(ii = unique(id1), .combine = rbind) %do% {x <- wg[id1 %in% ii, , drop = F];x[,2] <- x[,2]*nrow(x)/sum(x[,2]);return(x)}
#           wg2 <- ddply(wg, as.formula(paste("~", nmid1)), function(x) unique(x[,3]))
#           wg[,3] <- wg[,3]*nrow(wg2)/sum(wg2[,2])
#                 
#             wgt1 <- wg[,2]
#           wgt2 <- wg[,3]
#           wgt12 <- wgt2*wgt1
#         } else {
#             wgt1 <- rep(1, N)
#             wgt2 <- rep(1, N)
#             wgt12 <- rep(1, N)
#           }
#       if (grepl("-1",random1)) {
#           Z1 <- as((1/sqrt(wgt2))*as( rr1, "matrix"), "Matrix")
#           Z1n <- as(as( rr1, "matrix"), "Matrix")
#         } else {
#             Z1 <- as((1/sqrt(wgt2))*as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
#                                                                                     dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
#                                      "Matrix")
#       #       Z1n <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
#         #                                             dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
#         #                "Matrix")
#             }
#       q <- ncol(Z1)
#       n1 <- length(unique(id1))
#       colnames(Z1) <- paste(nmid1, colnames(Z1), sep = ".")
#       clnms1 <- colnames(Z1)
#       Zj <- foreach(i = unique(id1)) %do% {Z1[id1 %in% i, ,drop = F]}
#       Z1 <- bdiag(Zj)
#   #     Zjn <- foreach(i = unique(id1)) %do% {Z1n[id1 %in% i, ,drop = F]}
#     #     Z1n <- bdiag(Zjn)
# } else {
# 
#       Z1 <- NULL
#       clnms1 <- NULL
#       n1 <- NULL
#       QI <- NULL
# }
# 
#     wgt1i <- foreach(i = unique(id1)) %do% {wgt1[id1 %in% i]}
#   wgt2i <- foreach(i = unique(id1)) %do% {wgt2[id1 %in% i]}
#   wgt12i <- foreach(i = unique(id1)) %do% {wgt12[id1 %in% i]}
#   
#     Z <- cBind(diag(1/sqrt(wgt12)), Z1)
#   #Zn <- cBind(diag(nrow(Z1n)), Z1n)
#     ZI <- list(diag(1/sqrt(wgt12)), Z1)
#   #ZIn <- list(diag(nrow(Z1n)), Z1n)
#     
#     QI <- llply(ZI, function(x) x%*%t(x))
#   Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i,,drop = F]}
#   Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
#   #Zj <- foreach(i = unique(id1)) %do% {Z[id1 %in% i,,drop = F]}
#     #   Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("", llply(Qj, function(qj) qj[[i]])))
#     #   V <- bdiag(Vj)
#     
#     iM <- iMINQUE(Z, X, Y, QI, w0=1, w1=1, wgt2, wgt12)
#   print(iM$theta)
#   gc()
#     doIM <- T
#     while(doIM){
#       theta <- iM$theta
#       print(theta)
#       iM <- iMINQUE(Z, X, Y, QI, w0=iM$icc, w1 = 1-iM$icc, wgt2, wgt12)
#       doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
#     }
#     s2 <- iM$theta[1]
#   tau <- iM$theta[2]
#   ywj <- mapply(function(y, w){sum(w*y)/sum(w)}, Yj, wgt1i)
#   xwj <- mapply(function(x, w){as.matrix(apply(w*x, 2, sum))/sum(w)}, Xj, wgt1i, SIMPLIFY = F)  
#   gj <- llply(wgt1i, function(w) tau/(s2/sum(w)+tau))
#   if(dim(xwj[[1]])[1]==1) dij <- mapply(function(x, g, xw, w) {w*(x-as.numeric(g*xw))}, Xj, gj, xwj, wgt12i)
#   else dij <- mapply(function(x, g, xw, w) {w*t(t(x)-g*xw)}, Xj, gj, xwj, wgt12i)
#   P <- solve(Reduce("+", mapply(function(x, d){t(t(x)%*%d)}, Xj, dij)))
#   Q <- Reduce("+", mapply(function(y, d){t(t(y)%*%d)}, Yj, dij))
#   beta <- as.matrix(P%*%Q)
#   rownames(beta) <- colnames(X)
#   ranef <- mapply(function(g, y, x){y-t(x)%*%beta}, gj, ywj, xwj)
#   fit <- mapply(function(x, z, u) x%*%iM$beta+z%*%u, Xj, Zj, ranef)
#   fit <- do.call(rBind, fit)
#   residuals <- Y-fit
# #   gc()
#   #   sigma2 <- iM$theta[1]
#   #   names(sigma2) <- "sigma2"
#   #   TT <- fillT(iM$theta[-1])
#   #   dimnames(TT) <- list(clnms1, clnms1)
#   #   beta <- as.numeric(iM$beta)
#   #   names(beta) <- colnames(X)
#   #   
#   # #     w1 <- 1-iM$icc
#   # #     w <- 1/(1-w1)
#   # #     cj <- foreach(ii = unique(id1)) %do% {w1/(1+(sum(id1 %in% ii)-1)*w1)}
#   # #   rj <- mapply(function(y, z){ t(z[,1,drop = F])%*%y}, Yj, Zj)
#   # #     Sj <- mapply(function(x, z){ t(x)%*%z[,1,drop = F]}, Xj, Zj)
#   # #     #llply(Xj, function(x) matrix(apply(x, 2, sum, drop = F), ncol = 1))
#   # #     K <- Reduce("+", mapply(function(x, s, c){t(x)%*%x-c*s%*%t(s)}, Xj, Sj, cj))
#   # #    #B <- Reduce("+", mapply(function(x, y, c, r, s) {K%*%t(x)%*%y-c*r*K%*%s}, Xj, Yj, cj, rj, Sj))
#   # #   B <- K%*%t(X)%*%iM$V%*%Y
#   #   
#   #   V <- iM$V
#   #   Vj <- foreach(i = unique(id1)) %do% {V[id1 %in% i,id1 %in% i,drop = F]}
#   #   # Variances:
#   #   ccj <- t(X)%*%V%*%(Y-X%*%iM$beta)%*%t(Y-X%*%iM$beta)%*%V%*%X
#   #   Bcov <- (n1/(n1-1))*iM$P%*%ccj%*%iM$P
#   #   varcov <- mapply(function(z, x, v) (TT - TT%*%t(z)%*%v%*%(solve(v)-x%*%Bcov%*%t(x))%*%v%*%z%*%TT), Zj, Xj, Vj)
#   #   #Tcov <- (n1/(n1-1))*solve(iM$S)
#   #   ranef <- mapply(function(x, y, v, z) (TT%*%t(z)%*%v%*%(y - x%*%iM$beta)),Xj, Yj, Vj, Zj)
#   #   
#   #   fit <- mapply(function(x, z, u) x%*%iM$beta+z%*%u, Xj, Zj, ranef)
#   #   fit <- do.call(rBind, fit)
#   #   ranef <- llply(ranef, t)
#   #   ranef <- do.call(rBind, ranef)
#   #   rownames(ranef) <- unique(id1)
#   #   residuals <- Y-fit
#   #   gc()
#   #   vv <- do.call(cbind, llply(varcov, as.matrix))
#   #   dim(vv) <- c(dim(varcov[[1]]), length(varcov))
#   #   ranef <- as.data.frame(as.matrix(ranef))
#   #   attr(ranef, "postVar") <- vv
#     return(list(sigma2 = s2, TT = tau, beta = beta, N = N, n1 = n1, fitted = fit, resid = residuals))
#   
#     #return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, n1 = n1, ranef = list(ranef), cov = Bcov, fitted = fit, resid = residuals))
#   }
# 
#   fillSMINQUE2 <- function(Cjj, QI, wgt2) {
#       x <- matrix(0, ncol = length(QI), nrow = length(QI))
#       ns <- nrow(x)
#       cc <- combinations(ns,2,1:ns, repeats.allowed = T)
#       foreach(ii = 1:nrow(cc)) %do% {
#           k <- cc[ii, 1]
#           l <- cc[ii, 2]
#           s <- sum(diag(wgt2*Cjj%*%QI[[k]]%*%Cjj%*%QI[[l]]))
#           x[k,l] <- s
#           x[l, k] <- s
#           gc()
#         }
#       gc()
#       return(x)
#     }
# 
#   iMINQUE <- function(Z, X, Y, QI, w0, w1, wgt2, wgt12) {
#       Dw <- diag(c(rep(w0, nrow(Z)), rep(w1, ncol(Z)-nrow(Z))))
#       V <- solve(Z%*%Dw%*%t(Z))
#       XVX <- solve(t(X)%*%V%*%X)
#       Cjj <- V-V%*%X%*%XVX%*%t(X)%*%V#(V/wgt12)
#       B0 <- XVX%*%t(X)%*%V%*%Y
#       EB0 <- (Y-X%*%B0)%*%t(Y-X%*%B0)
#       
#         S <- fillSMINQUE2(Cjj, QI, wgt2)
#       #S <- fillSMINQUE2(V, QI, wgt2)
#         #fillS(Cjj, Qj)
#         WI <- laply(QI, function(qj) sum(diag(t(V%*%qj%*%V%*%EB0))))
#       #WI <- laply(QI, function(qj) sum(diag(t(Y)%*%Cjj%*%qj%*%Cjj%*%Y)))
#         theta0 <- ginv(S)%*%matrix(WI, ncol = 1)
#       icc <- theta0[2]/(sum(theta0))
#       return(list(icc = icc, theta = theta0, V = V, beta = B0, P = XVX))
#       #return(list(icc = icc, theta = theta0, V = V, P = XVX))
#   }