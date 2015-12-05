mcmcRES <- function(x, x0) {
  m  <- mean(x)
  RBias <- m/x0-1
  RMSE <- sqrt(mean((x/x0-1)^2))
  return(data.frame(Mean = m, RBias = RBias, RMSE = RMSE))
}

samplePOP <- function(dt, m = 35){
  info <- unique(dt[,c("IDSCHOOL", "IDCLASS", "psch", "pcl"), with = F], by = c("IDSCHOOL", "IDCLASS", "psch", "pcl"))
  idsch <- unique(info[, c("IDSCHOOL", "psch"), with = F])
  sidsch <- sample(idsch$IDSCHOOL, m, prob = idsch$psch)
  info <- info[IDSCHOOL %in% sidsch,  ]
  idclass <- info[, list(IDCLASS = sample(IDCLASS, 1, prob = pcl)), by = IDSCHOOL]
  smplDt <- dt[IDCLASS %in% idclass$IDCLASS, ]
  smplDt[, wsch := wsch/m]
  smplDt[, wcl := cj]
  smplDt[, wstd := 1]
  smplDt[, wtot := wsch*wcl*wstd]
  return(smplDt)
}


mySample <- function(dt, m = 75){
  info <- dt[,c("IDSCHOOL", "IDSTUD", "nj", "psch", "pstud", "eN"), with = F]
  idcl <- unique(info[, c("IDSCHOOL", "psch"), with = F])
  sidcl <- sample(idcl$IDSCHOOL, m, prob = idcl$psch)
  smplDt <- foreach(ii = sidcl, .combine = rbind) %do% {
    stud <- subset(info, IDSCHOOL %in% ii, c("IDSTUD", "nj", "eN"))
    n1 <- round(3/5*unique(stud$nj))
    n2 <- round(2/5*unique(stud$nj))
    ##print(c(n1, n2))
    stud1 <- subset(stud, eN<=0)$IDSTUD
    stud2 <- subset(stud, eN>0)$IDSTUD
    if(n1 < length(stud1) & n2 < length(stud2)){
      idstud1 <- sample(stud1, n1)
      idstud2 <- sample(stud2, n2)
      smpl1 <- subset(dt, IDSTUD %in% idstud1)
      smpl1$pstud <- n1/length(stud1)
      ##print(sum(smpl1$pstud))
      smpl2 <- subset(dt, IDSTUD %in% idstud2)
      smpl2$pstud <- n2/length(stud2)
      ##print(sum(smpl2$pstud))
      rbind(smpl1, smpl2)
    } else {
      idstud <- sample(stud$IDSTUD, unique(stud$nj))
      smpl1 <- subset(dt, IDSTUD %in% idstud)
      smpl1$pstud <- unique(stud$nj)/nrow(stud)
      smpl1
    }
  }
  smplDt[, psch := m*psch]
  return(smplDt)
}

bootSampleREML <- function(dt, idstrata = "IDSTRATE", idschool = "IDSCHOOL",
                           idstud = "IDSTUD", wgt = c("STUDWGT", "SCHWGT")) {
  strata <- unlist(dt[, idstrata, with = F])
  sd <- foreach(ii = unique(strata), .combine = rbind) %do% {
    sdata <- dt[strata %in% ii, ,drop = F]
    sch <- unique(unlist(sdata[,idschool, with = F]))
    nh <- length(sch)
    if (nh == 1) nh <- 2
    smSCH <- as.numeric(sample(as.character(sch), nh-1, replace = T))
    #smSCH <- sch
    std <- foreach(jj = smSCH, .combine = rbind) %do% {
      studdt <- sdata[sch %in% jj,,drop = F]
      idst <- unlist(studdt[, idstud, with = F])
      nhc <- length(idst)
      smplStud <- sample(idst, nhc-1, replace = F)
      studdt1 <- foreach(kk = smplStud, .combine = rbind) %do% studdt[idst %in% kk,,drop = F]
      studdt1$nhc <- nhc
      return(studdt1)
    }
    return(std)
  }
  return(sd)
}

bootSample <- function(dt, idstrata = "IDSTRATE", idschool = "IDSCHOOL",
                       idstud = "IDSTUD", wgt = c("STUDWGT", "SCHWGT")) {
  strata <- unlist(dt[, idstrata, with = F])
  sd <- foreach(ii = unique(strata), .combine = rbind) %do% {
    sdata <- dt[strata %in% ii, ,drop = F]
    sch <- unique(unlist(sdata[,idschool, with = F]))
    nh <- length(sch)
    if (nh == 1) nh <- 2
    smSCH <- as.numeric(sample(as.character(sch), nh-1, replace = T))
    #smSCH <- sch
    tc <- count(smSCH)
    colnames(tc) <- c(idschool, "tc")
    sdata <- merge(sdata, tc, by = idschool)
    sdata[, wgt[2] := sdata[, wgt[2], with = F]*nh/(nh-1)*sdata$tc, with = F]
    sch <- unlist(sdata[,idschool, with = F])
    std <- foreach(jj = smSCH, .combine = rbind) %do% {
      studdt <- sdata[sch %in% jj,,drop = F]
      idst <- unlist(studdt[, idstud, with = F])
      nhc <- length(idst)
      smplStud <- sample(idst, nhc-1, replace = F)
      #       tci <- count(smplStud)
      #       colnames(tci) <- c(idstud, "tci")
      #       studdt <- merge(studdt, tci)
      #       studdt[, wgt[1]] <- studdt[, wgt[1]]*nhc/(nhc-1)*studdt$tci#/studdt$tci
      #       idst <- studdt[, idstud]
      studdt1 <- foreach(kk = smplStud, .combine = rbind) %do% studdt[idst %in% kk,,drop = F]
      studdt1$nhc <- nhc
      return(studdt1)
    }
    tci <- count(std[, idstud, with = F])
    colnames(tci) <- c(idstud, "tci")
    std <- merge(std, tci, by = idstud)
    std[, wgt[1]:= std[, wgt[1], with = F]*nhc/(nhc-1)*tci/tc, with = F]
    #print(dim(std))
    std <- unique(std)
    #print(dim(std))
    return(std)
  }
  return(sd)
}


bootREML <- function(data = data.2011, R = 5,
                     form = "BSMMAT01 ~ 1+SSEX+(1|IDSCHOOL)",
                     idstrata = "IDSTRATE", idschool = "IDSCHOOL",
                     idstud = "IDSTUD") {
  #print("Iteration:")
  tt0 <- lmer(form, data = data)
  t0 <- lme4:::summary.merMod(tt0)
  t0$beta <- fixef(tt0)
  t0$sigma2 <- t0$sigma^2
  t0$TT <- t0$varcor[[1]]
  
  bootRes <- foreach(ii = 1:R, .combine = rbind) %do% {
    #print(paste("boot", ii))
    smpldt <- try(bootSampleREML(data, idstrata, idschool,
                                 idstud, wgt = NULL))
    if(class(smpldt)[1] == "try-error") return(NULL)
    res <- try(lmer(form, data = smpldt))
    if(class(res)[1] == "try-error") return(NULL)
    res1 <- lme4:::summary.merMod(res)
    cc <- c(fixef(res), res1$sigma^2, unlist(res1$varcor[[1]]))
    #names(cc) <- c("beta", "sigma2", "")
    return(cc)
  }
  #print("Collect")
  
  #collect results
  #   attrib  <- boutSIMPLE$attr
  #   dims <- attrib$dims
  #   nms <- attrib$nms
  #   bb <- boutSIMPLE[[1]]
  #   tt <- bb$t
  #   tt0 <- bb$t0
  tt <- bootRes
  # collect beta
  beta0 <- t0$beta
  beta  <- tt[,1:length(beta0), drop = F]
  colnames(beta) <- rownames(beta0)
  Bcov <- cov(beta)
  Bint <- t(t(beta)-as.numeric(beta0))
  Bint <- apply(Bint, 2, function(x) quantile(x, probs=c(0.025,0.975)))
  Bint <- rbind((t(beta0)-Bint[2,]), (t(beta0)-Bint[1,]))
  rownames(Bint) <- c("2.5%", "97.5%")
  beta <- apply(beta, 2, mean)
  Bbias <- beta - beta0
  beta <- beta0-Bbias
  beta <- round(cbind(Coefficient = beta, St.err = sqrt(diag(Bcov)), Bias = Bbias, t(Bint)),4)
  colnames(beta)[1:3] <- c("Coeff", "St.err", "Bias")
  attr(beta, "cov") <- Bcov
  
  tt <- tt[,-1:-length(beta0), drop = F]
  #   # collect runef
  #   rdim <- dims$ranef
  #   ranef <- tt[,1:(rdim[1]*rdim[2]), drop = F]
  #   ranef0 <- tt0[1:(rdim[1]*rdim[2]), drop = F]
  #   
  #   ranef <- foreach(i = 1:rdim[1]) %do% {
  #     wh <- (1:rdim[2]*rdim[1]- rdim[1])+i
  #     rr <- ranef[, wh, drop = F]
  #     colnames(rr) <- nms$ranef[[2]]
  #     rr0 <- ranef0[wh, drop = F]
  #     Rcov <- cov(rr)
  #     #Rint <- apply(rr, 2, function(x) quantile(x, c(.025, .975)) )
  #     Rint <- t(t(rr)-(rr0))
  #     Rint <- apply(Rint, 2, function(x) quantile(x, .975))
  #     Rint <- rbind((t(rr0)-Rint), (t(rr0)+Rint))
  #     rownames(Rint) <- c("2.5%", "97.5%")
  #     
  #     rr <- apply(rr, 2, mean)
  #     Rbias <- rr - rr0
  #     rr <- rr0 - Rbias
  #     rr <- cbind(Coefficient = rr, St.err = sqrt(diag(Rcov)), Bias = Rbias, t(Rint))
  #     attr(rr, "cov") <- Rcov
  #     return(rr)
  #   }
  #   names(ranef) <- nms$ranef[[1]]
  #   
  #   tt <- tt[,-1:-(rdim[1]*rdim[2]), drop = F]
  #   tt0 <- tt0[-1:-(rdim[1]*rdim[2]), drop = F]
  
  # collect sigma
  sigma0 <- t0$sigma2
  sigma  <- tt[,1, drop = F]
  colnames(sigma) <- "sigma2"
  Scov <- cov(sigma)
  #Sint <- apply(sigma, 2, function(x) quantile(x, c(.025, .975)) )
  Sint <- t(t(sigma)-(sigma0))
  Sint <- apply(Sint, 2, function(x) quantile(x, probs=c(0.025,0.975)))
  Sint <- rbind((t(sigma0)-Sint[2,]), (t(sigma0)-Sint[1,]))
  rownames(Sint) <- c("2.5%", "97.5%")
  
  sigma <- apply(sigma, 2, mean)
  Sbias <- sigma - sigma0
  sigma <- sigma0 - Sbias
  sigma <- cbind(Coefficient = sigma, St.err = sqrt(diag(Scov)), Bias = Sbias, t(Sint))
  attr(sigma, "cov") <- Scov
  
  tt <- tt[,-1, drop = F]
  
  #collect TT
  TT0 <- as.numeric(t0$TT)
  TT  <- tt
  #   perm <- apply(permutations(TTdim,2,1:TTdim, repeats.allowed = T), 1, paste, collapse = "")
  #   comb <- apply(combinations(TTdim,2,1:TTdim, repeats.allowed = T), 1, paste, collapse = "")
  #   wh <- perm %in% comb
  #   TT <- TT[,wh, drop = F]
  TTcov <- cov(TT)
  #TTint <- apply(TT, 2, function(x) quantile(x, c(.025, .975)) )
  TTint <- t(t(TT)-(TT0))
  TTint <- apply(TTint, 2, function(x) quantile(x, probs=c(0.025,0.975)))
  TTint <- rbind((t(TT0)-TTint[2,]), (t(TT0)-TTint[1,]))
  rownames(TTint) <- c("2.5%", "97.5%")
  
  TT <- apply(TT, 2, mean)
  TTbias <- TT - TT0
  TT <- TT0 - TTbias
  TT <- cbind(Coefficient = TT, St.err = sqrt(diag(TTcov)), Bias = TTbias, t(TTint))
  #   rownames(CTT) <- as.vector(outer(nms$TT[[1]], nms$TT[[1]], function(x, y) paste(x, y, sep = '.')))[wh]
  #   TT <- fillT(TT)
  #   attr(TT, "char") <- CTT
  rownames(TT) <- rep("tau00", nrow(TT))
  
  return(list(beta = beta, sigma2 = sigma, TT = TT, t0 = t0, result = bootRes, tt0 = tt0))
  #return(list(beta = beta, ranef = ranef, sigma2 = sigma, TT = TT))
}

bootMINQUE2 <- function(data = data.2011, FUN  = myMINQUE2, R = 5,
                        fixed = "BSMMAT01 ~ 1+SSEX", random = "~1|IDSCHOOL",
                        idstrata = "IDSTRATE", idschool = "IDSCHOOL",
                        idstud = "IDSTUD", wgt = c("STUDWGT", "SCHWGT"),
                        BFUN = bootSampleREML) {
  #print("Iteration:")
  t0 <- FUN(data, fixed = fixed, random = random, weights = wgt)
  
  bootRes <- foreach(ii = 1:R, .combine = rbind) %do% {
    #print(paste("boot", ii))
    smpldt <- try(BFUN(data, idstrata, idschool,
                       idstud, wgt))
    if(class(smpldt)[1] == "try-error") return(NULL)
    res <- try(FUN(smpldt, fixed = fixed, random = random, weights = wgt))
    if(class(res) == "try-error") return(NULL)
    cc <- c(res$beta, res$sigma2, unlist(res$TT))
    names(cc) <- c("beta", "sigma2", "")
    return(cc)
  }
  #print("Collect")
  
  #collect results
  #   attrib  <- boutSIMPLE$attr
  #   dims <- attrib$dims
  #   nms <- attrib$nms
  #   bb <- boutSIMPLE[[1]]
  #   tt <- bb$t
  #   tt0 <- bb$t0
  tt <- bootRes
  # collect beta
  beta0 <- t0$beta
  beta  <- tt[,1:length(beta0), drop = F]
  colnames(beta) <- rownames(beta0)
  Bcov <- cov(beta)
  Bint <- t(t(beta)-as.numeric(beta0))
  Bint <- apply(Bint, 2, function(x) quantile(x, probs=c(0.025,0.975)))
  Bint <- rbind((t(beta0)-Bint[2,]), (t(beta0)-Bint[1,]))
  rownames(Bint) <- c("2.5%", "97.5%")
  beta <- apply(beta, 2, mean)
  Bbias <- beta - beta0
  beta <- beta0-Bbias
  beta <- round(cbind(Coefficient = beta, St.err = sqrt(diag(Bcov)), Bias = Bbias, t(Bint)),4)
  colnames(beta)[1:3] <- c("Coeff", "St.err", "Bias")
  attr(beta, "cov") <- Bcov
  
  tt <- tt[,-1:-length(beta0), drop = F]
  #   # collect runef
  #   rdim <- dims$ranef
  #   ranef <- tt[,1:(rdim[1]*rdim[2]), drop = F]
  #   ranef0 <- tt0[1:(rdim[1]*rdim[2]), drop = F]
  #   
  #   ranef <- foreach(i = 1:rdim[1]) %do% {
  #     wh <- (1:rdim[2]*rdim[1]- rdim[1])+i
  #     rr <- ranef[, wh, drop = F]
  #     colnames(rr) <- nms$ranef[[2]]
  #     rr0 <- ranef0[wh, drop = F]
  #     Rcov <- cov(rr)
  #     #Rint <- apply(rr, 2, function(x) quantile(x, c(.025, .975)) )
  #     Rint <- t(t(rr)-(rr0))
  #     Rint <- apply(Rint, 2, function(x) quantile(x, .975))
  #     Rint <- rbind((t(rr0)-Rint), (t(rr0)+Rint))
  #     rownames(Rint) <- c("2.5%", "97.5%")
  #     
  #     rr <- apply(rr, 2, mean)
  #     Rbias <- rr - rr0
  #     rr <- rr0 - Rbias
  #     rr <- cbind(Coefficient = rr, St.err = sqrt(diag(Rcov)), Bias = Rbias, t(Rint))
  #     attr(rr, "cov") <- Rcov
  #     return(rr)
  #   }
  #   names(ranef) <- nms$ranef[[1]]
  #   
  #   tt <- tt[,-1:-(rdim[1]*rdim[2]), drop = F]
  #   tt0 <- tt0[-1:-(rdim[1]*rdim[2]), drop = F]
  
  # collect sigma
  sigma0 <- t0$sigma2
  sigma  <- tt[,1, drop = F]
  colnames(sigma) <- "sigma2"
  Scov <- cov(sigma)
  #Sint <- apply(sigma, 2, function(x) quantile(x, c(.025, .975)) )
  Sint <- t(t(sigma)-(sigma0))
  Sint <- apply(Sint, 2, function(x) quantile(x, probs=c(0.025,0.975)))
  Sint <- rbind((t(sigma0)-Sint[2,]), (t(sigma0)-Sint[1,]))
  rownames(Sint) <- c("2.5%", "97.5%")
  
  sigma <- apply(sigma, 2, mean)
  Sbias <- sigma - sigma0
  sigma <- sigma0 - Sbias
  sigma <- cbind(Coefficient = sigma, St.err = sqrt(diag(Scov)), Bias = Sbias, t(Sint))
  attr(sigma, "cov") <- Scov
  
  tt <- tt[,-1, drop = F]
  
  #collect TT
  TT0 <- as.numeric(t0$TT)
  TT  <- tt
  #   perm <- apply(permutations(TTdim,2,1:TTdim, repeats.allowed = T), 1, paste, collapse = "")
  #   comb <- apply(combinations(TTdim,2,1:TTdim, repeats.allowed = T), 1, paste, collapse = "")
  #   wh <- perm %in% comb
  #   TT <- TT[,wh, drop = F]
  TTcov <- cov(TT)
  #TTint <- apply(TT, 2, function(x) quantile(x, c(.025, .975)) )
  TTint <- t(t(TT)-(TT0))
  TTint <- apply(TTint, 2, function(x) quantile(x, probs=c(0.025,0.975)))
  TTint <- rbind((t(TT0)-TTint[2,]), (t(TT0)-TTint[1,]))
  rownames(TTint) <- c("2.5%", "97.5%")
  
  TT <- apply(TT, 2, mean)
  TTbias <- TT - TT0
  TT <- TT0 - TTbias
  TT <- cbind(Coefficient = TT, St.err = sqrt(diag(TTcov)), Bias = TTbias, t(TTint))
  #   rownames(CTT) <- as.vector(outer(nms$TT[[1]], nms$TT[[1]], function(x, y) paste(x, y, sep = '.')))[wh]
  #   TT <- fillT(TT)
  #   attr(TT, "char") <- CTT
  #rownames(TT) <- "tau00"
  
  return(list(beta = beta, sigma2 = sigma, TT = TT, t0 = t0, result = bootRes))
  #return(list(beta = beta, ranef = ranef, sigma2 = sigma, TT = TT))
}


MIsummary <- function(sm) {
  m <- length(sm)
  n <- sm[[1]]$devcomp$dims[1]
  
  covar <- suppressWarnings(llply(sm, function(x) x$vcov))
  coeff <- lapply(sm, coef)
  tau <- llply(sm, function(x) x$varcor)
  tt <- foreach(i = 1:length(sm[[1]]$varcor)) %do% llply(tau, function(x) x[[i]])
  tau <- llply(tt, function(x) Reduce('+', x)/m)
  sigma <- llply(sm, function(x) x$sigma^2)
  sigma <- Reduce('+', sigma)/m
  
  
  cc <- laply(coeff, function(x) x[, "Estimate", drop = F], .drop = F)
  theta <- apply(cc, 2, mean)
  uu <- as(Reduce('+', covar)/m, "dpoMatrix")
  mcc <- alply(apply(cc, 2, function(x) x-mean(x)), 1, 
               function(x) matrix(x, ncol=1) %*% matrix(x, nrow=1))
  B <- uu
  attributes(B)$x <- as.vector(Reduce('+', mcc)/(m-1))
  #   nom <- Reduce("+", list(uu, (m+1)/m*B))^2
  #   den <- Reduce("+", list(uu^2/(n-length(theta)),((m+1)/m)^2*B^2*1/(m-1)))
  #   DF <- diag(as(Reduce("/", list(nom, den)), "matrix"))
  DF <- diag(as(((m-1)*(1+Reduce("/", list(m*uu, B*(1+m))))^2), "matrix"))
  
  TT <- as(Reduce('+', list(uu, (1+1/m)*B)), "dpoMatrix")
  attributes(TT)$factors$correlation <- cov2cor(TT)
  
  theta <- cbind(theta, sqrt(diag(as(TT, "matrix"))))  
  theta <- round(cbind(theta, theta[,1]/theta[,2], round(2*(1 - pt(abs(theta[,1]/theta[,2]), DF)), 3)), 6)
  attributes(theta)$dimnames[[2]] <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  
  tau  <- llply(tau, function(x) {
    attr(x, "stddev") <- sqrt(diag(x))
    cv <- cov2cor(x)
    attributes(cv) <- attributes(attributes(x)$correlation)
    attr(x, "correlation") <- cv
    return(x)
  })
  
  rval <- sm[[1]]
  attributes(tau) <- attributes(rval$varcor)
  rval$varcor <- tau
  attributes(rval$varcor)$sc <- sqrt(sigma)
  rval$sigma <- sqrt(sigma)
  rval$vcov <- TT
  rval$coefficients <- round(theta, 4)
  
  rval
}

rwis <- function(n, mean, sigma){
  crossprod(rmvnorm(n , mean = mean, sigma=sigma))
} 


makePOPW <- function(M = 300, Nclass = 30, sigma2 = 100, tau00 = 1, tau01 = 0.5,
                     tau11 = 1, g00 = 450, g01 = 10, g10 = 30, g11 = 5){
  require(msm)
  require(data.table)
  struct <- 1:M
  uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
  N <- round(50*exp(uj))

  struct <- as.data.table(cbind(IDSCHOOL = struct, Nj = N))
  struct[, psch := Nj/sum(Nj)]
  
  x.grid = seq(1, 8, by = 1/3)
  struct[, W := sample(x.grid, M, replace = T)]
#   struct[Nj > 120, did := 1]
#   struct[Nj <= 120, did := 0]
#   didel <- sum(struct$did)
#   x.grid = seq(1, 8, by = 1/3)
#   struct[Nj > 120, W := sample(x.grid[14:22], didel, replace = T)+50]
#   struct[Nj <= 120, W := sample(x.grid[1:8], M - didel, replace = T)]
  
#    x.grid = seq(1, 8, by = 1/3)
#    struct[, W := sample(x.grid[1:8], nrow(struct), replace = T)]
#    struct[IDSCHOOL %in% sample(1:M, M*0.1), W := sample(x.grid[14:22], M*0.1, replace = T)+50]
#   struct[, W := rnorm(M, 10, 20)]
#   struct[IDSCHOOL %in% sample(1:M, M*0.05), W := rnorm(M*0.05, 500, 15)]
  
  
  # Make alpha  
  ttN <- matrix(c(tau00, tau01, tau01, tau11), nrow = 2)
  ddN <- mvrnorm(n = M, rep(0, 2), ttN)
  
  ttX <- matrix(c(1, sqrt(tau01/(sqrt(tau00)*sqrt(tau11))), sqrt(tau01/(sqrt(tau00)*sqrt(tau11))), 1), nrow = 2)
  ddX <- mvrnorm(n = M, rep(0, 2), ttX)
  
  struct[, u0N := ddN[, 1]]#rnorm(M, 0, sqrt(0.005))]
  struct[, u0X := ((ddX[, 1]^2 - 1)/sqrt(2))*sqrt(tau00)]
  
  struct[, u1N := ddN[, 2]]#rnorm(M, 0, sqrt(0.005))]
  struct[, u1X := ((ddX[, 2]^2 - 1)/sqrt(2))*sqrt(tau11)]
  
  class <- struct[, list(Nj, kj = sample(1:round(Nj/Nclass), Nj, replace = T)), by = IDSCHOOL]
  class[kj == 0, kj := 1]
  class[, cj := max(kj), by = "IDSCHOOL"]
  class[, pcl := 1/cj]
  
  struct <- merge(struct, class, by = c("IDSCHOOL", "Nj"))
  
  allval <- (rchisq(sum(N), 2)-2)/sqrt(2*2)*sqrt(sigma2)
  struct$eN <- rnorm(sum(N),0, sqrt(sigma2))
  struct$eX <- allval
  
  struct[, c("nj", "pstud", "IDSTUD") := list(length(eN), 1, 1:length(eN)), by = c("IDSCHOOL", "kj")]
  struct[, ptot := psch*pcl*pstud]
  struct[, X1 := rbinom(sum(N), 1, 0.2)]
  struct[, IDCLASS := paste0(IDSCHOOL, kj)]
  struct[, IDSTUD := paste0(IDSCHOOL, kj, IDSTUD)]
  
  struct[, c("wsch", "wcl", "wstd", "wtot") := list(1/psch, 1/pcl, 1/pstud, 1/ptot)]
  
  struct[, c("Y1", "Y2") := list(Y1 = g00+g01*W+g10*X1+g11*W*X1+eN+u0N+u1N*X1,
                                 Y2 = g00+g01*W+g10*X1+g11*W*X1+eX+u0X+u1X*X1)]
  
  return(struct)
}

makePOP3 <- function(M = 300){
  require(msm)
  struct <- 1:M
  uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
  Nj <- round(50*exp(uj))
  #ups(190, M, prob = c(1:95, rep(1,95)), replace = TRUE)+10
  struct <- as.data.table(cbind(IDSCHOOL = struct, Nj = Nj))
  # struct <- ddply(data.2011, ~IDSCHOOL, function(x) length(unique(x$IDSTUD)))
  # struct$IDSCHOOL <- 1:nrow(struct)
  # names(struct)[2] <- "Nj"
  struct[, psch := Nj/sum(Nj)]
  
  struct$W <- rbinom(M, 1, prob = 0.2)
  #Make u
  #     S1 <- matrix(c(1300,-180,-180,480),nrow=2,byrow=TRUE)
  #     mu1 <- c(1,1)
  #     S2 <- matrix(c(1300,-180,-180,480),nrow=2,byrow=TRUE)
  #     mu2 <- c(-1,-1)
  #   
  #     n <- M
  #     p1 <- 0.8
  #     n1 <- rbinom(1,size=n,prob=p1)  ## how many from first distribution?
  #     n2 <- n-n1
  #     val1 <- mvrnorm(n1,mu=mu1,Sigma=S1)
  #     val2 <- mvrnorm(n2,mu=mu2,Sigma=S2)
  #     allval <- rbind(val1,val2)      ## combine
  #     allval <- allval[sample(n,n),]
  #     u0j <- allval[, 1]
  #     u1j <- allval[, 2]
  #     uN <- rmvnorm(M, mean = c(0, 0), sigma = S2)
  #     # Make alpha   
  #     struct$aN <- 500+uN[,1]
  #     struct$aX <- 500+u0j
  #     # Make beta
  #     struct$bN <- -4+uN[,2]
  #     struct$bX <- -4+u1j
  #     S1 <- sqrt(0.5)
  #     mu1 <- c(-20)
  #     S2 <- sqrt(0.5)
  #     mu2 <- c(20)
  #     
  #     n <- M
  #     p1 <- 0.8
  #     n1 <- rbinom(n,size=1,prob=p1)  ## how many from first distribution?
  #     n2 <- 1-n1
  #     val1 <- rnorm(n,mu1,S1)
  #     val2 <- rnorm(n,mu2,S2)
  #     allval <- n1*val1+n2*val2      ## combine
  #     allval <- (allval[sample(n,n)]-(p1*mu1+(1 - p1)*mu2))/sqrt(p1*(mu1-p1*mu1-(1 - p1)*mu2)^2+(1-p1)*(mu2-p1*mu1-(1 - p1)*mu2)^2+S1^2)*sqrt(1300)
  # allval <- (rpois(M, 1)-1)*sqrt(1300)
  allval <- (rchisq(M, df = 1)-1)/sqrt(2)*sqrt(1300)
  # Make alpha   
  struct[, aN := 500+rnorm(M,0, sqrt(1300))]
  struct[, aX := 500+allval]
  
  
  
  # Make main error
  #     S1 <- sqrt(1)
  #     mu1 <- c(-10)
  #     S2 <- sqrt(1)
  #     mu2 <- c(10)
  #     
  #     n <- sum(struct$Nj)
  #     p1 <- 0.8
  #     n1 <- rbinom(n,1,prob=p1)  ## how many from first distribution?
  #     n2 <- abs(n1-1)
  #     val1 <- rnorm(n,mu1,S1)
  #     val2 <- rnorm(n,mu2,S2)
  #     allval <- n1*val1+n2*val2      ## combine
  #     allval <- (allval[sample(n,n)]-(p1*mu1+(1 - p1))*mu2)/sqrt(p1*(mu1-p1*mu1-(1 - p1)*mu2)^2+(1-p1)*(mu2-p1*mu1-(1 - p1)*mu2)^2+S1^2)*sqrt(2000)
  #allval <- (rpois(sum(struct$Nj), 2)-2)/sqrt(2)*sqrt(2000)
  allval <- (rchisq(sum(struct$Nj), 2)-2)/sqrt(2*2)*sqrt(2000)
  eN <- struct[, matrix(rnorm(Nj,0, sqrt(2000)), ncol = 1), by = IDSCHOOL]
  setnames(eN, "V1", "eN")
  eN$eN <- rnorm(nrow(eN),0, sqrt(2000))
  eN$eX <- allval
  
  
  struct <- merge(struct, eN, by = "IDSCHOOL")
  
  struct[, c("nj", "pstud") := list(round(ifelse(length(W)<30, length(W), ifelse(length(W)>=30&length(W)<60, length(W)/2, length(W)/3))), 1/length(W)), by = IDSCHOOL]
  struct[, c("ptot", "X1", "X2") := list(psch*pstud, rnorm(nrow(struct), 90, 20), rbinom(nrow(struct), 1, prob=0.5))]
  
  #   struct$Y1 <- struct$aN+struct$bN*struct$X+struct$eN
  #   struct$Y2 <- struct$aX+struct$bX*struct$X+struct$eN
  #   struct$Y3 <- struct$aN+struct$bN*struct$X+struct$eX
  #   struct$Y4 <- struct$aX+struct$bX*struct$X+struct$eX
  
  #   struct$Y1 <- struct$aN+20*struct$W+50*struct$X1-4*struct$X2+struct$eN
  #   struct$Y2 <- struct$aX+20*struct$W+50*struct$X1-4*struct$X2+struct$eN
  #   struct$Y3 <- struct$aN+20*struct$W+50*struct$X1-4*struct$X2+struct$eX
  #   struct$Y4 <- struct$aX+20*struct$W+50*struct$X1-4*struct$X2+struct$eX
  struct[, c("Y1", "Y2", "Y3", "Y4") := list(aN+20*W+50*X1-4*X2+eN,
                                             aX+20*W+50*X1-4*X2+eN,
                                             aN+20*W+50*X1-4*X2+eX,
                                             aX+20*W+50*X1-4*X2+eX)]
  
  #   struct[, c("Y1", "Y2", "Y3", "Y4") := list(aN+20*W-4*X2+eN,
  #                                              aX+20*W-4*X2+eN,
  #                                              aN+20*W-4*X2+eX,
  #                                              aX+20*W-4*X2+eX)]
  
  struct[, IDSTUD := 1:nrow(struct)]
  return(struct)
}


makePOP4 <- function(M = 30){
  struct <- 1:M
  uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
  Nj <- round(50*exp(uj))
 # Nj <- 30
  
  struct <- as.data.table(cbind(IDSCHOOL = struct, Nj = Nj))
  
  struct[, psch := Nj/sum(Nj)]
  
  struct$W <- rbinom(M, 1, prob = 0.2)#rnorm(M, 0, sqrt(1))
  
  # Make alpha  
  ttN <- matrix(c(0.005, 0.0025, 0.0025, 0.005), nrow = 2)
  ddN <- mvrnorm(n = M, rep(0, 2), ttN)
  
  ttX <- matrix(c(1, sqrt(0.5), sqrt(0.5), 1), nrow = 2)
  ddX <- mvrnorm(n = M, rep(0, 2), ttX)
  
  struct[, d0N := ddN[, 1]]#rnorm(M, 0, sqrt(0.005))]
  struct[, d0X := ((ddX[, 1]^2 - 1)/sqrt(2))*sqrt(0.005)]
  
  struct[, d1N := ddN[, 2]]#rnorm(M, 0, sqrt(0.005))]
  struct[, d1X := ((ddX[, 2]^2 - 1)/sqrt(2))*sqrt(0.005)]
  
  
  #(rchisq(sum(struct$Nj), 1)-2)/sqrt(2*2)*sqrt(0.5)
  eN <- struct[, matrix(rnorm(Nj,0, sqrt(0.5)), ncol = 1), by = IDSCHOOL]
  setnames(eN, "V1", "eN")
  eN$eN <- rnorm(nrow(eN),0, sqrt(0.5))
  eN$eX <- ((rchisq(sum(struct$Nj), 1) - 1)/sqrt(2))*sqrt(0.5)
  
  
  struct <- merge(struct, eN, by = "IDSCHOOL")
  
  struct[, c("nj", "pstud") := list(round(ifelse(length(W)<30, length(W), ifelse(length(W)>=30&length(W)<60, length(W)/2, length(W)/3))), 1/length(W)), by = IDSCHOOL]
  struct[, ptot := psch*pstud]
  
  #x.grid = seq(0, 8, by = 8/max(Nj))#rnorm(Nj, 0, 1) sample(x.grid, Nj[1], replace = T)
  struct[, X1 := rbinom(Nj[1], 1, 0.05), by = "IDSCHOOL"]

  struct[, c("Y1", "Y2") := list((1+0.3*W+d0N)+(0.3+0.3*W+d1N)*X1+eN,
                                 (1+0.3*W+d0X)+(0.3+0.3*W+d1X)*X1+eX)]
  
  struct[, IDSTUD := 1:nrow(struct)]
  return(struct)
}


makePOP6 <- function(M = 30){
  require(data.table)
  require(MASS)
  struct <- 1:M
  struct <- data.table(IDSCHOOL = struct)
  
  # Make second level errors
  
  struct[, d0N := rnorm(n = M, 0, sqrt(10))]
  struct[, d0P := rpois(n=M, lambda=10)-10]
  
  
  uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
  Nj <- round(50*exp(uj))
  
  struct[, Nj := Nj]
  
  struct[, psch := Nj/sum(Nj)]
  
  #
  eN <- struct[, matrix(rnorm(Nj,0, sqrt(20)), ncol = 1), by = IDSCHOOL]
  setnames(eN, "V1", "eN")
  eN$eN <- rnorm(nrow(eN),0, sqrt(20))
  eN$eP <- rpois(n=nrow(eN), lambda=20)-20
  
  
  struct <- merge(struct, eN, by = "IDSCHOOL")
  
  struct[, c("nj", "pstud") := list(round(ifelse(length(Nj)<30, length(Nj), ifelse(length(Nj)>=30&length(Nj)<60, length(Nj)/2, length(Nj)/3))), 1/length(Nj)), by = IDSCHOOL]
  struct$ptot <- struct$psch*struct$pstud
  struct$X1 <- rnorm(nrow(struct), 0, 1)
  
  struct[, c("Y1", "Y2") := list((1+d0N)+(0.3)*X1+eN,
                                 (1+d0P)+(0.3)*X1+eP)]
  
  struct[, IDSTUD := 1:nrow(struct)]
  return(struct)
}


makePOP7 <- function(M = 50){
  require(data.table)
  require(MASS)
  struct <- 1:M
  struct <- data.table(IDSCHOOL = struct)
  
  # Make second level errors
  
  struct[, d0N := rnorm(n = M, 0, sqrt(5))]
  u1 <- runif(M)
  u2 <- runif(M)
  x1 <- -log(u1)
  x2 <- ifelse(u2<0.5, -1, 1)
  struct[, d0P := 1/sqrt(2)*x1*x2*sqrt(5)]
  
  Nj <- c(rep(20,2), rep(25, 5), rep(30, 10), rep(35, 5), rep(40, 3), rep(20, 3),
          rep(25, 5), rep(30, 10), rep(35, 5), rep(40, 2))
#   uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
#   Nj <- round(50*exp(uj))
  
  struct[, Nj := Nj]
  
  struct[, psch := Nj/sum(Nj)]
  
  #
  eN <- struct[, matrix(rnorm(Nj,0, sqrt(100)), ncol = 1), by = IDSCHOOL]
  setnames(eN, "V1", "eN")
  eN$eN <- rnorm(nrow(eN),0, sqrt(100))
  eN$eN[1:10] <- rnorm(nrow(eN),0, sqrt(1000))[1:10]
  N <- nrow(eN)
  u1 <- runif(N)
  u2 <- runif(N)
  x1 <- -log(u1)
  x2 <- ifelse(u2<0.5, -1, 1)
  eN$eP <- 1/sqrt(2)*x1*x2*sqrt(100)
  eN$eP[1:10] <- (1/sqrt(2)*x1*x2*sqrt(1000))[1:10]
  
  struct <- merge(struct, eN, by = "IDSCHOOL")
  
  struct[, c("nj", "pstud") := list(round(ifelse(length(Nj)<30, length(Nj), ifelse(length(Nj)>=30&length(Nj)<60, length(Nj)/2, length(Nj)/3))), 1/length(Nj)), by = IDSCHOOL]
  struct$ptot <- struct$psch*struct$pstud
  struct$X1 <- c(rep(1, 465), rep(0, 480+555))
  struct$X2 <- c(rep(0, 465), rep(1, 480), rep(0, 555))
  struct$X3 <- c(rep(0, 465), rep(0, 480), rep(1, 555))
  struct$X4 <- floor(75*runif(sum(Nj)))+25
  
  struct[, c("Y1", "Y2") := list(-5*X1+2*X2+3*X3+X4+d0N+eN,
                                 -5*X1+2*X2+3*X3+X4+d0P+eP)]
  
  struct[, IDSTUD := 1:nrow(struct)]
  return(struct)
}


MYboot <- function(dt, frml, FUN = bfun, R = 10, ...) {
  bout <- boot(dt, FUN, R = R, 
               formul = frml, 
               parallel = "multicore", ...)
  nci <- norm.ci(bout)
  smm <- lme4:::summary.merMod(lmer(frml, dt))
  mod <- lmer(frml, dt)
  coef <- apply(bout$t, 2, mean)
  vcov <- cov(bout$t)
  nc <- length(coef(smm)[,1])
  sigma <- coef[nc+1]
  tau <- coef[(nc+2):length(coef), drop = F]
  coef <- coef[1:nc, drop = F]
  vcoef <- vcov[1:nc,1:nc, drop = F]
  vsigma <- vcov[nc+1,nc+1, drop = F]
  vtau <- vcov[(nc+2):length(coef), (nc+2):length(coef)]
  tau <- foreach(ii = 1:length(smm$varcor)) %do% {
    x <- smm$varcor[[ii]]
    tt <- tau[1:length(x)]
    tau <- tau[-1:-length(x)]
    tt <- matrix(tt, sqrt(length(x)))
    std <- sqrt(diag(tt))
    cr <- cov2cor(tt)
    attributes(tt) <- attributes(x)
    attr(tt, "stddev") <- std
    attr(tt, "correlation") <- cr
    return(tt)
  }
  coef <- cbind(coef, sqrt(diag(vcoef)))
  coef <- cbind(coef, coef[,1]/coef[,2])
  attributes(coef) <- attributes(smm$coefficients)
  
  rval <- smm
  attributes(tau) <- attributes(rval$varcor)
  rval$varcor <- tau
  attributes(rval$varcor)$sc <- sqrt(sigma)
  rval$sigma <- sqrt(sigma)
  attributes(rval$vcov)$x <- as.vector(vcoef)
  cv <- cov2cor(vcoef)
  attributes(attributes(rval$vcov)$factors$correlation)$sd <- sqrt(diag(vcoef))
  attributes(attributes(rval$vcov)$factors$correlation)$x <- as.vector(cov2cor(vcoef))
  rval$coefficients <- round(coef, 4)
  rval$nci  <- nci
  
  return(rval)
}

boot <- function (data, statistic, R, sim = "ordinary", stype = c("i", "f", "w"), 
                  strata = rep(1, n), L = NULL, m = 0, weights = NULL, 
                  ran.gen = function(d, p) d, mle = NULL, simple = FALSE, ..., 
                  parallel = c("no", "multicore", "snow"),
                  ncpus = getOption("boot.ncpus", 
                                    1L), cl = NULL, nsmpl = NULL, attrib = FALSE,
                  hierar = FALSE)
{
  call <- match.call()
  stype <- match.arg(stype)
  if (missing(parallel)) 
    parallel <- getOption("boot.parallel", "no")
  parallel <- match.arg(parallel)
  have_mc <- have_snow <- FALSE
  if (parallel != "no" && ncpus > 1L) {
    if (parallel == "multicore") 
      have_mc <- .Platform$OS.type != "windows"
    else if (parallel == "snow") 
      have_snow <- TRUE
    if (!have_mc && !have_snow) 
      ncpus <- 1L
  }
  if (simple && (sim != "ordinary" || stype != "i" || sum(m))) {
    warning("'simple=TRUE' is only valid for 'sim=\"ordinary\", stype=\"i\", n=0', so ignored")
    simple <- FALSE
  }
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  n <- NROW(data)
  if ((n == 0) || is.null(n)) 
    stop("no data in call to 'boot'")
  temp.str <- strata
  strata <- tapply(seq_len(n), as.numeric(strata))
  t0 <- if (sim != "parametric") {
    if ((sim == "antithetic") && is.null(L)) 
      L <- empinf(data = data, statistic = statistic, stype = stype, 
                  strata = strata, ...)
    if (sim != "ordinary") 
      m <- 0
    else if (any(m < 0)) 
      stop("negative value of 'm' supplied")
    if ((length(m) != 1L) && (length(m) != length(table(strata)))) 
      stop("length of 'm' incompatible with 'strata'")
    if ((sim == "ordinary") || (sim == "balanced")) {
      if (boot:::isMatrix(weights) && (nrow(weights) != length(R))) 
        stop("dimensions of 'R' and 'weights' do not match")
    }
    else weights <- NULL
    if (!is.null(weights)) 
      weights <- t(apply(matrix(weights, n, length(R), 
                                byrow = TRUE), 2L, normalize, strata))
    if (!simple) {
      i<- index.array(n, R, sim, strata, m, L, weights, nsmpl, hier = hierar)
    }
    original <- if (stype == "f") 
      rep(1, n)
    else if (stype == "w") {
      ns <- tabulate(strata)[strata]
      1/ns
    }
    else seq_len(n)
    if (sum(m) > 0L) {
      if(attrib) {
        t0 <-  statistic(data, original, rep(1, sum(m)), attrib, ...)
        attribt0 <- attributes(t0)
      } else 
        t0 <- statistic(data, original, rep(1, sum(m)), ...)
    }
    else {
      if(attrib) {
        t0 <-  statistic(data, original, attrib, ...)
        attribt0 <- attributes(t0)
      } else
        t0 <- statistic(data, original, ...)
    }
    rm(original)
    t0
  }
  else statistic(data, ...)
  pred.i <- NULL
  fn <- if (sim == "parametric") {
    ran.gen
    data
    mle
    function(r) {
      dd <- ran.gen(data, mle)
      statistic(dd, ...)
    }
  }
  else {
    if (!hierar) 
      
      if (!simple && ncol(i) > n) {
        pred.i <- as.matrix(i[, (n + 1L):ncol(i)])
        i <- i[, seq_len(n)]
      }
    
    if (stype %in% c("f", "w")) {
      f <- freq.array(i)
      rm(i)
      if (stype == "w") 
        f <- f/ns
      if (sum(m) == 0L) 
        function(r) statistic(data, f[r, ], ...)
      else function(r) statistic(data, f[r, ], pred.i[r, 
                                                      ], ...)
    }
    else if (sum(m) > 0L) 
      function(r) statistic(data, i[r, ], pred.i[r, ], 
                            ...)
    else if (simple) 
      function(r) statistic(data, index.array(n, 1, sim, 
                                              strata, m, L, weights), ...)
    else if (hierar) 
      function(r) statistic(data, i[[r]], ...)
    else
      function(r) statistic(data, i[r, ], ...)
  }
  RR <- sum(R)
  res <- if (ncpus > 1L && (have_mc || have_snow)) {
    if (have_mc) {
      parallel::mclapply(seq_len(RR), fn, mc.cores = ncpus)
    }
    else if (have_snow) {
      list(...)
      if (is.null(cl)) {
        cl <- parallel::makePSOCKcluster(rep("localhost", 
                                             ncpus))
        if (RNGkind()[1L] == "L'Ecuyer-CMRG") 
          parallel::clusterSetRNGStream(cl)
        res <- parallel::parLapply(cl, seq_len(RR), fn)
        parallel::stopCluster(cl)
        res
      }
      else parallel::parLapply(cl, seq_len(RR), fn)
    }
  }
  else lapply(seq_len(RR), fn)
  t.star <- matrix(, RR, length(t0))
  for (r in seq_len(RR)) t.star[r, ] <- res[[r]]
  if (is.null(weights)) 
    weights <- 1/tabulate(strata)[strata]
  
  bb <- boot:::boot.return(sim, t0, t.star, temp.str, R, data, statistic, 
                           stype, call, seed, L, m, pred.i, weights, ran.gen, mle)
  if (attrib)
    bb <- list(bb, attr = attribt0)
  return(bb)
}

index.array <- function (n, R, sim, strata = rep(1, n), m = 0, L = NULL, weights = NULL, nsmpl = NULL, hier = F) 
{
  indices <- NULL
  if (is.null(weights)) {
    if (sim == "ordinary") {
      indices <- ordinary.array(n, R, strata, nsmpl, hier)
      if (sum(m) > 0) 
        indices <- cbind(indices, extra.array(n, R, m, 
                                              strata))
    }
    else if (sim == "balanced") 
      indices <- balanced.array(n, R, strata)
    else if (sim == "antithetic") 
      indices <- antithetic.array(n, R, L, strata)
    else if (sim == "permutation") 
      indices <- permutation.array(n, R, strata)
  }
  else {
    if (sim == "ordinary") 
      indices <- importance.array(n, R, weights, strata)
    else if (sim == "balanced") 
      indices <- importance.array.bal(n, R, weights, strata)
  }
  indices
}

ordinary.array <- function (n, R, strata, nsmpl = NULL, hier = F) 
{
  inds <- as.integer(names(table(strata)))
  if (length(inds) == 1L) {
    if(!is.null(nsmpl)) {
      #print(nsmpl)
      output <- sample.int(n, nsmpl * R, replace = TRUE)
      dim(output) <- c(R, nsmpl)
    } else {
      output <- sample.int(n, n * R, replace = TRUE)
      dim(output) <- c(R, n)
    }
  }
  else {
    if(hier){
      output <- matrix(as.integer(0L), R, n)
      inds <- sample(inds, R*length(unique(inds)), replace = T)
      inds <- matrix(inds, nrow = R)
      output <- alply(inds, 1, function (x) {
        out <- foreach (is = x, .combine = c) %do% {
          gp <- seq_len(n)[strata == is]
          ss <- sample(gp, length(gp), replace = T)
          ss
        }})
    } else {
      output <- matrix(as.integer(0L), R, n)
      for (is in inds) {
        gp <- seq_len(n)[strata == is]
        output[, gp] <- if (length(gp) == 1) 
          rep(gp, R)
        else boot:::bsample(gp, R * length(gp))
      }
    }
  }
  output
}

bfun <- function(dat, orig, formul) {
  mod <- lme4:::summary.merMod(lmer(formul, dat[orig,]))
  c(coef(mod)[,1, drop = F], mod$sigma^2, unlist(mod$varcor))
}

iterMINQUE <- function(V, X, QI, theta, Y, id1, wgt2, wgt1){
  #1
  P <- solve(crossprod(X, V)%*%X)
  B <- tcrossprod(P, X)%*%V%*%Y
  EB <- tcrossprod(Y-X%*%B)
  
  Cjj <- V-V%*%X%*%P%*%crossprod(X, V)#(V*wgt1^2)
  #Cjj <- mapply(function(v, x) v-v%*%x%*%solve(t(x)%*%v%*%x)%*%t(x)%*%v, Vj, Xj)
  S <- fillSMINQUE(Cjj, QI, wgt2)
  #fillS(Cjj, Qj)
  WI <- laply(QI, function(qj) sum(diag(crossprod(Y, Cjj)%*%qj%*%Cjj%*%Y)))
  
  theta <- ginv(S)%*%matrix(WI, ncol = 1)
  Vj <- solve(Reduce("+", foreach(k = 1:length(theta)) %do% {theta[k]*QI[[k]]}))
  #theta0 <- c(3000, 1200)
  gc()
  #return(list(Vj = Vj, theta = theta, beta = B, S = S, WI = WI, P = P))
  return(list(Vj = Vj, theta = theta, S = S, WI = WI, P = P))
}

fillSMINQUE <- function(Cjj, QI, wgt2) {
  x <- matrix(0, ncol = length(QI), nrow = length(QI))
  ns <- nrow(x)
  cc <- combinations(ns,2,1:ns, repeats.allowed = T)
  foreach(ii = 1:nrow(cc)) %do% {
    k <- cc[ii, 1]
    l <- cc[ii, 2]
    s <- sum(diag(1/wgt2^2*Cjj%*%QI[[k]]%*%Cjj%*%QI[[l]]))
    x[k,l] <- s
    x[l, k] <- s
    gc()
  }
  gc()
  return(x)
}

myMINQUE <- function(dt, fixed, random1 = NULL, weights = NULL, apriori = NULL) {
  require("matrixcalc")
  #dt <- arrange(dt, IDSCHOOL)
  N <- nrow(dt)
  # Form Y and fixed effects data frame
  ff <- model.matrix(as.formula(fixed), model.frame(fixed, dt))
  Y <- as(as(model.frame(fixed, dt)[,1, drop = F], "matrix"), "Matrix")
  if (grepl("-1",fixed)&grepl(colnames(ff)[1], "(Intercept)")) {
    X <- as(as(ff[,-1, drop = F], "matrix"), "Matrix")
  } else {
    X <- as(as(ff, "matrix"), "Matrix")
  }
  
  if (!is.null(random1)) {
    id1 <- strsplit(random1, "\\|")
    random1 <- unlist(id1)[1]
    nmid1 <- unlist(id1)[2]
    rr1 <- model.frame(random1, dt)
    id1 <- model.frame(paste("~", nmid1), dt)[,1]
    if(!is.null(weights)){
      wg <- as.data.frame(dt[,c(nmid1, weights), with = F])
      wg <- foreach(ii = unique(id1)) %do% {x <- wg[id1 %in% ii, , drop = F];x[,2] <- x[,2]*nrow(x)/sum(x[,2]);return(x)}
      swg <- Reduce("+", llply(wg, function(x) unique(x[,3])))
      wg <- llply(wg, function(x) {x[,3] <- x[,3]*length(unique(id1))/swg; x$both <- x[, 2]*x[,3]; return(x)}) 
      
      
      wgt1i <- llply(wg, function(x) x[,2])
      wgt2i <- llply(wg, function(x) x[,3])
      wgt12i <- llply(wg, function(x) x[,4])
      
    } else {
      wg <- foreach(ii = unique(id1)) %do% {sum(id1 %in% ii)}
      wgt1i <- llply(wg, function(x) rep(1, x))
      wgt2i <- llply(wg, function(x) rep(1, x))
      wgt12i <- llply(wg, function(x) rep(1, x))
    }
    if (grepl("-1",random1)) {
      Z1 <- as(as( rr1, "matrix"), "Matrix")
    } else {
      Z1 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
                               dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
               "Matrix")
    }
    q <- ncol(Z1)
    colnames(Z1) <- paste(nmid1, colnames(Z1), sep = ".")
    clnms1 <- colnames(Z1)
    Z1 <- foreach(i = unique(id1)) %do% {Z1[id1 %in% i, ,drop = F]}
    Z1 <- mapply(function(z, w) {z*1/sqrt(w)}, Z1, wgt2i)
    Z <- bdiag(Z1)
    n1 <- length(unique(id1))
    l <- q*(q+1)/2
    TT1 <- formTT(q)
     QI <- llply(TT1, function(tt) llply(Z1, function(z) z%*%tt%*%t(z)))
  } else {
    Z1 <- NULL
    clnms1 <- NULL
    n1 <- NULL
    QI <- NULL
  }
  
  l <- l+1
  Qj <- c(list(llply(QI[[1]], function(zj) diag(nrow(zj)))), QI)
  Qj[[1]] <- foreach(i = 1:length(Qj[[1]])) %do% {1/wgt12i[[i]]*Qj[[1]][[i]]}
  QI <- llply(Qj, function(x) bdiag(x))
  Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i, ,drop = F]}
  X <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Xj[[ii]]), "Matrix")
  Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
  Y <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Yj[[ii]]), "Matrix")
  
  if (is.null(apriori)) apriori <- rep(1, length(QI))
  
  Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", mapply(function(qj, th) th*qj[[i]], Qj, apriori)))
  V <- bdiag(Vj)
  
  iM <- iMINQUE(V, X, Y, QI, wgt2i)
  ##print(iM$theta)
  gc()
  
#   theta <- iM$theta
#   #print(theta)
#   iM <- iMINQUE(iM$V, X, Y, QI, wgt2i)
  
  P <- ginv(as.matrix((crossprod(X, iM$V)%*%X)))
  iM$beta <- tcrossprod(P, X)%*%iM$V%*%Y
  gc()
  sigma2 <- iM$theta[1]
  names(sigma2) <- "sigma2"
  TT <- fillT(iM$theta[-1])
  diag(TT)[diag(TT) < 0] <- 0
  dimnames(TT) <- list(clnms1, clnms1)
  beta <- as.numeric(iM$beta)
  names(beta) <- colnames(X)
  
  return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, n1 = n1))
  
}

myMINQUE2 <- function(dt, fixed, random1 = NULL, weights = NULL, apriori = NULL,
                      iterate = FALSE) {
  #dt <- arrange(dt, IDSCHOOL)
  N <- nrow(dt)
  # Form Y and fixed effects data frame
  ff <- model.matrix(as.formula(fixed), model.frame(fixed, dt))
  Y <- as(as(model.frame(fixed, dt)[,1, drop = F], "matrix"), "Matrix")
  if (grepl("-1",fixed)&grepl(colnames(ff)[1], "(Intercept)")) {
    X <- as(as(ff[,-1, drop = F], "matrix"), "Matrix")
  } else {
    X <- as(as(ff, "matrix"), "Matrix")
  }
  
  if (!is.null(random1)) {
    id1 <- strsplit(random1, "\\|")
    random1 <- unlist(id1)[1]
    nmid1 <- unlist(id1)[2]
    rr1 <- model.frame(random1, dt)
    id1 <- model.frame(paste("~", nmid1), dt)[,1]
    if(!is.null(weights)){
      wg <- as.data.frame(dt[,c(nmid1, weights), with = F])
      wg <- foreach(ii = unique(id1)) %do% {x <- wg[id1 %in% ii, , drop = F];x[,2] <- x[,2]*nrow(x)/sum(x[,2]);return(x)}
      swg <- Reduce("+", llply(wg, function(x) unique(x[,3])))
      wg <- llply(wg, function(x) {x[,3] <- x[,3]*length(unique(id1))/swg; x$both <- x[, 2]*x[,3]; return(x)}) 
      
      
      wgt1i <- llply(wg, function(x) x[,2])
      wgt2i <- llply(wg, function(x) x[,3])
      wgt12i <- llply(wg, function(x) x[,4])
      
    } else {
      wg <- foreach(ii = unique(id1)) %do% {sum(id1 %in% ii)}
      wgt1i <- llply(wg, function(x) rep(1, x))
      wgt2i <- llply(wg, function(x) rep(1, x))
      wgt12i <- llply(wg, function(x) rep(1, x))
    }
    if (grepl("-1",random1)) {
      Z1 <- as(as( rr1, "matrix"), "Matrix")
    } else {
      Z1 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
                               dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
               "Matrix")
    }
    q <- ncol(Z1)
    colnames(Z1) <- paste(nmid1, colnames(Z1), sep = ".")
    clnms1 <- colnames(Z1)
    Z1 <- foreach(i = unique(id1)) %do% {Z1[id1 %in% i, ,drop = F]}
    Z1 <- mapply(function(z, w) {z*1/sqrt(w)}, Z1, wgt2i)
    Z <- bdiag(Z1)
    n1 <- length(unique(id1))
    l <- q*(q+1)/2
    TT1 <- formTT(q)
    QI <- llply(TT1, function(tt) llply(Z1, function(z) z%*%tt%*%t(z)))
  } else {
    Z1 <- NULL
    clnms1 <- NULL
    n1 <- NULL
    QI <- NULL
  }
  
  l <- l+1
  Qj <- c(list(llply(QI[[1]], function(zj) diag(nrow(zj)))), QI)
  Qj[[1]] <- foreach(i = 1:length(Qj[[1]])) %do% {1/wgt12i[[i]]*Qj[[1]][[i]]}
  QI <- llply(Qj, function(x) bdiag(x))
  Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i, ,drop = F]}
  X <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Xj[[ii]]), "Matrix")
  Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
  Y <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Yj[[ii]]), "Matrix")
  
  if (is.null(apriori)) apriori <- rep(1, length(QI))

  Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", mapply(function(qj, th) th*qj[[i]], Qj, apriori)))
  V <- bdiag(Vj)
  
  iM <- iMINQUE(V, X, Y, QI, wgt2i)
  ##print(iM$theta)
  gc()

  theta <- iM$theta
  #print(theta)
  iM <- iMINQUE(iM$V, X, Y, QI, wgt2i)
  
  if(iterate) {
    while(iterate){
      theta <- iM$theta
      iM <- iMINQUE(iM$V, X, Y, QI, wgt2i)
      if (all((theta - iM$theta)/theta < 0.00001)) iterate <- F
    }
  }

  P <- ginv(as.matrix((crossprod(X, iM$V)%*%X)))
  iM$beta <- tcrossprod(P, X)%*%iM$V%*%Y
  gc()
  sigma2 <- iM$theta[1]
  names(sigma2) <- "sigma2"
  TT <- fillT(iM$theta[-1])
  dimnames(TT) <- list(clnms1, clnms1)
  beta <- as.numeric(iM$beta)
  names(beta) <- colnames(X)
  
  return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, n1 = n1))
  
}

fillSMINQUE2 <- function(CQ, wgt2i) {
  x <- matrix(0, ncol = length(CQ), nrow = length(CQ))
  ns <- nrow(x)
  cc <- combinations(ns,2,1:ns, repeats.allowed = T)
  foreach(ii = 1:nrow(cc)) %do% {
    k <- cc[ii, 1]
    l <- cc[ii, 2]
    s <- sum(diag(unlist(wgt2i)*CQ[[k]]%*%CQ[[l]]))
    x[k,l] <- s
    x[l, k] <- s
    gc()
  }
  gc()
  return(x)
}

iMINQUE <- function(V, X, Y, QI, wgt2i) {
  #   Dw <- diag(c(rep(w0, nrow(Z)), rep(w1, ncol(Z)-nrow(Z))))
  #   V <- ginv(as.matrix(Z%*%Dw%*%t(Z)))
  XVX <- ginv(as.matrix((crossprod(X, V)%*%X)))
  Cjj <- V-V%*%X%*%XVX%*%crossprod(X, V)#(V*unlist(wgt2i))
  CQ <- llply(QI, function(qi) Cjj%*%qi)
  #   e <- (diag(nrow(Cjj)) - X%*%XVX%*%t(X)%*%V)%*%Y
  #   EB0 <- e%*%t(e)
  # #   B0 <- XVX%*%t(X)%*%V%*%Y
  # #   EB0 <- (Y-X%*%B0)%*%t(Y-X%*%B0)
  
  S <- fillSMINQUE2(CQ, wgt2i)
  #S <- fillSMINQUE2(V, QI, wgt2)
  #fillS(Cjj, Qj)
  #WI <- laply(QI, function(qj) sum(diag(t(V%*%qj%*%V%*%EB0))))
  WI <- laply(CQ, function(cq) sum(diag(crossprod(Y, cq)%*%Cjj%*%Y)))
  theta0 <- ginv(S)%*%matrix(WI, ncol = 1)
  tthh <- 1/theta0
  
  #icc <- theta0[2]/(sum(theta0))
  V <- ginv(as.matrix(Reduce("+", mapply(function(qi, th){qi*th}, QI, theta0))))
  return(list(theta = theta0, V = V, P = XVX))
  #   return(list(theta = theta0, V = V, beta = B0, P = XVX))
  #return(list(icc = icc, theta = theta0, V = V, P = XVX))
}

iMINQUE2 <- function(V, X, Y, QI, wgt2i, theta) {
  #   Dw <- diag(c(rep(w0, nrow(Z)), rep(w1, ncol(Z)-nrow(Z))))
  #   V <- ginv(as.matrix(Z%*%Dw%*%t(Z)))
  XVX <- ginv(as.matrix((crossprod(X, V)%*%X)))
  Cjj <- V-V%*%X%*%XVX%*%crossprod(X, V)#(V*unlist(wgt2i))
  #   e <- (diag(nrow(Cjj)) - X%*%XVX%*%t(X)%*%V)%*%Y
  #   EB0 <- e%*%t(e)
  B0 <- tcrossprod(XVX, X)%*%V%*%Y
  EB0 <- tcrossprod(Y-X%*%B0)
  CQ <- llply(QI, function(qi) {unlist(wgt2i)*Cjj%*%qi})
  
  #S <- fillSMINQUE2(V, QI, wgt2)
  #fillS(Cjj, Qj)
  WI <- laply(QI, function(qj) sum(diag(t(V%*%qj%*%V%*%EB0))))
  #WI <- laply(QI, function(qj) sum(diag(t(Y)%*%Cjj%*%qj%*%Cjj%*%Y)))
  theta0 <- mapply(function(th, cq, wi) {th/sum(diag(cq))*wi}, theta, CQ, WI)
  #icc <- theta0[2]/(sum(theta0))
  V <- ginv(as.matrix(Reduce("+", mapply(function(qi, th){qi*th/sum(theta0)}, QI, theta0))))
  return(list(theta = theta0, V = V, P = XVX))
  #   return(list(theta = theta0, V = V, beta = B0, P = XVX))
  #return(list(icc = icc, theta = theta0, V = V, P = XVX))
}


mySIMPLE <- function(dt, fixed, random1 = NULL, weights = NULL) {
  N <- nrow(dt)
  # Form Y and fixed effects data frame
  ff <- model.frame(fixed, dt)
  Y <- as(as(ff[,1, drop = F], "matrix"), "Matrix")
  if (grepl("-1",fixed)) {
    X <- as(as(ff[,-1, drop = F], "matrix"), "Matrix")
  } else {
    X <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(ff),
                            dimnames = list(NULL, "(Intercept)")), 
                     ff[,-1, drop = F]), "matrix"), "Matrix")
  }
  
  #   if(!is.null(weights)){
  #     wgt1 <- 1/sqrt(dt[,weights[1]])
  #     wgt2 <- 1/sqrt(dt[,weights[2]])
  #     wgt1 <- wgt2*wgt1
  #   } else {
  #     wgt1 <- rep(1, N)
  #     wgt2 <- rep(1, N)
  #   }
  
  if (!is.null(random1)) {
    id1 <- strsplit(random1, "\\|")
    random1 <- unlist(id1)[1]
    nmid1 <- unlist(id1)[2]
    rr1 <- model.frame(random1, dt)
    id1 <- model.frame(paste("~", nmid1), dt)[,1]
    if(!is.null(weights)){
      wg <- dt[,c(nmid1, weights)]
      wg <- ddply(wg, as.formula(paste("~", nmid1)), function(x) {x[,2] <- x[,2]*nrow(x)/sum(x[,2]);return(x)})
      #wg2 <- ddply(wg, as.formula(paste("~", nmid1)), function(x) unique(x[,3]))
      #wg[,3] <- wg[,3]*nrow(wg2)/sum(wg2[,2])
      
      wgt1 <- 1/sqrt(wg[,2])
      wgt2 <- 1/sqrt(wg[,3])
      wgt1 <- wgt2*wgt1
    } else {
      wgt1 <- rep(1, N)
      wgt2 <- rep(1, N)
    }
    if (grepl("-1",random1)) {
      Z1 <- as(wgt2*as( rr1, "matrix"), "Matrix")
    } else {
      Z1 <- as(wgt2*as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
                                    dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
               "Matrix")
    }
    q <- ncol(Z1)
    colnames(Z1) <- paste(nmid1, colnames(Z1), sep = ".")
    clnms1 <- colnames(Z1)
    Z1 <- foreach(i = unique(id1)) %do% {Z1[id1 %in% i, ,drop = F]}
    n1 <- length(unique(id1))
    l <- q*(q+1)/2
    TT1 <- formTT(q)
    QI <- llply(TT1, function(tt) llply(Z1, function(z) z%*%tt%*%t(z)))
  } else {
    Z1 <- NULL
    clnms1 <- NULL
    n1 <- NULL
    QI <- NULL
  }
  
  wgt1i <- foreach(i = unique(id1)) %do% {wgt1[id1 %in% i]}
  wgt2i <- foreach(i = unique(id1)) %do% {wgt2[id1 %in% i]}
  
  l <- l+1
  Qj <- c(list(llply(QI[[1]], function(zj) diag(nrow(zj)))), QI)
  Qj[[1]] <- foreach(i = 1:length(Qj[[1]])) %do% {wgt1i[[i]]^2*Qj[[1]][[i]]}
  Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i,,drop = F]}
  Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
  Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", llply(Qj, function(qj) qj[[i]])))
  XVX <- solve(crossprod(X))
  Cjj <- diag(N)-X%*%tcrossprod(XVX, X)
  Cjj <- foreach(i = unique(id1)) %do% {Cjj[id1 %in% i, id1 %in% i]}
  #Cjj <- mapply(function(v, x) diag(nrow(x))-x%*%solve(t(x)%*%x)%*%t(x), Vj, Xj)
  B0 <- solve(crossprod(X))%*%crossprod(X, Y)
  EB0 <- mapply(function(x,y) trossprod(y-x%*%B0), Xj, Yj)
  
  S <- fillSSIMPLE(Cjj, Qj, wgt2i)
  #fillS(Cjj, Qj)
  WI <- laply(Qj, function(qj) Reduce("+", foreach(j = 1:length(Vj)) %do% sum(diag(t(Vj[[j]]%*%qj[[j]]%*%Vj[[j]]%*%EB0[[j]])))))
  
  theta0 <- ginv(S)%*%matrix(WI, ncol = 1)
  Vj0 <- foreach(i = 1:length(unique(id1))) %do% 
    solve(Reduce("+", foreach(k = 1:length(theta0)) %do% {theta0[k]*Qj[[k]][[i]]}))
  #theta0 <- c(3000, 1200)
  iM <- iterSIMPLE(Vj0, Xj, Qj, theta0, Yj, id1, wgt2i, X)
  
  gc()
  doIM <- (round(sum(abs(iM$theta - theta0)), 4) != 0)
  while(doIM){
    theta <- iM$theta
    #print(theta)
    iM <- iterSIMPLE(iM$Vj, Xj, Qj, theta, Yj, id1, wgt2i, X)
    doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
  }
  
  gc()
  sigma2 <- iM$theta[1]
  names(sigma2) <- "sigma2"
  TT <- fillT(iM$theta[-1])
  dimnames(TT) <- list(clnms1, clnms1)
  beta <- as.numeric(iM$beta)
  names(beta) <- colnames(X)
  
  # Variances:
  ccj <- mapply(function(x,y, v) crossprod(x, v)%*%tcrossprod(y-x%*%iM$beta)%*%v%*%x, Xj, Yj, iM$Vj)
  Bcov <- (n1/(n1-1))*iM$P%*%Reduce("+", ccj)%*%iM$P
  varcov <- mapply(function(z, x, v, wgt) TT - TT%*%crossprod(z*unique(wgt), v)%*%(solve(v)-x%*%tcrossprod(Bcov, x))%*%v%*%(z*unique(wgt))%*%TT, Z1, Xj, iM$Vj, wgt2i)
  #Tcov <- (n1/(n1-1))*solve(iM$S)
  ranef <- mapply(function(x, y, v, z, wg) (TT%*%crossprod(z*unique(wgt), v)%*%(y - x%*%iM$beta)),Xj, Yj, iM$Vj, Z1, wgt2i)
  
  fit <- mapply(function(x, z, u, wgt) x%*%iM$beta+(z/unique(wgt))%*%u, Xj, Z1, ranef, wgt2i)
  fit <- do.call(rBind, fit)
  ranef <- llply(ranef, t)
  ranef <- do.call(rBind, ranef)
  rownames(ranef) <- unique(id1)
  residuals <- Y-fit
  gc()
  vv <- do.call(cbind, llply(varcov, as.matrix))
  dim(vv) <- c(dim(varcov[[1]]), length(varcov))
  ranef <- as.data.frame(as.matrix(ranef))
  attr(ranef, "postVar") <- vv
  
  return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, n1 = n1, ranef = list(ranef), cov = Bcov, fitted = fit, resid = residuals))
}

fillT <- function(tau) {
  ns <- round(sqrt(length(tau)*2))
  kk <- combinations(ns,2,1:ns, repeats.allowed = T)
  x <- matrix(0, ncol = ns, nrow = ns)
  for(ii in 1:nrow(kk)) {
    i <- kk[ii, 1]
    j <- kk[ii, 2]
    x[i,j] <- tau[ii]
    x[j, i] <- tau[ii]
  }
  return(x)
}

iterSIMPLE <- function(Vj, Xj, Qj, theta, Yj, id1, wgt2i, X){
  #1
  P <- solve(Reduce("+", mapply(function(x,v) crossprod(x, v)%*%x, Xj, Vj)))
  B <- P%*%Reduce("+", mapply(function(x,v, y) crossprod(x, v)%*%y, Xj, Vj, Yj))
  EB <- mapply(function(x,y) tcrossprod(y-x%*%B), Xj, Yj)
  XVX <- solve(crossprod(X, bdiag(Vj))%*%X)
  Cjj <- bdiag(Vj)-bdiag(Vj)%*%X%*%tcrossprod(XVX, X)%*%(bdiag(Vj)*unlist(wgt2i)^2)
  Cjj <- foreach(i = unique(id1)) %do% {Cjj[id1 %in% i, id1 %in% i]}
  #Cjj <- mapply(function(v, x) v-v%*%x%*%solve(t(x)%*%v%*%x)%*%t(x)%*%v, Vj, Xj)
  S <- fillSSIMPLE(Cjj, Qj, wgt2i)
  #fillS(Cjj, Qj)
  WI <- laply(Qj, function(qj) Reduce("+", foreach(j = 1:length(Vj)) %do% sum(diag(t(Vj[[j]]%*%qj[[j]]%*%Vj[[j]]%*%EB[[j]])))))
  
  theta <- ginv(S)%*%matrix(WI, ncol = 1)
  Vj <- foreach(i = 1:length(unique(id1))) %do% 
    solve(Reduce("+", foreach(k = 1:length(theta)) %do% {theta[k]*Qj[[k]][[i]]}))
  #theta0 <- c(3000, 1200)
  gc()
  return(list(Vj = Vj, theta = theta, beta = B, S = S, WI = WI, P = P))
}

fillSSIMPLE <- function(Vj, Qj, wgt2i) {
  x <- matrix(0, ncol = length(Qj), nrow = length(Qj))
  ns <- nrow(x)
  cc <- combinations(ns,2,1:ns, repeats.allowed = T)
  foreach(ii = 1:nrow(cc)) %do% {
    k <- cc[ii, 1]
    l <- cc[ii, 2]
    s <-  Reduce("+", foreach(j = 1:length(Vj)) %do% {1/(unique(wgt2i[[j]]))^2*sum(diag(Vj[[j]]%*%Qj[[k]][[j]]%*%Vj[[j]]%*%Qj[[l]][[j]]))})
    x[k,l] <- s
    x[l, k] <- s
    gc()
  }
  gc()
  return(x)
}

require(gtools)
myIGLS <- function(dt, fixed, random1 = NULL, weights = NULL) {
  #dt <- arrange(dt, IDSCHOOL)
  N <- nrow(dt)
  # Form Y and fixed effects data frame
  ff <- model.matrix(as.formula(fixed), model.frame(fixed, dt))
  Y <- as(as(model.frame(fixed, dt)[,1, drop = F], "matrix"), "Matrix")
  if (grepl("-1",fixed)) {
    X <- as(as(ff[,-1, drop = F], "matrix"), "Matrix")
  } else {
    X <- as(as(ff, "matrix"), "Matrix")
  }
  
  #   if(!is.null(weights)){
  #     wgt1 <- 1/sqrt(dt[,weights[1]])
  #     wgt2 <- 1/sqrt(dt[,weights[2]])
  #     wgt1 <- wgt2*wgt1
  #   } else {
  #     wgt1 <- rep(1, N)
  #     wgt2 <- rep(1, N)
  #   }
  #   
  if (!is.null(random1)) {
    id1 <- strsplit(random1, "\\|")
    random1 <- unlist(id1)[1]
    nmid1 <- unlist(id1)[2]
    rr1 <- model.frame(random1, dt)
    id1 <- model.frame(paste("~", nmid1), dt)[,1]
    if(!is.null(weights)){
      wg <- as.data.frame(dt[,c(nmid1, weights), with = F])
      wg <- foreach(ii = unique(id1)) %do% {x <- wg[id1 %in% ii, , drop = F];x[,2] <- x[,2]*nrow(x)/sum(x[,2]);return(x)}
      
      #wg <- foreach(ii = unique(id1), .combine = rbind) %do% {x <- wg[id1 %in% ii, , drop = F];x[,2] <- x[,2]*nrow(x)/sum(x[,2]);return(x)}
      #ddply(wg, as.formula(paste("~", nmid1)), function(x) {x[,2] <- x[,2]*nrow(x)/sum(x[,2]);return(x)})
      #wg2 <- ddply(wg, as.formula(paste("~", nmid1)), function(x) unique(x[,3]))
      swg <- Reduce("+", llply(wg, function(x) unique(x[,3])))
      wg <- llply(wg, function(x) {x[,3] <- x[,3]*length(unique(id1))/swg; x$both <- x[, 2]*x[,3]; return(x)})
      #wg[,3] <- wg[,3]*nrow(wg2)/sum(wg2[,2])  
      
      
      wgt1i <- llply(wg, function(x) 1/sqrt(x[,4]))
      wgt2i <- llply(wg, function(x) 1/sqrt(x[,3]))
      
    } else {
      wg <- foreach(ii = unique(id1)) %do% {sum(id1 %in% ii)}
      wgt1i <- llply(wg, function(x) rep(1, x))
      wgt2i <- llply(wg, function(x) rep(1, x))
    }
    if (grepl("-1",random1)) {
      Z1 <- as(as( rr1, "matrix"), "Matrix")
    } else {
      Z1 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
                               dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
               "Matrix")
    }
    q <- ncol(Z1)
    colnames(Z1) <- paste(nmid1, colnames(Z1), sep = ".")
    clnms1 <- colnames(Z1)
    Z1 <- foreach(i = unique(id1)) %do% {Z1[id1 %in% i, ,drop = F]}
    Z1 <- mapply(function(z, w) {z*w}, Z1, wgt2i)
    n1 <- length(unique(id1))
    l <- q*(q+1)/2
    TT1 <- formTT(q)
    QI <- llply(TT1, function(tt) llply(Z1, function(z) z%*%tt%*%t(z)))
  } else {
    Z1 <- NULL
    clnms1 <- NULL
    n1 <- NULL
    QI <- NULL
  }
  
  #   wgt1i <- foreach(i = unique(id1)) %do% {wgt1[id1 %in% i]}
  #   wgt2i <- foreach(i = unique(id1)) %do% {wgt2[id1 %in% i]}
  
  l <- l+1
  Qj <- c(list(llply(QI[[1]], function(zj) diag(nrow(zj)))), QI)
  Qj[[1]] <- foreach(i = 1:length(Qj[[1]])) %do% {wgt1i[[i]]^2*Qj[[1]][[i]]}
  Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i, ,drop = F]}
  X <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Xj[[ii]]), "Matrix")
  Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
  Y <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Yj[[ii]]), "Matrix")
  Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", llply(Qj, function(qj) qj[[i]])))
  V <- bdiag(Vj)
  B0 <- ginv(as.matrix(crossprod(X)))%*%crossprod(X, Y)
  EB0 <- mapply(function(x,y) tcrossprod(y-x%*%B0), Xj, Yj)
  
  S <- fillSGLS(Vj, Qj, wgt2i)
  #fillS(Cjj, Qj)
  WI <- laply(Qj, function(qj) Reduce("+", foreach(j = 1:length(Vj)) %do% sum(diag(t(Vj[[j]]%*%qj[[j]]%*%Vj[[j]]%*%EB0[[j]])))))
  
  theta0 <- ginv(S)%*%matrix(WI, ncol = 1)
  Vj0 <- foreach(i = 1:length(unique(id1))) %do% 
    ginv(as.matrix(Reduce("+", foreach(k = 1:length(theta0)) %do% {theta0[k]*Qj[[k]][[i]]})))
  #theta0 <- c(3000, 1200)
  iM <- iterGLS(Vj0, Xj, Qj, theta0, Yj, id1, wgt2i)
  
  gc()
  doIM <- (round(sum(abs(iM$theta - theta0)), 1) != 0)
  while(doIM){
    theta <- iM$theta
    #print(theta)
    iM <- iterGLS(iM$Vj, Xj, Qj, theta, Yj, id1, wgt2i)
    doIM <- (round(sum(abs(iM$theta - theta)), 1) != 0)
  }
  
  gc()
  sigma2 <- iM$theta[1]
  names(sigma2) <- "sigma2"
  TT <- fillT(iM$theta[-1])
  dimnames(TT) <- list(clnms1, clnms1)
  beta <- as.numeric(iM$beta)
  names(beta) <- colnames(X)
  
  # Variances:
  #   ccj <- mapply(function(x,y, v) t(x)%*%v%*%(y-x%*%iM$beta)%*%t(y-x%*%iM$beta)%*%v%*%x, Xj, Yj, iM$Vj)
  #   Bcov <- (n1/(n1-1))*iM$P%*%Reduce("+", ccj)%*%iM$P
  Vst <- mapply(function(z, w) {sigma2*diag(nrow(z))+(1/unique(w)^2*z)%*%tcrossprod(TT, z)}, Z1, wgt2i)
  Bcov <- iM$P%*%(Reduce("+", mapply(function(x, v, vst){crossprod(x, v)%*%vst%*%v%*%x}, Xj, iM$Vj, Vst)))%*%iM$P
  Cj <- mapply(function(vst, x, v) {vst-x%*%tcrossprod(Bcov, x)-2*vst%*%v%*%x%*%tcrossprod(iM$P, x)}, Vst, Xj, iM$Vj)
  PP <- mapply(function(w, z, v) {(unique(w)*trossprod(TT, z))%*%v}, wgt2i, Z1, iM$Vj)
  varcov <- mapply(function(pp, cj) {pp%*%cj%*%t(pp)}, PP, Cj)
  #varcov <- mapply(function(z, x, v, wgt) TT - TT%*%(t(z)*unique(wgt))%*%v%*%(solve(v)-x%*%Bcov%*%t(x))%*%v%*%(z*unique(wgt))%*%TT, Z1, Xj, iM$Vj, wgt2i)
  #varcov <- mapply(function(z, x, v) TT - TT%*%t(z)%*%v%*%z%*%TT, Z1, Xj, iM$Vj)
  
  #Tcov <- (n1/(n1-1))*solve(iM$S)
  ranef <- mapply(function(x, y, v, z, wgt) (TT%*%crossprod(z*unique(wgt), v)%*%(y - x%*%iM$beta)),Xj, Yj, iM$Vj, Z1, wgt2i)
  
  fit <- mapply(function(x, z, u, wgt) x%*%iM$beta+(z/unique(wgt))%*%u, Xj, Z1, ranef, wgt2i)
  fit <- do.call(rBind, fit)
  ranef <- llply(ranef, t)
  ranef <- do.call(rBind, ranef)
  rownames(ranef) <- unique(id1)
  residuals <- Y-fit
  gc()
  vv <- do.call(cbind, llply(varcov, as.matrix))
  dim(vv) <- c(dim(varcov[[1]]), length(varcov))
  ranef <- as.data.frame(as.matrix(ranef))
  attr(ranef, "postVar") <- vv
  
  return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, n1 = n1, ranef = list(ranef), cov = Bcov, fitted = fit, resid = residuals))
}

iterGLS <- function(Vj, Xj, Qj, theta, Yj, id1, wgt2i){
  #1
  P <- ginv(as.matrix(Reduce("+", mapply(function(x,v) crossprod(x, v)%*%x, Xj, Vj))))
  B <- P%*%Reduce("+", mapply(function(x,v, y) crossprod(x, v)%*%y, Xj, Vj, Yj))
  EB <- mapply(function(x,y) tcrossprod(y-x%*%B), Xj, Yj)
  
  S <- fillSGLS(Vj, Qj, wgt2i)
  #fillS(Cjj, Qj)
  WI <- laply(Qj, function(qj) Reduce("+", foreach(j = 1:length(Vj)) %do% sum(diag(t(Vj[[j]]%*%qj[[j]]%*%Vj[[j]]%*%EB[[j]])))))
  
  theta <- ginv(S)%*%matrix(WI, ncol = 1)
  Vj <- foreach(i = 1:length(unique(id1))) %do% 
    solve(as.matrix(Reduce("+", foreach(k = 1:length(theta)) %do% {theta[k]*Qj[[k]][[i]]})))
  #theta0 <- c(3000, 1200)
  gc()
  return(list(Vj = Vj, theta = theta, beta = B, S = S, WI = WI, P = P))
}

fillSGLS <- function(Vj, Qj, wgt2i) {
  x <- matrix(0, ncol = length(Qj), nrow = length(Qj))
  ns <- nrow(x)
  cc <- combinations(ns,2,1:ns, repeats.allowed = T)
  foreach(ii = 1:nrow(cc)) %do% {
    k <- cc[ii, 1]
    l <- cc[ii, 2]
    s <-  Reduce("+", foreach(j = 1:length(Vj)) %do% {1/(unique(wgt2i[[j]]))^2*sum(diag(Vj[[j]]%*%Qj[[k]][[j]]%*%Vj[[j]]%*%Qj[[l]][[j]]))})
    x[k,l] <- s
    x[l, k] <- s
    gc()
  }
  gc()
  return(x)
}

formTT <- function(k) {
  kk <- combinations(k,2,1:k, repeats.allowed = T)
  ns <- round(sqrt(nrow(kk)*2))
  TT <- foreach(ii = 1:nrow(kk)) %do% {
    x <- matrix(0, ncol = ns, nrow = ns)
    i <- kk[ii, 1]
    j <- kk[ii, 2]
    x[i,j] <- 1
    x[j, i] <- 1
    return(x)
  }
  return(TT)
}

bootMINQUE <- function(data = data.2011, FUN  = bfunSIMPLE, R = 100,
                       fixed = "BSMMAT01 ~ 1+SSEX", random = "~1+SSEX|IDSCHOOL",
                       strata = rep(1, nrow(data)), hierar = F, wgt = NULL) {
  #print("Iteration:")
  boutSIMPLE <- boot(data, FUN, R = R, 
                     formul = fixed, 
                     parallel = "multicore", random = random,
                     strata = strata, attrib = T, hierar = hierar, wgt = wgt)
  
  #collect results
  attrib  <- boutSIMPLE$attr
  dims <- attrib$dims
  nms <- attrib$nms
  bb <- boutSIMPLE[[1]]
  tt <- bb$t
  tt0 <- bb$t0
  # collect beta
  beta  <- tt[,1:dims$coef, drop = F]
  colnames(beta) <- nms$coef
  Bcov <- cov(beta)
  Bint <- t(t(beta)-(tt0[1:dims$coef, drop = F]))
  Bint <- apply(Bint, 2, function(x) quantile(abs(x), .975))
  Bint <- rbind((t(tt0[1:dims$coef, drop = F])-Bint), (t(tt0[1:dims$coef, drop = F])+Bint))
  rownames(Bint) <- c("2.5%", "97.5%")
  beta <- apply(beta, 2, mean)
  Bbias <- beta - tt0[1:dims$coef, drop = F]
  beta <- tt0[1:dims$coef, drop = F]-Bbias
  beta <- cbind(Coefficient = beta, St.err = sqrt(diag(Bcov)), Bias = Bbias, t(Bint))
  attr(beta, "cov") <- Bcov
  
  tt <- tt[,-1:-dims$coef, drop = F]
  tt0 <- tt0[-1:-dims$coef, drop = F]
  #   # collect runef
  #   rdim <- dims$ranef
  #   ranef <- tt[,1:(rdim[1]*rdim[2]), drop = F]
  #   ranef0 <- tt0[1:(rdim[1]*rdim[2]), drop = F]
  #   
  #   ranef <- foreach(i = 1:rdim[1]) %do% {
  #     wh <- (1:rdim[2]*rdim[1]- rdim[1])+i
  #     rr <- ranef[, wh, drop = F]
  #     colnames(rr) <- nms$ranef[[2]]
  #     rr0 <- ranef0[wh, drop = F]
  #     Rcov <- cov(rr)
  #     #Rint <- apply(rr, 2, function(x) quantile(x, c(.025, .975)) )
  #     Rint <- t(t(rr)-(rr0))
  #     Rint <- apply(Rint, 2, function(x) quantile(x, .975))
  #     Rint <- rbind((t(rr0)-Rint), (t(rr0)+Rint))
  #     rownames(Rint) <- c("2.5%", "97.5%")
  #     
  #     rr <- apply(rr, 2, mean)
  #     Rbias <- rr - rr0
  #     rr <- rr0 - Rbias
  #     rr <- cbind(Coefficient = rr, St.err = sqrt(diag(Rcov)), Bias = Rbias, t(Rint))
  #     attr(rr, "cov") <- Rcov
  #     return(rr)
  #   }
  #   names(ranef) <- nms$ranef[[1]]
  #   
  #   tt <- tt[,-1:-(rdim[1]*rdim[2]), drop = F]
  #   tt0 <- tt0[-1:-(rdim[1]*rdim[2]), drop = F]
  
  # collect sigma
  sigma  <- tt[,1:dims$sigma, drop = F]
  colnames(sigma) <- nms$sigma
  Scov <- cov(sigma)
  #Sint <- apply(sigma, 2, function(x) quantile(x, c(.025, .975)) )
  Sint <- t(t(sigma)-(tt0[1:dims$sigma, drop = F]))
  Sint <- apply(Sint, 2, function(x) quantile(x, .975))
  Sint <- rbind((t(tt0[1:dims$sigma, drop = F])-Sint), (t(tt0[1:dims$sigma, drop = F])+Sint))
  rownames(Sint) <- c("2.5%", "97.5%")
  
  sigma <- apply(sigma, 2, mean)
  Sbias <- sigma - tt0[1:dims$sigma, drop = F]
  sigma <- tt0[1:dims$sigma, drop = F] + Sbias
  sigma <- cbind(Coefficient = sigma, St.err = sqrt(diag(Scov)), Bias = Sbias, t(Sint))
  attr(sigma, "cov") <- Scov
  
  tt <- tt[,-1:-dims$sigma, drop = F]
  tt0 <- tt0[-1:-dims$sigma, drop = F]
  
  #collect TT
  TTdim <- dims$TT[1]
  TT  <- tt
  perm <- apply(permutations(TTdim,2,1:TTdim, repeats.allowed = T), 1, paste, collapse = "")
  comb <- apply(combinations(TTdim,2,1:TTdim, repeats.allowed = T), 1, paste, collapse = "")
  wh <- perm %in% comb
  TT <- TT[,wh, drop = F]
  TTcov <- cov(TT)
  #TTint <- apply(TT, 2, function(x) quantile(x, c(.025, .975)) )
  TTint <- t(t(TT)-(tt0[wh, drop = F]))
  TTint <- apply(TTint, 2, function(x) quantile(x, .975))
  TTint <- rbind((t(tt0[wh, drop = F])-TTint), (t(tt0[wh, drop = F])+TTint))
  rownames(TTint) <- c("2.5%", "97.5%")
  
  TT <- apply(TT, 2, mean)
  TTbias <- TT - tt0[wh, drop = F]
  TT <- tt0[wh, drop = F] - TTbias
  CTT <- cbind(Coefficient = TT, St.err = sqrt(diag(TTcov)), Bias = TTbias, t(TTint))
  rownames(CTT) <- as.vector(outer(nms$TT[[1]], nms$TT[[1]], function(x, y) paste(x, y, sep = '.')))[wh]
  TT <- fillT(TT)
  attr(TT, "char") <- CTT
  
  return(list(beta = beta, sigma2 = sigma, TT = TT))
  #return(list(beta = beta, ranef = ranef, sigma2 = sigma, TT = TT))
}

bfunMINQUE <- function(dat, orig, formul, random, attrib = FALSE, wgt = NULL) {
  #print("Iteration:")
  ivM <- myMINQUE2(dt = dat[orig, ],
                   fixed = formul,
                   random1 = random, weights = wgt)
  #   cc <- c(ivM$beta, unlist(ivM$ranef[[1]]), ivM$sigma2, unlist(ivM$TT))
  #   if (attrib) {
  #     attr(cc, "dims") <- list(coef = length(ivM$beta), ranef = dim(ivM$ranef[[1]]), sigma = 1, TT = dim(ivM$TT))
  #     attr(cc, "nms") <- list(coef = names(ivM$beta), ranef = dimnames(ivM$ranef[[1]]), sigma = "sigma2", TT = dimnames(ivM$TT))
  #   }
  cc <- c(ivM$beta, ivM$sigma2, unlist(ivM$TT))
  if (attrib) {
    attr(cc, "dims") <- list(coef = length(ivM$beta),  sigma = 1, TT = c(1,1))
    attr(cc, "nms") <- list(coef = names(ivM$beta), sigma = "sigma2", TT = dimnames(ivM$TT))
  }
  return(cc)
}

bfunIGLS <- function(dat, orig, formul, random, attrib = FALSE, wgt = NULL) {
  #print("Iteration:")
  ivM <- myIGLS(dt = dat[orig, ],
                fixed = formul,
                random1 = random, weights = wgt)
  #   cc <- c(ivM$beta, unlist(ivM$ranef[[1]]), ivM$sigma2, unlist(ivM$TT))
  #   if (attrib) {
  #     attr(cc, "dims") <- list(coef = length(ivM$beta), ranef = dim(ivM$ranef[[1]]), sigma = 1, TT = dim(ivM$TT))
  #     attr(cc, "nms") <- list(coef = names(ivM$beta), ranef = dimnames(ivM$ranef[[1]]), sigma = "sigma2", TT = dimnames(ivM$TT))
  #   }
  cc <- c(ivM$beta, ivM$sigma2, unlist(ivM$TT))
  if (attrib) {
    attr(cc, "dims") <- list(coef = length(ivM$beta),  sigma = 1, TT = c(1,1))
    attr(cc, "nms") <- list(coef = names(ivM$beta), sigma = "sigma2", TT = dimnames(ivM$TT))
  }
  return(cc)
}

f <- function(){
  pb <- txtProgressBar(min=1, max=n-1,style=3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb,count)
    Sys.sleep(0.01)
    flush.console()
    c(...)
  }
}

MyCombineREML <- function (results, variances, call = sys.call(), df.complete = Inf, 
          ...) 
{
  m <- length(results)
  oldcall <- attr(results, "call")
  if (missing(variances)) {
    variances <- suppressWarnings(lapply(results, vcov))
    results <- lapply(results, coef)
  }
  vbar <- variances[[1]]
  cbar <- results[[1]]
  for (i in 2:m) {
    cbar <- cbar + results[[i]]
    vbar <- vbar + variances[[i]]
  }
  cbar <- cbar/m
  vbar <- vbar/m
  evar <- var(do.call("rbind", results))
  r <- (1 + 1/m) * evar/vbar
  df <- (m - 1) * (1 + 1/r)^2
  if (is.matrix(df)) 
    df <- diag(df)
  if (is.finite(df.complete)) {
    dfobs <- ((df.complete + 1)/(df.complete + 3)) * df.complete * 
      vbar/(vbar + evar)
    if (is.matrix(dfobs)) 
      dfobs <- diag(dfobs)
    df <- 1/(1/dfobs + 1/df)
  }
  if (is.matrix(r)) 
    r <- diag(r)
  rval <- list(coefficients = cbar, variance = vbar + evar * 
                 (m + 1)/m, call = c(oldcall, call), nimp = m, df = df, 
               missinfo = (r + 2/(df + 3))/(r + 1))
  class(rval) <- "MIresult"
  rval
}

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}



simPop <- function(M = 100, formul="Y2 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL)", 
                   popF = makePOP4){
  pop1 <- popF(M = M)
  
  mm <- lmer(as.formula(formul), data = pop1)
  if(length(mm@optinfo$conv$lme4) != 0){
    return(simPop(M, formul, popF))
  } else {
    sm <- summary(mm)
    
    f.f <- strsplit(formul, "\\+\\(")
    r.f <- paste("~", gsub("\\)", "", f.f[[1]][2]))
    f.f <- f.f[[1]][1]
    min1 <- myMINQUE2(dt = pop1,
                      fixed = f.f,
                      random1 = r.f,
                      weights = NULL)
    gc()
  }
  return(c(fixef(mm), sm$sigma^2, unlist(sm$varcor[[1]]), min1$beta,
           min1$sigma2, unlist(min1$TT)))
  
}

simPopD <- function(M = 30, formul="Y1 ~ 1+W+X1+X1*W+(1+X1|IDSCHOOL)", 
                   popF = makePOP4){
  pop1 <- popF(M = M)
  
  mm <- lmer(as.formula(formul), data = pop1)
  if(length(mm@optinfo$conv$lme4) != 0){
    return(simPop(M, formul, popF))
  } else {
    sm <- summary(mm)
    
    f.f <- strsplit(formul, "\\+\\(")
    r.f <- paste("~", gsub("\\)", "", f.f[[1]][2]))
    f.f <- f.f[[1]][1]
    min1 <- myMINQUE(dt = pop1,
                      fixed = f.f,
                      random1 = r.f,
                      weights = NULL,
                      apriori = c(1, 0, 0, 0))
    min2 <- myMINQUE(dt = pop1,
                     fixed = f.f,
                     random1 = r.f,
                     weights = NULL,
                     apriori = c(1, 1, 1, 1))
    iv <- summary(lm(f.f, pop1))
    min3 <- myMINQUE(dt = pop1,
                     fixed = f.f,
                     random1 = r.f,
                     weights = NULL,
                     apriori = c(iv$sigma, 0, 0, 0))
    gc()
  }
  return(c(fixef(mm), min1$beta, min2$beta, min3$beta, 
           sm$sigma^2, min1$sigma2, min2$sigma2, min3$sigma2,
           unlist(sm$varcor[[1]]),  unlist(min1$TT),  unlist(min2$TT),  unlist(min3$TT)))
  
}

simPopB <- function(M = 100, formul="Y1 ~ 1+X1+X3+X4+(1|IDSCHOOL)", 
                    popF = makePOP7){
  pop1 <- popF(M = M)
  
  mm <- lmer(as.formula(formul), data = pop1)
  if(length(mm@optinfo$conv$lme4) != 0){
    return(simPop(M, formul, popF))
  } else {
    sm <- summary(mm)
    
    f.f <- strsplit(formul, "\\+\\(")
    r.f <- paste("~", gsub("\\)", "", f.f[[1]][2]))
    f.f <- f.f[[1]][1]
    min1 <- myMINQUE(dt = pop1,
                     fixed = f.f,
                     random1 = r.f,
                     weights = NULL,
                     apriori = c(1, 0))
    min2 <- myMINQUE(dt = pop1,
                     fixed = f.f,
                     random1 = r.f,
                     weights = NULL,
                     apriori = c(1, 1))
    iv <- summary(lm(f.f, pop1))
    min3 <- myMINQUE(dt = pop1,
                     fixed = f.f,
                     random1 = r.f,
                     weights = NULL,
                     apriori = c(iv$sigma, 0))
    gc()
  }
  return(c(fixef(mm), min1$beta, min2$beta, min3$beta, 
           sm$sigma^2, min1$sigma2, min2$sigma2, min3$sigma2,
           unlist(sm$varcor[[1]]),  unlist(min1$TT),  unlist(min2$TT),  unlist(min3$TT)))
  
}

simPopMy <- function(M = 100, m = 35, formul="Y1 ~ 1+X1+X3+X4+(1|IDSCHOOL)", 
                    popF = makePOPW, sigma2 = 2000, tau00 = 100, tau01 = 50,
                    tau11 = 100){
  pop1 <- popF(M = M, sigma2 = sigma2, tau00 = tau00, 
               tau01 = tau01, tau11 = tau11)
  smpl <- samplePOP(pop1, m)
  smpl$w1 <- smpl$wstd*smpl$wcl
  smpl$w2 <- smpl$wsch
  
  mm <- lmer(as.formula(formul), data = smpl)
  if(length(mm@optinfo$conv$lme4) != 0){
    return(simPopMy(M, formul, popF, sigma2 = sigma2, tau00 = tau00, 
                  tau01 = tau01, tau11 = tau11, m = m))
  } else {
    sm <- summary(mm)
    
    f.f <- strsplit(formul, "\\+\\(")
    r.f <- paste("~", gsub("\\)", "", f.f[[1]][2]))
    f.f <- f.f[[1]][1]
    min1 <- try(myMINQUE(dt = smpl,
                     fixed = f.f,
                     random1 = r.f,
                     weights = NULL,
                     apriori = c(1, 0, 0, 0)))
    if(class(min1)[1] == "try-error") min1 <- NULL
    min2 <- try(myMINQUE(dt = smpl,
                     fixed = f.f,
                     random1 = r.f,
                     weights = c("w1", "w2"),
                     apriori = c(1, 0, 0, 0)))
    if(class(min2)[1] == "try-error") min2 <- NULL
    min3 <- try(myMINQUE(dt = smpl,
                     fixed = f.f,
                     random1 = r.f,
                     weights = NULL,
                     apriori = c(1, 1, 1, 1)))
    if(class(min3)[1] == "try-error") min3 <- NULL
    min4 <- try(myMINQUE(dt = smpl,
                     fixed = f.f,
                     random1 = r.f,
                     weights = c("w1", "w2"),
                     apriori = c(1, 1, 1, 1)))
    if(class(min4)[1] == "try-error") min4 <- NULL
    iv <- summary(lm(f.f, smpl))$sigma^2
    min5 <- try(myMINQUE(dt = smpl,
                     fixed = f.f,
                     random1 = r.f,
                     weights = NULL,
                     apriori = c(iv, 1, 1, 1)))
    if(class(min5)[1] == "try-error") min5 <- NULL
    min6 <- try(myMINQUE(dt = smpl,
                     fixed = f.f,
                     random1 = r.f,
                     weights = c("w1", "w2"),
                     apriori = c(iv, 1, 1, 1)))
    if(class(min6)[1] == "try-error") min6 <- NULL
    gc()
  }
  return(c(fixef(mm), min1$beta, min2$beta, min3$beta, min4$beta,
           min5$beta, min6$beta,
           sm$sigma^2, min1$sigma2, min2$sigma2, min3$sigma2,
           min4$sigma2, min5$sigma2, min6$sigma2,
           unlist(sm$varcor[[1]]),  unlist(min1$TT),  unlist(min2$TT),
           unlist(min3$TT), unlist(min4$TT), unlist(min5$TT), unlist(min6$TT)))
  
}

makePOP5 <- function(M = 30){
  require(data.table)
  require(MASS)
  struct <- 1:M
  struct <- data.table(IDSCHOOL = struct)
  
  # Make second level errors
  ttN <- matrix(c(0.005, 0.0025, 0.0025, 0.005), nrow = 2)
  ddN <- mvrnorm(n = M, rep(0, 2), ttN)
  
  ttX <- matrix(c(1, sqrt(0.5), sqrt(0.5), 1), nrow = 2)
  ddX <- mvrnorm(n = M, rep(0, 2), ttX)
  
  struct[, d0N := ddN[, 1]]#rnorm(M, 0, sqrt(0.005))]
  struct[, d0X := ((ddX[, 1]^2 - 1)/sqrt(2))*sqrt(0.005)]
  
  struct[, d1N := ddN[, 2]]#rnorm(M, 0, sqrt(0.005))]
  struct[, d1X := ((ddX[, 2]^2 - 1)/sqrt(2))*sqrt(0.005)] 
  
  uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
  Nj <- round(50*exp(uj))
  
  struct[, Nj := Nj]
  
  struct[, psch := Nj/sum(Nj)]
  
  #
  eN <- struct[, matrix(rnorm(Nj,0, sqrt(0.5)), ncol = 1), by = IDSCHOOL]
  setnames(eN, "V1", "eN")
  eN$eN <- rnorm(nrow(eN),0, sqrt(0.5))
  eN$eX <- ((rchisq(sum(struct$Nj), 1) - 1)/sqrt(2))*sqrt(0.5)
  
  
  struct <- merge(struct, eN, by = "IDSCHOOL")
  
  struct[, c("nj", "pstud") := list(round(ifelse(length(Nj)<30, length(Nj), ifelse(length(Nj)>=30&length(Nj)<60, length(Nj)/2, length(Nj)/3))), 1/length(Nj)), by = IDSCHOOL]
  struct$ptot <- struct$psch*struct$pstud
  struct$X1 <- rnorm(nrow(struct), 0, 1)
  
  struct[, c("Y1", "Y2") := list((1+d0N)+(0.3+d1N)*X1+eN,
                                 (1+d0X)+(0.3+d1X)*X1+eX)]
  
  struct[, IDSTUD := 1:nrow(struct)]
  return(struct)
}