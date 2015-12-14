mcmcRES <- function(x, x0) {
  m  <- mean(x)
  RBias <- m/x0-1
  RMSE <- sqrt(mean((x/x0-1)^2))
  return(data.frame(Mean = m, RBias = RBias, RMSE = RMSE))
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

bootSampleREML_PV <- function(dt, idstrata = "IDSTRATE", idschool = "IDSCHOOL",
                           idstud = "IDSTUD") {
  info <- dt[[1]][[1]][, c(idstrata, idschool, idstud), with = F]
  info[, (names(info)) := llply(.SD, as.character), .SDcols = names(info)]
  strata <- unlist(info[, idstrata, with = F])
  fin <- lapply(1:length(dt[[1]]), function(i) data.table())
  sd <- foreach(ii = unique(strata), .combine = rbind) %do% {
    sdata <- info[strata %in% ii, ,drop = F]
    sch <- unique(unlist(sdata[,idschool, with = F]))
    nh <- length(sch)
    if (nh == 1) nh <- 2
    smSCH <- sample(as.character(sch), nh-1, replace = T)
    #smSCH <- sch
    std <- foreach(jj = smSCH, .combine = rbind) %do% {
      studdt <- sdata[sch %in% jj,,drop = F]
      idst <- unlist(studdt[, idstud, with = F])
      nhc <- length(idst)
      smplStud <- sample(idst, nhc-1, replace = F)
      setkeyv(studdt, idstud)
      studdt2 <- llply(dt[[1]], function(x) {setkeyv(x, idstud); x <- x[smplStud]; x$nhc <- nhc; return(x)})
      #studdt1 <- foreach(kk = smplStud, .combine = rbind) %do% studdt[idst %in% kk,,drop = F]
      fin <- mapply(rbind, fin, studdt2, SIMPLIFY=FALSE)
      return(NULL)
    }
    return(NULL)
  }
  fin <- imputationList(fin)
  return(fin)
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


bootREML_PV <- function(data = data.list, R = 5,
                     form = "BSMMAT01 ~ 1+SSEX+(1|IDSCHOOL)",
                     idstrata = "IDSTRATE", idschool = "IDSCHOOL",
                     idstud = "IDSTUD") {
  require(mitools)
  #print("Iteration:")
  pv <- length(data[[1]])
  tt0 <- with(data, lmer(form))
  t0 <- list()
  t0$models <- llply(tt0, lme4:::summary.merMod)
  t0$beta <- Reduce('+', llply(tt0, fixef))/pv
  t0$sigma2 <- Reduce('+', llply(t0$models, function(x) x$sigma^2))/pv
  t0$TT <- Reduce('+', llply(t0$models, function(x) x$varcor[[1]]))/pv

  
  bootRes <- foreach(ii = 1:R, .combine = rbind) %do% {
    #print(paste("boot", ii))
    smpldt <- try(bootSampleREML_PV(data, idstrata, idschool,
                                 idstud))#, wgt = NULL))
    if(class(smpldt)[1] == "try-error") return(NULL)
    res <- try(with(smpldt, lmer(form)))
    if(class(res)[1] == "try-error") return(NULL)
    res1 <- llply(res, lme4:::summary.merMod)
    cc <- c(Reduce('+', llply(res, fixef))/pv, Reduce('+', llply(t0$models, function(x) x$sigma^2))/pv, Reduce('+', llply(t0$models, function(x) x$varcor[[1]]))/pv)
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


makePOP4 <- function(M = 30){
  struct <- 1:M
  uj <- rtnorm(M, 0, sqrt(0.2), -1.5/sqrt(0.2), 1.5/sqrt(0.2))
  Nj <- round(50*exp(uj))
  #Nj <- 30

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
  struct[, c("ptot", "X1") := list(psch*pstud, rnorm(nrow(struct), 0, 1))]
  
  struct[, c("Y1", "Y2") := list((1+0.3*W+d0N)+(0.3+0.3*W+d1N)*X1+eN,
                                 (1+0.3*W+d0X)+(0.3+0.3*W+d1X)*X1+eX)]
  
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

myMINQUE <- function(dt, fixed, random1 = NULL, weights = NULL) {
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
      wg2 <- ddply(wg, as.formula(paste("~", nmid1)), function(x) unique(x[,3]))
      wg[,3] <- wg[,3]*nrow(wg2)/sum(wg2[,2])
      
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
  QI <- llply(Qj, function(qi) bdiag(qi))
  Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i,,drop = F]}
  Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
  Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", llply(Qj, function(qj) qj[[i]])))
  V <- bdiag(Vj)
  XVX <- solve(crossprod(X))
  Cjj <- diag(N)-X%*%tcrossprod(XVX,X)
  B0 <- tcrossprod(XVX,X)%*%Y
  EB0 <- tcrossprod(Y-X%*%B0)
  
  S <- fillSMINQUE(Cjj, QI, wgt2)
  #fillS(Cjj, Qj)
  WI <- laply(QI, function(qj) sum(diag(crossprod(Y,Cjj)%*%qj%*%Cjj%*%Y)))
  
  theta0 <- ginv(S)%*%matrix(WI, ncol = 1)
  Vj0 <- solve(Reduce("+", foreach(k = 1:length(theta0)) %do% {theta0[k]*QI[[k]]}))
  #theta0 <- c(3000, 1200)
  iM <- iterMINQUE(Vj0, X, QI, theta0, Y, id1, wgt2, wgt1)
  
  gc()
  doIM <- (round(sum(abs(iM$theta - theta0)), 4) != 0)
  while(doIM){
    theta <- iM$theta
    #print(theta)
    iM <- iterMINQUE(iM$Vj, X, QI, theta, Y, id1, wgt2, wgt1)
    doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
  }
  iM$beta <- tcrossprod(iM$P,X)%*%iM$Vj%*%Y
  
  gc()
  sigma2 <- iM$theta[1]
  names(sigma2) <- "sigma2"
  TT <- fillT(iM$theta[-1])
  dimnames(TT) <- list(clnms1, clnms1)
  beta <- as.numeric(iM$beta)
  names(beta) <- colnames(X)
  
  V <- iM$Vj
  Vj <- foreach(i = unique(id1)) %do% {V[id1 %in% i,id1 %in% i,drop = F]}
  # Variances:
  ccj <- crossprod(X, V)%*%tcrossprod(Y-X%*%iM$beta)%*%V%*%X
  Bcov <- (n1/(n1-1))*iM$P%*%ccj%*%iM$P
  varcov <- mapply(function(z, x, v, wgt) TT - TT%*%crossprod(z*unique(wgt), v)%*%(solve(v)-x%*%tcrossprod(Bcov,x))%*%v%*%(z*unique(wgt))%*%TT, Z1, Xj, Vj, wgt2i)
  #Tcov <- (n1/(n1-1))*solve(iM$S)
  ranef <- mapply(function(x, y, v, z, wgt) (TT%*%crossprod(z*unique(wgt), v)%*%(y - x%*%iM$beta)),Xj, Yj, Vj, Z1, wgt2i)
  
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


myMINQUE2 <- function(dt, fixed, random1 = NULL, weights = NULL, apriori = NULL) {
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
  
  #   wgt1i <- foreach(i = unique(id1)) %do% {wgt1[id1 %in% i]}
  #   wgt2i <- foreach(i = unique(id1)) %do% {wgt2[id1 %in% i]}
  #   wgt12i <- foreach(i = unique(id1)) %do% {wgt12[id1 %in% i]}
  l <- l+1
  Qj <- c(list(llply(QI[[1]], function(zj) diag(nrow(zj)))), QI)
  Qj[[1]] <- foreach(i = 1:length(Qj[[1]])) %do% {1/wgt12i[[i]]*Qj[[1]][[i]]}
  QI <- llply(Qj, function(x) bdiag(x))
  Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i, ,drop = F]}
  X <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Xj[[ii]]), "Matrix")
  Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
  Y <- as(foreach(ii = 1:length(Xj), .combine = rbind) %do% as.matrix(Yj[[ii]]), "Matrix")
  
  if (is.null(apriori)) apriori <- rep(1, length(QI))
  #   Z2 <- cBind(apriori[1]/sum(apriori)*diag(nrow(Y)), apriori[2]/sum(apriori)*Z)
  #   V <- Z2%*%t(Z2)
  
  Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", mapply(function(qj, th) th*qj[[i]], Qj, apriori)))
  V <- bdiag(Vj)
  
  #   Zj <- list(diag(1/sqrt(wgt12i)), Z1)
  #   ZI <- list(diag(1/sqrt(wgt12)), Z1)
  #ZIn <- list(diag(nrow(Z1n)), Z1n)
  #   
  #   QI <- llply(ZI, function(x) x%*%t(x))
  #   Xj <- foreach(i = unique(id1)) %do% {X[id1 %in% i,,drop = F]}
  #   Yj <- foreach(i = unique(id1)) %do% {Y[id1 %in% i,,drop = F]}
  #Zj <- foreach(i = unique(id1)) %do% {Z[id1 %in% i,,drop = F]}
  #   Vj <- foreach(i = 1:length(unique(id1))) %do% solve(Reduce("+", llply(Qj, function(qj) qj[[i]])))
  #   V <- bdiag(Vj)
  
  iM <- iMINQUE(V, X, Y, QI, wgt2i)
  ##print(iM$theta)
  gc()
  #   doIM <- T
  #   while(doIM){
  theta <- iM$theta
  #print(theta)
  iM <- iMINQUE(iM$V, X, Y, QI, wgt2i)
  #     doIM <- (round(sum(abs(iM$theta - theta))) != 0)
  #   }
  #   s2 <- iM$theta[1]
  #   tau <- iM$theta[2]
  #   ywj <- mapply(function(y, w){sum(w*y)/sum(w)}, Yj, wgt1i)
  #   xwj <- mapply(function(x, w){as.matrix(apply(w*x, 2, sum))/sum(w)}, Xj, wgt1i, SIMPLIFY = F)  
  #   gj <- llply(wgt1i, function(w) tau/(s2/sum(w)+tau))
  #   if(dim(xwj[[1]])[1]==1) dij <- mapply(function(x, g, xw, w) {w*(x-as.numeric(g*xw))}, Xj, gj, xwj, wgt12i)
  #   else dij <- mapply(function(x, g, xw, w) {w*t(t(x)-g*xw)}, Xj, gj, xwj, wgt12i)
  #   P <- ginv(as.matrix(Reduce("+", mapply(function(x, d){t(t(x)%*%d)}, Xj, dij))))
  #   Q <- Reduce("+", mapply(function(y, d){t(t(y)%*%d)}, Yj, dij))
  #   beta <- as.matrix(P%*%Q)
  #   rownames(beta) <- colnames(X)
  #   ranef <- mapply(function(g, y, x){y-t(x)%*%beta}, gj, ywj, xwj)
  #   fit <- mapply(function(x, z, u) x%*%iM$beta+z%*%u, Xj, Zj, ranef)
  #   fit <- do.call(rBind, fit)
  #   residuals <- Y-fit
  P <- ginv(as.matrix((crossprod(X, iM$V)%*%X)))
  iM$beta <- tcrossprod(P, X)%*%iM$V%*%Y
  gc()
  sigma2 <- iM$theta[1]
  names(sigma2) <- "sigma2"
  TT <- fillT(iM$theta[-1])
  dimnames(TT) <- list(clnms1, clnms1)
  beta <- as.numeric(iM$beta)
  names(beta) <- colnames(X)
  
  #     # Variances:
  #     #   ccj <- mapply(function(x,y, v) t(x)%*%v%*%(y-x%*%iM$beta)%*%t(y-x%*%iM$beta)%*%v%*%x, Xj, Yj, iM$Vj)
  #     #   Bcov <- (n1/(n1-1))*iM$P%*%Reduce("+", ccj)%*%iM$P
  #     Vst <- mapply(function(z, w) {sigma2*diag(nrow(z))+(1*unique(w)*z)%*%TT%*%t(z)}, Z1, wgt2i)
  #     Vj <- llply(Z1, function(z) ginv(as.matrix(z%*%TT%*%t(z)+sigma2)))
  #     Bcov <- iM$P%*%(Reduce("+", mapply(function(x, v, vst){t(x)%*%v%*%vst%*%v%*%x}, Xj, Vj, Vst)))%*%iM$P
  #     Cj <- mapply(function(vst, x, v) {vst-x%*%Bcov%*%t(x)-2*vst%*%v%*%x%*%iM$P%*%t(x)}, Vst, Xj, Vj)
  #     PP <- mapply(function(w, z, v) {(1/sqrt(unique(w))*TT%*%t(z))%*%v}, wgt2i, Z1, Vj)
  #     varcov <- mapply(function(pp, cj) {pp%*%cj%*%t(pp)}, PP, Cj)
  #     #varcov <- mapply(function(z, x, v, wgt) TT - TT%*%(t(z)*unique(wgt))%*%v%*%(solve(v)-x%*%Bcov%*%t(x))%*%v%*%(z*unique(wgt))%*%TT, Z1, Xj, iM$Vj, wgt2i)
  #     #varcov <- mapply(function(z, x, v) TT - TT%*%t(z)%*%v%*%z%*%TT, Z1, Xj, iM$Vj)
  #     
  #     #Tcov <- (n1/(n1-1))*solve(iM$S)
  #     ranef <- mapply(function(x, y, v, z, wgt) (TT%*%(t(z)*1/sqrt(unique(wgt)))%*%v%*%(y - x%*%iM$beta)),Xj, Yj, Vj, Z1, wgt2i)
  #     
  #     fit <- mapply(function(x, z, u, wgt) x%*%iM$beta+(z*sqrt(unique(wgt)))%*%u, Xj, Z1, ranef, wgt2i)
  #     fit <- do.call(rBind, fit)
  #     ranef <- llply(ranef, t)
  #     ranef <- do.call(rBind, ranef)
  #     rownames(ranef) <- unique(id1)
  #     residuals <- Y-fit
  #     gc()
  #     vv <- do.call(cbind, llply(varcov, as.matrix))
  #     dim(vv) <- c(dim(varcov[[1]]), length(varcov))
  #     ranef <- as.data.frame(as.matrix(ranef))
  #     attr(ranef, "postVar") <- vv
  
  #   return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, n1 = n1, ranef = list(ranef), cov = Bcov, fitted = fit, resid = residuals))
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

# mySumm <- function(.) { s <- sigma(.)
# c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) }
# (t0 <- mySumm(fm01ML)) # just three parameters
# ## alternatively:
# mySumm2 <- function(.) {
#   c(beta=fixef(.),sigma=sigma(.),sig01=unlist(VarCorr(.)))
# }

# 
# summary.MI <- function (object, subset = NULL, ...) {
#   if (length(object) == 0) {
#     stop('Invalid input for "subset"')
#   } else {
#     if (length(object) == 1) {
#       return(summary(object[[1]]))
#     }
#   }
#   
#   # Roman: This function isn't fecthing coefficients robustly. Something goes wrong. Contact package author. 
#   getcoef <- function(obj) {
#     # S4
#     if (!isS4(obj)) {
#       coef(obj)
#     } else {
#       if ("coef3" %in% slotNames(obj)) {
#         obj@coef3
#       } else {
#         obj@coef
#       }
#     }
#   }
#   
#   #
#   res <- list()
#   
#   # Get indices
#   subset <- if (is.null(subset)) {
#     1:length(object)
#   } else {
#     c(subset)
#   }
#   
#   # Compute the summary of all objects
#   for (k in subset) {
#     res[[k]] <- summary(object[[k]])
#   }
#   
#   
#   # Answer
#   ans <- list(
#     zelig = object[[1]]$name,
#     call = object[[1]]$result@call,
#     all = res
#   )
#   
#   #
#   coef1 <- se1 <- NULL
#   
#   #
#   for (k in subset) {
#     #       tmp <-  getcoef(res[[k]]) # Roman: I changed this to coef, not 100% sure if the output is the same
#     tmp <- coef(res[[k]])
#     coef1 <- cbind(coef1, tmp[, 1])
#     se1 <- cbind(se1, tmp[, 2])
#   }
#   
#   rows <- nrow(coef1)
#   Q <- apply(coef1, 1, mean)
#   U <- apply(se1^2, 1, mean)
#   B <- apply((coef1-Q)^2, 1, sum)/(length(subset)-1)
#   var <- U+(1+1/length(subset))*B
#   nu <- (length(subset)-1)*(1+U/((1+1/length(subset))*B))^2
#   
#   coef.table <- matrix(NA, nrow = rows, ncol = 4)
#   dimnames(coef.table) <- list(rownames(coef1),
#                                c("Value", "Std. Error", "t-stat", "p-value"))
#   coef.table[,1] <- Q
#   coef.table[,2] <- sqrt(var)
#   coef.table[,3] <- Q/sqrt(var)
#   coef.table[,4] <- pt(abs(Q/sqrt(var)), df=nu, lower.tail=F)*2
#   ans$coefficients <- coef.table
#   ans$cov.scaled <- ans$cov.unscaled <- NULL
#   
#   for (i in 1:length(ans)) {
#     if (is.numeric(ans[[i]]) && !names(ans)[i] %in% c("coefficients")) {
#       tmp <- NULL
#       for (j in subset) {
#         r <- res[[j]]
#         tmp <- cbind(tmp, r[[pmatch(names(ans)[i], names(res[[j]]))]])
#       }
#       ans[[i]] <- apply(tmp, 1, mean)
#     }
#   }
#   
#   class(ans) <- "summaryMI"
#   ans
# }