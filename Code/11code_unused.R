
# 
# iterMINQUE <- function(Tw, X, QI, kk, theta0, Y){
#   TwX <- Tw%*%X
#   C <- Tw-TwX%*%solve(t(X)%*%TwX)%*%t(TwX)
#   CY <- C%*%Y
#   WI <- makeWI(QI, CY)
#   gc()
#   CQ <- llply(QI, function(x) C%*%x)
#   S <- fillS(CQ)
#   #theta <- ginv(S)%*%as.matrix(WI, ncol = 1)
#   theta <- mapply(function(th, cq, wi){cat("thetai\n");print(th)
#     th*wi/sum(diag(cq))}, theta0, CQ, WI)
#   D <- solve(Reduce("+", mapply(function(th, qi) th*qi, as.list(theta), QI)))
#   gc()
#   return(list(D = D, theta = theta, sigma = sigma))
# }
# 
# 
# fillS <- function(pvi) {
#   x <- matrix(0, ncol = length(pvi), nrow = length(pvi))
#   ns <- nrow(x)
#   cc <- combinations(ns,2,1:ns, repeats.allowed = T)
#   foreach(ii = 1:nrow(cc)) %do% {
#     i <- cc[ii, 1]
#     j <- cc[ii, 2]
# #     print(i)
# #     print(round(pvi[[i]], 3))
# #     print(j)
# #     print(round(pvi[[j]], 3))
#     s <-  sum(diag((pvi[[i]]%*%pvi[[j]])))
#     x[i,j] <- s
#     x[j, i] <- s
#     gc()
#   }
#   gc()
#   return(x)
# }
# 
# formTT <- function(k) {
#   kk <- combinations(k,2,1:k, repeats.allowed = T)
#   ns <- round(sqrt(nrow(kk)*2))
#   TT <- foreach(ii = 1:nrow(kk)) %do% {
#     x <- matrix(0, ncol = ns, nrow = ns)
#     i <- kk[ii, 1]
#     j <- kk[ii, 2]
#     x[i,j] <- 1
#     x[j, i] <- 1
#     return(x)
#   }
#   return(TT)
# }
# 
# fillSigma <- function(theta, kk) {
#   ns <- round(sqrt(nrow(kk)*2))
#   x <- matrix(0, ncol = ns, nrow = ns)
#   for(ii in 1:nrow(kk)) {
#     i <- kk[ii, 1]
#     j <- kk[ii, 2]
#     x[i,j] <- theta[ii]
#     x[j, i] <- theta[ii]
#   }
#   return(x)
# }
# 
# makeQIWI <- function(HI, l, CY, k) {
#   kk <- combinations(k,2,1:k, repeats.allowed = T)
#   WI <- rep(0, l)
#   QI <- foreach(ii = 1:l) %do% {
#     i <- kk[ii, 1]
#     j <- kk[ii, 2]
#     if (i == j)
#       qi <-  tcrossprod(HI[[i]]) else {
#         ni <- ncol(HI[[i]])
#         nj <- ncol(HI[[j]])
#         I <- Matrix(1, nrow = ni, ncol = nj)
#         qi <- HI[[i]]%*%I%*%t(HI[[j]])+HI[[j]]%*%t(I)%*%t(HI[[i]])
#       }
#     print(sum(qi))
#     WI[ii] <- as(t(CY)%*%qi%*%CY, "numeric")
#     return(qi)
#   }
#   print(WI)
#   gc()
#   return(list(QI=QI, WI=WI, kk = kk))
# }
# 
# makeWI <- function(QI, CY) {
#   WI <- laply(QI, function(x) as(t(CY)%*%x%*%CY, "numeric"))
#   gc()
#   return(WI)
# }
# 
# myMINQUE <- function(dt, fixed, random1 = NULL, random2 = NULL) {
#   N <- nrow(dt)
#   # Form Y and fixed effects data frame
#   ff <- model.frame(fixed, dt)
#   Y <- as(as(ff[,1, drop = F], "matrix"), "Matrix")
#   if (grepl("-1",fixed)) {
#     X <- as(as(ff[,-1, drop = F], "matrix"), "Matrix")
#   } else {
#     X <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(ff),
#                             dimnames = list(NULL, "(Intercept)")), 
#                      ff[,-1, drop = F]), "matrix"), "Matrix")
#   }
#   
#   if (!is.null(random1)) {
#     id1 <- strsplit(random1, "\\|")
#     random1 <- unlist(id1)[1]
#     nmid1 <- unlist(id1)[2]
#     rr1 <- model.frame(random1, dt)
#     id1 <- model.frame(paste("~", nmid1), dt)[,1]
#     if (grepl("-1",random1)) {
#       HH1 <- as(as( rr1, "matrix"), "Matrix")
#     } else {
#       HH1 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr1),
#                                 dimnames = list(NULL, "(Intercept)")), rr1), "matrix"),
#                 "Matrix")
#     }
#     colnames(HH1) <- paste(nmid1, colnames(HH1), sep = ".")
#     clnms1 <- colnames(HH1)
#     HH1 <- alply(HH1, 2, function(x) Matrix(foreach(i = unique(id1), .combine = cbind) %do% {y <- x; y[!id1 %in% i] <- 0; return(y)}))
#     n1 <- length(unique(id1))
#   } else {
#     HH1 <- NULL
#     clnms1 <- NULL
#     n1 <- NULL
#   }
#   
#   if (!is.null(random2)) {
#     id2 <- strsplit(random2, "\\|")
#     random2 <- unlist(id2)[1]
#     nmid2 <- unlist(id2)[2]
#     rr2 <- model.frame(random2, dt)
#     id2 <- model.frame(paste("~", nmid2), dt)[,1]
#     if (grepl("-1",random2)) {
#       HH2 <- as(as( rr2, "matrix"), "Matrix")
#     } else {
#       HH2 <- as(as(cbind(matrix(1, ncol = 1, nrow = nrow(rr2),
#                                 dimnames = list(NULL, "(Intercept)")), rr2),
#                    "matrix"),
#                 "Matrix")
#     }
#     colnames(HH2) <- paste(nmid2, colnames(HH2), sep = ".")
#     clnms2 <- colnames(HH2)
#     HH2 <- alply(HH2, 2, function(x) Matrix(foreach(i = unique(id2), .combine = cbind) %do% {y <- x; y[!id2 %in% i] <- 0; return(y)}))    
#     n2 <- length(unique(id2))
#   } else {
#     HH2 <- NULL
#     clnms2 <- NULL
#     n2 <- NULL
#   }
#   HI <- c(list(diag(N)), HH1, HH2)
#   names(HI) <- c("sigma2", clnms1, clnms2)
#   gc()
#   # st1 <- system.time(minqueiv <- lmevarMINQUE(Y, X, HH, id))
#   # st2 <- system.time(minqueIiv <- lmevarMINQUEI(Y, X, HH, id))
#   
#   ivMin <- lmevarMINQUEI(Y, X, HI)
#   gc()
#   return(c(ivMin, N = N, n1 = n1, n2 = n2))
# }
# 
# lmevarMINQUE <- function(Y, X, HH, id) {
#   m <- ncol(X)
#   k <- ncol(HH)
#   
#   options(width = 180)
#   idu <- unique(id)
#   HI <- c(list(Diagonal(length(id),1)), alply(HH, 2, function(x) Matrix(foreach(i = idu, .combine = cbind) %do% {y <- x; y[!id %in% i] <- 0; return(y)})))
#   H <- do.call(cBind, HI)
#   Th <- solve(tcrossprod(H))
#   C <- Th-Th%*%X%*%solve(t(X)%*%Th%*%X)%*%t(X)%*%Th
#   suppressMessages(gc())
#   k <- k+1
#   l <- k*(k+1)/2
#   CY <- crossprod(C,Y)
#   QIWI <- makeQIWI(HI, l, CY, k)
#   gc()
#   CQ <- llply(QIWI[[1]], function(x) crossprod(C,x))
#   S <- ginv(fillS(CQ))
#   gc()
#   theta <- S%*%QIWI[[2]]
#   sigma <- fillSigma(theta, QIWI[[3]])
# 
#   D <- solve(Reduce("+", mapply(function(th, qi) th*qi, theta[,1], QIWI[[1]])))
#   beta <- as(solve(t(X)%*%D%*%X)%*%t(X)%*%D%*%Y, "numeric")
#   names(beta) <- colnames(X)
#   sigma2 <- sigma[1,1]
#   names(sigma2) <- "sigma2"
#   tau  <- sigma[2:k, 2:k]
#   dimnames(tau) <- list(colnames(HH), colnames(HH))
#   gc()
#   return(list(gamma = beta, sigma2 = sigma2, tauBeta = tau))
# }
# 
# makeQIWI2 <- function(HI, l, Ckk, Yk, k, idl) {
#   kk <- combinations(k,2,1:k, repeats.allowed = T)
#   WI <- rep(0, l)
#   QI <- foreach(ii = 1:l) %do% {
#     i <- kk[ii, 1]
#     j <- kk[ii, 2]
#     
#     if (i == j) 
#       qi <- tcrossprod(HI[[i]]) 
#     else {
#       ni <- ncol(HI[[i]])
#       nj <- ncol(HI[[j]])
#       I <- Matrix(1, nrow = ni, ncol = nj)
#       qi <- HI[[i]]%*%I%*%t(HI[[j]])+HI[[j]]%*%t(I)%*%t(HI[[i]])
#     }
#     qi <- foreach(jj = idl) %do% qi[jj,jj]
#     WI[ii] <- sum(mapply(function(y, c, q) as(t(y)%*%c%*%q%*%c%*%y, "numeric"), Yk, Ckk, qi))
#     return(qi)
#   }
#   gc()
#   return(list(QI=QI, WI=WI, kk = kk))
# }
# 
# fillS2 <- function(qi, ckk) {
#   x <- matrix(0, ncol = length(qi), nrow = length(qi))
#   ns <- nrow(x)
#   cc <- combinations(ns,2,1:ns, repeats.allowed = T)
#   foreach(ii = 1:nrow(cc)) %do% {
#     i <- cc[ii, 1]
#     j <- cc[ii, 2]
#     s <-  sum(mapply(function(ck, qii, qjj) sum(diag((ck%*%qii%*%ck%*%qjj))),
#                      ckk, qi[[i]], qi[[j]]))
#     x[i,j] <- s
#     x[j, i] <- s
#     gc()
#   }
#   gc()
#   return(x)
# }
# 
# makeWI2 <- function(QIk, Ckk, Yk) {
#   WI <- laply(QIk, function(x) sum(mapply(function(y, c, q) as(t(y)%*%c%*%q%*%c%*%y, "numeric"), Yk, Ckk, x)))
#   gc()
#   return(WI)
# }
# 
# iterMINQUE2 <- function(Tw, X, QIk, kk, theta0, Yk, idl){
#   TwX <- Tw%*%X
#   C <- Tw-TwX%*%solve(t(X)%*%TwX)%*%t(TwX)
#   Ckk <- foreach(ii = idl) %do% C[ii,ii]
#   
#   WI <- makeWI2(QIk, Ckk, Yk)
#   gc()
#   S <- ginv(fillS2(QIk, Ckk))
#   gc()
#   theta <- S%*%WI
#   D <- solve(Reduce("+", mapply(function(th, qi) th*do.call(bdiag, qi), as.list(theta), QIk)))
#   gc()
#   return(list(D = D, theta = theta, sigma = sigma))
# }
# 
# lmevarMINQUEI2 <- function(Y, X, HI, idd) {
#   m <- ncol(X)
#   k <- length(HI)
#   N <- nrow(Y)
#   idl <- foreach(ii = unique(idd)) %do% which(idd %in% ii)
#   
#   sigmaNms <- names(HI)
#   options(width = 180)
#   dimHI <- llply(HI, dim)
#   H <- do.call(cBind, HI)
#   Th <- solve(tcrossprod(H))
#   Th <- do.call(bdiag, foreach(ii = idl) %do% Th[ii,ii])
#   rm(list = c("H"))
#   ThX <- Th%*%X
#   C <- Th-ThX%*%solve(t(X)%*%ThX)%*%t(ThX)
#   rm(list = c("ThX", "Th"))
#   Ckk <- foreach(ii = idl) %do% C[ii,ii]
#   Yk <- foreach(ii = idl) %do% Y[ii, drop = F]
#   l <- k*(k+1)/2
#   QIWI <- makeQIWI2(HI, l, Ckk, Yk, k, idl)
# 
#   S <- ginv(fillS2(QIWI[[1]], Ckk))
#   gc()
#   theta0 <- S%*%QIWI[[2]]
# 
#   D0 <- solve(Reduce("+", mapply(function(th, qi) {th*do.call(bdiag, qi)}, theta0[,1], QIWI[[1]])))
#   
#   iM <- iterMINQUE2(D0, X, QIk = QIWI[[1]], kk = QIWI[[3]], theta0[,1], Yk, idl)
#   
# 
#   gc()
#   doIM <- (round(sum(abs(iM$theta - theta0[, 1])), 4) != 0)
#   while(doIM){
#     theta <- iM$theta
#     print(theta)
#     iM <- iterMINQUE2(iM$D, X, QIk = QIWI[[1]], kk = QIWI[[3]], theta, Yk, idl)
#     doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
#   }
#   
#   
#   sigma <- fillSigma(iM$theta, QIWI[[3]])
#   dimnames(sigma) <- list(sigmaNms, sigmaNms)
#   beta <- as(solve(t(X)%*%iM$D%*%X)%*%t(X)%*%iM$D%*%Y, "numeric")
#   names(beta) <- colnames(X)
#   sigma2 <- sigma[1,1]
#   names(sigma2) <- "sigma2"
#   tau  <- sigma[2:k, 2:k, drop = F]
#   gc()
#   return(list(gamma = beta, sigma2 = sigma2, tauBeta = tau))
# }
# 
# lmevarMINQUEI <- function(Y, X, HI) {
#   m <- ncol(X)
#   k <- length(HI)
#   N <- nrow(Y)
#   sigmaNms <- names(HI)
#   options(width = 180)
#   dimHI <- llply(HI, dim)
#   H <- do.call(cBind, HI)
#   Th <- solve(tcrossprod(H))
#   rm(list = c("H"))
#   ThX <- Th%*%X
#   #print(sum(diag(Th)))
#   C <- Th-ThX%*%solve(t(X)%*%ThX)%*%t(ThX)
#   #print(round(C,3)[14:22,14:22])
#   rm(list = c("ThX", "Th"))
#   
#   l <- k*(k+1)/2
#   CY <- C%*%Y
#   QIWI <- makeQIWI(HI, l, CY, k)
#   print(QIWI[[1]])
#   rm("HI")
#   gc()
#   CQ <- llply(QIWI[[1]], function(x) C%*%x)
# #   print(round(CQ[[2]], 3))
# #   print("CQ")
#   llply(CQ, function(x) print(diag(x)))
#   S <- ginv(fillS(CQ))
#   print(round(S, 3))
#   rm("CQ")
#   gc()
#   theta0 <- S%*%QIWI[[2]]
#   print(theta0)
#   rm("S")
#   D0 <- solve(Reduce("+", mapply(function(th, qi) {th*qi}, theta0[,1], QIWI[[1]])))
#   print(sum(D0))
#   iM <- iterMINQUE(D0, X, QI= QIWI[[1]], kk = QIWI[[3]], theta0[,1], Y)
#   gc()
#   doIM <- (round(sum(abs(iM$theta - theta0[, 1])), 4) != 0)
#   while(doIM){
#     theta <- iM$theta
#     iM <- iterMINQUE(iM$D, X, QI= QIWI[[1]], kk = QIWI[[3]], theta, Y)
#     doIM <- (round(sum(abs(iM$theta - theta)), 4) != 0)
#   }
#   
#   
#   sigma <- fillSigma(iM$theta, QIWI[[3]])
#   dimnames(sigma) <- list(sigmaNms, sigmaNms)
#   beta <- as(solve(t(X)%*%iM$D%*%X)%*%t(X)%*%iM$D%*%Y, "numeric")
#   names(beta) <- colnames(X)
#   sigma2 <- sigma[1,1]
#   names(sigma2) <- "sigma2"
#   tau  <- sigma[2:k, 2:k, drop = F]
#   gc()
#   return(list(gamma = beta, sigma2 = sigma2, tauBeta = tau))
# }






# summary.merMod <- function (object, ...) 
# {
#   resp <- object@resp
#   devC <- object@devcomp
#   dd <- devC$dims
#   cmp <- devC$cmp
#   useSc <- as.logical(dd[["useSc"]])
#   sig <- sigma(object)
#   REML <- isREML(object)
#   famL <- lme4:::famlink(resp = resp)
#   coefs <- cbind(Estimate = fixef(object), `Std. Error` = sig * 
#                    sqrt(diag(object@pp$unsc())))
#   if (nrow(coefs) > 0) {
#     coefs <- cbind(coefs, (cf3 <- coefs[, 1]/coefs[, 2]), 
#                    deparse.level = 0)
#     colnames(coefs)[3] <- paste(if (useSc) 
#       "t"
#                                 else "z", "value")
#     if (isGLMM(object)) 
#       coefs <- cbind(coefs, `Pr(>|z|)` = 2 * pnorm(abs(cf3), 
#                                                    lower.tail = FALSE))
#   }
#   llAIC <- lme4:::getLlikAIC(object)
#   varcor <- VarCorr(object)
#   structure(list(methTitle = lme4:::methTitle(object, dims = dd), 
#                  objClass = class(object), devcomp = devC, isLmer = is(resp, 
#                                                                        "lmerResp"), useScale = useSc, logLik = llAIC[["logLik"]], 
#                  family = famL$fami, link = famL$link, ngrps = sapply(object@flist, 
#                                                                       function(x) length(levels(x))), coefficients = coefs, 
#                  sigma = sig, vcov = vcov(object, correlation = TRUE, 
#                                           sigm = sig), varcor = varcor, AICtab = llAIC[["AICtab"]], 
#                  call = object@call), class = "summary.merMod")
# }
# 
# function (object, ...) 
# {
#   .local <- function (object, ddf = "Satterthwaite", ...) 
#   {
#     cl <- callNextMethod()
#     if (!is.null(ddf) && ddf == "lme4") 
#       return(cl)
#     else {
#       t.pval <- tryCatch({
#         totalAnovaRandLsmeans(model = object, ddf = "Satterthwaite", 
#                               isTtest = TRUE)$ttest$tpvalue
#       }, error = function(e) {
#         NULL
#       })
#       coefs.satt <- cbind(cl$coefficients, t.pval)
#       cl$coefficients <- coefs.satt
#       colnames(cl$coefficients)[4] <- "Pr(>|t|)"
#     }
#     return(cl)
#   }
#   .local(object, ...)
# }
# 
# lmerTest:::totalAnovaRandLsmeans <- function (model, ddf = "Satterthwaite", type = 3, alpha.random = 0.1, 
#           alpha.fixed = 0.05, reduce.fixed = TRUE, reduce.random = TRUE, 
#           lsmeans.calc = TRUE, difflsmeans.calc = TRUE, isTotal = FALSE, 
#           isAnova = FALSE, isRand = FALSE, isLSMEANS = FALSE, isDiffLSMEANS = FALSE, 
#           isTtest = FALSE, test.effs = NULL, method.grad = "simple") 
# {
#   if (!isRand && !(ddf %in% c("Satterthwaite", "Kenward-Roger"))) {
#     print("Error: parameter ddf is wrongly specified")
#     stop()
#   }
#   if (!isRand && !(type %in% c(1, 3))) {
#     print("Error: parameter type is wrongly specified")
#     stop()
#   }
#   data <- model.frame(model)
#   mm <- model.matrix(model)
#   l <- attr(mm, "contrasts")
#   contr <- l
#   if (isAnova || isTotal) {
#     if (length(which(unlist(contr) != "contr.SAS")) > 0) {
#       names.facs <- names(contr)
#       l <- as.list(rep("contr.SAS", length(names.facs)))
#       names(l) <- names(contr)
#       model <- updateModel(model, . ~ ., getREML(model), 
#                            l)
#     }
#   }
#   else {
#     model <- updateModel(model, . ~ ., getREML(model), l)
#   }
#   result <- NULL
#   anova.table <- NULL
#   result$response <- rownames(attr(terms(model), "factors"))[1]
#   if (isRand || isTotal || (ddf == "Kenward-Roger" && (isTotal || 
#                                                          isAnova))) {
#     model <- if (getREML(model) == 1) {
#       model
#     }
#     else {
#       warning("\n model has been refitted with REML=TRUE \n")
#       updateModel(model, . ~ ., reml = TRUE, l)
#     }
#   }
#   mf.final <- update.formula(formula(model), formula(model))
#   data <- data[complete.cases(data), ]
#   model <- updateModel(model, mf.final, getREML(model), l)
#   result$call <- model@call
#   result$corr.intsl <- checkCorr(model)
#   if (isRand || isTotal) {
#     result.rand <- elimZeroVarOrCorr(model, data, l)
#     model <- result.rand$model
#   }
#   if (class(model) == "lm" | class(model) == "gls") {
#     result <- saveResultsFixModel(result, model)
#     result$rand.table = NULL
#     return(result)
#   }
#   if (isRand || isTotal) {
#     if (isRand) 
#       reduce.random <- FALSE
#     result.rand <- elimRandEffs(model, data, alpha.random, 
#                                 reduce.random, l)
#     model <- result.rand$model
#     rt <- as.data.frame(result.rand$TAB.rand)
#     rt$Chi.DF <- as.integer(rt$Chi.DF)
#     if (!is.null(rt$elim.num)) 
#       rt$elim.num <- as.integer(rt$elim.num)
#     result$rand.table <- rt
#     if (isRand) 
#       return(result)
#   }
#   if (class(model) == "lm" | class(model) == "gls") 
#     return(saveResultsFixModel(result, model))
#   stop = FALSE
#   is.first.anova <- TRUE
#   is.first.sign <- TRUE
#   while (!stop) {
#     if (nrow(anova(model, ddf = "lme4")) == 0) {
#       if (is.null(anova.table)) {
#         if (isLSMEANS || isDiffLSMEANS) {
#           lsmeans.summ <- matrix(ncol = 7, nrow = 0)
#           colnames(lsmeans.summ) <- c("Estimate", "Standard Error", 
#                                       "DF", "t-value", "Lower CI", "Upper CI", 
#                                       "p-value")
#           lsmeans.summ <- as.data.frame(lsmeans.summ)
#           if (isLSMEANS) 
#             result$lsmeans.table <- lsmeans.summ
#           if (isDiffLSMEANS) 
#             result$diffs.lsmeans.table <- lsmeans.summ
#           return(result)
#         }
#         if (isTtest) {
#           rho <- rhoInit(model)
#           h <- hessian(function(x) Dev(rho, x), rho$param$vec.matr)
#           rho$A <- 2 * solve(h)
#           tsummary <- calculateTtest(rho, diag(rep(1, 
#                                                    length(rho$fixEffs))), length(rho$fixEffs), 
#                                      method.grad)
#           result$ttest <- list(df = tsummary[, "df"], 
#                                tvalue = tsummary[, "t value"], tpvalue = tsummary[, 
#                                                                                   "p-value"])
#         }
#         result$model <- model
#         result$anova.table <- anova(model, ddf = "lme4")
#         return(result)
#       }
#       break
#     }
#     rho <- rhoInit(model)
#     h <- myhess(function(x) Dev(rho, x), rho$param$vec.matr)
#     rho$A <- 2 * solve(h)
#     isposA <- all(eigen(rho$A)$values > 0)
#     if (!isposA) {
#       print("Asymptotic covariance matrix A is not positive!")
#     }
#     if (isTtest) {
#       tsummary <- calculateTtest(rho, diag(rep(1, length(rho$fixEffs))), 
#                                  length(rho$fixEffs), method.grad)
#       result$ttest <- list(df = tsummary[, "df"], tvalue = tsummary[, 
#                                                                     "t value"], tpvalue = tsummary[, "p-value"])
#       return(result)
#     }
#     if (isLSMEANS || isDiffLSMEANS) {
#       if (isLSMEANS) {
#         lsmeans.tab <- calcLSMEANS(model, data, rho, 
#                                    alpha.fixed, test.effs = test.effs, method.grad = method.grad, 
#                                    lsmeansORdiff = TRUE, l)
#         result$lsmeans.table <- lsmeans.tab$summ.data
#         result$diffs.lsmeans.table <- NULL
#       }
#       if (isDiffLSMEANS) {
#         lsmeans.tab <- calcLSMEANS(model, data, rho, 
#                                    alpha.fixed, test.effs = test.effs, method.grad = method.grad, 
#                                    lsmeansORdiff = FALSE, l)
#         result$diffs.lsmeans.table <- lsmeans.tab$summ.data
#         result$lsmeans.table <- NULL
#       }
#       return(result)
#     }
#     X.design.list <- createDesignMat(model, data)
#     X.design <- X.design.list$X.design
#     names.design.withLevels <- X.design.list$names.design.withLevels
#     nums.dummy.coefs <- getNumsDummyCoefs(model, data, l)
#     rho$nums.zeroCoefs <- nums.dummy.coefs$nums.zeroCoefs
#     rho$nums.Coefs <- nums.dummy.coefs$nums.Coefs
#     fullCoefs <- rep(0, ncol(X.design))
#     fullCoefs[rho$nums.Coefs] <- rho$fixEffs
#     test.terms <- attr(terms(model), "term.labels")
#     if (is.first.anova) {
#       anova.table <- initAnovaTable(model, reduce.fixed)
#       is.first.anova <- FALSE
#       elim.num <- 1
#     }
#     if (type == 3) 
#       L <- calcGeneralSetForHypothesis(X.design, rho)
#     if (type == 1) {
#       X <- X.design
#       p <- ncol(X)
#       XtX <- crossprod(X)
#       U <- doolittle(XtX)$U
#       d <- diag(U)
#       for (i in 1:nrow(U)) if (d[i] > 0) 
#         U[i, ] <- U[i, ]/d[i]
#       L <- U
#     }
#     resultFpvalueSS <- lapply(test.terms, calcFpvalueMAIN, 
#                               L = L, X.design = X.design, fullCoefs = fullCoefs, 
#                               model = model, rho = rho, ddf = ddf, method.grad = method.grad, 
#                               type = type)
#     anova.table <- fillAnovaTable(resultFpvalueSS, anova.table)
#     if (!reduce.fixed) 
#       break
#     else {
#       resNSelim <- elimNSFixedTerm(model, anova.table, 
#                                    data, alpha.fixed, elim.num, l)
#       if (is.null(resNSelim)) 
#         break
#       else {
#         model <- resNSelim$model
#         mf.final <- update.formula(formula(model), formula(model))
#         model <- updateModel(model, mf.final, getREML(model), 
#                              l)
#         anova.table <- resNSelim$anova.table
#         elim.num <- elim.num + 1
#       }
#     }
#   }
#   anova.table <- as.data.frame(anova.table)
#   anova.table$NumDF <- as.integer(anova.table$NumDF)
#   if (!is.null(anova.table$elim.num)) 
#     anova.table$elim.num <- as.integer(anova.table$elim.num)
#   if (isTotal || isAnova) {
#     result$anova.table <- anova.table
#     if (isAnova) 
#       return(result)
#   }
#   if (lsmeans.calc) {
#     lsmeans.tab <- calcLSMEANS(model, data, rho, alpha.fixed, 
#                                test.effs = test.effs, method.grad = method.grad, 
#                                lsmeansORdiff = TRUE, l)
#     result$lsmeans.table <- lsmeans.tab$summ.data
#   }
#   else {
#     result$lsmeans.table <- NULL
#   }
#   if (difflsmeans.calc) {
#     lsmeans.tab <- calcLSMEANS(model, data, rho, alpha.fixed, 
#                                test.effs = test.effs, method.grad = method.grad, 
#                                lsmeansORdiff = FALSE, l)
#     result$diffs.lsmeans.table <- lsmeans.tab$summ.data
#   }
#   else {
#     result$diffs.lsmeans.table <- NULL
#   }
#   tsummary <- calculateTtest(rho, diag(rep(1, length(rho$fixEffs))), 
#                              length(rho$fixEffs), method.grad)
#   result$ttest <- list(df = tsummary[, "df"], tvalue = tsummary[, 
#                                                                 "t value"], tpvalue = tsummary[, "p-value"])
#   mf.final <- update.formula(formula(model), formula(model))
#   model <- updateModel(model, mf.final, getREML(model), contr)
#   if (inherits(model, "merMod")) 
#     model <- as(model, "merModLmerTest")
#   result$model <- model
#   return(result)
# }
# 
# calculateTtest <- function (rho, Lc, nrow.res, method.grad) 
# {
#   resultTtest <- matrix(0, nrow = nrow.res, ncol = 4)
#   colnames(resultTtest) <- c("df", "t value", "p-value", "sqrt.varcor")
#   for (i in 1:nrow.res) {
#     g <- grad(function(x) Ct.rhbc(rho, x, t(Lc[, i])), rho$param$vec.matr, 
#               method = method.grad)
#     denom <- t(g) %*% rho$A %*% g
#     varcor <- Ct.rhbc(rho, rho$param$vec.matr, t(Lc[, i]))
#     resultTtest[i, 1] <- 2 * (varcor)^2/denom
#     resultTtest[i, 2] <- (Lc[, i] %*% rho$fixEffs)/sqrt(varcor)
#     resultTtest[i, 3] <- 2 * (1 - pt(abs(resultTtest[i, 2]), 
#                                      df = resultTtest[i, 1]))
#     resultTtest[i, 4] <- sqrt(varcor)
#   }
#   return(resultTtest)
# }


# lmevarMINQUEI <- function(Y, X, HH, id) {
#   m <- ncol(X)
#   k <- ncol(HH)
#   N <- length(id)
#   options(width = 180)
#   idu <- unique(id)
#   HI <- c(list(diag(N)), alply(HH, 2, function(x) Matrix(foreach(i = idu, .combine = cbind) %do% {y <- x; y[!id %in% i] <- 0; return(y)})))
#   H <- do.call(cBind, HI)
#   Th <- solve(tcrossprod(H))
#   C <- Th-Th%*%X%*%solve(t(X)%*%Th%*%X)%*%t(X)%*%Th
#   suppressMessages(gc())
#   k <- k+1
#   l <- k*(k+1)/2
#   CY <- crossprod(C,Y)
#   QIWI <- makeQIWI(HI, l, CY, k)
#   gc()
#   CQ <- llply(QIWI[[1]], function(x) crossprod(C,x))
#   S <- ginv(fillS(CQ))
#   gc()
#   theta <- S%*%QIWI[[2]]
#   sigma <- fillSigma(theta, QIWI[[3]])
#   
#   rho <- sigma[2,2]/sum(sigma[1,1], sigma[2,2])
#   w0 <- 1 - rho
#   w1 <- rho
#   require("matrixcalc")
#   Dw <- direct.sum(w0*diag(N), w1*diag(ncol(H)-N))
#   Tw <- solve(H%*%Dw%*%t(H))
#   C <- Tw-Tw%*%X%*%solve(t(X)%*%Tw%*%X)%*%t(X)%*%Tw
#   CY <- crossprod(C,Y)
#   QIWI[[2]] <- makeWI(QIWI[[1]], CY)
#   gc()
#   CQ <- llply(QIWI[[1]], function(x) crossprod(C,x))
#   S <- ginv(fillS(CQ))
#   gc()
#   theta <- S%*%QIWI[[2]]
#   sigma <- fillSigma(theta, QIWI[[3]])
#   
#   
#   D <- solve(Reduce("+", mapply(function(th, qi) th*qi, theta[,1], QIWI[[1]])))
#   beta <- as(solve(t(X)%*%D%*%X)%*%t(X)%*%D%*%Y, "numeric")
#   names(beta) <- colnames(X)
#   sigma2 <- sigma[1,1]
#   names(sigma2) <- "sigma2"
#   tau  <- sigma[2:k, 2:k]
#   dimnames(tau) <- list(colnames(HH), colnames(HH))
#   gc()
#   return(list(gamma = beta, sigma2 = sigma2, tauBeta = tau))
# }