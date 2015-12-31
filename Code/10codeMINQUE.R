

#################################################################
# MINQUE funkcija HLM (arba misriu efektu) modeliams vertinti
#################################################################
## Funkcija skirta vertinti HLM modelius MINQUE metodu. 
## Si funkcija vertina MINQUE su bet kokiomis a priori 
##   reiksmemis.
## Galimas iteratyvus metodas, galima pasirinkti norima 
##   iteraciju skaiciu.
## Galima naudoti imties svorius, t.y. PWMINQUE metoda.
myMINQUE <- function(dt, fixed, random1 = NULL, weights = NULL, apriori = NULL,
                     iterate = FALSE, i.nr = 1, max.dif = 0.00001) {
  # dt:      data.table objektas, kurio stulpeliuose patalpinti 
  #          modelio kintamieji;
  # fixed:   character tipo kintamasis su fiksuotos dalies formule,
  #          pvz.: "Y1 ~ 1+W+X1";
  # random1: character tipo kintamasis su atsitiktines dalies
  #          formule, pvz.: "~1+X1|IDSCHOOL";
  # weights: vektorius su imties svoriu stulpeliu pavadinimais,
  #          pirma pozicija - pirmo lygio svoriai, anta - antro;
  # apriori: numeric tipo vektorius su apriori reiksmemis,
  #          pirma pozicija $\sigma^2$, kitos - isskaidyta 
  #          atsitiktiniu efektu kovariacijos matrica;
  # iterate: loginis kintamasis, ar vykdyti iteratyvu metoda;
  # i.nr:    naudojamas tik tuomet, kai iterate = TRUE, zymi
  #          iteraciju skaiciu
  # max.dif: numeric tipo kintamasis, zymi didziausia skirtuma
  #          tarp iteraciju iverciu, kuomet ciklas stabdomas.
  
  require(matrixcalc)
  require(plyr)
  require(gtools)
  require(Matrix)
  require(data.table)
  require(foreach)
  
  N <- nrow(dt)
  # Suformojami duomenys ir reikiamos matricos
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
      wg <- dt[,c(nmid1, weights), with = F]
      setnames(wg, weights, c("w1", "w2"))
      #scale
      wg[, w1 := w1*length(w1)/sum(w1), by = nmid1]
      swg <- sum(wg[, unique(w2), by = nmid1]$V1)
      wg[, w2 := w2*length(unique(id1))/swg]
      wg[, w12 := w1*w2]
      wgt1i <- dlply(wg, as.formula(paste0("~", nmid1)), function(x) x$w1)
      wgt2i <- dlply(wg, as.formula(paste0("~", nmid1)), function(x) x$w2)
      wgt12i <- dlply(wg, as.formula(paste0("~", nmid1)), function(x) x$w12)
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
  
  # Pimoji MINQUE iteracija
  iM <- iMINQUE(V, X, Y, QI, wgt2i)
  gc()
  # Jei pasirenkamas I-MINQUE
  it <- 1
  if(iterate) {
    while(iterate){
      theta <- iM$theta
      iM <- iMINQUE(iM$V, X, Y, QI, wgt2i)
      it <- it + 1
      if (all((theta - iM$theta)/theta < max.dif) | it == i.nr) iterate <- F

    }
  }
  
  # Fiksuotu efektu vertinimas
  P <- ginv(as.matrix((crossprod(X, iM$V)%*%X)))
  iM$beta <- tcrossprod(P, X)%*%iM$V%*%Y
  gc()
  beta <- as.numeric(iM$beta)
  names(beta) <- colnames(X)
  # Atsitiktiniu efektu parametru iverciu apipavidalinimas
  sigma2 <- iM$theta[1]
  names(sigma2) <- "sigma2"
  TT <- fillT(iM$theta[-1])
  dimnames(TT) <- list(clnms1, clnms1)
  
  # Y ir u iverciai, paklaidos
  V <- iM$V
  Vj <- foreach(i = unique(id1)) %do% {V[id1 %in% i,id1 %in% i,drop = F]}
  # Variances:
  ccj <- crossprod(X, V)%*%tcrossprod(Y-X%*%iM$beta)%*%V%*%X
  ranef <- mapply(function(x, y, v, z, wgt) (TT%*%crossprod(z*unique(wgt), v)%*%(y - x%*%iM$beta)),Xj, Yj, Vj, Z1, wgt2i)
  
  fit <- mapply(function(x, z, u, wgt) x%*%iM$beta+(z/unique(wgt))%*%u, Xj, Z1, ranef, wgt2i)
  fit <- do.call(rBind, fit)
  ranef <- llply(ranef, t)
  ranef <- do.call(rBind, ranef)
  rownames(ranef) <- unique(id1)
  residuals <- Y-fit
  gc()
  ranef <- as.data.frame(as.matrix(ranef))

  return(list(sigma2 = sigma2, TT = TT, beta = beta, N = N, 
              n1 = n1, ranef = ranef, fit = fit, res = residuals))
}

# Papildomos MINQUE funkcijos

iMINQUE <- function(V, X, Y, QI, wgt2i) {
  
  XVX <- ginv(as.matrix((crossprod(X, V)%*%X)))
  Cjj <- V-V%*%X%*%XVX%*%crossprod(X, V)
  CQ <- llply(QI, function(qi) Cjj%*%qi)
  
  S <- fillSMINQUE(CQ, wgt2i)
  WI <- laply(CQ, function(cq) sum(diag(cq%*%Cjj%*%tcrossprod(Y))))
  theta0 <- ginv(S)%*%matrix(WI, ncol = 1)
  
  V <- ginv(as.matrix(Reduce("+", mapply(function(qi, th){qi*th}, QI, theta0))))
  return(list(theta = theta0, V = V))
  
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

fillSMINQUE <- function(CQ, wgt2i) {
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


#################################################################
# Funkcija duomenu generavimui (naudojama pavyzdyje zemiau)
#################################################################
# Pagal Weighting in MLwiN
makePOPW <- function(M = 300){
  require(msm)
  require(data.table)
  require(MASS)
  struct <- 1:M
  N <- 30
  
  # Antro lygio duomenys
  struct <- as.data.table(cbind(IDSCHOOL = struct, Nj = N))
  struct[, W := rbinom(M, 1, 0.5)]
  struct[, Nj := N]
  
  # Atsitiktiniai efektai 
  ttN <- matrix(c(1/4, sqrt(3)/8, sqrt(3)/8, 3/4), nrow = 2)
  ddN <- mvrnorm(n = M, rep(0, 2), ttN)
  
  struct[, u0N := ddN[, 1]]
  struct[, u1N := ddN[, 2]]
  
  mu <- mean(struct$u0N)
  sdu <- sd(struct$u0N)
  i1 <- mu-1.96*sdu
  i2 <- mu+1.96*sdu
  
  # Tikimybes
  struct[(u0N >i2 | u0N < i1) & abs(u1N)>1, pj := 0.225]
  struct[(u0N >i1 & u0N < i2) & abs(u1N)>1, pj := 0.425]
  struct[(u0N >i2 | u0N < i1) & abs(u1N)<=1, pj := 0.525]
  struct[(u0N >i1 & u0N < i2) & abs(u1N)<=1, pj := 0.725]
  
  # Pirmo lygio paklaidos
  e <- struct[, list(eN = rnorm(Nj), IDSTUD = 1:Nj), by = "IDSCHOOL"]
  struct <- merge(struct, e, by = "IDSCHOOL")
  
  # Pirmo lygio duomenys
  struct[, X := rnorm(M*N)]
  struct[, X1 := X-mean(X), by = "IDSCHOOL"]
  
  # Tikimybes
  struct[eN > 0, pi := 0.25]
  struct[eN <= 0, pi := 0.75]
  struct[, pij := pi*pj]
  struct[, c("wi", "wj", "wij") := list(1/pi, 1/pj, 1/pij)]
  
  struct[, IDSTUD := paste0(IDSCHOOL, "_", IDSTUD)]
  
  # DGP
  struct[, Y1 := 1+1*W+1*X1+eN+u0N+u1N*X1]
  
  return(struct)
}

#################################################################
# Naudojimosi pavyzdys
#################################################################
# Sugeneruojama populiacija su svoriais
pop1 <- makePOPW(20)

# Vertinimas be imties svoriu
min1 <- myMINQUE(dt = pop1,
                 fixed = "Y1 ~ 1+W+X1",
                 random1 = "~1+X1|IDSCHOOL",
                 weights = NULL,
                 apriori = c(1, 1, 1, 1))

min1$beta
min1$TT
min1$sigma

# Vertinimas su svoriais
min2 <- myMINQUE(dt = pop1,
                 fixed = "Y1 ~ 1+W+X1",
                 random1 = "~1+X1|IDSCHOOL",
                 weights = c("wi", "wj"),
                 apriori = c(1, 1, 1, 1))

min2$beta
min2$TT
min2$sigma


