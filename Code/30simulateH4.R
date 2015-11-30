library(lme4)
library(MASS)


# How many "schools"?
k <- 50
# How many "students" per "school"?
n <- 30
# Which ICC?
real.icc <- 0.6
# Which fixed effects?
beta <- c(1,2,3,4)
# Covariance matrices of the X variables (a positive-definite symmetric matrix)
sigma <- matrix(c(1,0,0,0,1,0,0,0,1), length(beta)-1)
# Covariance matrix of the random effects of X (or vector of variances where zero means no random effect)
sigmaXre <- matrix(c(rep(0,9)), length(beta)-1) #intercept-only model

data1 <- Simulate(k, n, real.icc, beta, sigma, sigmaXre)



Simulate <- function(k, n, icc, beta, sigma=NA, sigmaXre=NA){
  require(data.table)
  
  nk <- n*k
  nx <- length(beta)-1
  Z <- kronecker(diag(k), rep(1,n))
  
  # X matrix
  if (is.na(sigma[1])) sigma <- diag(nx)
  X <- mvrnorm(nk, rep(0,nx), sigma)
  
  # random effects of X
  if (!is.na(sigmaXre[1])){
    Xre <- t(matrix(rnorm(k*nx,rep(1,nx),sigmaXre),nrow=nx))
    Xre <- cbind(rep(1,nk), X * (Z %*% Xre))
  } else {
    Xre <- cbind(rep(1,nk), X)
  }
  X <- cbind(rep(1,nk), X)
  
  # create a factor to keep track of which "students" are in which "school"
  group <- as.factor(rep(1:k, each=n))
  
  # generate taus and epsilons
  tecov <- diag(c(icc, rep(1-icc,n)))
  te <- mvrnorm(k, rep(0,n+1), tecov)
  epsilons <- as.vector(t(te[,-1]))
  taus <- te[,1]
  
  # generate Y data
  ran <- Z %*% taus + epsilons
  Y <- Xre %*% beta + ran
  
  output <- list(Y, X[,2],X[,3],X[,4], group)
  output <- as.data.table(do.call(cbind, output))
  setnames(output, c("Y", "X1", "X2","X3", "group"))
  return(output)
}



# How many "schools"?
k <- 50
# How many "students" per "school"?
n <- 30
# Which ICC?
real.icc <- 0.6
# Which fixed effects?
beta <- c(1,2,3,4)
# Covariance matrices of the X variables (a positive-definite symmetric matrix)
sigma <- matrix(c(1,0,0,0,1,0,0,0,1), length(beta)-1)
# Covariance matrix of the random effects of X (or vector of variances where zero means no random effect)
sigmaXre <- matrix(c(), length(beta)-1) #intercept-only model


data1 <- Simulate(k, n, real.icc, beta, sigma, sigmaXre)

mod1 <- summary(lmer( Y ~ 1 + X1 + X2 + X3 + (1|group), data1))

################################################
################################################
library(devtools)
install_github("lebebr01/simReg")

library(simglm)
fixed <- ~ 1 + act + diff
fixed_param <- c(2, 0.5, 0.3)
cov_param <- list(mean = c(0, 0), sd = c(4, 3), 
                  var_type = c("single", "single", "single"))
n <- 150

temp_single <- sim_glm(fixed = fixed, fixed_param = fixed_param, 
                       cov_param = cov_param, 
                       n = n, data_str = "single")
head(temp_single)

fixed <- ~1 + diff + act
random <- ~1 
fixed_param <- c(2, 0.5, 0.3)
random_param <- list(random_var = 7, rand_gen = "rnorm", ther_sim = TRUE)
cov_param <- list(mean = c(0, 0), sd = c(2, 1.4), 
                  var_type = c("lvl1", "lvl2"))
n <- 150
p <- 30
data_str <- "cross"
temp_cross <- sim_glm(fixed, random, random3 = NULL, fixed_param,
                      random_param, random_param3 = NULL,
                      cov_param, k = NULL, n, p,
                      data_str = data_str)
head(temp_cross)

############################################################
############################################################
rm(list = ls())
#set.seed(2345)

N <- 30
unit.df = data.frame(unit = c(1:N), a = rnorm(N))

head(unit.df, 3)

unit.df <-  within(unit.df, {
  E.alpha.given.a <-  1 - 0.15 * a
  E.beta.given.a <-  3 + 0.3 * a
})
head(unit.df, 3)

library(mvtnorm)
q = 0.2
r = 0.9
s = 0.5
cov.matrix <-  matrix(c(q^2, r * q * s, r * q * s, s^2), nrow = 2,
                      byrow = TRUE)
random.effects <-  rmvnorm(N, mean = c(0, 0), sigma = cov.matrix)

unit.df$alpha = unit.df$E.alpha.given.a + random.effects[, 1]
unit.df$beta = unit.df$E.beta.given.a + random.effects[, 2]
head(unit.df, 3)

J = 30
M = J * N  #Total number of observations
x.grid = seq(0, 8, by = 8/J)[0:30]#seq(-4, 4, by = 8/J)[0:30]  #rnorm(J, 0, 1)

within.unit.df <-  data.frame(unit = sort(rep(c(1:N), J)), j = rep(c(1:J),N), 
                              x =sample(1:8, M, replace = T))#rep(x.grid, N))#rnorm(M, 0,1))


flat.df = merge(unit.df, within.unit.df)

flat.df <-  within(flat.df, y <-  alpha + x * beta + 0.75 * rnorm(n = M))


simple.df <-  flat.df[, c("unit", "a", "x", "y")]
head(simple.df, 3)

library(lme4)
my.lmer <-  lmer(y ~ x + (1 + x | unit), data = simple.df)
cat("AIC =", AIC(my.lmer))

my.lmer <-  lmer(y ~ x + a + x * a + (1 + x | unit), data = simple.df)
summary(my.lmer)

