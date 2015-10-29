gaussmix <- function(nsim,mean_1,mean_2,std_1,std_2,alpha){
  U <- runif(nsim)
  I <- as.numeric(U<alpha)
  y <- I*rnorm(nsim,mean=mean_1,sd=std_1)+
    (1-I)*rnorm(nsim,mean=mean_2,sd=std_2)
  return(y)
}

z1 <- gaussmix(1000,0,0,1,6,0.95)
z1_standardized <- (z1-mean(z1))/sqrt(var(z1))
z2 <- gaussmix(1000,3.9,0.5,5,0.5,0.80)
z2_standardized <- (z2-mean(z2))/sqrt(var(z2))
z3 <- rlnorm(1000)
z3_standardized <- (z3-mean(z3))/sqrt(var(z3))

par(mfrow=c(2,3))
hist(z1_standardized,xlim=c(-10,10),ylim=c(0,500),
     main="Histogram of 95% of N(0,1) and 5% of N(0,36)",
     col="blue",xlab=" ")
hist(z2_standardized,xlim=c(-10,10),ylim=c(0,500),
     main="Histogram of 80% of N(0,1) and 10% of N(3,1)",
     col="blue",xlab=" ")
hist(z3_standardized,xlim=c(-10,10),ylim=c(0,500),
     main="Histogram of samples of LN(0,1)",col="blue",xlab=" ")
##
plot(z1_standardized,type='l',
     main="1000 samples from a mixture N(0,1) and N(0,36)",
     col="blue",xlab="Samples",ylab="Mean",ylim=c(-10,10))
plot(z2_standardized,type='l',
     main="1000 samples from a mixture N(0,1) and N(3,1)",
     col="blue",xlab="Samples",ylab="Mean",ylim=c(-10,10))
plot(z3_standardized,type='l',
     main="1000 samples from LN(0,1)",
     col="blue",xlab="Samples",ylab="Mean",ylim=c(-10,10))



library(MASS)
S1 <- matrix(c(1300,-180,-180,480),nrow=2,byrow=TRUE)
mu1 <- c(1,1)
S2 <- matrix(c(1300,-180,-180,480),nrow=2,byrow=TRUE)
mu2 <- c(-1,-1)

n <- 1000
p1 <- 0.8
n1 <- rbinom(1,size=n,prob=p1)  ## how many from first distribution?
n2 <- n-n1
val1 <- mvrnorm(n1,mu=mu1,Sigma=S1)
val2 <- mvrnorm(n2,mu=mu2,Sigma=S2)
allval <- rbind(val1,val2)      ## combine
allval <- allval[sample(n,n),]
