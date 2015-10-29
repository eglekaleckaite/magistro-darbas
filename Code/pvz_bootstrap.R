d=read.delim("http://www.public.iastate.edu/~dnett/S511/SeedlingDryWeight2.txt")
d

plot(d[,2],d[,4]+rnorm(56,0,.2),
     xlab="Tray",ylab="Seedling Dry Weight",
     col=2*(1+(d[,1]=="B")),pch="-",cex=2)
legend("topright",c("Genotype A","Genotype B"),fill=c(2,4),border=c(2,4))

Tray=as.factor(d$Tray)
Geno=as.factor(d$Genotype)
y=d$SeedlingWeight

#Load the lme4 package.

library(lme4)

#Fit a linear mixed effects model with
#fixed effects for genotypes
#and random effects for trays.

o=lmer(y~Geno+(1|Tray))
summary(o)

#Note that lmer computes t-statistics
#but reports no p-values.  

beta.hat=fixef(o)
beta.hat

se=sqrt(diag(vcov(o)))
se

#We can use the simulate function to
#generate a new y vector by simulating
#from the fitted linear mixed effects model.

so=simulate(o)
so

#Now let's use parametric bootstrap to
#approximate the distribution of
#t=(estimate-parameter)/se(estimate).

#In this case, we will focus on
#estimation of beta2, which represents
#difference between the genotype means.

B=10000
tstar=rep(0,B)
set.seed(781)
for(b in 1:B){
  ystar=unlist(drop(simulate(o)))
  attributes(ystar) <- NULL
  ostar=lmer(ystar~Geno+(1|Tray))
  tstar[b]=(fixef(ostar)[2]-beta.hat[2])/sqrt(vcov(ostar)[2,2])
}

#Let's take a look at the distribution
#of bootstrap replications of t.

hist(tstar,probability=T,nclass=50,col=4)
box()


tquant=quantile(tstar,c(.025,.975))
tquant

#Lower endpoint of bootstrap confidence interval

beta.hat[2]-tquant[2]*se[2]

#Upper endpoint of bootstrap confidence interval

beta.hat[2]-tquant[1]*se[2]

#lme uses a t with 6 d.f. to approximate
#the distribution of t.
#Is that a good approximation in this case
#according to the bootstrap?

tt=seq(-8,8,by=.01)
lines(tt,dt(tt,6),col=2,lwd=2)

#The bootstrap distribution of t looks very similar
#to a t distribution with 6 degrees of freedom.
#However, such figures don't necessarily show
#whether there is good agreement in the tails of the
#distribution. Let's check the relevant quantiles.

qt(c(.025,.975),6)

tquant=quantile(tstar,c(.025,.975))
tquant

#Let's examine a parametric bootstrap test of
#the null hypothesis that the two genotype means
#are equal.

#The test statistic of interest is

T=abs(beta.hat[2]/se[2])
T

#Estimate parameters under the reduced model.

onull=lmer(y~1+(1|Tray))

#Generate T* values.

B=10000
Tstar=rep(0,B)
set.seed(3432)
for(b in 1:B){
  ystar=unlist(drop(simulate(onull)))
  attributes(ystar) <- NULL
  ostar=lmer(ystar~Geno+(1|Tray))
  Tstar[b]=abs(fixef(ostar)[2]/sqrt(vcov(ostar)[2,2]))
}

#Plot the distribution of T* with T.

hist(Tstar,col=4)
abline(v=T,col=2,lwd=2)
box()

#Compute parametric bootstrap p-value.

mean(Tstar>=T)

#This is very similar to the p-value produced by
#lme in this case.

library(nlme)
summary(lme(y~Geno,random=~1|Tray,data=d))

#Here is an example of a test procedure using
#nonparametric bootstrap resampling.

#Generate data from different distributions.
#Each distribution has a mean and variance.

set.seed(1393)
y1=9+2*rt(10,3)
y2=rnorm(15,6,1)

y1
y2

pp=function(y1,y2){
  x=rep(1:2,c(length(y1),length(y2)))
  plot(x,c(y1,y2),pch=16,col=2*x,xlim=c(0.5,2.5),
       ylab="Response",xlab="Treatment",axes=F)
  axis(2)
  axis(1,labels=1:2,at=1:2)
  my1=mean(y1)
  my2=mean(y2)
  if(abs(my1-my2)<0.000000001)
  {
    abline(h=my1,col="purple")
  }
  else
  {
    abline(h=my1,col=2)
    abline(h=my2,col=4)
  }
  box()
}

pp(y1,y2)

#Suppose we want to test if the means of the
#two distributions are equal.

#Compute a test statistic.

T=abs(t.test(y1,y2)$statistic)
T

#Compute zij values.

z1=y1-mean(y1)+(mean(y1)+mean(y2))/2

z2=y2-mean(y2)+(mean(y1)+mean(y2))/2

mean(z1)
mean(z2)

pp(z1,z2)

#Generate T* values.

B=10000
Tstar=rep(0,B)
set.seed(2398)
for(b in 1:B){
  z1star=sample(z1,replace=T)
  z2star=sample(z2,replace=T)
  Tstar[b]=abs(t.test(z1star,z2star)$statistic)
}

#Plot the distribution of T* with T.

hist(Tstar,col=4)
abline(v=T,col=2,lwd=2)
box()

#Compute parametric bootstrap p-value.

mean(Tstar>=T)

#Find the critical value of the 0.05 level test.

cv=quantile(Tstar,.95)

mean(Tstar>=cv)

#Add the critical value to the plot.

abline(v=cv,col=3,lwd=2)

#Use bootstrap resampling to estimate the power
#of the test when the mean of F1 is 3 units bigger
#than the mean of F2.

pp(z1+3,z2)

B=10000
Tstar=rep(0,B)
set.seed(2448)
for(b in 1:B){
  z1star=sample(z1+3,replace=T)
  z2star=sample(z2,replace=T)
  Tstar[b]=abs(t.test(z1star,z2star)$statistic)
}

mean(Tstar>=cv)

#Use bootstrap resampling to estimate the power
#of the test when the mean of F1 is 5 units bigger
#than the mean of F2.

pp(z1+5,z2)

B=10000
Tstar=rep(0,B)
set.seed(2888)
for(b in 1:B){
  z1star=sample(z1+5,replace=T)
  z2star=sample(z2,replace=T)
  Tstar[b]=abs(t.test(z1star,z2star)$statistic)
}

mean(Tstar>=cv)
#######################################################################
example<-read.table("example.txt", header=T)
fm1<-lmer(SCIENCE~URBAN + (1|GROUP), example)
summary(fm1)
coef(fm1)

with(fm1, {
  cc <- coef(.)$GROUP
  xyplot(SCIENCE ~ URBAN | GROUP,
         index.cond = function(x, y) coef(lm(y ~ x))[1],
         panel = function(x, y, groups, subscripts, ...) {
           panel.grid(h = -1, v = -1)
           panel.points(x, y, ...)
           subj <- as.character(GROUP[subscripts][1])
           panel.abline(cc[subj,1], cc[subj, 2])
         })
})


library(SASmixed)
library(lme4)
library(boot)

fm1Cult <- lmer(drywt ~ Inoc + Cult + (1|Block) + (1|Cult), data=Cultivation)
fixef(fm1Cult)


boot.fn <- function(data, indices){
  data <- data[indices, ]
  mod <- lmer(drywt ~ Inoc + Cult + (1|Block) + (1|Cult), data=data)
  fixef(mod)
}

set.seed(12345)
Out <- boot(data=Cultivation, statistic=boot.fn, R=99)
Out
###########################################
fm01ML <- lmer(Yield ~ 1|Batch, Dyestuff, REML = FALSE)
## see ?"profile-methods"
mySumm <- function(.) { s <- sigma(.)
                        c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) }
(t0 <- mySumm(fm01ML))

mySumm2 <- function(.) {
  c(beta=fixef(.),sigma=sigma(.),sig01=unlist(VarCorr(.)))
}

(t1 <- mySumm2(fm01ML))


system.time( boo01 <- bootMer(fm01ML, mySumm2, nsim = 100) )

## intercept
(bCI.1 <- boot.ci(boo01, index=1, type=c("norm", "basic", "perc")))# beta

## Residual standard deviation - original scale:
(bCI.2  <- boot.ci(boo01, index=2, type=c("norm", "basic", "perc")))
## Residual SD - transform to log scale:
(bCI.2l <- boot.ci(boo01, index=2, type=c("norm", "basic", "perc"),
                   h = log, hdot = function(.) 1/., hinv = exp))

## Among-batch variance:
(bCI.3 <- boot.ci(boo01, index=3, type=c("norm", "basic", "perc")))# sig01

## Graphical examination:
plot(boo01,index=3)

## Check stored values from a longer (1000-replicate) run:
load(system.file("testdata","boo01L.RData",package="lme4"))
plot(boo01L,index=3)