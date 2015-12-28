rm(list = ls())
library(nlme)
library(plyr)
library(gdata)
library(lme4)
library(mitools)
library(lmerTest)
library(robustlmm)
library(foreach)
library(Matrix)
library(gtools)
library(snow)
library(matrixcalc)
library(mitools)
library(MASS)
library(snow)
library(boot)
library(plyr)
library(mvtnorm)
library(expm)
library(foreach)
library(msm)
library(gtools)
library(MASS)
library(lme4)
library(gdata)
library(Matrix)
library(data.table)
library(ggplot2)
source("10code.R")

load("data.2011.RData")

############################################################################
varb <- c("IDSTUD", "IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "SMENG", "SMCONF", "SMVALUE", 
          "MSMENG", "MSMCONF", "MSMVALUE", "CSMENG", "CSMCONF", "CSMVALUE",
          "SMLIKE", "SBULLED", "SHEDRES", "TPROBL", "TAKADSEKM", "TMOKAPL", "KDYDIS",
          "CSMLIKE", "CSBULLED", "CSHEDRES", "MKDYDIS", "MKITKALB2",
          "MSMLIKE", "MSBULLED", "MSHEDRES", "MTPROBL", "MTAKADSEKM", "MTMOKAPL", "MKDYDIS",
          "TSATISF", "TBENDR", "TENGAGE", "TLYTIS", "TISSIL2", "TISSIL.1", 
          "MTSATISF", "MTBENDR", "MTENGAGE",
          "TISSIL.2", "TISSIL.3", "TPATIR", "TAMZIUS2", "TAMZIUS.1", "TAMZIUS.2",
          "TAMZIUS.3", "KDYDIS", "MDYDIS", "MSUDET2", "MSUDET.1", "MSUDET.2",
          "MSUDET.3", "MDISSAFE", "MSUCCESS", "MMATSHORT", "MAINCOME", "MAINCOME.1",
          "MAINCOME.2", "MAINCOME.3", "MVIETA2", "MVIETA.1", "MVIETA.2", "MVIETA.3",
          "MVIETA.4", "MVIETA.5", "MKITKALB2", "MKITKALB.1", "MKITKALB.2", "MKITKALB.3", 
          "MINSTRHWR", "STUDWGT", "SCHWGT", "TOTWGT", "WGTADJ1", "WGTADJ2", "WGTADJ3",
          "WGTFAC1", "WGTFAC2", "WGTFAC3", "IDSTRATE")
data.2011 <- data.2011[,c("BSMMAT01", varb), , with = F]

p <- ggplot(data=as.data.frame(qqnorm(data.2011$BSMMAT01, plot=F)), mapping=aes(x=x, y=y)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)+labs(x="Teoriniai kvantiliai", y = "Matematikos rezultatas")
pdf("Output/teorkv.pdf", width = 10)
p
dev.off()

varbs <- c("IDSTUD", "IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "SMENG", "SMCONF",
           "SMVALUE", 
          "MSMENG", "MSMCONF", "MSMVALUE", "CSMENG", "CSMCONF", "CSMVALUE",
          "SMLIKE", "SBULLED", "SHEDRES",
          "CSMLIKE", "CSBULLED", "CSHEDRES",
          "MSMLIKE", "MSBULLED", "MSHEDRES",
          "MDYDIS", "MSUDET2", "MSUDET.1", "MSUDET.2",
          "MSUDET.3", "MAINCOME", "MAINCOME.1",
          "MAINCOME.2", "MAINCOME.3", "MVIETA2", "MVIETA.1", "MVIETA.2",
           "MVIETA.3", "MSUCCESS",#"MMATSHORT",
          "MVIETA.4", "MVIETA.5",  
          "STUDWGT", "SCHWGT", "TOTWGT", "WGTADJ1", "WGTADJ2", "WGTADJ3",
          "WGTFAC1", "WGTFAC2", "WGTFAC3", "IDSTRATE")
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", varbs), with = F]))


########################################  
# NULL MODEL
######################################## 
lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDSCHOOL), data = data.2011, REML = T)
(sm0 <- summary(lme.0))

c0 <- confint(lme.0, method = "boot")
c0[1:2, ] <- c0[1:2, ]^2

rb01 <- try(bootREML(data = data.2011, R = 500,
                    form = "BSMMAT01 ~ 1+
               (1|IDSCHOOL)", idstrata = "IDSTRATE"))

########################################  
# STUDENT MODEL
######################################## 
lme.1 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+
               CSMCONF+
               CSHEDRES+
               #CSMLIKE+
               #CSMVALUE+
               #CSMENG+
               CSBULLED+
               (1|IDSCHOOL), data = data.2011, REML = F)
(sm1 <- summary(lme.1))


lme.2 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES+#CSMLIKE+CSMVALUE+CSMENG+
               CSBULLED+MDYDIS+MSMCONF+MSHEDRES+#MTMOKAPL+
               #MSUDET.3+MAINCOME.1+MVIETA.2+
               MSUCCESS+MAINCOME.1+MVIETA.4+
               (1|IDSCHOOL), data = data.2011, REML = F)
(sm2 <- summary(lme.2))


lme.3 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+
               CSHEDRES+#CSMLIKE+
               # CSMVALUE+
               #CSMENG+
               CSBULLED+MDYDIS+MSUCCESS+#MSMENG+
               #MSMVALUE+
               MSMLIKE+
               MSHEDRES+MAINCOME.1+MVIETA.4+#MSUDET.1+
               (1+CSHEDRES|IDSCHOOL), data = data.2011, REML = F)
(sm3 <- summary(lme.3))

rr <- ranef(lme.3, condVar =T)
ggCaterpillar(rr, F)


lme.4 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2:MVIETA.5+
               CSMCONF+CSHEDRES:MVIETA.5+
               CSBULLED:MVIETA.3+
               MDYDIS+
               MSUCCESS:MVIETA.4+
               MSMLIKE:MVIETA.4+#:as.factor(MVIETA2)+
               MSHEDRES+
               MAINCOME.1+MVIETA.4+#MSUDET.1+
               (1+CSHEDRES+CSMCONF|IDSCHOOL), data = data.2011, REML = T)
(sm4 <- summary(lme.4))
rr <- ranef(lme.4, condVar =T)
ggCaterpillar(rr, F)

fY <- fitted(lme.4)
Y <- data.2011[, c("IDSCHOOL", "BSMMAT01"), with = F]
Y$fY <- fY
R1 <- 1-sum(ddply(Y, ~IDSCHOOL, function(x) t(x$BSMMAT01-x$fY)%*%(x$BSMMAT01-x$fY))[,2])/sum(ddply(Y, ~IDSCHOOL, function(x) t(x$BSMMAT01-mean(Y$BSMMAT01))%*%(x$BSMMAT01-mean(Y$BSMMAT01)))[,2])

# lme.4 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2:MVIETA.5+
#                CSHEDRES*MDYDIS+CSHEDRES:MVIETA.5+
#                CSMCONF+CSMCONF:MVIETA.4+
#                CSMVALUE:I(MVIETA.2+MVIETA.3+MVIETA.5)+
#                CSMENG*MDYDIS+CSMENG:MVIETA.4+
#                CSBULLED:MVIETA.2+
#                MDYDIS+MDYDIS:MVIETA.1+
#                #MTMOKAPL:as.factor(MVIETA2)+
#                MSHEDRES+MSMCONF+
#                MVIETA.2+MSUDET.1+
#                (1+CSHEDRES+CSMCONF|IDSCHOOL), data = data.2011, REML = F)
# (sm4 <- summary(lme.4))
varbs <- c("IDSTUD", "IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "MVIETA.5",
             "CSMCONF", "CSHEDRES", "MDYDIS", "CSHEDRES",
             "CSBULLED", "MVIETA.3", "MSUCCESS", "MVIETA.4",
             "MSMLIKE", "MSHEDRES", "MAINCOME.1",
           "STUDWGT", "SCHWGT", "TOTWGT", "WGTADJ1", "WGTADJ2", "WGTADJ3",
           "WGTFAC1", "WGTFAC2", "WGTFAC3", "IDSTRATE")
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", varbs), with = F]))

rb0 <- try(bootREML(data = data.2011, R = 100,
                    form = "BSMMAT01 ~ 1+SSEX+STHMWRK2:MVIETA.5+
               CSMCONF+CSHEDRES*MDYDIS+CSHEDRES:MVIETA.5+
               CSBULLED:MVIETA.3+
               MDYDIS+
               MSUCCESS:MVIETA.4+
               MSMLIKE:MVIETA.5+MSMLIKE:MVIETA.4+#:as.factor(MVIETA2)+
               MSHEDRES+
               MAINCOME.1+MVIETA.4+
               (1+CSHEDRES+CSMCONF|IDSCHOOL)", idstrata = "IDSTRATE"))

mw1 <- try(bootMINQUE2(data = data.2011, R = 100,
                    fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2:MVIETA.5+
               CSMCONF+CSHEDRES*MDYDIS+CSHEDRES:MVIETA.5+
               CSBULLED:MVIETA.3+MDYDIS+MSUCCESS:MVIETA.4+
               MSMLIKE:MVIETA.5+MSMLIKE:MVIETA.4+
               MSHEDRES+MAINCOME.1+MVIETA.4",
               random="~1+CSHEDRES+CSMCONF|IDSCHOOL", idstrata = "IDSTRATE", wgt = NULL))

m1 <- try(bootMINQUE2(data = data.2011, R = 100,
                      fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2:MVIETA.5+
               CSMCONF+CSHEDRES*MDYDIS+CSHEDRES:MVIETA.5+
               CSBULLED:MVIETA.3+MDYDIS+MSUCCESS:MVIETA.4+
               MSMLIKE:MVIETA.5+MSMLIKE:MVIETA.4+
               MSHEDRES+MAINCOME.1+MVIETA.4",
                      random="~1+CSHEDRES+CSMCONF|IDSCHOOL", idstrata = "IDSTRATE", BFUN = bootSample))


# Trecio lygio netraukiam, nes AIC siek tiek padideja, bet informacijos sis pridejimas neprideda

# bandom prideti pirmo lygio atsitiktinius kintamuosius
lme.2 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
               #SMLIKE+
               #SMVALUE+SMENG+SBULLED+
               (1+SSEX|IDSCHOOL), data = data.2011)
(sm2 <- summary(lme.2))
# SSEX netinka, nes AIC padideja ir info neatnesa. Galima paziuret i simtakoji
rr <- ranef(lme.2, condVar =T)
#Sys.setlocale("LC_ALL", "Lithuanian")
colnames(rr[[1]]) <- c("Atsitiktinis postū mis", "Mokinio lyties atsitiktinis efektas")
pdf("Output/2lvlmokSSEXSimtakojis.pdf", width = 10)
ggCaterpillar(rr, T)
dev.off()

# Nors pagal simtakoji ir AIC iverciai pagereje, bet dispersijos stipriai isauga, o tai reiskia, kad ne paaiskinam dispersija, o ja sukeliam
lme.3 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
               #SMLIKE+
               #SMVALUE+SMENG+SBULLED+
               (1+SMCONF|IDSCHOOL), data = data.2011)
(sm3 <- summary(lme.3))
rr <- ranef(lme.3, condVar =T)
#Sys.setlocale("LC_ALL", "Lithuanian")
colnames(rr[[1]]) <- c("Atsitiktinis postū mis", "Mokinio pasitikė jimo savimi matematikoje atsitiktinis efektas")
pdf("Output/2lvlmokSMCONFSimtakojis.pdf", width = 12)
ggCaterpillar(rr, T)
dev.off()

### Pailiekam prie paprasto modelio be atsitiktiniu efektu (dar paklausti!)




# plot(fitted(lme.1), residuals(lme.1),
#      xlab = "Fitted Values", ylab = "Residuals")
# abline(h=0, lty=2)
# lines(smooth.spline(fitted(lme.1), residuals(lme.1)))
# 
# library(car)
# qqPlot(residuals(lme.1, type='pearson'))
# qqPlot(ranef(lme.1)[[1]][,1])
# 
# new.grp<-groupedData(BSMMAT01 ~ 1+SSEX|IDSCHOOL, data=data.2011)
# 
# library(lattice)
# trellis.par.set(col.whitebg())
# plot(new.grp)
# plot(new.grp,display=1)
# 
# plot(new.grp,display=1,collapse=1)


########################################  
# SCHOOL MODEL
########################################
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {x$MLYTIS <- mean(x$TLYTIS, na.rm = T);return(x)})

lme.3 <- lmer(BSMMAT01 ~ 1+MSUDET.3+I(MVIETA.5+MVIETA.4)+MDYDIS+
                MAINCOME.1+MAINCOME.3+TMOKAPL+TAKADSEKM+
                KDYDIS+TBENDR+
#                 MKITKALB.1+MAINCOME.1+MAINCOME.3+MINSTRHWR+
#                 I(MVIETA.1+MVIETA.2)+MSUDET.3+MDYDIS+MAINCOME.3+
#                 MMATSHORT:MVIETA.5+
#                 I(MVIETA.4+MVIETA.5)+I(MKITKALB.2+MKITKALB.3)+
                (1|IDSCHOOL), data = data.2011)
(sm3 <- summary(lme.3))

lme.4 <- lmer(BSMMAT01 ~ 1+MSUDET.3+I(MVIETA.5+MVIETA.4)+MDYDIS+
                MAINCOME.1+MAINCOME.3+TMOKAPL+TAKADSEKM+
                KDYDIS+TBENDR+
                #                 MKITKALB.1+MAINCOME.1+MAINCOME.3+MINSTRHWR+
                #                 I(MVIETA.1+MVIETA.2)+MSUDET.3+MDYDIS+MAINCOME.3+
                #                 MMATSHORT:MVIETA.5+
                #                 I(MVIETA.4+MVIETA.5)+I(MKITKALB.2+MKITKALB.3)+
                (1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm4 <- summary(lme.4))
########################################  
# Total MODEL
########################################
varb <- c("IDSTUD", "IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "SMENG", "SMCONF",
          "SHEDRES", "TAKADSEKM", "TMOKAPL", "KDYDIS", "TBENDR",
          "MDYDIS",  "MSUDET.1", "MSUDET.2",
          "MSUDET.3", "MAINCOME.1", "MAINCOME.2", "MAINCOME.3",
          "MVIETA.1", "MVIETA.2", "MVIETA.3",
          "MVIETA.4", "MVIETA.5", "STUDWGT", "SCHWGT", "IDSTRATE")
data.2011 <- drop.levels(na.omit(data.2011[,c("BSMMAT01", varb), with = F]))

########################################  
# NULL MODEL
######################################## 
lme.0 <-lmer(BSMMAT01 ~ 1+(1|IDCLASS)+(1|IDSCHOOL), data = data.2011)
(sm0 <- summary(lme.0))
lme.01 <-lmer(BSMMAT01 ~ 1+(1|IDSCHOOL), data = data.2011)
(sm01 <- summary(lme.01))


rb0 <- bootREML(data = data.2011, R = 500,
         form = "BSMMAT01 ~ 1+(1|IDSCHOOL)")



ci0 <- confint(lme.01)
ci0[1:2,] <- ci0[1:2,]^2

anova(lme.0,lme.01)
########################################
lme.4 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
               MSUDET.3+
               I(MVIETA.5+MVIETA.4)+MDYDIS+
               MAINCOME.1+MAINCOME.3+TMOKAPL+TAKADSEKM+
               KDYDIS+TBENDR+
               (1|IDSCHOOL), data = data.2011)
(sm4 <- summary(lme.4))
########################################
lme.5 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
               #MSUDET.3+
               I(MVIETA.5+MVIETA.4)+MDYDIS+
               MAINCOME.1+TAKADSEKM+
               #MAINCOME.3+
               #TMOKAPL+
               KDYDIS+TBENDR+
               (1|IDSCHOOL), data = data.2011)
(sm5 <- summary(lme.5))
########################################
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {x$MSHEDRES <- mean(x$SHEDRES, na.rm = T);return(x)})
#data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {x$SHEDRES2 <- x$SHEDRES-mean(x$SHEDRES, na.rm = T);return(x)})
#data.2011$SSEX <- abs(data.2011$SSEX-1)

lme.6 <-lmer(BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+
               #SSEX:SMCONF+SSEX:SHEDRES+
               SSEX:MSUDET.3+
               I(MVIETA.5+MVIETA.4)+MDYDIS+
               MAINCOME.1+MAINCOME.3+
               TMOKAPL+KDYDIS+TBENDR+
               (1|IDSCHOOL), data = data.2011)
(sm6 <- summary(lme.6))
rr <- ranef(lme.6, condVar =T)
#Sys.setlocale("LC_ALL", "Lithuanian")
#colnames(rr[[1]]) <- c("Atsitiktinis postū mis", "Mokinio pasitikė jimo savimi matematikoje atsitiktinis efektas")

ggCaterpillar(rr, T)

############# GLS #############################
gls1 <- myIGLS(dt = data.2011,
               fixed = "BSMMAT01 ~ 1",
               random1 = "~1|IDSCHOOL",
               weights = c("STUDWGT", "SCHWGT"))

gls1 <- myIGLS(dt = data.2011,
               fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+SSEX:MSUDET.3+I(MVIETA.5+MVIETA.4)+MDYDIS+MAINCOME.1+MAINCOME.3+TMOKAPL+KDYDIS+TBENDR",
               random1 = "~1|IDSCHOOL",
               weights = c("STUDWGT", "SCHWGT"))
cbind(gls1$beta, sqrt(diag(gls1$cov)))

############# MINQUE #############################
min1 <- myMINQUE(dt = data.2011,
                  fixed = "BSMMAT01 ~ 1",
                  random1 = "~1|IDSCHOOL",
                  weights = c("STUDWGT", "SCHWGT"))
bb <- bootMINQUE2(fixed = "BSMMAT01 ~ 1", 
                  random = "~1|IDSCHOOL", wgt = c("STUDWGT", "SCHWGT"),
                  idstrata = "IDSTRATE", R = 100, data = data.2011)
bb1 <- bootMINQUE2(fixed = "BSMMAT01 ~ 1", 
                  random = "~1|IDSCHOOL", wgt = NULL,
                  idstrata = "IDSTRATE", R = 100, data = data.2011)
save(bb1, file = "Output/TIMSSLtbootNullnowg.RData")
#save(bb, file = "Output/TIMSSLtbootNull.RData")
min1 <- myMINQUE2(dt = data.2011,
               fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+SSEX:MSUDET.3+I(MVIETA.5+MVIETA.4)+MDYDIS+MAINCOME.1+MAINCOME.3+TMOKAPL+KDYDIS+TBENDR",
               random1 = "~1|IDSCHOOL",
               weights = c("STUDWGT", "SCHWGT"))
cbind(min1$beta, sqrt(diag(min1$cov)))


bbIGLS1 <- bootMINQUE(data = , FUN  = bfunIGLS, R = 100,
                      fixed = "Y4 ~ 1", random = "~1|IDSCHOOL",
                      strata = smpl$IDSCHOOL, #hierar = T, 
                      wgt = c("w1", "w2"))
bb <- bootMINQUE2(fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+SSEX:MSUDET.3+I(MVIETA.5+MVIETA.4)+MDYDIS+MAINCOME.1+MAINCOME.3+TMOKAPL+KDYDIS+TBENDR", R = 1)

bb2 <- bootMINQUE2(fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+SSEX:MSUDET.3+I(MVIETA.5+MVIETA.4)+MDYDIS+MAINCOME.1+MAINCOME.3+TMOKAPL+KDYDIS+TBENDR", FUN  = myIGLS, R=3)
#save(bb2, file = "Output.IGLS.6.model")
t0 <- myIGLS(dt = data.2011,
               fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2+SMCONF+SHEDRES+SSEX:MSUDET.3+I(MVIETA.5+MVIETA.4)+MDYDIS+MAINCOME.1+MAINCOME.3+TMOKAPL+KDYDIS+TBENDR",
               random1 = "~1|IDSCHOOL",
               weights = c("STUDWGT", "SCHWGT"))

library(foreign)
varnames = c("BSMMAT01", "IDSTUD", "IDSCHOOL","IDCLASS", "SSEX","STHMWRK2"  ,
             "SMENG", "SMCONF","SHEDRES","TASEKM","TMOKAPL", "KDYDIS"  ,   
             "TBENDR","MDYDIS",  "MSUDET1",   "MSUDET2",   "MSUDET3"  ,
             "MINCOME1", "MINCOME2", "MINCOME3", "MVIETA1" ,  "MVIETA2" , 
             "MVIETA3",   "MVIETA4",   "MVIETA5",   "STUDWGT", "SCHWGT","IDSTRATE")
foreign:::writeForeignSPSS(data.2011, datafile="Output/data.2011.sav", codefile="Output/data.2011.sps",
              varnames = varnames)
mokdt <- unique(data.2011[, c("IDSCHOOL","MDYDIS",  "MSUDET.1",   "MSUDET.2",   "MSUDET.3"  ,
                       "MAINCOME.1", "MAINCOME.2", "MAINCOME.3", "MVIETA.1" ,  "MVIETA.2" , 
                       "MVIETA.3",   "MVIETA.4",   "MVIETA.5",   "SCHWGT","IDSTRATE")])

foreign:::writeForeignSPSS(mokdt, datafile="Output/mok.sav", codefile="Output/mok.sps",
                           varnames = c("IDSCHOOL", "MDYDIS",  "MSUDET1",   "MSUDET2",   "MSUDET3"  ,
                                        "MINCOME1", "MINCOME2", "MINCOME3", "MVIETA1" ,  "MVIETA2" , 
                                        "MVIETA3",   "MVIETA4",   "MVIETA5", "SCHWGT","IDSTRATE"))

names(data.2011)


###########################
# pvz
n1 <- 10
beta0 <- 50+rnorm(n1, 0, sqrt(0.25))
df <- data.frame(IDCLASS = 1:10, beta0 = beta0)
df <- foreach(ii = 1:10, .combine = rbind) %do% {data.frame(df[ii,], IDINDIV = paste(ii, 1:5, sep = ""))}
df$X <- rnorm(nrow(df), 10, sqrt(50))
df$e <- rnorm(nrow(df), 0, sqrt(0.5))
df$y <- df$beta0+4*df$X+df$e

min1 <- myMINQUE2(dt = df,
                  fixed = " y~ 1+X",
                  random1 = "~1|IDCLASS",
                  weights = NULL)

min1$beta
min1$TT
min1$sigma

df$idstrata  <-  1
bb <- bootMINQUE2(fixed = "y ~ 1+X", idschool = "IDCLASS",
                  idstud = "IDINDIV", random = "~1|IDCLASS", wgt = NULL,
                  idstrata = "idstrata", R = 100, data = df)
bb$beta
bb$TT
bb$sigma
