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

data.2011[, WGTFAC2 := WGTFAC2*WGTFAC3]
data.2011[, WTOT := WGTFAC2*WGTFAC1]
#############################################################################
#############################################################################
# Nulinis
#############################################################################
#############################################################################
wm <- data.2011[, sum(BSMMAT01*WTOT)/sum(WTOT)]

n <- mean(unlist(data.2011[, length(BSMMAT01), by = IDSCHOOL][, 2, with = F]))

rm0 <- lmer("BSMMAT01 ~ 1+(1|IDSCHOOL)", data.2011)
sm0 <- summary(rm0)

ICC(sm0, n)
R12(rm0)



min01 <- myMINQUE(dt = data.2011,
                     fixed = "BSMMAT01 ~ 1",
                     random1 = "~1|IDSCHOOL",
                     weights = NULL,
                     apriori = c(1, 1))

icc01 <- min01$TT/(min01$TT+min01$sigma2)

deff01 <- 1+(n-1)*icc01

r101 <- 1-sum(min01$res^2)/sum((rm0@frame[,1]-mean(rm0@frame[,1]))^2)


wmin01 <- myMINQUE(dt = data.2011,
                  fixed = "BSMMAT01 ~ 1",
                  random1 = "~1|IDSCHOOL",
                  weights = c("WGTFAC2", "WGTFAC1"),
                  apriori = c(1, 1))

iccw01 <- wmin01$TT/(wmin01$TT+wmin01$sigma2)

deffw01 <- 1+(n-1)*iccw01

r1w01 <- 1-sum(wmin01$res^2)/sum((rm0@frame[,1]-wm)^2)


iv <- summary(lm("BSMMAT01 ~ 1", data.2011))
iva <- data.2011[, as.list(c(coef(lm(BSMMAT01 ~ 1)))), by = "IDSCHOOL"]
setnames(iva, c("IDSCHOOL", "b0"))
tau0 <- try(gls(b0~1, data = iva))
if (class(tau0) == "try-error") browser()
iva$res0 <- tau0$residuals
apriori1 <- c(iv$sigma^2, tau0$sigma^2)/iv$sigma^2

min0th <- myMINQUE(dt = data.2011,
                  fixed = "BSMMAT01 ~ 1",
                  random1 = "~1|IDSCHOOL",
                  weights = NULL,
                  apriori = apriori1)

icc0th <- min0th$TT/(min0th$TT+min0th$sigma2)

deff0th <- 1+(n-1)*icc0th

r10th <- 1-sum(min0th$res^2)/sum((rm0@frame[,1]-mean(rm0@frame[,1]))^2)

iv <- summary(lm(formula = "BSMMAT01 ~ 1", data = data.2011, weights = data.2011$WTOT))
iva <- data.2011[, as.list(c(coef(lm(BSMMAT01 ~ 1, weights = WGTFAC2)), 
                        WGTFAC1 = unique(WGTFAC1))), by = "IDSCHOOL"]
setnames(iva, c("IDSCHOOL", "b0", "w2"))
iva <- na.omit(iva)
tau0 <- try(summary(lm(b0~1, data = iva, weights = w2)))
if (class(tau0) == "try-error") browser()
iva$res0 <- tau0$residuals
apriori2 <- c(iv$sigma^2, tau0$sigma^2)/iv$sigma^2


wmin0th <- myMINQUE(dt = data.2011,
                   fixed = "BSMMAT01 ~ 1",
                   random1 = "~1|IDSCHOOL",
                   weights = c("WGTFAC2", "WGTFAC1"),
                   apriori = apriori2)

iccw0th <- wmin0th$TT/(wmin0th$TT+wmin0th$sigma2)

deffw0th <- 1+(n-1)*iccw0th

r1w0th <- 1-sum(wmin0th$res^2)/sum((rm0@frame[,1]-wm)^2)

#############################################################################
#############################################################################
# Galutinis
#############################################################################
#############################################################################
rm1 <- lmer("BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES+MVIETA.2+
              MSUCCESS+MDYDIS+
            (1+CSHEDRES|IDSCHOOL)", data.2011)
sm1 <- summary(rm1)

r1r1 <- R12(rm1)

min11 <- myMINQUE(dt = data.2011,
                  fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES+MVIETA.2+
                           MSUCCESS+MDYDIS",
                  random1 = "~1+CSHEDRES|IDSCHOOL",
                  weights = NULL,
                  apriori = c(1, 1, 1, 1))

r111 <- 1-sum(min11$res^2)/sum((rm1@frame[,1]-mean(rm1@frame[,1]))^2)


wmin11 <- myMINQUE(dt = data.2011,
                   fixed ="BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES+MVIETA.2+
                           MSUCCESS+MDYDIS",
                   random1 = "~1+CSHEDRES|IDSCHOOL",
                   weights = c("WGTFAC2", "WGTFAC1"),
                   apriori = c(1, 1, 1, 1))

r1w11 <- 1-sum(wmin11$res^2)/sum((rm1@frame[,1]-wm)^2)


iv <- summary(lm("BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES+MVIETA.2+
                 MSUCCESS+MDYDIS", data.2011))
iva <- data.2011[, as.list(c(coef(lm(BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES))[c(1,5)], 
                             W1=unique(MVIETA.2), W2=unique(MSUCCESS), W3=unique(MDYDIS))), by = "IDSCHOOL"]
setnames(iva, c("IDSCHOOL", "b0", "b1", "W1", "W2", "W3"))
tau0 <- try(gls(b0~1+W1+W2+W3, data = iva))
if (class(tau0) == "try-error") browser()
iva$res0 <- tau0$residuals
iva <- na.omit(iva)
tau1 <- try(gls(b1~1, data = iva))
iva$res1 <- tau1$residuals
apriori1 <- c(iv$sigma^2, tau0$sigma^2, 
              cov(iva$res0, iva$res1), tau1$sigma^2)/iv$sigma^2

min1th <- myMINQUE(dt = data.2011,
                   fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES+MVIETA.2+
                           MSUCCESS+MDYDIS",
                   random1 = "~1+CSHEDRES|IDSCHOOL",
                   weights = NULL,
                   apriori = apriori1)


r11th <- 1-sum(min1th$res^2)/sum((rm1@frame[,1]-mean(rm1@frame[,1]))^2)



iv <- summary(lm("BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES+MVIETA.2+
                 MSUCCESS+MDYDIS", data.2011, weights = WTOT))
iva <- data.2011[, as.list(c(coef(lm(BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES))[c(1,5)], 
                             W1=unique(MVIETA.2), W2=unique(MSUCCESS), W3=unique(MDYDIS), w = unique(WGTFAC1))), by = "IDSCHOOL"]
setnames(iva, c("IDSCHOOL", "b0", "b1", "W1", "W2", "W3", "w"))
tau0 <- try(summary(lm(b0~1+W1+W2+W3, data = iva, weights = w)))
if (class(tau0) == "try-error") browser()
iva$res0 <- tau0$residuals
iva <- na.omit(iva)
tau1 <- try(summary(lm(b1~1, data = iva, weights = w)))
iva$res1 <- tau1$residuals
apriori2 <- c(iv$sigma^2, tau0$sigma^2, 
              cov(iva$res0, iva$res1), tau1$sigma^2)/iv$sigma^2

wmin1th <- myMINQUE(dt = data.2011,
                    fixed = "BSMMAT01 ~ 1+SSEX+STHMWRK2+CSMCONF+CSHEDRES+MVIETA.2+
                           MSUCCESS+MDYDIS",
                    random1 = "~1+CSHEDRES|IDSCHOOL",
                    weights = c("WGTFAC2", "WGTFAC1"),
                    apriori = apriori2)


r1w1th <- 1-sum(wmin1th$res^2)/sum((rm1@frame[,1]-wm)^2)



cc <- cbind(coef(sm1)[,1], min11$beta, wmin11$beta, min1th$beta, wmin1th$beta)
xtable(cc, digits = 3)

ss <- matrix(c(sm1$sigma^2, min11$sigma2, wmin11$sigma2, min1th$sigma2, wmin1th$sigma2), nrow = 1)
xtable(ss, digits = 3)

tt <- cbind(as.numeric(sm1$varcor[[1]]), as.numeric(min11$TT), as.numeric(wmin11$TT), as.numeric(min1th$TT), as.numeric(wmin1th$TT))
xtable(tt, digits = 3)

rr <- matrix(c(r1r1, r111, r1w11, r11th, r1w1th), nrow =1)
xtable(rr, digits = 3)
