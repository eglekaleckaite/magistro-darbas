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
library(simFrame)
library(foreach)
library(msm)
library(gtools)
library(MASS)
library(lme4)
library(gdata)
library(Matrix)
library(survey)
library(ggplot2)
library(data.table)
library(scales)
source("10code.R")
Sys.setlocale("LC_ALL", "Lithuanian")

load("data.2011.RData")

############################################################################
varb <- c("IDSTUD", "IDSCHOOL", "IDCLASS", "SSEX", "STHMWRK2", "SMENG", "SMCONF", "SMVALUE", 
          "SMLIKE", "SBULLED", "SHEDRES", "TPROBL", "TAKADSEKM", "TMOKAPL", "TCONF",
          "TSATISF", "TBENDR", "TENGAGE", "TLYTIS", "TISSIL2", "TISSIL.1", 
          "TISSIL.2", "TISSIL.3", "TPATIR", "TAMZIUS2", "TAMZIUS.1", "TAMZIUS.2",
          "TAMZIUS.3", "KDYDIS", "MDYDIS", "MSUDET2", "MSUDET.1", "MSUDET.2",
          "MSUDET.3", "MDISSAFE", "MSUCCESS", "MMATSHORT", "MAINCOME", "MAINCOME.1",
          "MAINCOME.2", "MAINCOME.3", "MVIETA2", "MVIETA.1", "MVIETA.2", "MVIETA.3",
          "MVIETA.4", "MVIETA.5", "MKITKALB2", "MKITKALB.1", "MKITKALB.2", "MKITKALB.3", 
          "MINSTRHWR", "STUDWGT", "SCHWGT", "IDSTRATE", "TOTWGT", "STHMWRK")
data.2011 <- data.2011[,c("BSMMAT01", varb), with = F]
data.2011 <- subset(data.2011, !is.na(SSEX))
data.2011$SSEX <- as.factor(data.2011$SSEX)
levels(data.2011$SSEX) <- c("Mergaite", "Berniukas")
#data.2011$BSMMAT01 <- data.2011$BSMMAT01*data.2011$TOTWGT/sum(data.2011$TOTWGT)*nrow(data.2011)

############################################################################
# Mokinio
############################################################################
# SSEX
p <- ggplot(data.2011, aes(SSEX, BSMMAT01, weight = TOTWGT, fill = SSEX ))
p <- p + geom_boxplot()+ scale_fill_grey(start =1, end =0.5, name = "Mokinio lytis")+theme_bw()+
  labs(x="", y = "Matematikos rezultatas")+
  stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 2, fill= "white")+
stat_summary(fun.y=mean, geom="line", aes(group=1)) +theme(text = element_text(size=20))

pdf("Output/SSEX.pdf", width = 10)
p
dev.off()

# STHMWRK
dd <- subset(data.2011, !is.na(STHMWRK2))
dd$STHMWRK2 <- as.factor(dd$STHMWRK2)
levels(dd$STHMWRK2) <- c("45 min ir maziau", "daugiau nei 45 min")
p <- ggplot(dd, aes(STHMWRK2, BSMMAT01, weight = TOTWGT, fill = SSEX))
p <- p + geom_boxplot()+ scale_fill_grey(start =1, end =0.5, name = "Mokinio lytis")+theme_bw()+
  labs(x="Laikas skiriamas matematikos namu  darbams per savaite", y = "Matematikos rezultatas")+
  stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 2, fill= "white")+
  stat_summary(fun.y=mean, geom="line", aes(group=1)) +theme(text = element_text(size=20))

pdf("Output/STHMWRK2.pdf", width = 10)
p
dev.off()

# SMENG
dd <- subset(data.2011, !is.na(SMENG))
#dd$SMENG <- as.factor(dd$SMENG)
p <- ggplot(dd, aes(SMENG, BSMMAT01, weight = TOTWGT, fill = SSEX))
p <- p + geom_boxplot()+ scale_fill_grey(start =0, end =0.9, name = "Mokinio lytis")+theme_bw()+
  stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 2, fill= "white")+
  stat_summary(fun.y=mean, geom="line", aes(group=1))+
  labs(x="Mokinio susidomejimas matematika", y = "Matematikos rezultatas")+
  stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 2, fill= "white")+
  stat_summary(fun.y=mean, geom="line", aes(group=1)) +theme(text = element_text(size=20))

pdf("Output/SMENG.pdf", width = 15)
p
dev.off()

xyplot(BSMMAT01 ~ SMENG, data=dd,
       auto.key=list(columns=1, lines=TRUE, points=FALSE,
                     space = "right"),
       type=c('p','r'), ylab=list('Matemtikos rezultatas', cex = 2), 
       xlab=list('Kaip savimi pasitiki mokinys matematikoje', cex = 2), col = "black",
       scales=list(x=list(at=NULL,
                          labels = NULL,
                          cex = 1.5)), col=grey.colors(seq(0, 1, 1/length(unique(dd$SMENG)))))


col.regions=grey.colors(5, 0.95, 0.55, 2.2)


p <- qplot(SMENG, BSMMAT01, data=dd, colour=factor(SMENG) )+theme_bw()
           p + scale_colour_grey()+theme(axis.text.x=element_blank())
















MSSEX <- data.2011[, c("BSMMAT01", "SSEX", "TOTWGT"), with = F]
MSSEX$BSMMAT01[MSSEX$SSEX == 0] <- MSSEX$BSMMAT01[MSSEX$SSEX == 0]*MSSEX$TOTWGT[MSSEX$SSEX == 0]/sum(MSSEX$TOTWGT[MSSEX$SSEX == 0])*nrow(MSSEX[MSSEX$SSEX == 0,])
MSSEX$BSMMAT01[MSSEX$SSEX == 1] <- MSSEX$BSMMAT01[MSSEX$SSEX == 1]*MSSEX$TOTWGT[MSSEX$SSEX == 1]/sum(MSSEX$TOTWGT[MSSEX$SSEX == 1])*nrow(MSSEX[MSSEX$SSEX == 1,])

p <- ggplot(MSSEX, aes(factor(SSEX), BSMMAT01))
p + geom_boxplot()

p <- ggplot(data.2011, aes(factor(SSEX), BSMMAT01, weight = TOTWGT))

p + geom_boxplot()

MSSEX <- data.2011[, c("BSMMAT01", "SSEX", "TOTWGT"), with = F]
MSSEX <- ddply(MSSEX, ~SSEX, function(x) sum(x$BSMMAT01*x$TOTWGT)/sum(x$TOTWGT))
MSSEX <- ddply(MSSEX, ~SSEX, function(x) mean(x$BSMMAT01))

a <- ggplot(data = MSSEX, aes(x = SSEX, y = V1, weight = TOTWGT))
a <- a + geom_point(size = 5)
qplot()

a <- ggplot(data = msleep, aes(x = log(bodywt), y = sleep_rem/sleep_total, col = vore))
a <- a + geom_point(size = 5)

data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, 
                    fpc = ~fpc)
opar<-par(mfrow=c(1,3))
svyhist(~enroll, dstrat, main="Survey weighted",col="purple",ylim=c(0,1.3e-3))
hist(apistrat$enroll,  main="Sample unweighted",col="purple",prob=TRUE,ylim=c(0,1.3e-3))
hist(apipop$enroll,  main="Population",col="purple",prob=TRUE,ylim=c(0,1.3e-3))

par(mfrow=c(1,1))
svyboxplot(enroll~stype,dstrat)
svyboxplot(enroll~1,dstrat)
par(opar)