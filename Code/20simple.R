### ??? neveikia


rm(list = ls())
library(nlme)
library(plyr)
library(gdata)
library(lme4)
library(mitools)
library(lmerTest)
library(robustlmm)
library(data.table)
source("10code.R")

load("data.1995.RData")
load("data.1999.RData")
load("data.2003.RData")
load("data.2007.RData")
load("data.2011.RData")

# Is Dudaites
lme0 <- lme(BSMMAT01~1, random=~1|IDSCHOOL, data=data.2003)
(sm0 <- summary(lme0))
(std.er0 <- sm0$tTable[2])
lme1 <- lme(BSMMAT01~1+MDYDIS+MSOCR, random=~1|IDSCHOOL, data=data.2003, na.action = na.omit)
sm1 <- summary(lme1)
(std.er1 <- sm1$tTable[2])
lme2 <- lme(BSMMAT01~1+BCBGCOMU, random=~1|IDSCHOOL, data=data.2003[!is.na(data.2003$BCBGTENR),])
sm2 <- summary(lme2)
(std.er2 <- sm1$tTable[2])

### 1995 ###
d1995.stud$for.mean <- d1995.stud$BSMMAT01*d1995.stud$TOTWGT
sum(d1995.stud$for.mean)/sum(d1995.stud$TOTWGT)

lme0 <-lmer(BSMMAT01 ~ 1+(1|IDSCHOOL), data = data.1995[!is.na(data.1995$BCDGTENR),], weights = data.1995[!is.na(data.1995$BCDGTENR),"TOTWGT"])
(sm0 <- summary(lme0))
t00.0 <- attributes(sm0$varcor$IDSCHOOL)$stddev^2

lme1 <-lmer(BSMMAT01 ~ 1+BCDGTENR+(1|IDSCHOOL), data = data.1995[!is.na(data.1995$BCDGTENR),])
(sm1 <- summary(lme1))
t00.1 <- attributes(sm1$varcor$IDSCHOOL)$stddev^2

(t00.0-t00.1)/t00.0

lme2 <-lmer(BSMMAT01 ~ 1+BCBGCOMM+(1|IDSCHOOL), data = data.1995[!is.na(data.1995$BCBGCOMM),])
(sm2 <- summary(lme2))
t00.2 <- attributes(sm2$varcor$IDSCHOOL)$stddev^2

(t00.1-t00.2)/t00.1
### 1999 ###
d1999.stud$for.mean <- d1999.stud$BSMMAT02*d1999.stud$TOTWGT
sum(d1999.stud$for.mean)/sum(d1999.stud$TOTWGT)

lme0 <-lmer(BSMMAT02 ~ 1+(1|IDSCHOOL), data = d1999.stud, REML = T, weights = d1999.stud$TOTWGT)
(sm0 <- summary(lme0))
t00.0 <- attributes(sm0$varcor$IDSCHOOL)$stddev^2

### 2003 ###
d2003.stud$for.mean <- d2003.stud$bsmmat03*d2003.stud$totwgt
sum(d2003.stud$for.mean)/sum(d2003.stud$totwgt)

lme0 <-lmer(bsmmat03 ~ 1+(1|idschool), data = data.2003)

(sm0 <- summary(lme0))
t00.0 <- attributes(sm0$varcor$idschool)$stddev^2

lme1 <-lmer(bsmmat01 ~ 1+MDYDIS+(1|idschool), data = data.2003)
(sm1 <- summary(lme1))
t00.1 <- attributes(sm1$varcor$idschool)$stddev^2

(t00.0-t00.1)/t00.0

lme2 <-lmer(bsmmat01 ~ 1+MVIETOVE+(1|idschool), data = data.2003)
(sm2 <- summary(lme2))
t00.2 <- attributes(sm2$varcor[[1]])$stddev^2

(t00.0-t00.2)/t00.0

lme3 <-lmer(bsmmat01 ~ 1+MSOCR+(1|idschool), data = data.2003)
(sm3 <- summary(lme3))
t00.3 <- attributes(sm3$varcor$idschool)$stddev^2

(t00.0-t00.3)/t00.0

lme4 <-lmer(bsmmat01 ~ 1+MSOCR+MDYDIS+(1|idschool), data = data.2003)
(sm4 <- summary(lme4))
t00.4 <- attributes(sm4$varcor$idschool)$stddev^2

(t00.0-t00.4)/t00.0


######################### Klases ##################################
data.2003 <- merge(remove.vars(d2003.stud.teach, "DPCDATE"), remove.vars(d2003.teach.m, "DPCDATE"), by = names(d2003.teach.m)[names(d2003.teach.m) %in% names(d2003.stud.teach)], all = T)

data.2003$class.size <- 1
data.2003$class.size[data.2003$BTBMSTUD >24] <- 2

### 2003 ###
lme0 <-lmer(BSMMAT01 ~ 1+(1|IDCLASS), data = data.2003[!is.na(data.2003$class.size),])
(sm0 <- summary(lme0))
t00.0 <- attributes(sm0$varcor$IDCLASS)$stddev^2

lme1 <-lmer(BSMMAT01 ~ 1+class.size+(1|IDCLASS), data = data.2003[!is.na(data.2003$class.size),])
(sm1 <- summary(lme1))
t00.1 <- attributes(sm1$varcor$IDCLASS)$stddev^2

(t00.0-t00.1)/t00.0

lme2 <-lmer(BSMMAT01 ~ 1+BTBMADHY+BTBMADDW+(1|IDCLASS), data = data.2003[!is.na(data.2003$BTBMADHY),])
(sm2 <- summary(lme2))
t00.2 <- attributes(sm2$varcor$IDCLASS)$stddev^2

(t00.0-t00.2)/t00.0

lme3 <-lmer(BSMMAT01 ~ 1+BCBGSBED+(1|IDSCHOOL), data = data.2003[!is.na(data.2003$BCBGSBED),])
(sm3 <- summary(lme3))
t00.3 <- attributes(sm3$varcor$IDSCHOOL)$stddev^2

(t00.0-t00.3)/t00.0

lme4 <-lmer(BSMMAT01 ~ 1+MTYPE+MDYDIS+SES+(1+SES|IDSCHOOL), data = data.2003)
(sm4 <- summary(lme4))
t00.4 <- attributes(sm4$varcor$IDSCHOOL)$stddev^2

(t00.0-t00.4)/t00.0

bs4=bootMer(lme4,nsim=1000, FUN = mySumm)


with(lme4, {
  cc <- coef(.)$IDSCHOOL
  xyplot(BSMMAT01 ~ SES|IDSCHOOL,
         index.cond = function(x, y) coef(lm(y ~ x))[1],
         panel = function(x, y, groups, subscripts, ...) {
           panel.grid(h = -1, v = -1)
           panel.points(x, y, ...)
           subj <- as.character(IDSCHOOL[subscripts][1])
           panel.abline(cc[subj,1], cc[subj, 2])
         })
})
########## PV ###############
varb <- c("IDSCHOOL", "MVIETOVE", "MDYDIS", "MTYPE", "SES", "MSOCR2")
d2003 <- list(data.2003[,c("BSMMAT01", varb)], data.2003[,c("BSMMAT02", varb)], 
              data.2003[,c("BSMMAT03", varb)], data.2003[,c("BSMMAT04", varb)],
              data.2003[,c("BSMMAT05", varb)])
d2003 <- llply(d2003, function(x) {names(x) <- gsub("0[1-5]", "", names(x));return(x)})
corr1 <- cor(data.2003[,c("MVIETOVE", "MDYDIS", "MTYPE", "SES", "MSOCR2")], use = "pairwise.complete.obs")
d2003 <- imputationList(d2003)

models<-with(d2003, lmer(BSMMAT ~ 1+MVIETOVE+MDYDIS+MTYPE+SES+(1+SES|IDSCHOOL)))

MIsummary(models)
 


# Sweave ('models_exmpl.Rnw', encoding='utf8')

