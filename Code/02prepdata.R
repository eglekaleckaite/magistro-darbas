rm(list = ls())
library(plyr)
library(gdata)
library(psych)
library(car)
library(data.table)

vrbs <- read.csv("variables_all.csv", header = T, check.names = F, stringsAsFactors = F)
#vrbs <- read.table("variables.txt", header = T, sep = "\t", check.names = F, stringsAsFactors = F)
names(vrbs)[1] <- "kintamasis"
source("10code.R")

#################################################################################
########################## 1995 #################################################
#################################################################################
load("data.1995.RData")
vrbs.1995 <- vrbs[,c("1995", "kintamasis")]
vrbs.1995 <- vrbs.1995[vrbs.1995[,1] != "", ]
setnames(data.1995, vrbs.1995[,"1995"], vrbs.1995[,"kintamasis"])

#Svoriai:
data.1995[, STUDWGT := TOTWGT/SCHWGT]

data.1995[, SSEX := abs(SSEX - 2)]
data.1995[, TLYTIS := abs(TLYTIS - 2)]

data.1995[, TAMZIUS2 := 1]
data.1995[is.na(TAMZIUS), TAMZIUS2 := NA]
data.1995[TAMZIUS == 3 | TAMZIUS == 4, TAMZIUS2 := 2]
data.1995[TAMZIUS == 5 | TAMZIUS == 6, TAMZIUS2:= 3]

dummy.tamzius <- dummy.code(data.1995$TAMZIUS2)
dummy.tamzius[is.na(data.1995$TAMZIUS2)] <- NA
colnames(dummy.tamzius)  <- paste("TAMZIUS", colnames(dummy.tamzius), sep = ".")
dummy.tamzius <- as.data.table(dummy.tamzius)
data.1995 <- cbind(data.1995, dummy.tamzius)

data.1995[, TISSIL2 := TISSIL]
# data.1995[TISSIL == 3, TISSIL2 := 1]
# data.1995[TISSIL == 5, TISSIL2 := 2]
# data.1995[TISSIL == 6, TISSIL2 := 3]

dummy.tissil <- dummy.code(data.1995$TISSIL2)
dummy.tissil[is.na(data.1995$TISSIL2)] <- NA
colnames(dummy.tissil)  <- paste("TISSIL", colnames(dummy.tissil), sep = ".")
dummy.tissil <- as.data.table(dummy.tissil)
data.1995 <- cbind(data.1995, dummy.tissil)

data.1995$MSUDET2 <- 1
data.1995$MSUDET2[is.na(data.1995$MSUDET)] <- NA
data.1995$MSUDET2[data.1995$MSUDET == 1] <- 3
data.1995$MSUDET2[data.1995$MSUDET == 2] <- 2

dummy.msudet <- dummy.code(data.1995$MSUDET2)
dummy.msudet[is.na(data.1995$MSUDET2)] <- NA
colnames(dummy.msudet)  <- paste("MSUDET", colnames(dummy.msudet), sep = ".")
dummy.msudet <- as.data.table(dummy.msudet)
data.1995 <- cbind(data.1995, dummy.msudet)

data.1995[, MVIETA2 := MVIETA]
data.1995[MVIETA == 1, MVIETA2 := 4]
data.1995[MVIETA == 2, MVIETA2 := 3]
data.1995[MVIETA == 3, MVIETA2 := 2]
data.1995[MVIETA == 4, MVIETA2 := 1]

dummy.mvieta <- dummy.code(data.1995$MVIETA2)
dummy.mvieta[is.na(data.1995$MVIETA2)] <- NA
colnames(dummy.mvieta)  <- paste("MVIETA", colnames(dummy.mvieta), sep = ".")
dummy.mvieta <- as.data.table(dummy.mvieta)
data.1995 <- cbind(data.1995, dummy.mvieta)

data.1995[, MKITKALB2 := MKITKALB]
data.1995[MKITKALB == 2 | MKITKALB == 3, MKITKALB2 := 2]
data.1995[MKITKALB == 4 | MKITKALB == 5, MKITKALB2 := 3]

dummy.mkitkalb <- dummy.code(data.1995$MKITKALB2)
dummy.mkitkalb[is.na(data.1995$MKITKALB2)] <- NA
colnames(dummy.mkitkalb)  <- paste("MKITKALB", colnames(dummy.mkitkalb), sep = ".")
dummy.mkitkalb <- as.data.table(dummy.mkitkalb)
data.1995 <- cbind(data.1995, dummy.mkitkalb)

data.1995 <- as.data.table(data.1995)

save(data.1995, file = "data.1995.RData")

#################################################################################
########################## 1999 #################################################
#################################################################################
load("data.1999.RData")
vrbs.1999 <- vrbs[,c("1999", "kintamasis")]
vrbs.1999 <- vrbs.1999[vrbs.1999[,1] != "", ]
setnames(data.1999, vrbs.1999[,"1999"], vrbs.1999[,"kintamasis"])

#Svoriai:
data.1999[, STUDWGT := TOTWGT/SCHWGT]

data.1999[, SSEX := abs(SSEX - 2)]
data.1999[, TLYTIS := abs(TLYTIS - 2)]

data.1999[, TAMZIUS2 := 1]
data.1999[is.na(TAMZIUS), TAMZIUS2 := NA]
data.1999[TAMZIUS == 3 | TAMZIUS == 4, TAMZIUS2 := 2]
data.1999[TAMZIUS == 5 | TAMZIUS == 6, TAMZIUS2:= 3]

dummy.tamzius <- dummy.code(data.1999$TAMZIUS2)
dummy.tamzius[is.na(data.1999$TAMZIUS2)] <- NA
colnames(dummy.tamzius)  <- paste("TAMZIUS", colnames(dummy.tamzius), sep = ".")
dummy.tamzius <- as.data.table(dummy.tamzius)
data.1999 <- cbind(data.1999, dummy.tamzius)

data.1999[, TISSIL2 := TISSIL]
# data.1999[TISSIL == 3, TISSIL2 := 1]
# data.1999[TISSIL == 5, TISSIL2 := 2]
# data.1999[TISSIL == 6, TISSIL2 := 3]

dummy.tissil <- dummy.code(data.1999$TISSIL2)
dummy.tissil[is.na(data.1999$TISSIL2)] <- NA
colnames(dummy.tissil)  <- paste("TISSIL", colnames(dummy.tissil), sep = ".")
dummy.tissil <- as.data.table(dummy.tissil)
data.1999 <- cbind(data.1999, dummy.tissil)

data.1999$MSUDET2 <- 1
data.1999$MSUDET2[is.na(data.1999$MSUDET)] <- NA
data.1999$MSUDET2[data.1999$MSUDET == 1] <- 3
data.1999$MSUDET2[data.1999$MSUDET == 2] <- 2

dummy.msudet <- dummy.code(data.1999$MSUDET2)
dummy.msudet[is.na(data.1999$MSUDET2)] <- NA
colnames(dummy.msudet)  <- paste("MSUDET", colnames(dummy.msudet), sep = ".")
dummy.msudet <- as.data.table(dummy.msudet)
data.1999 <- cbind(data.1999, dummy.msudet)

data.1999[, MVIETA2 := MVIETA]
data.1999[MVIETA == 1, MVIETA2 := 4]
data.1999[MVIETA == 2, MVIETA2 := 3]
data.1999[MVIETA == 3, MVIETA2 := 2]
data.1999[MVIETA == 4, MVIETA2 := 1]

dummy.mvieta <- dummy.code(data.1999$MVIETA2)
dummy.mvieta[is.na(data.1999$MVIETA2)] <- NA
colnames(dummy.mvieta)  <- paste("MVIETA", colnames(dummy.mvieta), sep = ".")
dummy.mvieta <- as.data.table(dummy.mvieta)
data.1999 <- cbind(data.1999, dummy.mvieta)

data.1999 <- as.data.table(data.1999)

save(data.1999, file = "data.1999.RData")


#################################################################################
########################## 2003 #################################################
#################################################################################
load("data.2003.RData")
vrbs.2003 <- vrbs[,c("2003", "kintamasis")]
vrbs.2003 <- vrbs.2003[vrbs.2003[,1] != "", ]
setnames(data.2003, vrbs.2003[,"2003"], vrbs.2003[,"kintamasis"])

#Svoriai:
data.2003[, STUDWGT := TOTWGT/SCHWGT]

data.2003[, SSEX := abs(SSEX - 2)]
data.2003[, TLYTIS := abs(TLYTIS - 2)]

data.2003[, STHMWRK2 := STHMWRK]

data.2003[, TAMZIUS2 := 1]
data.2003[is.na(TAMZIUS), TAMZIUS2 := NA]
data.2003[TAMZIUS == 3 | TAMZIUS == 4, TAMZIUS2 := 2]
data.2003[TAMZIUS == 5 | TAMZIUS == 6, TAMZIUS2:= 3]

dummy.tamzius <- dummy.code(data.2003$TAMZIUS2)
dummy.tamzius[is.na(data.2003$TAMZIUS2)] <- NA
colnames(dummy.tamzius)  <- paste("TAMZIUS", colnames(dummy.tamzius), sep = ".")
dummy.tamzius <- as.data.table(dummy.tamzius)
data.2003 <- cbind(data.2003, dummy.tamzius)

data.2003[, TISSIL2 := TISSIL]
data.2003[TISSIL == 3, TISSIL2 := 1]
data.2003[TISSIL == 5, TISSIL2 := 2]
data.2003[TISSIL == 6, TISSIL2 := 3]

dummy.tissil <- dummy.code(data.2003$TISSIL2)
dummy.tissil[is.na(data.2003$TISSIL2)] <- NA
colnames(dummy.tissil)  <- paste("TISSIL", colnames(dummy.tissil), sep = ".")
dummy.tissil <- as.data.table(dummy.tissil)
data.2003 <- cbind(data.2003, dummy.tissil)

data.2003$MSUDET2 <- 1
data.2003$MSUDET2[is.na(data.2003$MSUDET)] <- NA
data.2003$MSUDET2[data.2003$MSUDET == 1] <- 3
data.2003$MSUDET2[data.2003$MSUDET == 2] <- 2

dummy.msudet <- dummy.code(data.2003$MSUDET2)
dummy.msudet[is.na(data.2003$MSUDET2)] <- NA
colnames(dummy.msudet)  <- paste("MSUDET", colnames(dummy.msudet), sep = ".")
dummy.msudet <- as.data.table(dummy.msudet)
data.2003 <- cbind(data.2003, dummy.msudet)

data.2003[, MVIETA2 := MVIETA]
data.2003[MVIETA == 1, MVIETA2 := 5]
data.2003[MVIETA == 2, MVIETA2 := 4]
data.2003[MVIETA == 4, MVIETA2 := 2]
data.2003[MVIETA == 5, MVIETA2 := 1]

dummy.mvieta <- dummy.code(data.2003$MVIETA2)
dummy.mvieta[is.na(data.2003$MVIETA2)] <- NA
colnames(dummy.mvieta)  <- paste("MVIETA", colnames(dummy.mvieta), sep = ".")
dummy.mvieta <- as.data.table(dummy.mvieta)
data.2003 <- cbind(data.2003, dummy.mvieta)

data.2003[, MKITKALB2 := MKITKALB]
data.2003[MKITKALB == 2 | MKITKALB == 3, MKITKALB2 := 2]
data.2003[MKITKALB == 4 | MKITKALB == 5, MKITKALB2 := 3]

dummy.mkitkalb <- dummy.code(data.2003$MKITKALB2)
dummy.mkitkalb[is.na(data.2003$MKITKALB2)] <- NA
colnames(dummy.mkitkalb)  <- paste("MKITKALB", colnames(dummy.mkitkalb), sep = ".")
dummy.mkitkalb <- as.data.table(dummy.mkitkalb)
data.2003 <- cbind(data.2003, dummy.mkitkalb)

data.2003 <- as.data.table(data.2003)

save(data.2003, file = "data.2003.RData")

#################################################################################
########################## 2007 #################################################
#################################################################################
load("data.2007.RData")
vrbs.2007 <- vrbs[,c("2007", "kintamasis")]
vrbs.2007 <- vrbs.2007[vrbs.2007[,1] != "", ]
data.2007 <- rename.vars(data.2007, vrbs.2007[,"2007"], vrbs.2007[,"kintamasis"])

#Svoriai:
data.2007[, STUDWGT := TOTWGT/SCHWGT]

data.2007[, SSEX := abs(SSEX - 2)]
data.2007[, TLYTIS := abs(TLYTIS - 2)]

data.2007[, STHMWRK2 := STHMWRK]
data.2007[STHMWRK == 3, STHMWRK2 := 1]
data.2007[STHMWRK == 1 | STHMWRK == 2, STHMWRK2 := 0]

data.2007[, TAMZIUS2 := 1]
#data.2007[is.na(TAMZIUS), TAMZIUS2 := NA]
data.2007[TAMZIUS == 3 | TAMZIUS == 4, TAMZIUS2 := 2]
data.2007[TAMZIUS == 5 | TAMZIUS == 6, TAMZIUS2:= 3]

dummy.tamzius <- dummy.code(data.2007$TAMZIUS2)
dummy.tamzius[is.na(data.2007$TAMZIUS2)] <- NA
colnames(dummy.tamzius)  <- paste("TAMZIUS", colnames(dummy.tamzius), sep = ".")
dummy.tamzius <- as.data.table(dummy.tamzius)
data.2007 <- cbind(data.2007, dummy.tamzius)

data.2007[, TISSIL2 := TISSIL]
data.2007[TISSIL == 4, TISSIL2 := 1]
data.2007[TISSIL == 5, TISSIL2 := 2]
data.2007[TISSIL == 6, TISSIL2 := 3]

dummy.tissil <- dummy.code(data.2007$TISSIL2)
dummy.tissil[is.na(data.2007$TISSIL2)] <- NA
colnames(dummy.tissil)  <- paste("TISSIL", colnames(dummy.tissil), sep = ".")
dummy.tissil <- as.data.table(dummy.tissil)
data.2007 <- cbind(data.2007, dummy.tissil)

data.2007$MSUDET2 <- 1
data.2007$MSUDET2[is.na(data.2007$MSUDET)] <- NA
data.2007$MSUDET2[data.2007$MSUDET == 1] <- 3
data.2007$MSUDET2[data.2007$MSUDET == 2] <- 2

dummy.msudet <- dummy.code(data.2007$MSUDET2)
dummy.msudet[is.na(data.2007$MSUDET2)] <- NA
colnames(dummy.msudet)  <- paste("MSUDET", colnames(dummy.msudet), sep = ".")
dummy.msudet <- as.data.table(dummy.msudet)
data.2007 <- cbind(data.2007, dummy.msudet)

data.2007[, MVIETA2 := MVIETA]
data.2007[MVIETA == 1, MVIETA2 := 5]
data.2007[MVIETA == 2, MVIETA2 := 4]
data.2007[MVIETA == 4, MVIETA2 := 2]
data.2007[MVIETA == 5, MVIETA2 := 1]

dummy.mvieta <- dummy.code(data.2007$MVIETA2)
dummy.mvieta[is.na(data.2007$MVIETA2)] <- NA
colnames(dummy.mvieta)  <- paste("MVIETA", colnames(dummy.mvieta), sep = ".")
dummy.mvieta <- as.data.table(dummy.mvieta)
data.2007 <- cbind(data.2007, dummy.mvieta)

data.2007[, MKITKALB2 := MKITKALB]
data.2007[MKITKALB == 2 | MKITKALB == 3, MKITKALB2 := 2]
data.2007[MKITKALB == 4 | MKITKALB == 5, MKITKALB2 := 3]

dummy.mkitkalb <- dummy.code(data.2007$MKITKALB2)
dummy.mkitkalb[is.na(data.2007$MKITKALB2)] <- NA
colnames(dummy.mkitkalb)  <- paste("MKITKALB", colnames(dummy.mkitkalb), sep = ".")
dummy.mkitkalb <- as.data.table(dummy.mkitkalb)
data.2007 <- cbind(data.2007, dummy.mkitkalb)

data.2007 <- as.data.table(data.2007)

save(data.2007, file = "data.2007.RData")
#################################################################################
########################## 2011 #################################################
#################################################################################
load("data.2011.RData")
vrbs.2011 <- vrbs[,c("2011", "kintamasis")]
vrbs.2011 <- vrbs.2011[vrbs.2011[,1] != "", ]
data.2011 <- rename.vars(data.2011, vrbs.2011[,"2011"], vrbs.2011[,"kintamasis"])

#Svoriai:
data.2011[, STUDWGT := TOTWGT/SCHWGT]
#data.2011 <- ddply(data.2011, ~ IDSCHOOL, function(x) {x$STUDWGT <- x$STUDWGT*nrow(x)/sum(x$STUDWGT);return(x)})

# data.2011 <- ddply(data.2011, ~ IDSCHOOL, function(x) 
#   {x$SCHWGT <- unique(x$SCHWGT)*length(unique(data.2011$IDSCHOOL))/sum(unique(data.2011$SCHWGT));return(x)})


data.2011[, SSEX := abs(SSEX - 2)]
data.2011[, TLYTIS := abs(TLYTIS - 2)]

data.2011[, STHMWRK2 := STHMWRK]
data.2011[STHMWRK == 3, STHMWRK2 := 1]
data.2011[STHMWRK == 1 | STHMWRK == 2, STHMWRK2 := 0]

data.2011[, TAMZIUS2 := 1]
data.2011[is.na(TAMZIUS), TAMZIUS2 := NA]
data.2011[TAMZIUS == 3 | TAMZIUS == 4, TAMZIUS2 := 2]
data.2011[TAMZIUS == 5 | TAMZIUS == 6, TAMZIUS2:= 3]

dummy.tamzius <- dummy.code(data.2011$TAMZIUS2)
dummy.tamzius[is.na(data.2011$TAMZIUS2)] <- NA
colnames(dummy.tamzius)  <- paste("TAMZIUS", colnames(dummy.tamzius), sep = ".")
dummy.tamzius <- as.data.table(dummy.tamzius)
data.2011 <- cbind(data.2011, dummy.tamzius)

data.2011[, TISSIL2 := TISSIL]
data.2011[TISSIL == 4, TISSIL2 := 1]
data.2011[TISSIL == 5, TISSIL2 := 2]
data.2011[TISSIL == 6, TISSIL2 := 3]

dummy.tissil <- dummy.code(data.2011$TISSIL2)
dummy.tissil[is.na(data.2011$TISSIL2)] <- NA
colnames(dummy.tissil)  <- paste("TISSIL", colnames(dummy.tissil), sep = ".")
dummy.tissil <- as.data.table(dummy.tissil)
data.2011 <- cbind(data.2011, dummy.tissil)

data.2011$MSUDET2 <- 1
data.2011$MSUDET2[is.na(data.2011$MSUDET)] <- NA
data.2011$MSUDET2[data.2011$MSUDET == 1] <- 3
data.2011$MSUDET2[data.2011$MSUDET == 2] <- 2

dummy.msudet <- dummy.code(data.2011$MSUDET2)
dummy.msudet[is.na(data.2011$MSUDET2)] <- NA
colnames(dummy.msudet)  <- paste("MSUDET", colnames(dummy.msudet), sep = ".")
dummy.msudet <- as.data.table(dummy.msudet)
data.2011 <- cbind(data.2011, dummy.msudet)

dummy.maincome <- dummy.code(data.2011$MAINCOME)
dummy.maincome[is.na(data.2011$MAINCOME)] <- NA
colnames(dummy.maincome)  <- paste("MAINCOME", colnames(dummy.maincome), sep = ".")
dummy.maincome <- as.data.table(dummy.maincome)
data.2011 <- cbind(data.2011, dummy.maincome)


data.2011[, MVIETA2 := MVIETA]
data.2011[MVIETA == 1, MVIETA2 := 5]
data.2011[MVIETA == 2, MVIETA2 := 4]
data.2011[MVIETA == 4, MVIETA2 := 2]
data.2011[MVIETA == 5, MVIETA2 := 1]

dummy.mvieta <- dummy.code(data.2011$MVIETA2)
dummy.mvieta[is.na(data.2011$MVIETA2)] <- NA
colnames(dummy.mvieta)  <- paste("MVIETA", colnames(dummy.mvieta), sep = ".")
dummy.mvieta <- as.data.table(dummy.mvieta)
data.2011 <- cbind(data.2011, dummy.mvieta)

data.2011[, MKITKALB2 := MKITKALB]
data.2011[MKITKALB == 2 | MKITKALB == 3, MKITKALB2 := 2]
data.2011[MKITKALB == 4 | MKITKALB == 5, MKITKALB2 := 3]

dummy.mkitkalb <- dummy.code(data.2011$MKITKALB2)
dummy.mkitkalb[is.na(data.2011$MKITKALB2)] <- NA
colnames(dummy.mkitkalb)  <- paste("MKITKALB", colnames(dummy.mkitkalb), sep = ".")
dummy.mkitkalb <- as.data.table(dummy.mkitkalb)
data.2011 <- cbind(data.2011, dummy.mkitkalb)

data.2011$STUDRES <- 2
data.2011$STUDRES[is.na(data.2011$INTERNET)] <- NA
data.2011$STUDRES[data.2011$INTERNET == 2 | data.2011$ROOM == 2] <- 1
data.2011$STUDRES[data.2011$INTERNET == 1 | data.2011$ROOM == 1] <- 3
data.2011$STHEDUC[!is.na(data.2011$STHEDUC)]  <- mapvalues(data.2011$STHEDUC[!is.na(data.2011$STHEDUC)], from = 1:5, to = 5:1)

data.2011$MFEDUC  <- data.2011$STHEDUC
data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLPREDUC <- sum(x$STHEDUC*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHPREDUC <- sum(x$STHEDUC*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

data.2011$TOTPOSS1  <-  data.2011$SHEDRES
data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLTOTPOS1 <- sum(x$SHEDRES*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHTOTPOS1 <- sum(x$SHEDRES*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

data.2011$TOTPOSS2 <- data.2011$SNSUPP
data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLTOTPOS2 <- sum(x$SNSUPP*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHTOTPOS2 <- sum(x$SNSUPP*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

# 
# # NBOOKS
data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLNBOOKS <- sum(x$NBOOKS*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHNBOOKS <- sum(x$NBOOKS*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})


# ACADEMIC PRESSURE
data.2011 <- ddply(data.2011, ~IDSTUD, function(x) {
  x$MTPRES <- mean (as.numeric(x[,c("BSBG11A", "BSBG11B", "BSBG11C", "BSBG11D")]), na.rm = T)
  return(x)})
data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLMTPRES <- sum(x$MTPRES*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHMTPRES <- sum(x$MTPRES*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

# Student attitudes toward math
# SMENG, SMCONF, SMVALUE, SMLIKE
data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLMENG <- sum(x$SMENG*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHMENG <- sum(x$SMENG*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLCONF <- sum(x$SMCONF*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHCONF <- sum(x$SMCONF*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLVALUE <- sum(x$SMVALUE*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHVALUE <- sum(x$SMVALUE*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLLIKE <- sum(x$SMLIKE*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHLIKE <- sum(x$SMLIKE*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

# Klimatas = SBULLED
data.2011 <- ddply(data.2011, ~IDCLASS, function(x) {
  x$CLBULLED <- sum(x$SBULLED*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})
data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {
  x$SHBULLED <- sum(x$SBULLED*x$HOUWGT)/sum(x$HOUWGT)
  return(x)})

data.2011 <-as.data.table(data.2011)
########################################  
# Suvidurninam ir nucentruojam
######################################## 
data.2011[, MSMENG := sum(SMENG*TOTWGT, na.rm = T)/sum(TOTWGT, na.rm = T), by = IDSCHOOL]
data.2011[, CSMENG := SMENG - MSMENG]

data.2011[, MSMCONF := sum(SMCONF*TOTWGT, na.rm = T)/sum(TOTWGT, na.rm = T), by = IDSCHOOL]
data.2011[, CSMCONF := SMCONF - MSMCONF]

data.2011[, MSMVALUE := sum(SMVALUE*TOTWGT, na.rm = T)/sum(TOTWGT, na.rm = T), by = IDSCHOOL]
data.2011[, CSMVALUE := SMVALUE - MSMVALUE]

data.2011[, MSMLIKE := sum(SMLIKE*TOTWGT, na.rm = T)/sum(TOTWGT, na.rm = T), by = IDSCHOOL]
data.2011[, CSMLIKE := SMLIKE - MSMLIKE]

data.2011[, MSBULLED := sum(SBULLED*TOTWGT, na.rm = T)/sum(TOTWGT, na.rm = T), by = IDSCHOOL]
data.2011[, CSBULLED := SBULLED - MSBULLED]

data.2011[, MSHEDRES := sum(SHEDRES*TOTWGT, na.rm = T)/sum(TOTWGT, na.rm = T), by = IDSCHOOL]
data.2011[, CSHEDRES := SHEDRES - MSHEDRES]

### Mokytojo
data.2011[, CLSWGT := WGTADJ2*WGTADJ3*WGTFAC2*WGTFAC3]

data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {y <- unique(x[,c("IDCLASS", "TPROBL", "CLSWGT")]);x$MTPROBL <- sum(y$TPROBL*y$CLSWGT, na.rm = T)/sum(y$CLSWGT, na.rm = T);return(x)})
data.2011$CTPROBL <- data.2011$TPROBL-data.2011$MTPROBL

data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {y <- unique(x[,c("IDCLASS", "TAKADSEKM", "CLSWGT")]);x$MTAKADSEKM <- sum(y$TAKADSEKM*y$CLSWGT, na.rm = T)/sum(y$CLSWGT, na.rm = T);return(x)})
data.2011$CTAKADSEKM <- data.2011$TAKADSEKM-data.2011$MTAKADSEKM

data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {y <- unique(x[,c("IDCLASS", "TMOKAPL", "CLSWGT")]);x$MTMOKAPL <- sum(y$TMOKAPL*y$CLSWGT, na.rm = T)/sum(y$CLSWGT, na.rm = T);return(x)})
data.2011$CTMOKAPL <- data.2011$TMOKAPL-data.2011$MTMOKAPL

data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {y <- unique(x[,c("IDCLASS", "TCONF", "CLSWGT")]);x$MTCONF <- sum(y$TCONF*y$CLSWGT, na.rm = T)/sum(y$CLSWGT, na.rm = T);return(x)})
data.2011$CTCONF <- data.2011$TCONF-data.2011$MTCONF

data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {y <- unique(x[,c("IDCLASS", "TSATISF", "CLSWGT")]);x$MTSATISF <- sum(y$TSATISF*y$CLSWGT, na.rm = T)/sum(y$CLSWGT, na.rm = T);return(x)})
data.2011$CTSATISF <- data.2011$TSATISF-data.2011$MTSATISF

data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {y <- unique(x[,c("IDCLASS", "TBENDR", "CLSWGT")]);x$MTBENDR <- sum(y$TBENDR*y$CLSWGT, na.rm = T)/sum(y$CLSWGT, na.rm = T);return(x)})
data.2011$CTBENDR <- data.2011$TBENDR-data.2011$MTBENDR

data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {y <- unique(x[,c("IDCLASS", "TENGAGE", "CLSWGT")]);x$MTENGAGE <- sum(y$TENGAGE*y$CLSWGT, na.rm = T)/sum(y$CLSWGT, na.rm = T);return(x)})
data.2011$CTENGAGE <- data.2011$TENGAGE-data.2011$MTENGAGE

data.2011 <- ddply(data.2011, ~IDSCHOOL, function(x) {y <- unique(x[,c("IDCLASS", "KDYDIS", "CLSWGT")]);x$MKDYDIS <- sum(y$KDYDIS*y$CLSWGT, na.rm = T)/sum(y$CLSWGT, na.rm = T);return(x)})
data.2011$CKDYDIS <- data.2011$KDYDIS-data.2011$MKDYDIS

data.2011 <- as.data.table(data.2011)

save(data.2011, file = "data.2011.RData")
