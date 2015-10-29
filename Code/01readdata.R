rm(list = ls())
library(foreign)
library(gdata)
library(plyr)
library(data.table)
# d1995.school <- read.spss("../Data/1995/BCGLTU1.sav", to.data.frame = T)
# d1995.stud.teach <- read.spss("../Data/1995/BLGLTU1.sav", to.data.frame = T)
# d1995.assesment <- read.spss("../Data/1995/BSALTU1.sav", to.data.frame = T)
# d1995.stud <- read.spss("../Data/1995/BSGLTU1.sav", to.data.frame = T)
# d1995.teach.m <- read.spss("../Data/1995/BTMLTU1.sav", to.data.frame = T)
# d1995.teach.s <- read.spss("../Data/1995/BTSLTU1.sav", to.data.frame = T)

d1995.school <- remove.vars(read.spss("../Data/1999/BCGLTUm1.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d1995.stud.teach <- remove.vars(read.spss("../Data/1999/BSTLTUm1.sav", to.data.frame = T, use.value.labels=FALSE), "DPCDATE")
tid.1995 <- unique(subset(d1995.stud.teach, IDGRADER == 2 & MATSUBJ == 1)[, c("IDTEACH", "IDSTUD", "IDLINK")])
d1995.assesment <- remove.vars(read.spss("../Data/1999/BSALTUm1.sav", to.data.frame = T, use.value.labels=FALSE), "DPCDATE")
d1995.assesment <- subset(d1995.assesment, IDGRADE == 8)
d1995.stud <- remove.vars(read.spss("../Data/1999/BSGLTUm1.sav", to.data.frame = T, use.value.labels=FALSE), "DPCDATE")
d1995.stud <- subset(d1995.stud, IDGRADE == 8)
d1995.teach.m <- remove.vars(read.spss("../Data/1999/BTMLTUm1.sav", to.data.frame = T, use.value.labels=FALSE), "DPCDATE")
d1995.teach.s <- remove.vars(read.spss("../Data/1999/BTSLTUm1.sav", to.data.frame = T, use.value.labels=FALSE), "DPCDATE")
tid.1995 <- na.omit(unique(subset(tid.1995, IDSTUD %in% unique(d1995.stud$IDSTUD))))
d1995.teach.m <- subset(d1995.teach.m, IDTEACH %in% unique(tid.1995$IDTEACH))

data.1995 <- merge(d1995.school, d1995.stud, by = names(d1995.stud)[names(d1995.stud) %in% names(d1995.school)], all = T)
data.1995 <- merge(data.1995, tid.1995, all.x = T)
data.1995 <- merge(data.1995, d1995.teach.m, by = names(d1995.teach.m)[names(d1995.teach.m) %in% names(data.1995)])
data.1995 <- as.data.table(data.1995)
data.1995 <- data.1995[!is.na(BSMMAT01),]
save(data.1995, file = "data.1995.RData")
gc()
#######################################################
rm(list = ls())
d1999.school <- remove.vars(read.spss("../Data/1999/BCGLTUm2.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d1999.stud.teach <- remove.vars(read.spss("../Data/1999/BSTLTUm2.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d1999.assesment <- remove.vars(read.spss("../Data/1999/BSALTUm2.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d1999.stud <- remove.vars(read.spss("../Data/1999/BSGLTUm2.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d1999.teach.m <- remove.vars(read.spss("../Data/1999/BTMLTUm2.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d1999.teach.s <- remove.vars(read.spss("../Data/1999/BTSLTUm2.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d1999.teach.reliab <- remove.vars(read.spss("../Data/1999/BSRLTUm2.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
tid.1999 <- unique(subset(d1999.stud.teach, IDGRADER == 2 & MATSUBJ == 1)[, c("IDTEACH", "IDSTUD")])
tid.1999 <- unique(subset(tid.1999, IDSTUD %in% unique(d1999.stud$IDSTUD)))

d1999.stud$WGTFAC1 <- round(d1999.stud$WGTFAC1+0.000001, 2)
data.1999 <- merge(d1999.school, d1999.stud, by = names(d1999.stud)[names(d1999.stud) %in% names(d1999.school)], all = T)
data.1999 <- merge(data.1999, tid.1999, all = T)
data.1999 <- merge(data.1999, d1999.teach.m, by = names(d1999.teach.m)[names(d1999.teach.m) %in% names(data.1999)])

data.1999 <- data.1999[!is.na(data.1999$BSDGHERI),]
data.1999 <- ddply(data.1999, ~ IDSCHOOL, function(x) {x$MHOMERES <- mean(as.numeric(x$BSDGHERI)); return(x)})
#d1999.teach.m <- unique(data.1999[,c("IDSCHOOL", "MHOMERES")])
data.1999 <- as.data.table(data.1999)
data.1999 <- data.1999[!is.na(BSMMAT01),]

save(data.1999, file = "data.1999.RData")
gc()
#######################################################
rm(list = ls())
d2003.school <- remove.vars(read.spss("../Data/2003/BCGLTUm3.sav", to.data.frame = T, use.value.labels=FALSE), "dpcdate")
names(d2003.school) <- toupper(names(d2003.school))
d2003.school$WGTADJ1 <- round(d2003.school$WGTADJ1, 5)
d2003.stud.teach <- remove.vars(read.spss("../Data/2003/BSTLTUm3.sav", to.data.frame = T, use.value.labels=FALSE), "dpcdate")
d2003.assesment <- remove.vars(read.spss("../Data/2003/BSALTUm3.sav", to.data.frame = T, use.value.labels=FALSE), "dpcdate")
d2003.stud <- remove.vars(read.spss("../Data/2003/BSGLTUm3.sav", to.data.frame = T, use.value.labels=FALSE), "dpcdate")
names(d2003.stud) <- toupper(names(d2003.stud))
d2003.stud$WGTADJ1 <- round(d2003.stud$WGTADJ1, 5)
d2003.teach.m <- remove.vars(read.spss("../Data/2003/t03_spss_2/t03_spss_2/BTMLTUm3.sav", to.data.frame = T, use.value.labels=FALSE), "dpcdate")
names(d2003.teach.m) <- toupper(names(d2003.teach.m))
d2003.teach.m <- d2003.teach.m[!duplicated(d2003.teach.m$IDTEACH),]
d2003.teach.s <- remove.vars(read.spss("../Data/2003/BTSLTUm3.sav", to.data.frame = T, use.value.labels=FALSE), "dpcdate")
d2003.teach.reliab <- remove.vars(read.spss("../Data/2003/BSRLTUm3.sav", to.data.frame = T, use.value.labels=FALSE), "dpcdate")
tid.2003 <- unique(subset(d2003.stud.teach, IDGRADER == 2 & MATSUBJ == 1)[, c("IDTEACH", "IDSTUD", "IDCLASS")])


data.2003 <- merge(d2003.school, d2003.stud, by = names(d2003.stud)[names(d2003.stud) %in% names(d2003.school)][-6], all = T)
data.2003 <- merge(data.2003, tid.2003)
data.2003 <- merge(data.2003, d2003.teach.m, by = names(d2003.teach.m)[names(d2003.teach.m) %in% names(data.2003)], all = T)
data.2003 <- as.data.table(data.2003)
data.2003 <- data.2003[!is.na(BSMMAT01),]

save(data.2003, file = "data.2003.RData")
gc()
#######################################################
rm(list = ls())
d2007.school <- remove.vars(read.spss("../Data/2007/BCGLTUm4.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2007.school$WGTADJ1 <- round(d2007.school$WGTADJ1, 6)
d2007.stud.teach <- remove.vars(read.spss("../Data/2007/BSTLTUm4.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2007.assesment <- remove.vars(read.spss("../Data/2007/BSALTUm4.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2007.stud <- remove.vars(read.spss("../Data/2007/BSGLTUm4.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2007.teach.m <- remove.vars(read.spss("../Data/2007/BTMLTUm4.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2007.teach.m <- d2007.teach.m[!duplicated(d2007.teach.m$IDTEACH),]
d2007.teach.s <- remove.vars(read.spss("../Data/2007/BTSLTUm4.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2007.teach.reliab <- remove.vars(read.spss("../Data/2007/BSRLTUm4.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
tid.2007 <- unique(subset(d2007.stud.teach, IDGRADER == 2 & MATSUBJ == 1)[, c("IDTEACH", "IDSTUD")])

data.2007 <- merge(d2007.school, d2007.stud, by = names(d2007.stud)[names(d2007.stud) %in% names(d2007.school)], all = T)
data.2007 <- merge(data.2007, tid.2007, all = T)
data.2007 <- merge(data.2007, d2007.teach.m, by = names(d2007.teach.m)[names(d2007.teach.m) %in% names(data.2007)])
data.2007 <- as.data.table(data.2007)
data.2007 <- data.2007[!is.na(BSMMAT01),]

save(data.2007, file = "data.2007.RData")
gc()
#######################################################
rm(list = ls())
d2011.school <- remove.vars(read.spss("../Data/2011/BCGLTUm5.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2011.stud.teach <- remove.vars(read.spss("../Data/2011/BSTLTUm5.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2011.assesment <- remove.vars(read.spss("../Data/2011/BSALTUm5.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2011.stud <- remove.vars(read.spss("../Data/2011/BSGLTUm5.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2011.teach.m <- remove.vars(read.spss("../Data/2011/BTMLTUm5.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
#d2011.teach.m <- d2011.teach.m[!duplicated(d2011.teach.m$IDTEACH),]
d2011.teach.s <- remove.vars(read.spss("../Data/2011/BTSLTUm5.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
d2011.teach.reliab <- remove.vars(read.spss("../Data/2011/BSRLTUm5.sav", to.data.frame = T, use.value.labels=F), "DPCDATE")
tid.2011 <- unique(subset(d2011.stud.teach, IDGRADER == 2 & MATSUBJ == 1)[, c("IDTEALIN", "IDSTUD")])

data.2011 <- merge(d2011.school, d2011.stud, by = names(d2011.stud)[names(d2011.stud) %in% names(d2011.school)], all = T)
data.2011 <- merge(data.2011, tid.2011, all = T)
data.2011 <- merge(data.2011, d2011.teach.m, by = names(d2011.teach.m)[names(d2011.teach.m) %in% names(data.2011)])
data.2011 <- as.data.table(data.2011)
data.2011 <- data.2011[!is.na(BSMMAT01),]
save(data.2011, file = "data.2011.RData")
rm(list = ls())
#######################################################
