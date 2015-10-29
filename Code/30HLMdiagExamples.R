library(HLMdiag)
data(Exam, package = "mlmRev")
sepLM <- adjust_lmList(normexam ~ standLRT + sex + schgend | school, data = Exam)
confint(sepLM)

data(sleepstudy, package = "lme4")
fm <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
# Deleting every Subject
fmDel <- case_delete(model = fm, group = "Subject", type = "both")
# Deleting only subject 308
del308 <- case_delete(model = fm, group = "Subject", type = "both", delete = 308)
# Deleting a subset of subjects
delSubset <- case_delete(model = fm, group = "Subject", type = "both", delete = 308:310)

wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
wages.sepLM <- adjust_lmList(lnw ~ exper | id, data = wages)
rancoef.eb <- coef(wages.fm1)$id
rancoef.ols <- coef(wages.sepLM)
compare_eb_ls(eb = rancoef.eb, ols = rancoef.ols, identify = 0.01)

data(sleepstudy, package = "lme4")
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
varcomp.mer(fm1)


