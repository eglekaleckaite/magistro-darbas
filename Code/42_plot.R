library(lme4)
library(ggplot2) # Plotting
data("Orthodont",package="MEMSS")
fm1 <- lmer(
  formula = distance ~ age*Sex + (age|Subject)
  , data = Orthodont
)
newdat <- expand.grid(
  age=c(8,10,12,14)
  , Sex=c("Male","Female")
  , distance = 0
)
mm <- model.matrix(terms(fm1),newdat)
newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(fm1),mm))
tvar1 <- pvar1+VarCorr(fm1)$Subject[1]  ## must be adapted for more complex models
newdat <- data.frame(
  newdat
  , plo = newdat$distance-2*sqrt(pvar1)
  , phi = newdat$distance+2*sqrt(pvar1)
  , tlo = newdat$distance-2*sqrt(tvar1)
  , thi = newdat$distance+2*sqrt(tvar1)
)
#plot confidence
g0 <- ggplot(newdat, aes(x=age, y=distance, colour=Sex))+geom_point()
g0 + geom_errorbar(aes(ymin = plo, ymax = phi))+
  ggtitle("CI based on fixed-effects uncertainty ONLY")
#plot prediction
g0 + geom_errorbar(aes(ymin = tlo, ymax = thi))+
  ggtitle("CI based on FE uncertainty + RE variance")




library(ggplot2)
library(lme4)
library(multcomp)
dataset <- expand.grid(experiment = factor(seq_len(10)), status = factor(c("N", "D", "R"), levels = c("N", "D", "R")), reps = seq_len(10))
dataset$value <- rnorm(nrow(dataset), sd = 0.23) + with(dataset, rnorm(length(levels(experiment)), sd = 0.256)[experiment] + ifelse(status == "D", 0.205, ifelse(status == "R", 0.887, 0))) + 2.78
model <- lmer(value~status+(1|experiment), data = dataset)
tmp <- as.data.frame(confint(glht(model, mcp(status = "Tukey")))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()

tmp <- as.data.frame(confint(glht(model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()

model <- lmer(value ~ 0 + status + (1|experiment), data = dataset)
tmp <- as.data.frame(confint(glht(model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()



set.seed(101)
dataset <- expand.grid(experiment = factor(seq_len(10)), 
                       status = factor(c("N", "D", "R"), levels = c("N", "D", "R")), 
                       reps = seq_len(10))
X <- model.matrix(~status,dataset)
dataset <- transform(dataset, 
                     value=rnorm(nrow(dataset), sd = 0.23) +   ## residual
                       rnorm(length(levels(experiment)), sd = 0.256)[experiment] +  ## block effects
                       X %*% c(2.78,0.205,0.887)) 



library(lme4)
model <- lmer(value~status+(1|experiment), data = dataset)

library(coefplot)
coefplot(model)


library(coefplot2)
coefplot2(model)





library(plyr)

d <- data.frame(
  state = rep(c('NY', 'CA'), c(10, 10)), 
  year = rep(1:10, 2), 
  response = c(rnorm(10), rnorm(10))
)

# Create a list of models
# dlply = data frame -> list
models <- dlply(d, ~ state, function(df) { 
  lm(response ~ year, data = df)
})

# Extract the coefficients in a useful form
# ldply = list -> data frame
ldply(models, coef)

# We can get the predictions in a similar way, but we need
# to cast to a data frame so the numbers come out as rows,
# not columns.
predictions <- ldply(models, as.data.frame(predict))



library(lme4)
d <- data.frame(state=rep(c('NY', 'CA'), c(10, 10)), year=rep(1:10, 2), response=c(rnorm(10), rnorm(10)))
fits <- lmList(response ~ year | state, data=d)


op <- par(mfrow=c(2,4))
invisible(lapply(fits@.Data, plot))




#b3 <- lmer (dollars ~ 1 + I(refund/days) + (1 | month) + (1 | regime), data=elect5)



require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)

hdp <- read.csv("http://www.ats.ucla.edu/stat/data/hdp.csv")
hdp <- within(hdp, {
  Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
  DID <- factor(DID)
  HID <- factor(HID)
})

ggpairs(hdp[, c("IL6", "CRP", "LengthofStay", "Experience")])


ggplot(hdp, aes(x = CancerStage, y = LengthofStay)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

tmp <- melt(hdp[, c("CancerStage", "IL6", "CRP")], id.vars="CancerStage")
ggplot(tmp, aes(x = CancerStage, y = value)) +
  geom_jitter(alpha = .1) +
  geom_violin(alpha = .75) +
  facet_grid(variable ~ .) +
  scale_y_sqrt()


tmp <- melt(hdp[, c("remission", "IL6", "CRP", "LengthofStay", "Experience")],
            id.vars="remission")
ggplot(tmp, aes(factor(remission), y = value, fill=factor(remission))) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free_y")


