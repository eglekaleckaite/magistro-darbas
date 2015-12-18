library(ggplot2)

rm(list = ls())

lmc <- list()
load("Output/mcmcY1_simulMy_2000_100_50_100_20.RData")
lmc[["11"]] <- mcmc
load("Output/mcmcY1_simulMy_2000_100_50_100_35.RData")
lmc[["12"]] <- mcmc[1:500,]
load("Output/mcmcY1_simulMy_2000_100_50_100_80.RData")
lmc[["13"]] <- mcmc
load("Output/mcmcY1_simulMy_2000_800_400_800_20.RData")
lmc[["21"]] <- mcmc
load("Output/mcmcY1_simulMy_2000_800_400_800_35.RData")
lmc[["22"]] <- mcmc
load("Output/mcmcY1_simulMy_2000_800_400_800_80.RData")
lmc[["23"]] <- mcmc
load("Output/mcmcY1_simulMy_2000_2000_1000_2000_20.RData")
lmc[["31"]] <- mcmc
load("Output/mcmcY1_simulMy_2000_2000_1000_2000_35.RData")
lmc[["32"]] <- mcmc
# load("Output/mcmcY1_simulMy_2000_2000_1000_2000_80.RData")
# lmc[["33"]] <- mcmc

probs = c(0.01, 0.05, 0.1, 0.2,
          0.3, 0.4, 0.5, 0.6, 0.7,
          0.8, 0.9, 0.95, 0.99)

sigma2REML <- sapply(lmc, function(x) quantile(x[,29]/2000,
                                           probs = probs))
EQMs2REML <- apply(sigma2REML, 1, max)



sigma2MINQUE <- sapply(lmc, function(x) quantile(x[,30]/2000,
                                               probs = probs))
EQMs2MINQUE <- apply(sigma2MINQUE, 1, max)

plot(probs, EQMs2REML, type = "l")
lines(probs, EQMs2MINQUE, type = "b")

dt <- rbind(data.frame(Percentiliai = probs, Metodas = "REML", EQM = EQMs2REML),
            data.frame(Percentiliai = probs, Metodas = "MINQUE(0)", EQM = EQMs2MINQUE))

a <- ggplot(data = dt, aes(x = Percentiliai, y = EQM, linetype = Metodas))
a <- a + geom_line()+ggtitle(expression(sigma^2))

#############################################################################
#############################################################################
# Y2
#############################################################################
#############################################################################
rm(list = ls())

lmc <- list()
load("Output/mcmcY2_simulMy_2000_100_50_100_20.RData")
lmc[["11"]] <- mcmc
load("Output/mcmcY2_simulMy_2000_100_50_100_35.RData")
lmc[["12"]] <- mcmc[1:500,]
load("Output/mcmcY2_simulMy_2000_100_50_100_80.RData")
lmc[["13"]] <- mcmc
load("Output/mcmcY2_simulMy_2000_800_400_800_20.RData")
lmc[["21"]] <- mcmc
load("Output/mcmcY2_simulMy_2000_800_400_800_35.RData")
lmc[["22"]] <- mcmc
load("Output/mcmcY2_simulMy_2000_800_400_800_80.RData")
lmc[["23"]] <- mcmc
load("Output/mcmcY2_simulMy_2000_2000_1000_2000_20.RData")
lmc[["31"]] <- mcmc
load("Output/mcmcY2_simulMy_2000_2000_1000_2000_35.RData")
lmc[["32"]] <- mcmc
load("Output/mcmcY2_simulMy_2000_2000_1000_2000_80.RData")
lmc[["33"]] <- mcmc

probs = c(0.01, 0.05, 0.1, 0.2,
          0.3, 0.4, 0.5, 0.6, 0.7,
          0.8, 0.9, 0.95, 0.99)

sigma2REML <- sapply(lmc, function(x) quantile(x[,29]/2000,
                                               probs = probs))
EQMs2REML <- apply(sigma2REML, 1, max)



sigma2MINQUE <- sapply(lmc, function(x) quantile(x[,31]/2000,
                                                 probs = probs))
EQMs2MINQUE <- apply(sigma2MINQUE, 1, max)

plot(probs, EQMs2REML, type = "l")
lines(probs, EQMs2MINQUE, type = "b")

#############################################################################
#############################################################################
# Y2
#############################################################################
#############################################################################


