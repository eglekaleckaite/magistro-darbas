rm(list = ls())
library(ggplot2)
rm(list = ls())
library(foreach)
library(data.table)
library(plyr)
library(reshape2)

probs = c(0.01, 0.05, 0.1, 0.2,
          0.3, 0.4, 0.5, 0.6, 0.7,
          0.8, 0.9, 0.95, 0.99)



############################################################################
############################################################################
## EQM
############################################################################
############################################################################
load("Output/mcmc_quantile_all.RData")

probs1 = c("1%", "5%", "10%", "20%",
           "30%", "40%", "50%", "60%", "70%",
           "80%", "90%", "95%", "99%")
setnames(qres, probs1, as.character(probs))
mqres <- qres[, lapply(.SD, max), .SDcols = as.character(probs), 
              by = c("Parametras", "Metodas", "Paklaidos")]

mmqres <- melt(mqres, id = c("Parametras", "Metodas", "Paklaidos"),
               variable.name = "Percentiliai", value.name = "EQM")
mmqres[, Percentiliai := as.numeric(Percentiliai)]


a <- ggplot(data = mmqres[Paklaidos == "N"& Parametras %in% c("sigma2", "tau00", "tau01", "tau11")], 
            aes(x = Percentiliai, y = EQM, linetype = Metodas))
a <- a + geom_line()+ggtitle(expression(sigma^2))+ facet_grid(. ~ Parametras)


a <- ggplot(data = mmqres[Paklaidos == "N"& Parametras %in% c("g00", "g01", "g10", "g11")], 
            aes(x = Percentiliai, y = EQM, linetype = Metodas))
a <- a + geom_line()+ggtitle(expression(sigma^2))+ facet_grid(. ~ Parametras)


a <- ggplot(data = mmqres[Paklaidos == "N"& Parametras %in% c("tau11")], 
            aes(x = Percentiliai, y = EQM, linetype = Metodas))
a <- a + geom_line()+ggtitle(expression(sigma^2))


############################################################################
############################################################################
## Calculate quantiles
############################################################################
############################################################################
# 
# 
# load("Output/mcmcY1_simulMy.RData")
# load("Output/mcmcY2_simulMy.RData")
# 
# trth <- data.table(Parametras = c("g00", "g01", "g10", "g11", "sigma2"),
#                    Tikroji = c(450, 10, 30, 5, 2000))
# 
# rres <- foreach(cc = c("g00", "g01", "g10", "g11", "sigma2"), .combine = rbind) %do% {
#   data.table(Parametras = cc, Paklaidos = "N", 
#              ldply(rresY1[[cc]],
#                    function(x) ldply(x, function(y) ldply(y, function(z) ldply(z, function(w){
#                      #w[w<0] <- 0
#                      dd <- data.table(t(quantile(w/trth[Parametras == cc, Tikroji], 
#                                                probs = probs)))
#                      return(dd)
#                    }, .id = "V"), .id = "P"), .id = "Balansas"), .id = "Metodas"))
# }
# 
# 
# rres2 <- foreach(cc = c("g00", "g01", "g10", "g11", "sigma2"), .combine = rbind) %do% {
#   data.table(Parametras = cc, Paklaidos = "X", 
#              ldply(rresY2[[cc]],
#                    function(x) ldply(x, function(y) ldply(y, function(z) ldply(z, function(w){
#                      #w[w<0] <- 0
#                      dd <- data.table(t(quantile(w/trth[Parametras == cc, Tikroji], 
#                                                  probs = probs)))
#                      return(dd)
#                    }, .id = "V"), .id = "P"), .id = "Balansas"), .id = "Metodas"))
# }
# 
# ttau <- data.table(V = c("100_50_100", "800_400_800", "2000_1000_2000"), 
#                    tau00 = c(100, 800, 2000),tau01 = c(50, 400, 1000),
#                    tau11 = c(100, 800, 2000))
# 
# rres3 <- foreach(cc = c("tau00", "tau01", "tau11"), .combine = rbind) %do% {
#   data.table(Parametras = cc, Paklaidos = "X", 
#              ldply(rresY2[[cc]],
#                    function(x) ldply(x, function(y) ldply(y, function(z) {
#                      foreach(bb = names(z), .combine = rbind) %do% {
#                        w <- z[[bb]]
#                        w[w<0] <- 0
#                        dd <- data.table(V = bb, t(quantile(w/unlist(ttau[V == bb, cc, with = F]), 
#                                                            probs = probs)))
#                        return(dd)
#                      }}, .id = "P"), .id = "Balansas"), .id = "Metodas"))
# }
# 
# rres4 <- foreach(cc = c("tau00", "tau01", "tau11"), .combine = rbind) %do% {
#   data.table(Parametras = cc, Paklaidos = "N", 
#              ldply(rresY1[[cc]],
#                    function(x) ldply(x, function(y) ldply(y, function(z) {
#                      foreach(bb = names(z), .combine = rbind) %do% {
#                        w <- z[[bb]]
#                        w[w<0] <- 0
#                        dd <- data.table(V = bb, t(quantile(w/unlist(ttau[V == bb, cc, with = F]), 
#                                                            probs = probs)))
#                        return(dd)
#                      }}, .id = "P"), .id = "Balansas"), .id = "Metodas"))
# }
# 
# 
# qres <- rbind(rres, rres2, rres3, rres4)
# 
# save(qres, file = "Output/mcmc_quantile_all.RData")



# 
# lmc <- list()
# load("Output/mcmcY1_simulMy_2000_100_50_100_20.RData")
# lmc[["11"]] <- mcmc
# load("Output/mcmcY1_simulMy_2000_100_50_100_35.RData")
# lmc[["12"]] <- mcmc[1:500,]
# load("Output/mcmcY1_simulMy_2000_100_50_100_80.RData")
# lmc[["13"]] <- mcmc
# load("Output/mcmcY1_simulMy_2000_800_400_800_20.RData")
# lmc[["21"]] <- mcmc
# load("Output/mcmcY1_simulMy_2000_800_400_800_35.RData")
# lmc[["22"]] <- mcmc
# load("Output/mcmcY1_simulMy_2000_800_400_800_80.RData")
# lmc[["23"]] <- mcmc
# load("Output/mcmcY1_simulMy_2000_2000_1000_2000_20.RData")
# lmc[["31"]] <- mcmc
# load("Output/mcmcY1_simulMy_2000_2000_1000_2000_35.RData")
# lmc[["32"]] <- mcmc
# # load("Output/mcmcY1_simulMy_2000_2000_1000_2000_80.RData")
# # lmc[["33"]] <- mcmc



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


