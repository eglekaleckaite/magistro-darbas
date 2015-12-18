rm(list = ls())
library(foreach)
library(data.table)
library(plyr)


load("Output/mcmcY1_simulMy.RData")
load("Output/mcmcY2_simulMy.RData")
############################################################################
############################################################################
## Results statistics
############################################################################
############################################################################
# unscale
trth <- data.table(Parametras = c("g00", "g01", "g10", "g11", "sigma2"),
                   Tikroji = c(450, 10, 30, 5, 2000))

rres <- foreach(cc = c("g00", "g01", "g10", "g11", "sigma2"), .combine = rbind) %do% {
  data.table(Parametras = cc, Paklaidos = "N", 
             ldply(rresY1[[cc]],
                   function(x) ldply(x, function(y) ldply(y, function(z) ldply(z, function(w){
                     w[w<0] <- 0
                     dd <- data.table(th = mean(w), MRBIAS = mean(w/trth[Parametras == cc, Tikroji]-1), MRMSE = mean((w/trth[Parametras == cc, Tikroji]-1)^2))
                     return(dd)
                   }, .id = "V"), .id = "P"), .id = "Balansas"), .id = "Metodas"))
}


rres2 <- foreach(cc = c("g00", "g01", "g10", "g11", "sigma2"), .combine = rbind) %do% {
  data.table(Parametras = cc, Paklaidos = "X", 
             ldply(rresY2[[cc]],
                   function(x) ldply(x, function(y) ldply(y, function(z) ldply(z, function(w){
                     w[w<0] <- 0
                     dd <- data.table(th = mean(w), MRBIAS = mean(w/trth[Parametras == cc, Tikroji]-1), MRMSE = mean((w/trth[Parametras == cc, Tikroji]-1)^2))
                     return(dd)
                   }, .id = "V"), .id = "P"), .id = "Balansas"), .id = "Metodas"))
}

ttau <- data.table(V = c("100_50_100", "800_400_800", "2000_1000_2000"), 
                   tau00 = c(100, 800, 2000),tau01 = c(50, 400, 1000),
                   tau11 = c(100, 800, 2000))

rres3 <- foreach(cc = c("tau00", "tau01", "tau11"), .combine = rbind) %do% {
  data.table(Parametras = cc, Paklaidos = "X", 
             ldply(rresY2[[cc]],
                   function(x) ldply(x, function(y) ldply(y, function(z) {
                     foreach(bb = names(z), .combine = rbind) %do% {
                       w <- z[[bb]]
                       w[w<0] <- 0
                       dd <- data.table(V = bb, th = mean(w), MRBIAS = mean(w/unlist(ttau[V == bb, cc, with = F])-1), MRMSE = mean((w/unlist(ttau[V == bb, cc, with = F])-1)^2))
                       return(dd)
                     }}, .id = "P"), .id = "Balansas"), .id = "Metodas"))
}

rres4 <- foreach(cc = c("tau00", "tau01", "tau11"), .combine = rbind) %do% {
  data.table(Parametras = cc, Paklaidos = "N", 
             ldply(rresY1[[cc]],
                   function(x) ldply(x, function(y) ldply(y, function(z) {
                     foreach(bb = names(z), .combine = rbind) %do% {
                       w <- z[[bb]]
                       w[w<0] <- 0
                       dd <- data.table(V = bb, th = mean(w), MRBIAS = mean(w/unlist(ttau[V == bb, cc, with = F])-1), MRMSE = mean((w/unlist(ttau[V == bb, cc, with = F])-1)^2))
                       return(dd)
                     }}, .id = "P"), .id = "Balansas"), .id = "Metodas"))
}


rres <- rbind(rres, rres2, rres3, rres4)

save(rres, file = "Output/mcmc_stats_all.RData")



############################################################################
############################################################################
## combine output files
############################################################################
############################################################################

# rresY2 <- list()
# 
# foreach(aa = c("20", "35", "80"), .final = NULL) %do% {
#   foreach(bb = c("100_50_100", "800_400_800", "2000_1000_2000"), .final = NULL) %do% {
#     load(paste0("Output/mcmcY2_simulMy_2000_", bb, "_", aa, ".RData"))
#     # REML
#     dd <- alply(mcmc[, c(1:4, 29, 36, 37, 39)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY2[[cc]][["REML"]][["UB"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUE0
#     dd <- alply(mcmc[, c(5:8, 30, 40, 41, 43)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY2[[cc]][["MINQUE(0)"]][["UB"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUE1
#     dd <- alply(mcmc[, c(13:16, 32, 48, 49, 51)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY2[[cc]][["MINQUE(1)"]][["UB"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     
#     load(paste0("Output/mcmcY2_simulMy_2000_", bb, "_", aa, "_add.RData"))
#     #MINQUEth
#     dd <- alply(mcmc[, c(5:8, 10, 15, 16, 18)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY2[[cc]][["MINQUE(th)"]][["UB"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     NULL
#   }
#   NULL
# }
# 
# 
# foreach(aa = c("20", "35", "80"), .final = NULL) %do% {
#   foreach(bb = c("100_50_100", "800_400_800", "2000_1000_2000"), .final = NULL) %do% {
#     load(paste0("Output/mcmcY2_simulMy_2000_", bb, "_", aa, "_fixed.RData"))
#     # REML
#     dd <- alply(mcmc[, c(1:4, 17, 21, 22, 24)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY2[[cc]][["REML"]][["B"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUE0
#     dd <- alply(mcmc[, c(5:8, 18, 25, 26, 28)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY2[[cc]][["MINQUE(0)"]][["B"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUE1
#     dd <- alply(mcmc[, c(9:12, 19, 29, 30, 32)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY2[[cc]][["MINQUE(1)"]][["B"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUEth
#     dd <- alply(mcmc[, c(13:16, 20, 33, 34, 36)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY2[[cc]][["MINQUE(th)"]][["B"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     NULL
#   }
#   NULL
# }
# 
# save(rresY2, file = "Output/mcmcY2_simulMy.RData")

