rm(list = ls())
library(foreach)
library(data.table)
library(plyr)

############################################################################
############################################################################
## Y1
############################################################################
############################################################################
load("Output/mcmcY1_simulMy.RData")
load("Output/mcmcY2_simulMy.RData")




# 
# rresY2 <- list()
# 
# foreach(aa = c("20", "35", "80"), .final = NULL) %do% {
#   foreach(bb = c("100_50_100", "800_400_800", "2000_1000_2000"), .final = NULL) %do% {
#     load(paste0("Output/mcmcY2_simulMy_2000_", bb, "_", aa, ".RData"))
#     rresY2[["REML"]][["UB"]][[aa]][[bb]] <- alply(mcmc[, c(1:4, 29, 36, 37, 39)], 2)
#     names(rresY2[["REML"]][["UB"]][[aa]][[bb]]) <- c("g00", "g01", "g10", "g11", 
#                                                   "sigma2", 
#                                                   "tau00", "tau01", "tau11")
#     rresY2[["MINQUE(0)"]][["UB"]][[aa]][[bb]] <- alply(mcmc[, c(5:8, 30, 40, 41, 43)], 2)
#     names(rresY2[["MINQUE(0)"]][["UB"]][[aa]][[bb]]) <- c("g00", "g01", "g10", "g11", 
#                                                        "sigma2", 
#                                                        "tau00", "tau01", "tau11")
#     rresY2[["MINQUE(1)"]][["UB"]][[aa]][[bb]] <- alply(mcmc[, c(13:16, 32, 48, 49, 51)], 2)
#     names(rresY2[["MINQUE(1)"]][["UB"]][[aa]][[bb]]) <- c("g00", "g01", "g10", "g11", 
#                                                        "sigma2", 
#                                                        "tau00", "tau01", "tau11")
#     
#     load(paste0("Output/mcmcY2_simulMy_2000_", bb, "_", aa, "_add.RData"))
#     rresY2[["MINQUE(th)"]][["UB"]][[aa]][[bb]] <- alply(mcmc[, c(5:8, 10, 15, 16, 18)], 2)
#     names(rresY2[["MINQUE(th)"]][["UB"]][[aa]][[bb]]) <- c("g00", "g01", "g10", "g11", 
#                                                         "sigma2", 
#                                                         "tau00", "tau01", "tau11")
#     NULL
#   }
#   NULL
# }
# 
# 
# foreach(aa = c("20", "35", "80"), .final = NULL) %do% {
#   foreach(bb = c("100_50_100", "800_400_800", "2000_1000_2000"), .final = NULL) %do% {
#     load(paste0("Output/mcmcY2_simulMy_2000_", bb, "_", aa, "_fixed.RData"))
#     rresY2[["REML"]][["B"]][[aa]][[bb]] <- alply(mcmc[, c(1:4, 17, 21, 22, 24)], 2)
#     names(rresY2[["REML"]][["B"]][[aa]][[bb]]) <- c("g00", "g01", "g10", "g11", 
#                                                      "sigma2", 
#                                                      "tau00", "tau01", "tau11")
#     rresY2[["MINQUE(0)"]][["B"]][[aa]][[bb]] <- alply(mcmc[, c(5:8, 18, 25, 26, 28)], 2)
#     names(rresY2[["MINQUE(0)"]][["B"]][[aa]][[bb]]) <- c("g00", "g01", "g10", "g11", 
#                                                           "sigma2", 
#                                                           "tau00", "tau01", "tau11")
#     rresY2[["MINQUE(1)"]][["B"]][[aa]][[bb]] <- alply(mcmc[, c(9:12, 19, 29, 30, 32)], 2)
#     names(rresY2[["MINQUE(1)"]][["B"]][[aa]][[bb]]) <- c("g00", "g01", "g10", "g11", 
#                                                           "sigma2", 
#                                                           "tau00", "tau01", "tau11")
#     rresY2[["MINQUE(th)"]][["B"]][[aa]][[bb]] <- alply(mcmc[, c(13:16, 20, 33, 34, 36)], 2)
#     names(rresY2[["MINQUE(th)"]][["B"]][[aa]][[bb]]) <- c("g00", "g01", "g10", "g11", 
#                                                            "sigma2", 
#                                                            "tau00", "tau01", "tau11")
#     NULL
#   }
#   NULL
# }
# 
# save(rresY2, file = "Output/mcmcY2_simulMy.RData")
# 
