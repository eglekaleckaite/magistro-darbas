rm(list = ls())
library(foreach)
library(data.table)
library(plyr)
library(reshape2)
library(xtable)

source("10code.R")

############################################################################
############################################################################
## Statistics
############################################################################
############################################################################
load("Output/mcmc_stats_all.RData")
rres[Balansas == "UB" & P == "20", P := "P1"]
rres[Balansas == "B" & P == "20", P := "P2"]
rres[Balansas == "UB" & P == "35", P := "P3"]
rres[Balansas == "B" & P == "35", P := "P4"]
rres[Balansas == "UB" & P == "80", P := "P5"]
rres[Balansas == "B" & P == "80", P := "P6"]
rres[, Balansas := NULL]

rres[V == "100_50_100", V := "V1"]
rres[V == "800_400_800", V := "V2"]
rres[V == "2000_1000_2000", V := "V3"]

parr <- data.table(Parametras = c("g00", "g01", "g10", "g11", "sigma2", "tau00", "tau01", "tau11"),
                   Z = c("\\hat{\\gamma}_{00}", "\\hat{\\gamma}_{01}", "\\hat{\\gamma}_{10}", "\\hat{\\gamma}_{11}",
                         "\\hat{\\sigma}^2", "\\hat{\\tau}_{00}", "\\hat{\\tau}_{01}", "\\hat{\\tau}_{11}"))

pres <- foreach(cc = c("g00", "g01", "g10", "g11", "sigma2", "tau00", "tau01", "tau11")) %do% {
  pres <- rres[Parametras == cc]
  pres <- pres[, list(Metodas = Metodas, th = th, MRBIAS = boldmin(MRBIAS),
              MRMSE = boldmin(MRMSE)), by = c("P", "V", "Paklaidos")]
  pres <- arrange(pres, P, V, Paklaidos, Metodas)
  ppres <- cbind(pres[Metodas == "REML", ], pres[Metodas == "MINQUE(0)", ],
                  pres[Metodas == "MINQUE(1)", ], pres[Metodas == "MINQUE(th)", ])
  ppres[, c(3, 4, 8:11, 15:18, 22:25) := NULL]
  
  ppres[, P := c("\\multirow{6}{*}{P1}", rep("", 5), 
                 "\\multirow{6}{*}{P2}", rep("", 5),
                 "\\multirow{6}{*}{P3}", rep("", 5),
                 "\\multirow{6}{*}{P4}", rep("", 5),
                 "\\multirow{6}{*}{P5}", rep("", 5),
                 "\\multirow{6}{*}{P6}", rep("", 5))]
  ppres[, V := rep(c("\\multirow{2}{*}{V1}", "", "\\multirow{2}{*}{V2}", "", "\\multirow{2}{*}{V3}", ""), 6)]
  setnames(ppres, c("", "", paste0("$", unlist(parr[Parametras == cc, Z]), "$"), "MRBIAS", "MRMSE", paste0("$", unlist(parr[Parametras == cc, Z]), "$"), "MRBIAS", "MRMSE", paste0("$", unlist(parr[Parametras == cc, Z]), "$"), "MRBIAS", "MRMSE", paste0("$", unlist(parr[Parametras == cc, Z]), "$"), "MRBIAS", "MRMSE"))
   return(ppres)
}
names(pres) <- c("g00", "g01", "g10", "g11", "sigma2", "tau00", "tau01", "tau11")


addtorow <- list()
addtorow$pos <- list(-1, 6, 12, 18, 24, 30)
addtorow$command <- c(" & & \\multicolumn{3}{c|}{REML}&\\multicolumn{3}{c|}{MINQUE(0)}&\\multicolumn{3}{c|}{MINQUE(1)}&\\multicolumn{3}{c|}{MINQUE($\\theta$)}\\\\",
                      "\\hline \\hline", "\\hline \\hline", "\\hline \\hline", "\\hline \\hline", "\\hline \\hline")

print( xtable(pres[["tau11"]], digits=3, 
              align = c("c","c","c|","c","c","c|","c","c","c|","c","c","c|","c","c","c|")),
              include.rownames = FALSE, 
       add.to.row = addtorow, sanitize.colnames.function = identity, 
       sanitize.text.function = identity, size = "footnotesize",
       floating.environment = "sidewaystable")

# ############################################################################
# ############################################################################
# ## Compound statistics
# ############################################################################
# ############################################################################
# load("Output/mcmc_stats_all.RData")
# rres[Balansas == "UB" & P == "20", P := "P1"]
# rres[Balansas == "B" & P == "20", P := "P2"]
# rres[Balansas == "UB" & P == "35", P := "P3"]
# rres[Balansas == "B" & P == "35", P := "P4"]
# rres[Balansas == "UB" & P == "80", P := "P5"]
# rres[Balansas == "B" & P == "80", P := "P6"]
# rres[, Balansas := NULL]
# 
# rres[V == "100_50_100", V := "V1"]
# rres[V == "800_400_800", V := "V2"]
# rres[V == "2000_1000_2000", V := "V3"]
# 
# fres <- rres[Parametras %in% c("g00", "g01", "g10", "g11"), ]
# rrres <- rres[Parametras %in% c("sigma2", "tau00", "tau01", "tau11"), ]
# 
# fcres <- fres[, list(CAMRBIAS = mean(abs(MRBIAS)), CRMSE = mean(MRMSE)),
#               by = c("Paklaidos", "Metodas", "P", "V")]
# rcres <- rrres[, list(CAMRBIAS = mean(abs(MRBIAS)), CRMSE = mean(MRMSE)),
#                by = c("Paklaidos", "Metodas", "P", "V")]
# 
# fcres <- fcres[, list(Metodas = Metodas, CAMRBIAS = boldmin(CAMRBIAS),
#                       CRMSE = boldmin(CRMSE)), by = c("P", "V", "Paklaidos")]
# rcres <- rcres[, list(Metodas = Metodas, CAMRBIAS = boldmin(CAMRBIAS),
#                       CRMSE = boldmin(CRMSE)), by = c("P", "V", "Paklaidos")]
# 
# fcres <- arrange(fcres, P, V, Paklaidos, Metodas)
# rcres <- arrange(rcres, P, V, Paklaidos, Metodas)
# 
# ffcres <- cbind(fcres[Metodas == "REML", ], fcres[Metodas == "MINQUE(0)", ],
#                 fcres[Metodas == "MINQUE(1)", ], fcres[Metodas == "MINQUE(th)", ])
# 
# ffcres[, c(3, 4, 7:10, 13:16, 19:22) := NULL]
# 
# rrcres <- cbind(rcres[Metodas == "REML", ], rcres[Metodas == "MINQUE(0)", ],
#                 rcres[Metodas == "MINQUE(1)", ], rcres[Metodas == "MINQUE(th)", ])
# 
# rrcres[, c(3, 4, 7:10, 13:16, 19:22) := NULL]
# 
# ffcres[, P := c("\\multirow{6}{*}{P1}", rep("", 5), 
#                 "\\multirow{6}{*}{P2}", rep("", 5),
#                 "\\multirow{6}{*}{P3}", rep("", 5),
#                 "\\multirow{6}{*}{P4}", rep("", 5),
#                 "\\multirow{6}{*}{P5}", rep("", 5),
#                 "\\multirow{6}{*}{P6}", rep("", 5))]
# rrcres[, P := c("\\multirow{6}{*}{P1}", rep("", 5), 
#                "\\multirow{6}{*}{P2}", rep("", 5),
#                "\\multirow{6}{*}{P3}", rep("", 5),
#                "\\multirow{6}{*}{P4}", rep("", 5),
#                "\\multirow{6}{*}{P5}", rep("", 5),
#                "\\multirow{6}{*}{P6}", rep("", 5))]
# 
# ffcres[, V := rep(c("\\multirow{2}{*}{V1}", "", "\\multirow{2}{*}{V2}", "", "\\multirow{2}{*}{V3}", ""), 6)]
# rrcres[, V := rep(c("\\multirow{2}{*}{V1}", "", "\\multirow{2}{*}{V2}", "", "\\multirow{2}{*}{V3}", ""), 6)]
# 
# addtorow <- list()
# addtorow$pos <- list(-1, 6, 12, 18, 24, 30)
# addtorow$command <- c(" & & \\multicolumn{2}{c|}{REML}&\\multicolumn{2}{c|}{MINQUE(0)}&\\multicolumn{2}{c|}{MINQUE(1)}&\\multicolumn{2}{c|}{MINQUE($\\theta$)}\\\\",
#                       "\\hline \\hline", "\\hline \\hline", "\\hline \\hline", "\\hline \\hline", "\\hline \\hline")
# 
# 
# print( xtable(ffcres, digits=3, 
#               align = c("c","c","c|","c","c|","c","c|","c","c|","c","c|")),
#        include.rownames = FALSE, 
#        add.to.row = addtorow, sanitize.colnames.function = identity, 
#        sanitize.text.function = identity, size = "footnotesize",
#        floating.environment = "sidewaystable")
# print( xtable(rrcres, digits=3, 
#               align = c("c","c","c|","c","c|","c","c|","c","c|","c","c|")),
#        include.rownames = FALSE, 
#        add.to.row = addtorow, sanitize.colnames.function = identity, 
#        sanitize.text.function = identity, size = "footnotesize",
#        floating.environment = "sidewaystable")

############################################################################
############################################################################
## Results statistics
############################################################################
############################################################################
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
#                      dd <- data.table(th = mean(w), MRBIAS = mean(w/trth[Parametras == cc, Tikroji]-1), MRMSE = mean((w/trth[Parametras == cc, Tikroji]-1)^2))
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
#                      dd <- data.table(th = mean(w), MRBIAS = mean(w/trth[Parametras == cc, Tikroji]-1), MRMSE = mean((w/trth[Parametras == cc, Tikroji]-1)^2))
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
#                        if(cc!="tau01")
#                          w[w<0] <- 0
#                        dd <- data.table(V = bb, th = mean(w), MRBIAS = mean(w/unlist(ttau[V == bb, cc, with = F])-1), MRMSE = mean((w/unlist(ttau[V == bb, cc, with = F])-1)^2))
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
#                        if(cc!="tau01")
#                          w[w<0] <- 0
#                        dd <- data.table(V = bb, th = mean(w), MRBIAS = mean(w/unlist(ttau[V == bb, cc, with = F])-1), MRMSE = mean((w/unlist(ttau[V == bb, cc, with = F])-1)^2))
#                        return(dd)
#                      }}, .id = "P"), .id = "Balansas"), .id = "Metodas"))
# }
# 
# 
# rres <- rbind(rres, rres2, rres3, rres4)
# 
# save(rres, file = "Output/mcmc_stats_all.RData")



############################################################################
############################################################################
## combine output files
############################################################################
############################################################################

# rresY1 <- list()
# 
# foreach(aa = c("20", "35", "80"), .final = NULL) %do% {
#   foreach(bb = c("100_50_100", "800_400_800", "2000_1000_2000"), .final = NULL) %do% {
#     load(paste0("Output/mcmcY1_simulMy_2000_", bb, "_", aa, ".RData"))
#     # REML
#     dd <- alply(mcmc[, c(1:4, 29, 36, 37, 39)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY1[[cc]][["REML"]][["UB"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUE0
#     dd <- alply(mcmc[, c(5:8, 30, 40, 41, 43)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY1[[cc]][["MINQUE(0)"]][["UB"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUE1
#     dd <- alply(mcmc[, c(13:16, 32, 48, 49, 51)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY1[[cc]][["MINQUE(1)"]][["UB"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     
#     load(paste0("Output/mcmcY1_simulMy_2000_", bb, "_", aa, "_add.RData"))
#     #MINQUEth
#     dd <- alply(mcmc[, c(5:8, 10, 15, 16, 18)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY1[[cc]][["MINQUE(th)"]][["UB"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     NULL
#   }
#   NULL
# }
# 
# 
# foreach(aa = c("20", "35", "80"), .final = NULL) %do% {
#   foreach(bb = c("100_50_100", "800_400_800", "2000_1000_2000"), .final = NULL) %do% {
#     load(paste0("Output/mcmcY1_simulMy_2000_", bb, "_", aa, "_fixed.RData"))
#     # REML
#     dd <- alply(mcmc[, c(1:4, 17, 21, 22, 24)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY1[[cc]][["REML"]][["B"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUE0
#     dd <- alply(mcmc[, c(5:8, 18, 25, 26, 28)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY1[[cc]][["MINQUE(0)"]][["B"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUE1
#     dd <- alply(mcmc[, c(9:12, 19, 29, 30, 32)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY1[[cc]][["MINQUE(1)"]][["B"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     #MINQUEth
#     dd <- alply(mcmc[, c(13:16, 20, 33, 34, 36)], 2)
#     names(dd) <- c("g00", "g01", "g10", "g11", 
#                    "sigma2", 
#                    "tau00", "tau01", "tau11")
#     foreach(cc = names(dd), .final = NULL) %do% {
#       rresY1[[cc]][["MINQUE(th)"]][["B"]][[aa]][[bb]] <- dd[[cc]]
#     }
#     NULL
#   }
#   NULL
# }
# 
# 
# save(rresY1, file = "Output/mcmcY1_simulMy.RData")

