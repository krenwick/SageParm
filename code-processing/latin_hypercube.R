################################################################################
# Latin hypercube sampling to select text values for parameters
# Ranges for parameters are for A. tridentata
################################################################################
library(lhs)

setwd("~/Documents/SageParm/automate_tests")

# I have 12 parameters to vary
# I want at least 10*nparm sets

set.seed(1950)
X <- maximinLHS(640, 15)
Y <- matrix(NA, nrow(X), ncol(X))
Y[, 1] <- round(qunif(X[, 1], 6, 21),1) # SLA, full range from lit
Y[, 2] <- round(qunif(X[, 2], 1350, 5220),1) # k_latosa, min/max from Ganskopp 1986
Y[, 3] <- round(qunif(X[, 3], .35, .65),2) # gmin, standard +/- 30%
Y[, 4] <- round(qunif(X[, 4], .5, 1),2) # ltor_max, exact range in global pft vals
Y[, 5] <- round(qunif(X[, 5], .056, .104),2) # greff_min, standard +/- 30%
Y[, 6] <- round(qunif(X[, 6], 0.6, 1),1) # root_upper
Y[, 7] <- round(qunif(X[, 7], .05, .1),2) # turnover_sap, full range in global pfts 
Y[, 8] <- round(qunif(X[, 8], -5.2, -2.8 ),1) # pstemp_min, boreal +/- 30%
Y[, 9] <- round(qunif(X[, 9], 7, 13),1) # pstemp_low, boreal +/- 30%
Y[, 10] <- round(qunif(X[, 10], .05, .2),2) # est_max, exact range in global vals
Y[, 11] <- round(qunif(X[, 11], 26.6, 49.4),1) #pstemp_max, bor/temp +/-30%
Y[, 12] <- round(qunif(X[, 12], 17.5, 32.5),1) #pstemp_hi, bor/temp +/-30%
Y[, 13] <- round(qunif(X[, 13], 100, 600),1) #kchill_b, 100-600 existing range in pfts
Y[, 14] <- round(qunif(X[, 14], 100, 300),1) #phengdd5ramp, standard +/- 50%
Y[, 15] <- round(qunif(X[, 15], .25, 1),2) #leaflong, range of feasible values

# gsub: replaces all occurences of a pattern
ins  <- readLines("autofit_hyalite.ins")
for(i in 1:nrow(X)) {
  tx  <- gsub(pattern = "slaval", replace = Y[i,1], x = ins)
  tx  <- gsub(pattern = "k_latosaval", replace = Y[i,2], x = tx)
  tx  <- gsub(pattern = "gminval", replace = Y[i,3], x = tx)
  tx  <- gsub(pattern = "ltor_maxval", replace = Y[i,4], x = tx)
  tx  <- gsub(pattern = "greff_minval", replace = Y[i,5], x = tx)
  tx  <- gsub(pattern = "rootdistval", replace = Y[i,6], x = tx)
  tx  <- gsub(pattern = "rootdist2", replace = (1-Y[i,6]), x = tx)
  tx  <- gsub(pattern = "turnover_sapval", replace = Y[i,7], x = tx)
  tx  <- gsub(pattern = "pstemp_minval", replace = Y[i,8], x = tx)
  tx  <- gsub(pattern = "pstemp_lowval", replace = Y[i,9], x = tx)
  tx  <- gsub(pattern = "est_maxval", replace = Y[i,10], x = tx)
  tx  <- gsub(pattern = "pstemp_maxval", replace = Y[i,11], x = tx)
  tx  <- gsub(pattern = "pstemp_hival", replace = Y[i,12], x = tx)
  tx  <- gsub(pattern = "k_chillbval", replace = Y[i,13], x = tx)
  tx  <- gsub(pattern = "phengdd5rampval", replace = Y[i,14], x = tx)
  tx  <- gsub(pattern = "leaflongval", replace = Y[i,15], x = tx)
  insname <-paste("slaa.",Y[i,1],"_lasa.",Y[i,2],"_gmin.",Y[i,3],"_ltor.",Y[i,4],
                  "_gref.",Y[i,5],"_root.",Y[i,6],"_tsap.",Y[i,7],"_psmi.",
                  Y[i,8],"_pslo.",Y[i,9],"_estm.",Y[i,10],"_psma.",Y[i,11],
                  "_pshi.",Y[i,12],"_chil.",Y[i,13],"_pgdd.",Y[i,14],"_llon.",Y[i,15], sep="")
  tx  <- gsub(pattern = "outval", replace = paste(insname,".out",sep=""), x = tx)
  writeLines(tx, con=paste("ins/",insname,".ins", sep=""))
}

# split into folders with 32 files each
system("./split_into_subsets.sh")

