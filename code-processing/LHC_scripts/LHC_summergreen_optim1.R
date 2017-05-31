################################################################################
# Latin hypercube sampling to select text values for parameters
# Ranges for parameters are for A. tridentata
# ROUND 2: these runs will be used for optimization on subset of parameters:
# sla, root_up, ltor_max, latosa, phengdd5, pstemp_max, pstemp_high, pstemp_lo, pstemp_min
################################################################################
library(lhs)

setwd("~/Documents/SageParm/automate_tests")
insfile <- "optim1_summergreen.ins" # name of ins file to use
nparms <- 9

set.seed(1930)
X <- maximinLHS(nparms*30, nparms)
Y <- matrix(NA, nrow(X), ncol(X))
Y[, 1] <- round(qunif(X[, 1], 6, 21),0) # SLA, full range from lit
Y[, 2] <- round(qunif(X[, 2], .5, 1),1) # ltor_max, exact range in global pft vals
Y[, 3] <- round(qunif(X[, 3], 0.6, 1),1) # root_upper
Y[, 4] <- round(qunif(X[, 4], 1350, 5220),1) # k_latosa, min/max from Ganskopp 1986
Y[, 5] <- round(qunif(X[, 5], 7, 13),1) # pstemp_low, boreal +/- 30%
Y[, 6] <- round(qunif(X[, 6], -5.2, -2.8 ),1) # pstemp_min, boreal +/- 30%
Y[, 7] <- round(qunif(X[, 7], 26.6, 49.4),1) #pstemp_max, bor/temp +/-30%
Y[, 8] <- round(qunif(X[, 8], 17.5, 32.5),1) #pstemp_hi, bor/temp +/-30%
Y[, 9] <- round(qunif(X[, 9], 100, 300),1) #phengdd5ramp, standard +/- 50%

# gsub: replaces all occurences of a pattern
ins  <- readLines(insfile)
for(i in 1:nrow(X)) {
  tx  <- gsub(pattern = "slaval", replace = Y[i,1], x = ins)
  tx  <- gsub(pattern = "ltor_maxval", replace = Y[i,2], x = tx)
  tx  <- gsub(pattern = "rootdistval", replace = Y[i,3], x = tx)
  tx  <- gsub(pattern = "rootdist2", replace = (1-Y[i,3]), x = tx)
  tx  <- gsub(pattern = "k_latosaval", replace = Y[i,4], x = tx)
  tx  <- gsub(pattern = "pstemp_lowval", replace = Y[i,5], x = tx)
  tx  <- gsub(pattern = "pstemp_minval", replace = Y[i,6], x = tx)
  tx  <- gsub(pattern = "pstemp_maxval", replace = Y[i,7], x = tx)
  tx  <- gsub(pattern = "pstemp_hival", replace = Y[i,8], x = tx)
  tx  <- gsub(pattern = "phengdd5rampval", replace = Y[i,9], x = tx)
  insname <-paste("slaa.",Y[i,1],"_ltor.",Y[i,2],"_root.",Y[i,3],"_lasa.",Y[i,4],
                  "_pslo.",Y[i,5],"_psmi.",Y[i,6],"_psma.",Y[i,7],"_pshi.",Y[i,8],
                  "_pgdd.",Y[i,9], sep="")
  tx  <- gsub(pattern = "outval", replace = paste(insname,".out",sep=""), x = tx)
  writeLines(tx, con=paste("ins/",insname,".ins", sep=""))
}

# split into folders with 32 files each
system("./split_into_subsets.sh")

