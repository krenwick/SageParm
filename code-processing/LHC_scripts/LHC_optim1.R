################################################################################
# Latin hypercube sampling to select text values for parameters
# Ranges for parameters are for A. tridentata
# ROUND 2: these runs will be used for optimization on subset of parameters
################################################################################
library(lhs)

setwd("~/Documents/SageParm/automate_tests")
insfile <- "optim1_evergreen.ins" # name of ins file to use
nparms <- 5

# I have 12 parameters to vary
# I want ~ 10*10 sets (100... too few!)

set.seed(1940)
X <- maximinLHS(nparms*30, nparms)
Y <- matrix(NA, nrow(X), ncol(X))
Y[, 1] <- round(qunif(X[, 1], 6, 21),0) # SLA, full range from lit
Y[, 2] <- round(qunif(X[, 2], .5, 1),1) # ltor_max, exact range in global pft vals
Y[, 3] <- round(qunif(X[, 3], 0.6, 1),1) # root_upper
Y[, 4] <- round(qunif(X[, 4], 7, 13),1) # pstemp_low, boreal +/- 30%
Y[, 5] <- round(qunif(X[, 5], -5.2, -2.8 ),1) # pstemp_min, boreal +/- 30%

# gsub: replaces all occurences of a pattern
ins  <- readLines(insfile)
for(i in 1:nrow(X)) {
  tx  <- gsub(pattern = "slaval", replace = Y[i,1], x = ins)
  tx  <- gsub(pattern = "ltor_maxval", replace = Y[i,2], x = tx)
  tx  <- gsub(pattern = "rootdistval", replace = Y[i,3], x = tx)
  tx  <- gsub(pattern = "rootdist2", replace = (1-Y[i,3]), x = tx)
  tx  <- gsub(pattern = "pstemp_lowval", replace = Y[i,4], x = tx)
  tx  <- gsub(pattern = "pstemp_minval", replace = Y[i,5], x = tx)
  insname <-paste("slaa.",Y[i,1],"_ltor.",Y[i,2],"_root.",Y[i,3],"_pslo.",Y[i,4],"_psmi.",Y[i,5], sep="")
  tx  <- gsub(pattern = "outval", replace = paste(insname,".out",sep=""), x = tx)
  writeLines(tx, con=paste("ins/",insname,".ins", sep=""))
}

# split into folders with 32 files each
system("./split_into_subsets.sh")

