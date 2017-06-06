################################################################################
# Use Levenberg-Marquardt NLS algorithm to optimize parms from summergreen run
# Also code to try optimizing with L-BFGS-B algorithm
################################################################################
rm(list=ls())

#library(minpack.lm)
library(tidyverse)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm")

# Read in monthly GPP data
df3 <- read.csv("data/RCflux_15_16.csv")
GPP <- df3 %>% filter(Variable=="GPP")

# Read in modis LAI data:


# Function to run LPJ-GUESS and return vector of residuals.
# First argument to function must be par
# Depends on ins and flux data already existing in memory

# First, read ins into memory:
ins  <- readLines("LPJfiles/summergreen_optim1_LM.ins") # only outputs GPP
LPJG <- function(par) {
  tx  <- gsub(pattern = "slaval", replace = par[1], x = ins)
  tx  <- gsub(pattern = "k_latosaval", replace = par[2], x = tx)
  tx  <- gsub(pattern = "ltor_maxval", replace = par[3], x = tx)
  tx  <- gsub(pattern = "rootdistval", replace = par[4], x = tx)
  tx  <- gsub(pattern = "rootdist2", replace = (1-par[4]), x = tx)
  tx  <- gsub(pattern = "pstemp_minval", replace = par[5], x = tx)
  tx  <- gsub(pattern = "pstemp_lowval", replace = par[6], x = tx)
  tx  <- gsub(pattern = "pstemp_maxval", replace = par[7], x = tx)
  tx  <- gsub(pattern = "pstemp_hival", replace = par[8], x = tx)
  tx  <- gsub(pattern = "phengdd5rampval", replace = par[9], x = tx)
  writeLines(tx, con="LMopt/tempins.ins")
  
  cat(par, "\n")
  # Run model using new ins file
  system("/Users/poulterlab1/version-control/LPJ-GUESS/ModelFiles/modules/./guess /Users/poulterlab1/Documents/SageParm/LMopt/tempins.ins")
  
  print("Parameter values:")
  print(round(par,2))
  # Read in output from model
  mod <- fread("ModOut/Levenberg_M/mgpp.txt", header=T) %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,Model, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable="GPP") %>%
    select(Year, Month,Variable,Site,Model)
  b <- merge(mod,GPP, by=c("Year","Month","Variable","Site"))
  #return (b$Model - b$Tower) # if returning raw residuals (for nls.lm)
  # if returning sum squared residuals (for optim)
  # resid <- b %>% mutate(resid2=(Model-Tower)^2) %>% 
  #   group_by(Variable) %>% summarise(SSR=sum(resid2))
  # return (resid$SSR)
  # For RMSE:
  resid <- b %>% mutate(resid2=(Model-Tower)^2) %>% 
    group_by(Variable) %>% summarise(SSR=sqrt(mean(resid2)))
  return (as.numeric(resid$SSR))
}


start <- c(8,3200, .7, .8,-4,10, 38,25,200) # starting values
low <- c(6,1350, .5, .6, -5.2,7, 26.6,17.5,100) #lower bound
up <- c(21,5220, 1, 1, -2.8,13, 49.4,32.5,300) #upper bound for ech parameter (default is inf)
steps <- c(2,50,.1,.1,1,1,1,1,50)
ptm <- proc.time()
t1 <- nls.lm(par=start,lower=low,upper=up,fn=LPJG)
proc.time() - ptm
summary(t1)
print(t1)

ptm <- proc.time()
LPJG(start)
proc.time() - ptm # 1.85 minutes to run once

# Finished
# > summary(t1)
# Error in chol.default(object$hessian) : 
#   the leading minor of order 4 is not positive definite
# > print(t1)
# Nonlinear regression via the Levenberg-Marquardt algorithm
# parameter estimates: 14, 3200, 0.5, 1, 0.08, 0.6, 0.1, -4, 10, 0.2, 38, 25, 100, 200, 0.5 
# residual sum-of-squares: 0.05135
# reason terminated: Relative error between `par' and the solution is at most `ptol'.

# Message from running with new optim1 summergreen parms:
# Nonlinear regression via the Levenberg-Marquardt algorithm
# parameter estimates: 14, 3200, 1, 0.6, -4, 10, 38, 25, 200 
# residual sum-of-squares: 0.05214
# reason terminated: The cosine of the angle between `fvec' and any column of the Jacobian is at most `gtol' in absolute value.

################################################################################
# Re-try with the other function:
ptm <- proc.time()
t1 <- nlsLM(par=start,lower=low,upper=up,formula=LPJG, trace=T)
proc.time() - ptm
summary(t1)

# Try with optim
ptm <- proc.time()
t1 <- optim(par=start,lower=low,upper=up,fn=LPJG, method="L-BFGS-B",
            control=list(trace=6,ndeps=steps))
proc.time() - ptm # took 35 minutes

# Try just BFGS:
ptm <- proc.time()
t1 <- optim(par=start,lower=low,upper=up,fn=LPJG, method="BFGS",
            control=list(trace=6,ndeps=steps))
proc.time() - ptm 

# Result:
# Finished
# At iterate     0  f=     0.030245  |proj g|=            0
# 
# iterations 0
# function evaluations 1
# segments explored during Cauchy searches 0
# BFGS updates skipped 0
# active bounds at final generalized Cauchy point 0
# norm of the final projected gradient 0
# final function value 0.030245
# 
# X = 8 3200 0.7 0.8 -4 10 38 25 200 
# F = 0.030245
# final  value 0.030245 
# converged
# Warning message:
#   In optim(par = start, lower = low, upper = up, fn = LPJG, method = "BFGS",  :
#              bounds can only be used with method L-BFGS-B (or Brent)
#            > proc.time() - ptm
#            user   system  elapsed 
#            175.179 1380.500 1559.141 

# Finished
# At iterate     0  f=     0.030245  |proj g|=            0
# 
# iterations 0
# function evaluations 1
# segments explored during Cauchy searches 0
# BFGS updates skipped 0
# active bounds at final generalized Cauchy point 0
# norm of the final projected gradient 0
# final function value 0.030245
# 
# X = 8 3200 0.7 0.8 -4 10 38 25 200 
# F = 0.030245
# final  value 0.030245 
# converged
# > proc.time() - ptm
# user   system  elapsed 
# 171.435 1357.418 1532.158 
t1$par
t1$value
t1$counts
t1$convergence #if default pgtol is zero, cost function can't be negative
t1$message

# I suspect that this will never work if the parameters are on such diff scales
# Try again but convert parms to a 0-1 scale... seems tricky

#####################
#install.packages("hydromad", repos="http://hydromad.catchment.org")
library(hydromad)
ptm <- proc.time()
t1 <- SCEoptim(par=start,lower=low,upper=up,FUN=LPJG, 
            control=list(trace=6, returnpop=T))
proc.time() - ptm

# said "multiple contraction" once
#############################
library(DEoptim)
DE1 <- DEoptim(lower=low,upper=up,fn=LPJG, 
               control=DEoptim.control(trace=6, itermax=400,  
                                       parallelType=1, packages=c("tidyr","data.table"), 
                                       parVar=c("df4","ins")))

