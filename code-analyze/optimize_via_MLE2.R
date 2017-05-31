################################################################################
# Optimize parameters for LPJ-GUESS
# 1. Emulate likelihood fxn based on output from LHC runs
# 2. Use mle2 fxn in bbmle package to estimate parms using MLE
# 3. Use MCMC... alternative to MLE
################################################################################
rm(list=ls())
library(data.table)
library(GPfit) # Gaussian Process modeling
library(bbmle) # MLE estimation from likelihood
library(lhs) # latin hypercube probabilities
library(tidyverse)

#---- Set working directory ----
setwd("~/Documents/SageParm")

# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv")

# Pull in data from the LHC model runs------------------------------------------
mod <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet")
for(var in vars) {
  data=paste("automate_tests/merged/",var,".txt", sep="")
  b <- fread(data, header=T)
  names(b)[16] <- "file"
  # select appropriate years (2014-2015)
  b1 <- b %>% dplyr::mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,Model, Jan:Dec) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    dplyr::mutate(Variable=var) %>%
    dplyr::select(Year, Month,Site,Variable,Model,file)
  
  mod <- rbind.data.frame(mod,b1)
}

# Get variable names from model to match up with tower data
mod2 <- mod %>%
  spread(Variable,Model) %>%
  rename(NEE=mnee, GPP=mgpp) %>%
  mutate(Rsp=mra+mrh) %>%
  mutate(ET=maet+mevap) %>%
  dplyr::select(Year,Month,Site,file,GPP,NEE,Rsp,ET) %>%
  gather(Variable, Model, GPP:ET)

# Merge with flux data
b <- merge(mod2,df3, by=c("Year","Month","Site","Variable"))

##############################################################
# GP_fit only needs two arguments:
# X: matrix of parameters/probs (640 x 12 for LHC runs)
# Y: vector of output (640 observations). 
# fits fxn so can generate new output (likelihood) without running full model
# Then, can use MCMC and replace LPJ-GUESS run at each iteration by running
# the gaussian likelihood function.
# Huh. in this example it only works for 1 time step, 1 site.
# I guess that's why we might want the paramters to predict a likelihood- that's okay
###############################################################
# get matrix of parameter probabilities
set.seed(1950)
probs <- randomLHS(640, 12)

# hand calculate the likelihood for each parm set
# loop over parameter sets to calculate -log(L) for each
nsets=640 # number of parm sets from latin hypercube
loglike=as.vector(rep(0,nsets))
sets=unique(b$file)
for(i in 1:nsets) {
  f=sets[i]
  b2=b[b$file==f,]
  loglike[i] = -sum(dnorm(b2$Tower, b2$Model, log=TRUE))
}
loglike # success!

# use GP_fit to emulate the likelihood function
# very slow when many parameters
nparms <- 12

ptm <- proc.time()
#GP <- GP_fit(probs[seq(1, nrow(probs), 10),1:nparms],loglike[1:70]) # it works! (with 50, also 100)
GP <- GP_fit(probs[1:640,1:nparms],loglike[1:640], corr = list(type = "matern", nu = 3/2)) # it works! (with 50, also 100)
proc.time() - ptm 
print(GP)
# 10 runs 3 parms- no (20 and 30 don't work either)
# 40 runs 3 parms- yes
# 40 runs and 5 parms: yes, 50 runs: yes, 70 runs: yes
# works: 60 r 6 parm
# 70 & 7 works, but B4 not estimable
# 8 parameter breaks it- several unidentifiable
# 600 runs is too many, even with just 3 parms. Gah! took 1679.324 s, didn't converge
# that equals 27.99 min.

# CHECK MODEL MAKES DIFFERENT PREDICTIONS
xnew1=t(as.matrix(runif(nparms,0,1)))
xnew2=t(as.matrix(runif(nparms,0,1)))
predict.GP(GP,xnew1)$Y_hat
predict.GP(GP,xnew2)$Y_hat # different from above? Okay good!

#############################################################################
# Make log likelihood into a function:
LL1 <- function (beta1,beta2,beta3,beta4,beta5,beta6,beta7){
  return(predict.GP(GP,cbind(beta1,beta2,beta3,beta4,beta5,beta6,beta7))$Y_hat) 
}


#Now find the MLE's of the parameters
m1= mle2(minuslogl=LL1, start=list(beta1=.5, beta2=.5,beta3=.5,beta4=.5,beta5=.5,
                                   beta6=.5,beta7=.5))
summary(m1)
coef(m1) # should all be between 0-1

B1 <- round(qunif(coef(m1)[1], 6, 21),0) # SLA, full range from lit
B2 <- round(qunif(coef(m1)[2], 1350, 5220),0) # k_latosa, min/max from Ganskopp 1986
B3 <- round(qunif(coef(m1)[3], .35, .65),2) # gmin, standard +/- 30%
B4 <- round(qunif(coef(m1)[4], .5, 1),1) # ltor_max, exact range in global pft vals
B5 <- round(qunif(coef(m1)[5], .056, .104),2) # greff_min, standard +/- 30%
B6 <- round(qunif(coef(m1)[6], 0.6, 1),1) # root_upper
B7 <- round(qunif(coef(m1)[7], .01, .2),2) # turnover_sap, took values from Ben's paper
B8 <- round(qunif(coef(m1)[8], -5.2, -2.8 ),1) # pstemp_min, boreal +/- 30%
B9 <- round(qunif(coef(m1)[9], 7, 13),1) # pstemp_low, boreal +/- 30%
B10 <- round(qunif(coef(m1)[10], .05, .2),2) # est_max, exact range in global vals
B11 <- round(qunif(coef(m1)[11], 26.6, 49.4),1) #pstemp_max, bor/temp +/-30%
B12 <- round(qunif(coef(m1)[12], 17.5, 32.5),1) #pstemp_hi, bor/temp +/-30%
cbind(B1,B2,B3,B4,B5,B6,B7)#,B8)#,B9,B10,B11,B12)

