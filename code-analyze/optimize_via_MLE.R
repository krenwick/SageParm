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

# Caluclate RMSE for each file, just GPP, separate sites
# Use GPP because no parm affects NEE (RPCC analysis)
# pull out best 5% (top 32 runs) for each site
nruns <- -16 # number runs to select. Negative takes from bottom (min RMSE)
bb2 <- b %>%
  filter(Variable=="GPP") %>%
  mutate(SE=(Tower-Model)^2) %>%
  group_by(file,Site) %>%
  summarise(RMSE=sqrt(mean(SE))) %>%
  mutate(id1=file) %>%
  separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
  dplyr::select(-cut) %>%
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi"),
           sep = "_",extra="drop") %>%
  separate(pstemp_hi, into=c("pstemp_hi","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(Site,parameter) 

b2 <- bb2 %>%
  top_n(n = nruns, wt = RMSE) %>%
  summarise(min=min(value),mean=mean(value),max=max(value), SD=sd(value),
            Q1=quantile(value,probs=.25), Q3=quantile(value,probs=.75)) %>%
  gather(stat, value,min:Q3) %>%
  spread(Site,value)

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
head(b)
dim(b)
# get vector of measured GPP
y = b$Tower
  
# get vector of modeled GPP
mu = b$Model

###############################################################################
# APPROACH 1: model after a systems ecol lab
###############################################################################

# Model of likelihood funtion
LL3 = function(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, y, sigma){
  mu = mu
  return(loglike = -sum(dnorm(y, mu, sigma, log=TRUE)))
}

# Define list of starting values (use standard from LPJ)
# order is: SLA, latos, gmin, ltor_max,greff, rootup,turnover_sap, psmin, pslo,
#est_max,psmax,pshi
start= list(P1=30,P2=6000,P3=.5,P4=1,P5=.08,P6=.6,P7=.1,P8=-4,P9=10,P10=.2,
            P11=38,P12=25, sigma = 0.01)
m3 <- mle2(minuslogl=LL3, start=start, data=list(y=y),control=list(maxit=5000))
# with mle2, data can be list. Diff sites?

#summary of results
summary(m3)
confint(m3)
plot(profile(m3))
k1 = coef(m3)[1]
k2 = coef(m3)[2]
k3 = coef(m3)[3]
# this definitely doesn't work. Need my likelihood emulator to be the function
################################################################################
# Approach 2: like above, but function is the likelihood emulator
################################################################################
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
#GP <- GP_fit(probs,loglike) # taking forever: 640 too many?

# Try with subset of data:
ptm <- proc.time()
GP <- GP_fit(probs[1:100,],loglike[1:100]) # it works! (with 50, also 100)
proc.time() - ptm
print(GP)
class(GP)
summary(GP)
length(GP) # 7. This is the "SS" that gets used in other functions
length(GP[1]) # 100 x 12 matrix, identical to X (probs) fed into function
GP[2] # Y- the likelihoods fed into function
GP[3] # sig2
GP[4] # beta values (1-12)
GP[5] # delta?
GP[6] # nugget threshold
GP[7] # correlation parm

# Does it take longer to fit mle2 vs just GP_fit?
ptm <- proc.time()
GP <- GP_fit(probs[1:20,],loglike[1:20]) # it works! (with 50, also 100)
proc.time() - ptm #86.647 

ptm <- proc.time()
fit <- mle2(GP_fit(probs[1:20,],loglike[1:20]))
proc.time() - ptm

# Would running GP with fewer parameters make a difference?

# try running GP_fit in parallel- DONT THINK THIS WORK

# I think with Pecan the paralelize optimizing for different sites- or diff parameters??
# Estimate via MLE:
start= list(P1=30,P2=6000,P3=.5,P4=1,P5=.08,P6=.6,P7=.1,P8=-4,P9=10,P10=.2,
            P11=38,P12=25, sigma = 0.01)
m3 <- mle2(minuslogl=GP, start=start, data=list(y=y),control=list(maxit=5000))
m3 <- mle2(minuslogl=GP,data=list(y=y),control=list(maxit=5000))

# Toy example from mle2 documentation
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
d <- data.frame(x,y)

## in general it is best practice to use the `data' argument,
##  but variables can also be drawn from the global environment
LL <- function(ymax=15, xhalf=6)
  -sum(stats::dpois(y, lambda=ymax/(1+x/xhalf), log=TRUE))
## uses default parameters of LL
(fit <- mle2(LL))
fit1F <- mle2(LL, fixed=list(xhalf=6))
coef(fit1F)
coef(fit1F,exclude.fixed=TRUE)

#Understand LL
try <- LL()
try2 <- LL(12,7)

# Toy example with my stuff
# Need to give this a bunch of new parameter values
parms <- rep(.2,12)
LL <- function(parms) {
  predict.GP(GP)$Y_hat
}
Y <- predict.GP(GP)
rnorm(1, Y$Y_hat, sqrt(Y$MSE))
GP <- GP_fit(probs[1:20,],loglike[1:20])
fit <- mle2(LL(parms))

################################################################################
# Approach 3: use multivariate normal for likelihooh
# Currently not working
################################################################################
library(mvtnorm)
# Get the likelihood
n = 12 # number parameter sets
for(i in 1:nsets) {
  f=sets[i]
  b2=b[b$file==f,]
  loglike[i] = (-sum(dmvnorm(b2$Tower, mean=b2$Model, log = TRUE)))
}

# likelihood emulator
ptm <- proc.time()
GP <- GP_fit(probs[1:30,],loglike[1:30]) # it works! (with 30)
proc.time() - ptm #120.404

# Need to give this a bunch of new parameter values
parms <- rmvnorm(100,mean=rep(0,12))
LL <- function() {
    predict.GP(GP)$Y_hat
}
fit <- mle2(LL)

fit <- mle2(LL(parms))
Y <- predict.GP(GP)
rnorm(1, Y$Y_hat, sqrt(Y$MSE))
GP <- GP_fit(probs[1:20,],loglike[1:20])
fit <- mle2(LL)

#val <- (-sum(dmvnorm(myY, mean=rep(mu, n), diag(tauv, n),log = TRUE)))


# prepare for parallelization

dcores <- parallel::detectCores() - 1
ncores <- min(max(dcores, 1), nsets)

cl <- parallel::makeCluster(ncores, type="FORK")

## Parallel fit for GPs
GPmodel <- parallel::parLapply(cl, probs, function(x) GPfit::GP_fit(X = x, loglike))
parallel::stopCluster(cl)

# if works, move on to estimate parms via MLE or Bayes--------------------------

# Estimate via MLE:
m3 <- mle2(minuslogl=GP, start=start, data=list(y=y),control=list(maxit=5000))

# Estimate via Bayesian method:-------------------------------------------------


################################################################################
#THREE POOL model likelihood function and call to bbmle
# looks like M1, M2... are different x variables
LL3 = function(M1, M2, M3, k1, k2, k3, CDI, t, y, sigma){
  mu = model3(M1=obs$M1, M2=obs$M2, M3=obs$M3, k1=k1, k2=k2, k3=k3, CDI=obs$CDI, t=obs$t)
  return(loglike = -sum(dnorm(y, mu, sigma, log=TRUE)))
}

m3 = mle2(minuslogl = LL3, start = list(k1=0.5, k2 =0.5, k3=0.5, sigma = 0.01), data = list(y=obs$M_t), control=list(maxit=5000))








################################################################################
# Following code is from the PeCan project, pda.emulator
################################################################################
# snippet from Systems Part2:
return(loglike = -sum(dnorm(y, mu, sigma, log=TRUE)))
# y is actual data, mu is model output, sigma is error
# I think what is going on in the Pecan function is that the model is run for
# n different sites. Not sure though

# Get the likelihood
val <- (-sum(dmvnorm(myY, rep(mu, n), S + diag(tauv, n), log = TRUE))
## Generate emulator on SS, return a list

# prepare for parallelization
dcores <- parallel::detectCores() - 1
ncores <- min(max(dcores, 1), length(SS))

cl <- parallel::makeCluster(ncores, type="FORK")

## Parallel fit for GPs
GPmodel <- parallel::parLapply(cl, SS, function(x) GPfit::GP_fit(X = x[, -ncol(x), drop = FALSE], Y = x[, ncol(x), drop = FALSE]))
#GPmodel <- lapply(SS, function(x) GPfit::GP_fit(X = x[, -ncol(x), drop = FALSE], Y = x[, ncol(x), drop = FALSE]))
parallel::stopCluster(cl)

gp <- GPmodel

## Change the priors to unif(0,1) for mcmc.GP
prior.all[prior.ind.all, ] <- rep(c("unif", 0, 1, "NA"), each = length(prior.ind.all))

## Set up prior functions accordingly
prior.fn.all <- pda.define.prior.fn(prior.all)

# start the clock
ptm.start <- proc.time()

# prepare for parallelization
dcores <- parallel::detectCores() - 1
ncores <- min(max(dcores, 1), settings$assim.batch$chain)

logger.setOutputFile(file.path(settings$outdir, "pda.log"))

current.step <- "pre-MCMC"
save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)

cl <- parallel::makeCluster(ncores, type="FORK", outfile = file.path(settings$outdir, "pda.log"))

## Sample posterior from emulator
mcmc.out <- parallel::parLapply(cl, 1:settings$assim.batch$chain, function(chain) {
  mcmc.GP(gp          = gp, ## Emulator(s)
          x0          = init.list[[chain]],     ## Initial conditions
          nmcmc       = settings$assim.batch$iter,       ## Number of reps
          rng         = rng,       ## range
          format      = "lin",      ## "lin"ear vs "log" of LogLikelihood 
          mix         = mix,     ## Jump "each" dimension independently or update them "joint"ly
          jmp0        = jmp.list[[chain]],  ## Initial jump size
          ar.target   = settings$assim.batch$jump$ar.target,   ## Target acceptance rate
          priors      = prior.fn.all$dprior[prior.ind.all], ## priors
          settings    = settings,
          run.block   = (run.normal | run.round),  
          n.of.obs    = n.of.obs,
          llik.fn     = llik.fn,
          resume.list = resume.list[[chain]]
  )
})

parallel::stopCluster(cl)
current.step <- "post-MCMC"
save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)

# Stop the clock
ptm.finish <- proc.time() - ptm.start
logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(settings$assim.batch$iter), " iterations."))

SS <- numeric(length(gp))

gp <- GP

get.y <- function(gp, xnew, n.of.obs, llik.fn, priors, settings) {
  
  SS <- numeric(length(gp))
  
  X <- matrix(unlist(xnew), nrow = 1, byrow = TRUE)
  
  for(igp in seq_along(gp)){
    Y <- GPfit::predict.GP(gp[[igp]], X[, 1:ncol(gp[[igp]]$X), drop=FALSE])
    # likelihood <- Y$Y_hat
    # likelihood <- rnorm(1, Y$Y_hat, sqrt(Y$MSE))
    SS[igp] <- rnorm(1, Y$Y_hat, sqrt(Y$MSE))
  }
  
  llik.par <- pda.calc.llik.par(settings, n.of.obs, SS)
  likelihood <- pda.calc.llik(SS, llik.fn, llik.par)
  
  prior.prob <- calculate.prior(xnew, priors)
  posterior.prob <- likelihood + prior.prob
  
  # return likelihood parameters
  par <- unlist(sapply(llik.par, `[[` , "par"))
  
  return(list(posterior.prob = posterior.prob, par = par))
  
} # get.y