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
library(zoo) # deal with dates

#---- Set working directory ----
setwd("~/Documents/SageParm")
outfolder <- "ModOut/LHC_optim1/"
seed <- 1940
nparm <- 5
nrun <- 150

# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv")

# Read in LAI data:
lai <- read.csv("data/ReynoldsC/veg_data/2016_LAI_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) %>%
  group_by(Site,replicate,Frame) %>%
  summarise(lai=sum(lai)) %>%
  group_by(Site) %>%
  summarise(lai=mean(lai))


# Pull in data from the LHC model runs------------------------------------------
mod <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai")
for(var in vars) {
  data=paste(outfolder,var,".txt", sep="")
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
b <- merge(mod2,df3, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="GPP")

# Narrow the number of observations


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
# Calculaate the probabilities (parm values) associated with each set:
ps <- b %>% group_by(file) %>%
  summarise(Model=mean(Model)) %>%
  mutate(file2=file) %>%
  separate(file2, into=c("id","cut"), sep=-5, extra="drop") %>%
  dplyr::select(-cut) %>%
  separate(id, into=c("sla","ltor","root","pslo","psmi"),
           sep = "_",extra="drop") %>%
  gather(parameter,value, sla:psmi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name) %>%
  mutate(value=as.numeric(value)) %>%
  spread(parameter,value) %>%
  # convert values into probabilities
  mutate_each(funs(max,min),ltor:sla) %>%
  ungroup() %>%
  mutate(sla=(sla-sla_min)/(sla_max-sla_min)) %>%
  mutate(ltor=(ltor-ltor_min)/(ltor_max-ltor_min)) %>%
  mutate(pslo=(pslo-pslo_min)/(pslo_max-pslo_min)) %>%
  mutate(psmi=(psmi-psmi_min)/(psmi_max-psmi_min)) %>%
  select(file, sla,ltor,root,pslo,psmi)

loglike=as.vector(rep(0,nsets))
file=unique(b$file)
for(i in 1:nsets) {
  f=file[i]
  b2=b[b$file==f,]
  loglike[i] = -sum(dnorm(b2$Tower, b2$Model, log=TRUE))
}
ll2 <- cbind.data.frame(loglike,file)
both <- merge(ll2,ps, by="file")
nrow(both) == nrun
ncol(both) == nparm+2

# success!

##################################################

d=nparms

# DEFAULTS:
set.seed(123)
ptm <- proc.time()
GP <- GP_fit(both[,3:7],both[,2], corr=list(type="exponential",power=1.95),
             nug_thres = 20, control = c(200 * d, 80 * d, 2 * d), maxit=100) # does upping maxit help? no it's worse
proc.time() - ptm
print(GP, digits=2)

# PLAY WITH TUNING PARAMETERS:
set.seed(123)
GP <- GP_fit(both[,3:7],both[,2], corr=list(type="exponential", power=1.95),
             nug_thres = 20, control = c(100 * d, 80 * d, 2 * d), maxit=200, trace=T)
# does upping maxit help? no it's worse
print(GP, digits=2)
plot(GP)

set.seed(123)
# if type = "matern", have seen nu values of 5/2 or 3/2
GP <- GP_fit(both[,3:7],both[,2], corr = list(type = "matern", nu = 5/2),
             nug_thres = 20, control = c(200 * d, 80 * d, 2 * d), maxit=200, trace=T)
# does upping maxit help? no it's worse
print(GP, digits=2)


# CHECK MODEL MAKES DIFFERENT PREDICTIONS
xnew1=t(as.matrix(runif(nparms,0,1)))
xnew2=t(as.matrix(runif(nparms,0,1)))
predict.GP(GP,xnew1)$Y_hat
predict.GP(GP,xnew2)$Y_hat # different from above? Okay good!

#############################################################################
# Make log likelihood into a function:
LL1 <- function (beta1,beta2,beta3,beta4,beta5){
  return(predict.GP(GP,cbind(beta1,beta2,beta3,beta4,beta5))$Y_hat) 
}
# Test likelihood function with proposed starting values to make sure finite (Bolker)
#LL1(.5,.5,.5,.5,.5,.5,.5) #12818.75. Seems fine.
#min(loglike) # 11349. Should I use the parms that get this as start values??

#Now find the MLE's of the parameters, default optimizer
m1= mle2(minuslogl=LL1, start=list(beta1=.5, beta2=.5,beta3=.5,beta4=.5,beta5=.5))
summary(m1)
coef(m1)
print(m1)
profile(m1)
warnings()
m1$convergence

# Use nlminb optimizer so can set upper and lower bounds
m2 = mle2(minuslogl=LL1, start=list(beta1=.5, beta2=.5,beta3=.5,beta4=.5,beta5=.5,
                                    beta6=.5,beta7=.5), optimizer="nlminb", 
          lower=c(beta1=0, beta2=0,beta3=0,beta4=0,beta5=0,
                  beta6=0,beta7=0), upper=c(beta1=1, beta2=1,beta3=1,beta4=1,beta5=1,
                                            beta6=1,beta7=1))

coef(m2)
# Optimizer used in Verbeek et al 2011: L-BFGS-B
# This is derivative-based, doesn't work well for bumpy surface (Bolker)
m3 = mle2(minuslogl=LL1, start=list(beta1=.5, beta2=.5,beta3=.5,beta4=.5,beta5=.5,
                                    beta6=.5,beta7=.5), method="L-BFGS-B", 
          lower=c(beta1=0, beta2=0,beta3=0,beta4=0,beta5=0,
                  beta6=0,beta7=0), upper=c(beta1=1, beta2=1,beta3=1,beta4=1,beta5=1,
                                            beta6=1,beta7=1))
coef(m3)

# non-derivative based method:
m4 = mle2(minuslogl=LL1, start=list(beta1=.5, beta2=.5,beta3=.5,beta4=.5,beta5=.5,
                                    beta6=.5,beta7=.5), method="Nelder-Mead")
coef(m4) # actually got value for beta5

# type of simulated annealing- more robust but slower
m5 = mle2(minuslogl=LL1, start=list(beta1=.5, beta2=.5,beta3=.5,beta4=.5,beta5=.5,
                                    beta6=.5,beta7=.5), method="SANN")
coef(m5)

# new method
#library(emdbook)
#MSBfit = metropSB(fn = LL1, start=c(beta1=.5, beta2=.5,beta3=.5,beta4=.5,beta5=.5,
                                   # beta6=.5,beta7=.5)) # not working

allm <- rbind(coef(m1),coef(m2),coef(m3),coef(m4),coef(m5))
round(allm,2)
confint(m2) # gives info on convergence
confint(m1)
confint(m4)
vcov(m1)
cov2cor(vcov(m1))

################################################################################
# Convert probabilities into parameter estimates
# Ranges must match what was used in original LHC script!
################################################################################
B1 <- round(qunif(coef(m1)[1], 6, 21),0) # SLA, full range from lit
B2 <- round(qunif(coef(m1)[2], .5, 1),1) # ltor_max, exact range in global pft vals
B3 <- round(qunif(coef(m1)[3], 0.6, 1),1) # root_upper
B4 <- round(qunif(coef(m1)[4], 7, 13),1) # pstemp_low, boreal +/- 30%
B5 <- round(qunif(coef(m1)[5], -5.2, -2.8 ),1) # pstemp_min, boreal +/- 30%

betas <- cbind(B1,B2,B3,B4,B5)
##############################################################
# Make new ins file with these parameter values:
insfile <- "automate_tests/optim1_evergreen.ins" # name of ins file to use
# gsub: replaces all occurences of a pattern
ins  <- readLines(insfile)
  tx  <- gsub(pattern = "slaval", replace = betas[1], x = ins)
  tx  <- gsub(pattern = "ltor_maxval", replace = betas[2], x = tx)
  tx  <- gsub(pattern = "rootdistval", replace = betas[3], x = tx)
  tx  <- gsub(pattern = "rootdist2", replace = (1-betas[3]), x = tx)
  tx  <- gsub(pattern = "pstemp_lowval", replace = betas[4], x = tx)
  tx  <- gsub(pattern = "pstemp_minval", replace = betas[5], x = tx)
  insname <- "optim1_selectedparms"
  tx  <- gsub(pattern = "outval", replace = paste(insname,".out",sep=""), x = tx)
  tx  <- gsub(pattern = "Output_LHC_optim1", replace = "Output_localruns", x = tx)
  writeLines(tx, con=paste("NewIns/",insname,".ins", sep=""))

# Run model with new ins then compare output to flux data:
system("/Users/poulterlab1/version-control/LPJ-GUESS/ModelFiles/modules/./guess /Users/poulterlab1/Documents/SageParm/NewIns/optim1_selectedparms.ins")

# Pull output back in to R:
gpp <- fread(paste("Output_localruns/mgpp/",insname,".out",sep=""), header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  gather(Month,Model, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="GPP") %>%
  dplyr::select(Year, Month,Site,Variable,Model) %>%
  spread(Variable,Model) %>%
  dplyr::select(Year,Month,Site,GPP) %>%
  gather(Variable, Model, GPP)

# Merge with flux data
New <- merge(gpp,df3, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="GPP") %>%
  gather(Source, GPP, Model:Tower) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D))

# Plot the result
ggplot(data=New, aes(x=Date, y=GPP, color=Source, linetype=Source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site)
  scale_linetype_manual(name="Source",
                        breaks=c("Evergreen","Raingreen","Summergreen","Tower"),
                        labels=c("Evergreen","Raingreen","Summergreen","Tower"),
                        values=c("dashed", "dashed","dashed","solid"))+
  scale_color_manual(name="Source",
                     breaks=c("Evergreen","Raingreen","Summergreen","Tower"),
                     labels=c("Evergreen","Raingreen","Summergreen","Tower"),
                     values=c("darkcyan","deepskyblue","purple","black")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kg~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
flux

ggsave("figures/GPP_ever_summer_flux.pdf", plot=flux,
       width = 169, height = 140, units = 'mm')
