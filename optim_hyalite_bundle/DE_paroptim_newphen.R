################################################################################
# Use the DEoptim package ON HYALITE and IN PARALLEL
################################################################################
rm(list=ls())
library(DEoptim)
library(dtplyr)
library(tidyr)
library(zoo)

# SET WORKING DIRECTORY:
setwd("./")

# to test on home laptop:
#setwd("~/Documents/SageParm/optim_hyalite_bundle")

# Read in monthly GPP data
GPP <- read.csv("RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="GPP")

# Pull in LAI data from MODIS
mod <- read.csv("lai_gpp.csv") %>%
  dplyr::mutate(Latitude=round(Latitude,2)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  dplyr::rename(Year=year) %>%
  dplyr::mutate(Month=month.abb[month]) %>%
  tidyr::gather(Variable, Tower, LAI:GPP) %>%
  dplyr::filter(Variable=="LAI") %>%
  dplyr::select(Year,Month,Variable,Site,Tower)  

# Merge flux with MODIS LAI
df4 <- rbind.data.frame(mod,GPP)

df4 %>% dplyr::group_by(Variable) %>% dplyr::summarise(min=min(Tower),
                                                       mean=mean(Tower),max=max(Tower))

df4 %>% dplyr::mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  dplyr::mutate(Date=as.Date(D)) %>%
  dplyr::filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(Total=sum(Tower),mean=mean(Tower))
# mean of LAI is 6.2 times higher than GPP

# Read in field LAI and cover
field <- read.csv("FieldLaiCover.csv")

# Function to run LPJ-GUESS and return vector of residuals----------------------
# Depends on ins and flux data already existing in memory (ins and df4)
# Depends on tidyr and data.table packages

# First, read ins into memory:
ins  <- readLines("optim2_newphen.ins") # only outputs GPP and LAI

# NOTE: to run in paralle might need to wrap in foreach
# Otherwise would over-write the temp ins file (I think)
LPJG <- function(par) {
  random <- runif(1,0,100)
  tx  <- gsub(pattern = "slaval", replace = par[1], x = ins)
  tx  <- gsub(pattern = "k_latosaval", replace = par[2], x = tx)
  tx  <- gsub(pattern = "ltor_maxval", replace = par[3], x = tx)
  tx  <- gsub(pattern = "rootdistval", replace = par[4], x = tx)
  tx  <- gsub(pattern = "rootdist2", replace = (1-par[4]), x = tx)
  tx  <- gsub(pattern = "phen_winterval", replace = par[5], x = tx)
  tx  <- gsub(pattern = "pstemp_lowval", replace = par[6], x = tx)
  tx  <- gsub(pattern = "pstemp_minval", replace = par[7], x = tx)
  tx  <- gsub(pattern = "apheng", replace = par[8], x = tx)
  tx  <- gsub(pattern = "aphenval", replace = par[9], x = tx)
  tx  <- gsub(pattern = "phengdd5g", replace = par[10], x = tx)
  tx  <- gsub(pattern = "randomval", replace = random, x = tx)
  insname <- paste("./tempins",random,".ins",sep="")
  writeLines(tx, con=insname)
  
  print("Parameter values:")
  print(round(par,2))
  
  # Run model using new ins file
  system(sprintf("./guess /local/job/$SLURM_JOB_ID/%s", insname))
  
  # Read in output from model
  mgpp <- fread(paste("mgpp_",random,".txt",sep=""), header=T) %>% dplyr::mutate(Variable="GPP")
  mlai <- fread(paste("mlai_",random,".txt",sep=""), header=T) %>% dplyr::mutate(Variable="LAI")
  out <- rbind.data.frame(mgpp,mlai) %>%
    dplyr::mutate(Year=Year+860) %>% 
    dplyr::filter(Year>=2014) %>%
    tidyr::gather(Month,Model, Jan:Dec) %>%
    dplyr::mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
    dplyr::mutate(Date=as.Date(D)) %>%
    dplyr::filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    dplyr::select(Year, Month,Variable,Site,Model) 
  b <- merge(out,df4, by=c("Year","Month","Variable","Site"))
  # resid <- b %>% dplyr::mutate(resid1=(Model-Tower)) %>% 
  #   dplyr::mutate(resid1=ifelse(Variable=="LAI",resid1*.019,resid2))
# NEW COST FXN:
  resid <- b %>% dplyr::mutate(resid1=abs((Model-Tower))) %>% 
    dplyr::group_by(Site,Variable) %>%
    dplyr::summarise(sumresid=sum(resid1), meanresid=mean(resid1), 
             # meandata=mean(Tower)) %>%
             meandata=sum(Tower)) %>%
    dplyr::mutate(CV=sumresid/meandata) %>%
    dplyr::group_by(Site) %>%
    dplyr::summarise(Sum=sum(CV)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(cost=sum(Sum)/length(unique(b$Site)))
	# Read in LAI and % cover
#   lai1 <- fread(paste("lai_",random,".txt",sep=""), header=T) %>% dplyr::mutate(Variable="LAI")
#   cov1 <- fread(paste("fpc_",random,".txt",sep=""), header=T) %>% dplyr::mutate(Variable="FPC")
#   lc <- rbind.data.frame(lai1,cov1) %>%
# 	  dplyr::filter(Year==1156) %>%
# 	  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
#     dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
#     dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
#     dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site))
#   lc2 <- merge(lc, field, by=c("Site","Variable")) %>%
#     mutate(Adiff=(ARTR-sage)^2, tdiff=(Total-total)^2, b=Adiff+tdiff) %>%
#     dplyr::summarise(SS=sum(b))
  #SSR <- resid %>% dplyr::summarise(SSR=sum(resid2))
  #print(round(SSR,2))
  #return (as.numeric(SSR+lc2*.756)) # weight so annual LAI+FPC=all monthly GPP
  #return (as.numeric(SSR+lc2*.3825)) # weight so annual LAI+FPC=1 yr of monthly GPP
  #return (as.numeric(SSR))
  # NEW cost function- from Hufkens et al. 2016
  return(as.numeric(resid$cost))
}

low <- c(6,1350, .5, .6, 0,7, -5.2,30,30,50) #lower bound
up <- c(21,5220, 1, 1, 1,13,-2.8,240,240, 150) #upper bound for ech parameter (default is inf)

#DEoptim.control(itermax=400)
DE1 <- DEoptim(lower=low,upper=up,fn=LPJG, 
               control=DEoptim.control(trace=6, itermax=200,  
                                       parallelType=1, packages=c("tidyr","dplyr","data.table","zoo"), 
                                       parVar=c("df4","field","ins")))

Description <- "optimized based on monthly lai and gpp, NEW model and NEW cost fxn"
  
save.image("NewPhenImage_mgl_newcost.RData")


