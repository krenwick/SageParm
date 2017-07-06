################################################################################
# Use the DEoptim package ON HYALITE and IN PARALLEL
################################################################################
rm(list=ls())
library(DEoptim)
library(dtplyr)
library(tidyr)

# SET WORKING DIRECTORY:
setwd("./")

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
# mean of LAI is 6.33 times higher than GPP

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
  tx  <- gsub(pattern = "pstemp_maxval", replace = par[7], x = tx)
  tx  <- gsub(pattern = "pstemp_hival", replace = par[8], x = tx)
  tx  <- gsub(pattern = "aphenval", replace = par[9], x = tx)
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
    dplyr::filter(Year>=2015) %>%
    tidyr::gather(Month,Model, Jan:Dec) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    dplyr::select(Year, Month,Variable,Site,Model)
  b <- merge(out,df4, by=c("Year","Month","Variable","Site"))
  resid <- b %>% dplyr::mutate(resid2=(Model-Tower)^2) %>% 
    dplyr::mutate(resid2=ifelse(Variable=="LAI",resid2*.025,resid2))
  SSR <- resid %>% dplyr::summarise(SSR=sum(resid2))
  print(round(SSR,2))
  return (as.numeric(SSR))
}

low <- c(6,1350, .5, .6, 0,7, 26.6,17.5,30) #lower bound
up <- c(21,5220, 1, 1, 1,13, 49.4,32.5,365) #upper bound for ech parameter (default is inf)

DE1 <- DEoptim(lower=low,upper=up,fn=LPJG, 
               control=DEoptim.control(trace=6,  
                                       parallelType=1, packages=c("tidyr","dplyr","data.table"), 
                                       parVar=c("df4","ins")))

save.image("NewPhenImage.RData")


