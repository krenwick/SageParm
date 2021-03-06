################################################################################
# Use the SCEoptim package ON HYALITE
################################################################################
rm(list=ls())
library(hydromad)
library(data.table)
library(tidyverse)

# SET WORKING DIRECTORY:
system("pwd=`pwd`; $pwd 2> dummyfile.txt")
dir <- fread("dummyfile.txt")
n<- colnames(dir)[2]
n2 <- substr(n, 1, nchar(n)-1)
setwd(n2)

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
ins  <- readLines("summergreen_optim1_LM.ins") # only outputs GPP

# NOTE: to run in paralle might need to wrap in foreach
# Otherwise would over-write the temp ins file (I think)
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
  writeLines(tx, con="./tempins2.ins")
  
  print("Parameter values:")
  print(round(par,2))
  
  # Run model using new ins file
  #system("srun -n 1 /local/job/$SLURM_JOB_ID/./guess tempins2.ins")
  system("/local/job/$SLURM_JOB_ID/./runjob.sh")

  # Read in output from model
  mgpp <- fread("mgpp.txt", header=T) %>% dplyr::mutate(Variable="GPP")
  mlai <- fread("mlai.txt", header=T) %>% dplyr::mutate(Variable="LAI")
  out <- rbind.data.frame(mgpp,mlai) %>%
    dplyr::mutate(Year=Year+860) %>% 
    dplyr::filter(Year>=2014) %>%
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
  print(SSR)
  return (as.numeric(SSR))
}


start <- c(8,3200, .7, .8,-4,10, 38,25,200) # starting values
low <- c(6,1350, .5, .6, -5.2,7, 26.6,17.5,100) #lower bound
up <- c(21,5220, 1, 1, -2.8,13, 49.4,32.5,300) #upper bound for ech parameter (default is inf)

SCE1 <- SCEoptim(par=start,lower=low,upper=up,FUN=LPJG, 
               control=list(trace=6, returnpop=T, maxit=1000))

save(SCE1,"SCE1.RData")


