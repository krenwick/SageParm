################################################################################
# Examine annual LAI and GPP
################################################################################
rm(list=ls()) 
library(tidyverse); theme_set(theme_bw(base_size=18))
library(zoo)
library(data.table)
library(grid)
library(gridExtra)
library(xtable)

# Set working directory
setwd("~/Documents/SageParm")
outname1 <- "mgl_disturb_summergrass1"
outname2 <- "mgl_disturb_summergrass2"


# Pull GPP from optimization model runs into R:
gpp1 <- fread(paste("Output_localruns/mgpp_",outname1,".txt",sep=""), header=T) %>%
  gather(Month,Model, Jan:Dec)
gpp2 <- fread(paste("Output_localruns/mgpp_",outname2,".txt",sep=""), header=T) %>%
  gather(Month,Model2, Jan:Dec) 

# Pull LAI from optimization model runs into R:
lai1 <- fread(paste("Output_localruns/mlai_",outname1,".txt",sep=""), header=T) %>%
  gather(Month,Model, Jan:Dec)
lai2 <- fread(paste("Output_localruns/mlai_",outname2,".txt",sep=""), header=T) %>%
  gather(Month,Model2, Jan:Dec)

gpps <- Reduce(function(...) merge(..., all=TRUE), list(gpp1,gpp2)) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="GPP") %>%
  dplyr::select(-Lon,-Lat) %>%
  gather(Source, GPP, Model:Model2) %>%
  group_by(Source, Site, Year) %>%
  summarize(AnnualGPP=sum(GPP))
filter(gpps, Source=="Model2")

lais <- Reduce(function(...) merge(..., all=TRUE), list(lai1,lai2)) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2016) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="LAI") %>%
  dplyr::select(-Lon,-Lat) %>%
  group_by(Site,Year) %>%
  summarize(LAI=mean(Model2))

# read in annual LAI to verify is max
# Look at LAI- annual
a1 <- fread(paste("Output_localruns/lai_",outname1,".txt",sep=""), header=T) %>%
  mutate(Source="Model")
a2 <- fread(paste("Output_localruns/lai_",outname2,".txt",sep=""), header=T) %>%
  mutate(Source="Model2")
a3 <- rbind.data.frame(a1,a2) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2016) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  filter(Source=="Model2") %>%
  select(Site, Year, Total)
a3
lais


