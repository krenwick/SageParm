################################################################################
# Figure out LAI issues!
# 1. Is it sage parms, or something else? Run with just grass.
# Yep, LAI still way too high with just grass
# 2. Tweak grass parameters- better if leaves don't live so long?
################################################################################
library(tidyverse)
library(data.table)
library(zoo)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm")

# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
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

lai <- fread("testout/tests_mlai.out", header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  gather(Month,Model, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="LAI") %>%
  dplyr::select(Year, Month,Site,Variable,Model) %>%
  spread(Variable,Model) %>%
  dplyr::select(Year,Month,Site,LAI) %>%
  gather(Variable, Model, LAI)

# Merge model out with modis data
LAIall <- merge(lai,mod, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="LAI") %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Year>=2016)
LAIall %>% group_by(Site) %>%
  filter(Model==max(Model)) %>%
  dplyr::summarize(Mod=mean(Model))

# Flux GPP
GPP <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="GPP") %>%
  spread(Variable,Tower) %>%
  mutate(Source="Tower")

gpp <- fread("testout/tests_mgpp.out", header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  gather(Month,Model, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::select(Year, Month,Site,Model) %>%
  rename(GPP=Model) %>%
  dplyr::mutate(Source="Model")
GPPall <- rbind.data.frame(GPP,gpp) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Year>=2016)

# Plot it
L <- LAIall %>% gather(Source, LAI, Model:Tower)
ggplot(data=L, aes(x=Date,y=LAI,color=Source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()


ggplot(data=GPPall, aes(x=Date,y=GPP,color=Source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
