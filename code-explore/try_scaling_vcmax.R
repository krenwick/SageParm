################################################################################
# Examine output from standard run then modifying vcmax
################################################################################
rm(list=ls())
library(tidyverse)
library(data.table)
library(zoo)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm")

################################################################################
# Read in the flux/lai data:
# Read in GPP flux data
GPP <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="GPP") %>%
  spread(Variable,Tower) %>%
  mutate(Source="Tower")

# Pull in LAI data from MODIS
modis <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
  dplyr::mutate(Latitude=round(Latitude,2)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  dplyr::rename(Year=year) %>%
  dplyr::mutate(Month=month.abb[month]) %>%
  select(-GPP) %>%
  mutate(Source="Modis") %>%
  select(Year,Month,Site,LAI,Source)

################################################################################
# Pull DOWNRAMP output into R:
gpp1 <- fread("testout/downramp_mgpp.out", header=T) %>%
  mutate(Source="Downramp")
gpp2 <- fread("testout/optim_mgpp.out", header=T) %>%
  mutate(Source="NewPhen")
gpp3 <- fread("testout/vcmod3_mgpp.out", header=T) %>%
  mutate(Source="VcMod3")
gpp4 <- fread("testout/DRalphaa_mgpp.out", header=T) %>%
  mutate(Source="DRalphaa")
lai1 <- fread("testout/downramp_mlai.out", header=T) %>%
  mutate(Source="Downramp")
lai2 <- fread("testout/optim_mlai.out", header=T) %>%
  mutate(Source="NewPhen")
lai3 <- fread("testout/vcmod3_mlai.out", header=T) %>%
  mutate(Source="VcMod3")
lai4 <- fread("testout/DRalphaa_mlai.out", header=T) %>%
  mutate(Source="DRalphaa")

gpp <- rbind.data.frame(gpp1,gpp2,gpp3,gpp4) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  gather(Month,GPP, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year,Month,Site,GPP,Source) 
GPPall <- rbind(gpp,GPP) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2015-01-01", Date<"2016-01-01") %>%
  mutate(Source=factor(Source, levels=c("Tower","NewPhen","VcMod3","Downramp",
                                        "DRalphaa"),ordered=T))

lai <- rbind.data.frame(lai1,lai2,lai3,lai4) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  gather(Month,LAI, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year,Month,Site,LAI,Source)
LAIall <- rbind(lai,modis) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2014-01-01", Date<"2016-01-01") %>%
  mutate(Source=factor(Source, levels=c("Modis","NewPhen","VcMod3","Downramp",
                                        "DRalphaa"), ordered=T))

################################################################################
# Plot the result
################################################################################
# Plot GPP:
ggplot(data=GPPall, aes(x=Date, y=GPP, color=Source)) +
  geom_point() +
  geom_line() +
 # scale_color_manual(name="Model",
                    # breaks=c("Model","VcMod","Tower"),
                    # labels=c("Downramp","NewPhenOptim","Tower"),
                     #values=c("dodgerblue4","black","forestgreen")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()


# Plot LAI:
ggplot(data=LAIall, aes(x=Date, y=LAI, color=Source)) +
  geom_point() +
  geom_line() +
  #scale_color_manual(name="Model",
                    # breaks=c("Model","VcMod","Tower"),
                     #labels=c("Downramp","NewPhenOptim","MODIS"),
                     #values=c("dodgerblue4","black","forestgreen")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab("LAI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

#VcMod = 2 -> drops both LAI and GPP
#VcMod = 3 -> way lower for both
#VcMod = 4 -> doesn't grow at all
#VcMod = .5 -> also drops GPP and LAI

#VcMod = 1.5 -> still lower!
# 1.2: close, but lower!
# .7: lower!

