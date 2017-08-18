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
gpp1 <- fread("testout/alphaa.5_mgpp.out", header=T) %>%
  mutate(Source="alphaa.5")
gpp2 <- fread("testout/alphaa.7_mgpp.out", header=T) %>%
  mutate(Source="alphaa.7")
gpp3 <- fread("testout/alphaa.3_mgpp.out", header=T) %>%
  mutate(Source="alphaa.3")
gpp4 <- fread("testout/alphaa.8_mgpp.out", header=T) %>%
  mutate(Source="alphaa.8")
lai1 <- fread("testout/alphaa.5_mlai.out", header=T) %>%
  mutate(Source="alphaa.5")
lai2 <- fread("testout/alphaa.7_mlai.out", header=T) %>%
  mutate(Source="alphaa.7")
lai3 <- fread("testout/alphaa.3_mlai.out", header=T) %>%
  mutate(Source="alphaa.3")
lai4 <- fread("testout/alphaa.8_mlai.out", header=T) %>%
  mutate(Source="alphaa.8")

gpp <- rbind.data.frame(gpp1,gpp2,gpp3,gpp4) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  gather(Month,GPP, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year,Month,Site,GPP,Source) 
GPPall <- rbind(gpp,GPP) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2015-01-01", Date<"2016-01-01") #%>%
  #mutate(Source=factor(Source, levels=c("Tower","NewPhen","VcMod3","Downramp",
                                      #  "DRalphaa"),ordered=T))

lai <- rbind.data.frame(lai1,lai2,lai3,lai4) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  gather(Month,LAI, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year,Month,Site,LAI,Source)
LAIall <- rbind(lai,modis) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2015-01-01", Date<"2016-01-01") #%>%
  #mutate(Source=factor(Source, levels=c("Modis","NewPhen","VcMod3","Downramp",
                                        #"DRalphaa"), ordered=T))

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

################################################################################
# RE-DO all this shit with new phenology routine
################################################################################
gpp1 <- fread("testout/alphaa.5_mgpp.out", header=T) %>%
  mutate(Source="alphaa.5")
gpp2 <- fread("testout/alphaa.7_mgpp.out", header=T) %>%
  mutate(Source="alphaa.7")
gpp3 <- fread("testout/nalphaa.9_mgpp.out", header=T) %>%
  mutate(Source="nalphaa.9")
gpp4 <- fread("testout/nalphaa.8nalphaa.8_mgpp.out", header=T) %>%
  mutate(Source="nalphaa.8")
lai1 <- fread("testout/alphaa.5_mlai.out", header=T) %>%
  mutate(Source="alphaa.5")
lai2 <- fread("testout/alphaa.7_mlai.out", header=T) %>%
  mutate(Source="alphaa.7")
lai3 <- fread("testout/nalphaa.9_mlai.out", header=T) %>%
  mutate(Source="nalphaa.9")
lai4 <- fread("testout/nalphaa.8nalphaa.8_mlai.out", header=T) %>%
  mutate(Source="nalphaa.8")

gpp <- rbind.data.frame(gpp1,gpp2,gpp3,gpp4) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  gather(Month,GPP, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year,Month,Site,GPP,Source) 
GPPall <- rbind(gpp,GPP) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2015-01-01", Date<"2016-01-01") #%>%
#mutate(Source=factor(Source, levels=c("Tower","NewPhen","VcMod3","Downramp",
#  "DRalphaa"),ordered=T))

lai <- rbind.data.frame(lai1,lai2,lai3,lai4) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  gather(Month,LAI, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year,Month,Site,LAI,Source)
LAIall <- rbind(lai,modis) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2015-01-01", Date<"2016-01-01") #%>%
#mutate(Source=factor(Source, levels=c("Modis","NewPhen","VcMod3","Downramp",
#"DRalphaa"), ordered=T))

################################################################################
# Plot the result
################################################################################
# Plot GPP:
ggplot(data=GPPall, aes(x=Date, y=GPP, color=Source)) +
  geom_point() +
  geom_line() +
  scale_color_manual(name="Model",
  # breaks=c("Model","VcMod","Tower"),
  # labels=c("Downramp","NewPhenOptim","Tower"),
  values=c("darkslategray1","gray", "deeppink","deeppink3","black")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()


# Plot LAI:
ggplot(data=LAIall, aes(x=Date, y=LAI, color=Source)) +
  geom_point() +
  geom_line() +
  scale_color_manual(name="Model",
                     # breaks=c("Model","VcMod","Tower"),
                     # labels=c("Downramp","NewPhenOptim","Tower"),
                     values=c("darkslategray1","gray", "deeppink","deeppink3","black")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab("LAI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

