################################################################################
# Plot monthly GPP from flux vs. modis
################################################################################
library(tidyverse)
library(data.table)
library(zoo)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm")

################################################################################
# Get baseline data on GPP, NEE, and LAI
################################################################################
# Read in GPP flux data
GPP <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="GPP") %>%
  rename(Value=Tower) %>%
  mutate(Source="Tower")

# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
  dplyr::mutate(Latitude=round(Latitude,2)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  dplyr::rename(Year=year) %>%
  dplyr::mutate(Month=month.abb[month]) %>%
  tidyr::gather(Variable, Value, LAI:GPP) %>%
  mutate(Source="MODIS") %>%
  dplyr::select(Year,Month,Variable,Site,Value,Source)  

# Merge flux with MODIS 
df4 <- rbind.data.frame(mod,GPP) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2014-09-01") %>%
  filter(Variable=="GPP")

# Plot MODIS and Tower GPP
p1 <- ggplot(data=df4, aes(x=Date, y=Value, color=Source)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}~month^{-1}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("figures/GPP_EC_vs_MODIS.pdf", plot=p1,
       width = 169, height = 140, units = 'mm')
