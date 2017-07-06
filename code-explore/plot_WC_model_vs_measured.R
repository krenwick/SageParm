################################################################################
# Look at measured water content vs. modeled
# This uses original parameters, STATSGO soil
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)
#library(xtable)

# Set working directory
setwd("~/Documents/SageParm")

# Read in water content from each site:
# In LPJ-GUESS, upper soil layer is 50cm, lower is 50-150cm
# Take simple average of measurements within each layer
wc1 <- read.csv("data/ReynoldsC/138j09HighSage.soil.csv") %>%
  mutate(Source="Station", Site="h08ec") %>% # to 120cm
  mutate(MU=rowMeans(.[,4:5]), 
         ML=rowMeans(.[,6:8])) %>%
  select(jday,hour,year,Source,Site,MU,ML)
wc2 <- read.csv("data/ReynoldsC/098d.soil.csv")  %>%
  mutate(Source="Station", Site="wbsec") %>% # to 90cm
  mutate(MU=rowMeans(.[,4:6]), 
         ML=rowMeans(.[,7:8])) %>%
  select(jday,hour,year,Source,Site,MU,ML)
wc3 <- read.csv("data/ReynoldsC/127d.soil.csv") %>%
  mutate(Source="Station", Site="losec") %>% # to 90cm
  mutate(MU=rowMeans(.[,4:6]), 
         ML=rowMeans(.[,7:8])) %>%
  select(jday,hour,year,Source,Site,MU,ML)
wc4 <- read.csv("data/ReynoldsC/mbsec.soil.csv") %>%
  mutate(Source="Station", Site="mbsec") %>% # to 90cm
  mutate(MU=rowMeans(.[,4:6]), 
         ML=rowMeans(.[,7:8])) %>%
  select(jday,hour,year,Source,Site,MU,ML)

wc <- rbind.data.frame(wc1,wc2,wc3,wc4) %>%
  mutate(day=paste(year,jday, sep="-"))
# dplyr requires POSIXct (as opposed to lt)
wc$date=as.POSIXct(strptime(wc$day, "%Y-%j"))
wcb <- wc %>%
  mutate(Month=format(date, format="%b")) %>%
  rename(Year=year) %>%
  select(-jday,-hour,-day,-date) %>%
  group_by(Year,Month, Source,Site) %>%
  summarise_each(funs(mean))
# This data goes Oct 2014-Jan 2017

# Read in WC_upper output from best LHC run
GU <- read.table("./RCout/orig_ever_mwcont_upper.out", header=T) %>%
  gather(Month, value,Jan:Dec) %>%
  mutate(m=match(Month, month.abb), Year=Year+860) %>%
  mutate(yearmon=as.yearmon(paste(Year,m, sep="-"))) %>%
  filter(yearmon>=2014) %>%
  rename(Meanupper=value) %>%
  mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year, Month, Site, Meanupper)

GL <- read.table("./RCout/orig_ever_mwcont_lower.out", header=T) %>%
  gather(Month, value,Jan:Dec) %>%
  mutate(m=match(Month, month.abb), Year=Year+860) %>%
  mutate(yearmon=as.yearmon(paste(Year,m, sep="-"))) %>%
  filter(yearmon>=2014) %>%
  rename(Meanlower=value) %>%
  mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year, Month, Site, Meanlower)

# NOTE: wcont is volumentric wc as fraction of water holding capacity
# Can't compare directly- must multiply by AWC (avail water capacity)
# then add in volumetric wc at wilting point

guess <- merge(GU,GL, by=c("Year","Month","Site")) %>%
  mutate(Source="Model") %>%
  select(Year,Month,Source,Site:Meanlower) %>%
  mutate(MU=ifelse(Site=="mbsec", Meanupper*.176330+141.247369/500,NA)) %>%
  mutate(MU=ifelse(Site=="losec", Meanupper*.138201+64.846348/500,MU)) %>%
  mutate(MU=ifelse(Site=="wbsec", Meanupper*.167734+167.529689/500,MU)) %>%
  mutate(MU=ifelse(Site=="h08ec", Meanupper*.160884+194.074911/500,MU)) %>%
  mutate(ML=ifelse(Site=="mbsec", Meanlower*.172115+144.133832/500,NA)) %>%
  mutate(ML=ifelse(Site=="losec", Meanlower*.108566+37.7166292/500,ML)) %>%
  mutate(ML=ifelse(Site=="wbsec", Meanlower*.242110+70.847545/500,ML)) %>%
  mutate(ML=ifelse(Site=="h08ec", Meanlower*.135257+256.709712/500,ML)) %>%
  select(-Meanupper,-Meanlower)


both <- rbind.data.frame(wcb,guess)
both$D <- as.yearmon(paste(both$Year, both$Month), "%Y %b")
both$Date <- as.Date(both$D)

################################################################################
# Make some plots to compare WC
################################################################################
ggplot(data=both, aes(x=Date, y=MU, color=Source, linetype=Source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site)

ggplot(data=both, aes(x=Date, y=ML, color=Source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site)

# Just look at water content as measured
ggplot(data=both, aes(x=Date, y=MU, color=Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Source)

ggplot(data=both, aes(x=Date, y=ML, color=Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Source)

