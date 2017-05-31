################################################################################
# Look at data using new phenology routine
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)
#library(xtable)

# Set working directory
setwd("~/Documents/SageParm")

# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv") %>%
  filter(Variable=="GPP") %>%
  select(-Variable)

# Pull in data from new phenology model run:
dpath <- "/Users/poulterlab1/Documents/SageParm/RCout/"
phen <- read.table(paste(dpath, "phen_mgpp.out", sep=""), header=T) %>%
  gather(Month, value,Jan:Dec) %>%
  mutate(m=match(Month, month.abb), Year=Year+860) %>%
  mutate(yearmon=as.yearmon(paste(Year,m, sep="-"))) %>%
  filter(yearmon>=2014) %>%
  rename(Phen=value) %>%
  mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year, Month, Site, Phen)


# Pull in data from initial ("standard parameters") model runs (daymet3_mgpp)
# OR: Get data from "best LHC" run
s1 <- read.table("./RCout/bestLHC_mgpp.out", header=T) %>%
  gather(Month, value,Jan:Dec) %>%
  mutate(m=match(Month, month.abb), Year=Year+860) %>%
  mutate(yearmon=as.yearmon(paste(Year,m, sep="-"))) %>%
  filter(yearmon>=2014) %>%
  rename(Standard=value) %>%
  mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  select(Year, Month, Site, Standard)

all <- merge(phen,df3, by=c("Year","Month","Site")) %>%
  merge(.,s1, by=c("Year","Month","Site")) %>%
  gather(Source, GPP, Phen:Standard)
all$Date <- as.yearmon(paste(all$Year, all$Month), "%Y %b")
all$D2 <- as.Date(all$Date)

# Make a plot!
ggplot(data=all, aes(x=D2, y=GPP, color=Source)) +
  geom_point() +
  geom_line() +
  scale_color_manual(name="Source",
                     breaks=c("Tower","Standard","Phen"),
                     labels=c("Tower","Best LHC","2 cohorts"),
                     values=c("darkcyan","deepskyblue","black")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kg~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Increase APHEN_MAX: all values increase, especially fall
# Increase phen_winter: depends on site. big drop at mbsec
# Drop phen_winter: spring decline, relatively minor, winter LAI is lower/better
# increase ALPHAA to .7: up at wbsec, down at mbsec
# decrease ALPHAA to .3: big drop at wbsec, little change at others
#Increas phenramp from 65 to 90: diff barely perceptible
# increase to 150: still barely perceptible
# drop SLA from 19 to 8: 
