################################################################################
# Plot best runs from LHC against flux data
# 4 panels, 3 lines
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)
#library(xtable)

# Set working directory
setwd("~/Documents/SageParm")

# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv")

# Pull in data from the LHC model runs
mod <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet")
for(var in vars) {
  data=paste("automate_tests/merged/",var,".txt", sep="")
  b <- fread(data, header=T)
  names(b)[16] <- "file"
  # select appropriate years (2014-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,Model, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Site,Variable,Model,file)
  
  mod <- rbind.data.frame(mod,b1)
}

# Get variable names from model to match up with tower data
mod2 <- mod %>%
  spread(Variable,Model) %>%
  rename(NEE=mnee, GPP=mgpp) %>%
  mutate(Rsp=mra+mrh) %>%
  mutate(ET=maet+mevap) %>%
  select(Year,Month,Site,file,GPP,NEE,Rsp,ET) %>%
  gather(Variable, Model, GPP:ET)

# Merge with flux data
b <- merge(mod2,df3, by=c("Year","Month","Site","Variable"))

# Caluclate RMSE for each file, just GPP, separate sites
# Use GPP because no parm affects NEE (RPCC analysis)
# pull out best single run for each site and all together
nruns <- -1 # number runs to select. Negative takes from bottom (min RMSE)

# Get model and tower data for best individual runs
bb2 <- b %>%
  filter(Variable=="GPP") %>%
  mutate(SE=(Tower-Model)^2) %>%
  group_by(file,Site) %>%
  mutate(RMSE=sqrt(mean(SE))) %>%
  group_by(Site) %>%
  top_n(n = nruns, wt = RMSE) %>%
  select(Year, Month, Site, Model, Tower)
#Get model data for best overal parm set
bb3 <- b %>%
  filter(Variable=="GPP") %>%
  mutate(SE=(Tower-Model)^2) %>%
  group_by(file) %>%
  mutate(RMSE=sqrt(mean(SE))) %>%
  group_by(Site) %>%
  top_n(n = nruns, wt = RMSE) %>%
  select(Year, Month, Site, Model, Tower) %>%
  rename(AllMod=Model)

all <- merge(bb2,bb3, by=c("Year","Month","Site","Tower")) %>%
  gather(Source, GPP, Tower:AllMod)
all$Date <- as.yearmon(paste(all$Year, all$Month), "%Y %b")
all$D2 <- as.Date(all$Date)

# Make a plot!
ggplot(data=all, aes(x=D2, y=GPP, color=Source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site)
