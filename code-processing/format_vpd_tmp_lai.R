################################################################################
# Read in and merge 2015-2016 flux data
# Output: daily min temp, daily max temp, avg daytime vpd
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(imputeTS)

#----- SET WORKING DIRECTORY ----- 
setwd("~/Documents/SageParm")
# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Documents/SageParm/data/"

############ Get station data and put in propper format
# Get station data for 2014- sent via email by Gerald Flerchinger, ARS
RC176 <- read.csv(paste(dpath, "ReynoldsC/176.wea.30min.2015.csv", sep="")) %>%
  mutate(Source="Station", Site="mbsec")
RC098c <- read.csv(paste(dpath, "ReynoldsC/098c.wea.30min.2015.csv", sep="")) %>%
  mutate(Source="Station", Site="wbsec")
RC127 <- read.csv(paste(dpath, "ReynoldsC/127.wea.30min.2015.csv", sep="")) %>%
  mutate(Source="Station", Site="losec")
RC138j10 <- read.csv(paste(dpath, "ReynoldsC/138j10.wea.30min.2015.csv", sep="")) %>%
  mutate(Source="Station", Site="h08ec")
RCweather <- rbind(RC176,RC098c,RC127,RC138j10)

# Convert 30-min temp to daily max/min
RC2 <- RCweather %>% mutate(date=as.Date(datetimec)) %>%
  # calculate VPD:
  mutate(VPD=(1-rh3*.01)*(610.7*10^(7.5*tmp3/(237.3+tmp3)))) %>%
  mutate(radJ=sol*1800) %>%
  group_by(date,Site) %>% # group by day to calculate days with precip
  mutate(maxT=max(tmp3), minT=min(tmp3)) %>%
  filter(sol>0) %>%
  dplyr::summarize(meanVPD=mean(VPD)/1000, maxT=mean(maxT),minT=mean(minT),
                   TotRad=sum(radJ)/1000000)

# Pull in and format MODIS LAI
m1 <- read.csv("data/ReynoldsC/MODIS/mbsage-MCD15A2-005-results.csv") %>%
  select(Date, Latitude,MCD15A2_005_Lai_1km) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  rename(LAI=MCD15A2_005_Lai_1km) %>%
  spread(Latitude, LAI) # must be short form for merge

# create full time series
Date <- as.Date(seq(min(m1$Date),max(m1$Date),by="day"))
D1 <- as_tibble(Date) %>%
  rename(Date=value)

# Merge in fake TS
lai2 <- merge(D1,m1, by="Date", all=T)

# Interpolate missing values
lai3 <-  na.interpolation(lai2) %>% # default method is linear
  gather(Latitude, LAI,`43.064483`:`43.167545`) %>%
  dplyr::mutate(Latitude=round(as.numeric(Latitude),2)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  rename(date=Date) %>%
  select(date,Site,LAI)
head(lai3)

both <- merge(lai3,RC2,by=c("date","Site"))
write.csv(both, "./data/VPD_tmp_lai.csv", row.names=F)
