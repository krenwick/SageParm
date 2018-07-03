#############################################################################
# Pull in MODIS spreadheets and figure out how they work
# COnvert units and time step so matches LPJ-GUESS
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt
library(imputeTS)

setwd("~/Documents/SageParm/")

# Pull in MODIS 1 (LAI). LAI is unitless.
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
lai3 <-  na.interpolation(lai2) # default method is linear

# Make monthly
lai4 <- lai3 %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month, year) %>%
  summarise_each(funs(mean)) %>%
  gather(Latitude, LAI,`43.064483`:`43.167545`)

# OKAY! Try second file (NPP and GPP)- start with NPP
n1 <- read.csv("data/ReynoldsC/MODIS/mbsage-MOD17A3-055-results.csv") %>%
  select(Date, Latitude,MOD17A3_055_Npp_1km) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  rename(NPP=MOD17A3_055_Npp_1km) %>%
  #mutate(NPP=NPP/8) %>%
  spread(Latitude, NPP) # must be short form for merge

# create full time series
Date <- as.Date(seq(min(n1$Date),max(n1$Date),by="day"))
D1 <- as_tibble(Date) %>%
  rename(Date=value)

# Merge in fake TS
npp2 <- merge(D1,n1, by="Date", all=T)

# Interpolate missing values
npp3 <-  na.interpolation(npp2) # default method is linear

# Make monthly
npp4 <- npp3 %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month, year) %>%
  summarise_each(funs(sum)) %>%
  gather(Latitude, NPP,`43.064483`:`43.167545`)

# Third variable: GPP, kg C m-2 (no need to convert)----------------------------
g1 <- read.csv("data/ReynoldsC/MODIS/sage2-MOD17A2H-006-results.csv") %>%
  select(Date, Latitude,MOD17A2H_006_Gpp_500m) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  rename(GPP=MOD17A2H_006_Gpp_500m) %>%
  mutate(GPP=GPP/8) %>%
  spread(Latitude, GPP) # must be short form for merge

# create full time series
Date <- as.Date(seq(min(g1$Date),max(g1$Date),by="day"))
D1 <- as_tibble(Date) %>%
  rename(Date=value)

# Merge in fake TS
gpp2 <- merge(D1,g1, by="Date", all=T)

# Interpolate missing values
gpp3 <-  na.interpolation(gpp2) # default method is linear

# Make monthly
gpp4 <- gpp3 %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month, year) %>%
  summarise_each(funs(sum),`43.064483`:`43.167545`) %>%
  gather(Latitude, GPP,`43.064483`:`43.167545`)

##############################
# Compare sum over all years- should be same as original
gpp4 %>% 
  ungroup() %>%
  #group_by(year) %>%
  group_by(Latitude) %>% 
  #filter(year>="2015") %>%
  summarise(total=sum(GPP))

read.csv("data/ReynoldsC/MODIS/sage2-MOD17A2H-006-results.csv") %>%
  rename(GPP=MOD17A2H_006_Gpp_500m) %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  #group_by(year) %>%
  #filter(Date>="2015-01-01") %>%
  group_by(Latitude) %>% 
  summarise(Total=sum(GPP))

gpp3 %>% summarise_each(funs(sum)) # same as GPP4
########################################################################
# Merge all 3 variable
a1 <- merge(lai4,gpp4, by=c("month","year","Latitude"), all=T)
a2 <- merge(a1, npp4, by=c("month","year","Latitude"), all=T)

# Export to data folder
write.csv(a1, "./data/ReynoldsC/MODIS/lai_gpp.csv", row.names=F)

# What is the long-term monthly mean across sites?
a2 %>% group_by(month) %>% summarise_each(funs(mean))

