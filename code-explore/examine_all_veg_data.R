################################################################################
# Pull in veg data and re-format in long form, make plots to examine
# Output data in format comprable to LPJ-GUESS
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)

# Set working directory
setwd("~/Documents/SageParm")

# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv")

# Read in LAI data, canopy cover, and basal cover
lai <- read.csv("data/ReynoldsC/veg_data/2016_LAI_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 
cover <- read.csv("data/ReynoldsC/veg_data/2016_canopycover_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 
basal <- read.csv("data/ReynoldsC/veg_data/2016_basalcover_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 

# Drop green hits, spread, and lump forbs + grass
# This kinda doesn't matter since LPJ lumps all PFTs in monthly
lai2 <- select(lai, -greenhits) %>%
  spread(growthform,lai) %>%
  mutate(C3=forb+grass, Total=shrub+C3) %>%
  select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  select(-replicate) %>%
  summarise_each(funs(mean,sd,min,max)) 
lai2

# what percent sagebrush at each site?
lai2 %>% mutate(percSage=shrub_mean/(shrub_mean+C3_mean))
# wbs: .69%, .25 absolute
# los: .69%, .30 absolute
# mbs: .55%, .67 absolute

# Re-format the cover data:
head(cover)
cover2 <- select(cover, -firsthits) %>%
  spread(growthform,canopycover) %>%
  mutate(C3=forb+grass, Total=C3+shrub) %>%
  select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  select(-replicate) %>%
  summarise_each(funs(mean,sd,min,max)) 
cover2
cover2 %>% mutate(percSage=shrub_mean/(shrub_mean+C3_mean))
# wbs: .31
# los: .45
# mbs: .62

# Basal cover data doesn't actually seem that useful.

# Plots to visualize total LAI:
lai3 <- lai %>% group_by(Site,replicate,Frame) %>%
  summarise(lai=sum(lai))
ggplot(data=lai3, aes(x=Site,y=lai)) +
  geom_boxplot(notch=T) 

# group by rep:
lai4 <- lai %>% group_by(Site,replicate,Frame) %>%
  summarise(lai=sum(lai)) %>%
  group_by(Site,replicate) %>%
  summarise(lai=mean(lai))
ggplot(data=lai4,aes(x=Site, y=lai)) +
  geom_point()

# Plots to visualize cover:
cover3 <- cover %>% group_by(Site,replicate,Frame) %>%
  summarise(cover=sum(canopycover))
ggplot(data=cover3, aes(x=Site,y=cover)) +
  geom_boxplot(notch=T)

# group by rep:
cover4 <- cover %>% group_by(Site,replicate) %>%
  summarise(cover=mean(canopycover))
ggplot(data=cover4,aes(x=Site, y=cover)) +
  geom_point()
# Merge all into DF: Site, Measure, PFT, Value

# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv")
mod2 <- mod %>% filter(year==2016)
mod2 %>% filter(month==5)
mod2 %>% filter(month==6)
mod2 %>% filter(month==7)

# Merge MODIS lai and npp with field measurements to compare
# Sampling dates for LAI:
#WBS1: 5-17 to 6-11-2016
#LOS1: 6-28 to 6-29-2016
#MBS1: 7-20-2016

# To properly compare MODIS, should read in the bi-weekly data:
m1 <- read.csv("data/ReynoldsC/MODIS/mbsage-MCD15A2-005-results.csv") %>%
  select(Date, Latitude,MCD15A2_005_Lai_1km) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  rename(LAI=MCD15A2_005_Lai_1km) %>%
  spread(Latitude, LAI) # must be short form for merge

test<- m1 %>% filter(Date>="2015-01-01") %>%
  summarise_each(funs(max))

# Get dates to match the sampling dates: (must use quotes to filter by date!)
m2 <- m1 %>% filter(Date > "2016-05-01") %>%
  filter(Date=="2016-06-01" |Date=="2016-06-25" | Date=="2016-07-19")
m3 <- m1 %>% filter(Date>="2016-01-01") %>%
  mutate(Source="MODIS") %>%
  rename(mbs=`43.064483`,wbs=`43.167545`,los=`43.1438884`) %>%
  select(-`43.120711`) %>%
  gather(Site, LAI, mbs:wbs)
m3

# Get Pat's LAI data in the right format:
Site <- c("wbs","los","mbs")
Date <- c("2016-05-30","2016-06-29","2016-07-20")
field1 <- cbind.data.frame(Site,Date) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  mutate(Source="Field")
field <- merge(field1,lai4, by="Site") %>%
  select(-replicate) %>%
  group_by(Site,Date,Source) %>%
  summarise(LAI=mean(lai))
both <- merge(m3,field,by=c("Date","Source","Site","LAI"),all=T)

# Plot them both together to verify quality of MODIS:
ggplot(data=both, aes(x=Date,y=LAI, color=Source)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  facet_wrap(~Site)
# YES! I feel good about using MODIS

# Let's plot the monthly time series for MODIS to see if it is smoother:
