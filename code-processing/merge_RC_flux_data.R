################################################################################
# Read in and merge 2015-2016 flux data
# Output: cleaned, merged file
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))

#----- SET WORKING DIRECTORY ----- 
setwd("~/Documents/SageParm")

# Read in flux data from 4 RC sites
# NEE + GPP = Rsp
df <- read.csv("data/ReynoldsC/DailyFluxes.2015updated.csv") %>%
  mutate(date=as.Date(day,origin="2014-12-31")) %>%
  mutate(Month=format(date, format="%b")) %>%
  rename(Year=year) 

# Pull in and process the 2016 data
df2 <- read.csv("data/ReynoldsC/DailyFluxes.2016.csv") %>%
  mutate(date=as.Date(day,origin="2015-12-31")) %>%
  mutate(Month=format(date, format="%b")) %>%
  rename(Year=year)
df3 <- rbind.data.frame(df,df2) 

# look for month/site combos with missing data
check <- df3 %>%
  gather(Site, Tower, ETwbsec:Rspmbsec) %>%
  na.omit() %>%
  group_by(Year,Month,Site) %>%
  summarise(n=n()) %>%
  filter(n<28)
check # Must cut losec Dec. 2014 and Feb. 2015
# NOTE: if use 2016, cut Feb. leap day

df4 <- df3 %>%
  group_by(Year,Month) %>%
  select(-jday, -day, -date) %>%
  summarise_each(funs(sum)) %>% # If missing value -> NA
  gather(Site, Tower, ETwbsec:Rspmbsec) %>%
  separate(Site, into=c("Variable","Site"), sep=-6) %>%
  mutate(Tower=ifelse(Variable=="GPP", Tower*0.001, Tower)) %>% # conver to kg
  mutate(Tower=ifelse(Variable=="NEE", Tower*0.001, Tower)) %>% # conver to kg
  mutate(Tower=ifelse(Variable=="Rsp", Tower*0.001, Tower)) %>% # conver to kg
  na.omit() # Cuts Dec-Feb GPP,NEE,Rsp for losec

# Output data
write.csv(df4,"data/RCflux_15_16.csv", row.names=F)
