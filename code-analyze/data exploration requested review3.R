################################################################################
# Dig into LAI and GPP reference data
################################################################################
rm(list=ls()) 
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)
library(grid)
library(gridExtra)

# Set working directory
setwd("~/Documents/SageParm")
outname1 <- "mgl_disturb_summergrass1"
outname2 <- "mgl_disturb_summergrass2"

# Journal Specifications for figure size
# Agricultural and Forest Meteorology
col1 <- 90 # 1 column width, in mm
col1.5 <- 140
col2 <- 190 # 2 column width, in mm
#########################################################
# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv")
df3$D <- as.yearmon(paste(df3$Year, df3$Month), "%Y %b")
df3$Date <- as.Date(df3$D)

# Get annual average
anGPP <- df3 %>% filter(Variable=="GPP"&Year>=2015) %>%
  group_by(Year, Site) %>%
  summarize(GPP=sum(Tower))
mean(anGPP$GPP)

# Get total GPP by site for water year 2015 (Oct-Sept)
df3 %>% filter(Variable=="GPP") %>%
  filter(Date>="2014-10-01"&Date<="2015-09-01") %>%
  group_by(Site) %>%
  summarize(GPP=sum(Tower))

# Get total GPP by site for water year 2016
df3 %>% filter(Variable=="GPP") %>%
  filter(Date>="2015-10-01"&Date<="2016-09-01") %>%
  group_by(Site) %>%
  summarize(GPP=sum(Tower))


# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
  mutate(Latitude=round(Latitude,2)) %>%
  mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  rename(Year=year) %>%
  mutate(Month=month.abb[month]) %>%
  gather(Variable, MODIS, LAI:GPP) %>%
  select(Year,Month,Variable,Site,MODIS,Date)  

# What is the annual max LAI at each site?
an <- mod %>% filter(Variable=="LAI" & Year>=2015) %>%
  group_by(Year, Site) %>%
  summarize(LAI=max(MODIS))
mean(an$LAI)
mod %>% filter(Variable=="LAI" & Year>=2015) %>%
  summarize(mean=mean(MODIS))

# Merge flux with MODIS LAI
df4 <- merge(df3,mod,by=c("Year","Month","Variable","Site"), all=T) %>%
  mutate(Date=as.Date(Date)) %>%
  filter(Date>="2014-10-01")
table(df4$Variable)

# Plot scatterplot of GPP vs. GPP
df5 <- df4 %>% filter(Variable=="GPP")
ggplot(data=df5, aes(x=Tower, y=MODIS, color=Site)) +
  geom_point() +
  geom_abline(xintercept=0,slope=1) +
  xlim(0,.25) +
  ylim(0,.25) +
  facet_wrap(~Site)

# Plot scatterplot of LAI vs. GPP (MODIS)
df6 <- df4 %>% select(-Tower) %>%
  spread(Variable, MODIS)
ggplot(data=df6, aes(x=GPP, y=LAI, color=Site)) +
  geom_point() +
  ggtitle("MODIS GPP vs. MODIS LAI")

# Plot Scatterplot of tower GPP vs. MODIS LAI
t2 <- filter(df3, Variable=="GPP") %>%
  rename(TowerGPP=Tower)
mod2 <- mod %>% filter(Variable=="LAI") %>%
  rename(MODISLAI=MODIS)
df7 <- merge(t2,mod2,by=c("Year","Month","Site"), all=T) %>%
  mutate(Date=as.Date(Date)) %>%
  filter(Date>="2014-10-01") %>%
  na.omit()

ggplot(data=df7, aes(x=TowerGPP, y=MODISLAI, color=Site)) +
  geom_point() +
  ggtitle("Tower GPP vs. MODIS LAI")

################################################################################
# Look into data quality with MODIS LAI
################################################################################
# Pull in MODIS 1 (LAI). LAI is unitless.
m1 <- read.csv("data/ReynoldsC/MODIS/mbsage-MCD15A2-005-results.csv") %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  mutate(QualIssue=ifelse(MCD15A2_005_FparLai_QC_MODLAND=="0b0",0,1)) %>%
  mutate(Latitude=round(Latitude,2)) %>%
  mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  mutate(Site=ifelse(Latitude==43.17, "wbsec",Site))

names(m1)
table(m1$MCD15A2_005_FparLai_QC)
table(m1$MCD15A2_005_FparLai_QC_bitmask)
table(m1$MCD15A2_005_FparLai_QC_MODLAND)
table(m1$MCD15A2_005_FparLai_QC_MODLAND_Description) # good- 2 categories (other: 46)
table(m1$MCD15A2_005_FparLai_QC_Sensor)
table(m1$MCD15A2_005_FparLai_QC_Sensor_Description) # 2 categories- aqua(113), terra(235)
table(m1$MCD15A2_005_FparLai_QC_DeadDetector)
table(m1$MCD15A2_005_FparLai_QC_DeadDetector_Description) # all the same- detectors fine
table(m1$MCD15A2_005_FparLai_QC_CloudState)
table(m1$MCD15A2_005_FparLai_QC_CloudState_Description) # 3 categories: present(65), not(262), or mixed(21)
table(m1$MCD15A2_005_FparLai_QC_SCF_QC)
table(m1$MCD15A2_005_FparLai_QC_SCF_QC_Description) # 3 categories: 
# main failed geom (24)
# main failed other (22) (these add to 46)
# main method used 
table(m1$MCD15A2_005_FparLai_QC_SCF_QC, m1$MCD15A2_005_FparLai_QC_CloudState_Description)

# What percent of data good?
302/(302+46)

# Plot time series
ggplot(m1, aes(x=Date, y=QualIssue)) +
  geom_point(aes(color=MCD15A2_005_FparLai_QC_SCF_QC)) +
  #geom_point(aes(color=MCD15A2_005_FparLai_QC_CloudState_Description)) +
  scale_color_discrete(name="") +
  geom_line() +
  facet_wrap(~Site) 

# Plot LAI
ggplot(m1, aes(x=Date, y=MCD15A2_005_Lai_1km)) +
  geom_point(aes(color=MCD15A2_005_FparLai_QC_SCF_QC)) +
  #geom_point(aes(color=MCD15A2_005_FparLai_QC_CloudState_Description)) +
  scale_color_discrete(name="") +
  geom_line() +
  facet_wrap(~Site) 

# Look at which months affected
m2 <- m1 %>% filter(MCD15A2_005_FparLai_QC_SCF_QC!="0b000")
table(m2$Date)
# ALL bad data occurs Nov 17th - Feb 26th (15); Nov 17 - Mar 29th

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

