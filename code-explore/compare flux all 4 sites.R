#############################################################################
# Plot climate: station vs. Daymet.
# Plot model output, compared to flux, for 2015
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=12)) # sized for ppt
library(zoo) #time series
detach("package:raster", unload=T) # so select works

# paths to data and folder for figures
fpath <- "/Users/poulterlab1/Documents/SageParm/figures/"
cpath <- "/Users/poulterlab1/Documents/LPJGUESS/Climate/Daymet/" # climate data

# Path to model output:
dpath <- "/Users/poulterlab1/Documents/SageParm/RCout/"

################################################################################
# 1. READ IN DATA

# Pull in site list to get coordinates
sites <- read.csv(paste(dpath, "../data/ReynoldsC/EC-CoreSites-coords.csv", sep="")) %>%
  mutate(Lon=round(Lon,4), Lat=round(Lat,4))

# Read in flux data from 4 RC sites
sitelist <- c("wbsec","losec","h08ec","mbsec")
df <- read.csv(paste(dpath, "../data/ReynoldsC/DailyFluxes2015.csv", sep="")) %>%
  mutate(date=as.Date(day,origin=as.yearmon("2015-01-01"))) %>%
  mutate(yearmon=format(date, format="%b %Y")) %>%
  mutate(Source="Tower") %>%
  mutate(yearmon=as.yearmon(yearmon)) %>%
  group_by(yearmon, Source) %>%
  select(-jday, -year, -day, -date) %>%
  summarise_each(funs(sum)) %>%
  gather(Site, Value, ETwbsec:Rspmbsec) %>%
  separate(Site, into=c("Variable","Site"), sep=-6) %>%
  mutate(Value=ifelse(Variable=="GPP", Value*0.001, Value)) %>% # conver to kg
  mutate(Value=ifelse(Variable=="NEE", Value*-0.001, Value)) %>% # conver to kg
  mutate(Value=ifelse(Variable=="Rsp", Value*0.001, Value)) # conver to kg
head(df)
unique(df$Site)
unique(df$Variable)

# Test to figure out sign convention for NEE
df2 <- spread(df, Variable, Value) %>%
  #mutate(NEE_minus_others=NEE-GPP-Rsp) %>%
  mutate(GPP_minus_Rsp=GPP-Rsp) %>%
  mutate(Rsp_minus_GPP=Rsp-GPP) %>%
  #mutate(GPP_plus_Rsp=GPP+Rsp) 
head(df2, 30)
tail(df2,20)

# Read in model output for the 4 sites
# Read in all monthly model output data in a loop-------------------------------
vars <- c("maet", "mevap", "mgpp","mintercep","mlai","mnee","mnpp","mpet",
          "mra","mrh","mrunoff","mwcont_lower","mwcont_upper")
modout <- NULL
for(var in vars) {
  s1 <- read.table(paste(dpath, "daymet3_",var,".out", sep=""), header=T) %>%
    gather(Month, value,Jan:Dec) %>%
    mutate(Source="Model", variable=var)
  modout <- rbind(modout,s1)
}

modout2 <- modout %>%
  mutate(m=match(Month, month.abb), Year=Year+860) %>%
  mutate(yearmon=as.yearmon(paste(Year,m, sep="-"))) %>%
  filter(yearmon>=2014)
head(modout2)
table(modout2$Lon, modout2$Lat)
table(sites$Lon, sites$Lat)
modout3 <- merge(modout2, sites, by=c("Lon","Lat")) %>%
  select(yearmon, Source,variable,Site,value) %>%
  rename(Value=value, Variable=variable) %>%
  mutate(Variable=ifelse(Variable=="mgpp","GPP", Variable)) %>%
  mutate(Variable=ifelse(Variable=="mnee","NEE", Variable)) %>%
  spread(Variable, Value) %>%
  mutate(Rsp=mra+mrh, ET=mevap+maet) %>%
  gather(Variable,Value,GPP:ET)
head(modout3)

# Read in all annual PFT model output data in a loop----------------------------
vars <- c("anpp","cmass","dens","fpc","lai")
aout <- NULL
for(var in vars) {
  s1 <- read.table(paste(dpath, "daymet3_",var,".out", sep=""), header=T) %>%
    mutate(Source="Model", variable=var)
  aout <- rbind(aout,s1)
}
aout2 <- mutate(aout, Year=Year+860)

# Combine flux data with model output-------------------------------------
df2 <- data.frame(df)
flux <- rbind.data.frame(df2,modout3) %>%
  filter(yearmon>=min(df2$yearmon))
flux$Date <- as.Date(as.yearmon(flux$yearmon,format="%y-%m"))
head(flux)

################################################################################
# Make Plots
ggplot(data=filter(flux, Variable=="GPP"), 
       aes(x=Date, y=Value, linetype=Source, color=Site)) +
  geom_line() +
  geom_point() +
  #scale_color_manual(values=ggcolor) +
  ylab("Monthly GPP") +
  xlab("Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Site)

# Plot NEE
ggplot(data=filter(flux, Variable=="NEE"), 
       aes(x=Date, y=Value, linetype=Source, color=Site)) +
  geom_line() +
  geom_point() +
  #scale_color_manual(values=ggcolor) +
  ylab("Monthly NEE") +
  xlab("Date") +
  geom_hline(yintercept=0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Site)

# Plot respiration
ggplot(data=filter(flux, Variable=="Rsp"), 
       aes(x=Date, y=Value, linetype=Source, color=Site)) +
  geom_line() +
  geom_point() +
  #scale_color_manual(values=ggcolor) +
  ylab("Monthly Respiration") +
  xlab("Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Site)

# Plot ET
ggplot(data=filter(flux, Variable=="ET"), 
       aes(x=Date, y=Value, linetype=Source, color=Site)) +
  geom_line() +
  geom_point() +
  #scale_color_manual(values=ggcolor) +
  ylab("Monthly ET") +
  xlab("Date") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Site)
