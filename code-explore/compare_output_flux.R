################################################################################
# Compare LPJ-GUESS output using Reynold's Creek weather data vs. Daymet
# RC data is for station 176, near mtn big sage plot
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt
library(zoo)

# Path to model output:
dpath <- "/Users/poulterlab1/Documents/SageParm/testout/"

# Read in flux data from the tower near mt sage site----------------------------
df <- read.csv(paste(dpath, "../data/ReynoldsC/DailyFluxes2015.csv", sep="")) %>%
  mutate(date=as.Date(day,origin=as.yearmon("2015-01-01"))) %>%
  mutate(yearmon=format(date, format="%b %Y")) %>%
  mutate(Source="Tower") %>%
  mutate(yearmon=as.yearmon(yearmon)) %>%
  group_by(yearmon, Source) 

df_GPP <- df %>%
  summarise(GPP=sum(GPPmbsec)*0.001) %>% # conver to kg
  dplyr::select(yearmon, GPP, Source)

# Note: sign seems reversed for NEE, hence mult. by negative .001  
df_NEE <- df %>%
  summarise(NEE=sum(NEEmbsec)*-0.001) %>% # conver to kg
  dplyr::select(yearmon, NEE, Source) 

df_ET <- df %>%
  summarise(ET=sum(ETmbsec)) %>% # units are mm
  dplyr::select(yearmon, ET, Source)

df_R <- df %>%
  summarise(Rsp=sum(Rspmbsec)*0.001) %>% # conver to kg
  dplyr::select(yearmon, Rsp, Source) 

# Read in all monthly model output data in a loop-------------------------------
vars <- c("maet", "mevap", "mgpp","mintercep","mlai","mnee","mnpp","mpet",
          "mra","mrh","mrunoff","mwcont_lower","mwcont_upper")
modout <- NULL
for(var in vars) {
  s1 <- read.table(paste(dpath, "station_",var,".out", sep=""), header=T) %>%
    gather(Month, value,Jan:Dec) %>%
    mutate(Source="Station176", variable=var)
  d1 <- read.table(paste(dpath, "daymet_",var,".out", sep=""), header=T) %>%
    gather(Month, value,Jan:Dec) %>%
    mutate(Source="Daymet", variable=var)
  b <- rbind(s1,d1)
  modout <- rbind(modout,b)
}

modout2 <- modout %>%
  mutate(m=match(Month, month.abb), Year=Year+860) %>%
  mutate(yearmon=as.yearmon(paste(Year,m, sep="-"))) %>%
  filter(yearmon>=2014)

# Read in all annual PFT model output data in a loop-------------------------------
vars <- c("anpp","cmass","dens","fpc","lai")
aout <- NULL
for(var in vars) {
  s1 <- read.table(paste(dpath, "station_",var,".out", sep=""), header=T) %>%
    mutate(Source="Station176", variable=var)
  d1 <- read.table(paste(dpath, "daymet_",var,".out", sep=""), header=T) %>%
    mutate(Source="Daymet", variable=var)
  b <- rbind(s1,d1)
  aout <- rbind(aout,b)
}
aout2 <- mutate(aout, Year=Year+860)

# Combine GPP with flux data
GPP1 <- modout2 %>% filter(variable=="mgpp") %>%
  select(yearmon, value, Source) %>%
  rename(GPP=value)
GPP <- rbind.data.frame(GPP1, df_GPP)
# Combine NEE with flux data
NEE1 <- modout2 %>% filter(variable=="mnee") %>%
  select(yearmon, value, Source) %>%
  rename(NEE=value)
NEE <- rbind.data.frame(NEE1, df_NEE)
# Combine respiration with flux data
Rsp1 <- modout2 %>% filter(variable=="mra"|variable=="mrh") %>%
  group_by(yearmon,Source) %>%
  summarise(Rsp=sum(value)) %>%
  select(yearmon, Rsp, Source) 
Rsp <- rbind.data.frame(Rsp1, df_R)
# Combine ET with flux data
ET1 <- modout2 %>% filter(variable=="maet") %>%
  select(yearmon, value, Source) %>%
  rename(ET=value)
ET <- rbind.data.frame(ET1, df_ET)


#############################################
# Check: NEE should equal GPP + R:
df2 <- df %>% mutate(NEE2=-GPPmbsec+Rspmbsec) %>%
  select(ETmbsec,GPPmbsec,Rspmbsec,NEEmbsec,NEE2,yearmon)
head(df2) # Perfect!

# Check guess output: NEE should equal GPP + ra + rh
head(modout2)
mo3 <- modout2 %>% spread(variable,value) %>%
  mutate(NEE2=mgpp-mra-mrh) %>%
  select(maet:mgpp,mra:mrh,mnee,NEE2)
head(mo3) # OK, minus some rounding errors

##########################################
# Plot modeled GPP: Daymet vs. Station driver data
ggcolor <- c("#00BFC4","#F8766D","#000000")
ggplot(data=GPP, aes(x=yearmon, y=GPP, color=Source)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=ggcolor) +
  ylab("Monthly GPP") +
  xlab("Date")

# Plot NEE
ggplot(data=NEE, aes(x=yearmon, y=NEE, color=Source)) +
  geom_line() +
  geom_point() +
  ylab("Monthly NEE") +
  xlab("Date")

# Plot respiration
ggplot(data=Rsp, aes(x=yearmon, y=Rsp, color=Source)) +
  geom_line() +
  geom_point() +
  ylab("Monthly Respiration") +
  xlab("Date")

# Plot ET
ggplot(data=ET, aes(x=yearmon, y=ET, color=Source)) +
  geom_line() +
  geom_point() +
  ylab("Monthly Evapotranspiration") +
  xlab("Date")

# Plot all other variables: Daymet vs. Station drivers
mo3 <- modout %>%
  mutate(m=match(Month, month.abb), Year=Year+860) %>%
  mutate(yearmon=as.yearmon(paste(Year,m, sep="-"))) %>%
  filter(yearmon>=2014) 

ggplot(data=filter(mo3, variable=="mnpp"), 
       aes(x=yearmon, y=value, color=Source)) +
  geom_line() + geom_point() +
  ylab("Monthly NPP") +
  xlab("Date")

p1 <- ggplot(data=filter(mo3, variable=="mlai"), 
       aes(x=yearmon, y=value, color=Source)) +
  geom_line() + geom_point() +
  ylab("Monthly LAI") +
  xlab("Date")
ggplot_build(p1)

###################################################
# Plot some annual variables
ao3 <- filter(aout2, Year>2000) %>%
  gather(PFT, Value, PJ:C4)
ggplot(data=filter(ao3, variable=="fpc"), 
       aes(x=Year, y=Value, color=PFT, shape=Source)) +
  geom_line() + geom_point() +
  ylab("Annual FPC") +
  xlab("Year") +
  geom_vline(xintercept=2014, linetype="dashed")

ggplot(data=filter(ao3, variable=="anpp"), 
       aes(x=Year, y=Value, color=PFT, shape=Source)) +
  geom_line() + geom_point() +
  ylab("Annual NPP") +
  xlab("Year") +
  geom_vline(xintercept=2014, linetype="dashed")
table(ao3$variable)

ggplot(data=filter(ao3, variable=="cmass"), 
       aes(x=Year, y=Value, color=PFT, shape=Source)) +
  geom_line() + geom_point() +
  ylab("Annual Biomass") +
  xlab("Year") +
  geom_vline(xintercept=2014, linetype="dashed")

ggplot(data=filter(ao3, variable=="dens"), 
       aes(x=Year, y=Value, color=PFT, shape=Source)) +
  geom_line() + geom_point() +
  ylab("Annual Density") +
  xlab("Year") +
  geom_vline(xintercept=2014, linetype="dashed")

# NPP over all of time
ggplot(data=filter(aout2, variable=="anpp", Year>1900), 
       aes(x=Year, y=ARTR, color=Source, shape=Source)) +
  geom_line() + geom_point() +
  ylab("Annual NPP") +
  xlab("Year") +
  geom_vline(xintercept=2014, linetype="dashed")

