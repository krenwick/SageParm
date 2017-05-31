################################################################################
# Plot evergreen, evergreen- new soil model, flux data
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)

# Set working directory
setwd("~/Documents/SageParm")

# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv")

# Pull in data from the EVERGREEN model runs
ever <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai")
for(var in vars) {
  data=paste("RCout/orig_ever_",var,".out", sep="")
  b <- fread(data, header=T)
  # select appropriate years (2014-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,OldSoil, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Site,Variable,OldSoil)
  
  ever <- rbind.data.frame(ever,b1)
}

# Pull in data from the EVERGREEN- NEW SOIL model runs
everSM <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai")
for(var in vars) {
  data=paste("RCout/origSM_ever_",var,".out", sep="")
  b <- fread(data, header=T)
  # select appropriate years (2014-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,NewSoil, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Site,Variable,NewSoil)
  
  everSM <- rbind.data.frame(everSM,b1)
}

# Pull in data from the EVERGREEN- VARY DEPTH model runs
everSM2 <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai")
for(var in vars) {
  data=paste("RCout/origSM2_ever_",var,".out", sep="")
  b <- fread(data, header=T)
  # select appropriate years (2014-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,NewSoil2, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Site,Variable,NewSoil2)
  
  everSM2 <- rbind.data.frame(everSM2,b1)
}

all <- merge(ever,everSM, by=c("Year","Month","Variable","Site")) %>%
  merge(.,everSM2,by=c("Year","Month","Variable","Site")) %>%
  mutate(Variable=replace(Variable,Variable=="mgpp","GPP")) %>%
  mutate(Variable=replace(Variable,Variable=="mnee","NEE")) %>%
  merge(.,df3, by=c("Year","Month","Variable","Site"))

all$D <- as.yearmon(paste(all$Year, all$Month), "%Y %b")
all$Date <- as.Date(all$D)

# Make a plot for GPP
GPP <- filter(all, Variable=="GPP") %>%
  gather(Source, GPP, OldSoil:Tower)

flux <- ggplot(data=GPP, aes(x=Date, y=GPP, color=Source, linetype=Source)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(name="Source",
                        breaks=c("OldSoil","NewSoil","NewSoil2","Tower"),
                        labels=c("OldSoil","NewSoil","NewSoil2","Tower"),
                        values=c("dashed","dashed","dashed","solid"))+
  scale_color_manual(name="Source",
                     breaks=c("OldSoil","NewSoil","NewSoil2","Tower"),
                     labels=c("OldSoil","NewSoil","NewSoil2","Tower"),
                     values=c("darkcyan","deepskyblue","purple","black")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kg~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
flux

ggsave("figures/GPP_newsoilmod_flux.pdf", plot=flux,
       width = 169, height = 140, units = 'mm')

# Make a plot for NEE
NEE <- filter(all, Variable=="NEE") %>%
  gather(Source, NEE, OldSoil:Tower)

flux2 <- ggplot(data=NEE, aes(x=Date, y=NEE, color=Source, linetype=Source)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(name="Source",
                        breaks=c("OldSoil","NewSoil","Tower"),
                        labels=c("OldSoil","NewSoil","Tower"),
                        values=c("dashed", "dashed","solid"))+
  scale_color_manual(name="Source",
                     breaks=c("OldSoil","NewSoil","Tower"),
                     labels=c("OldSoil","NewSoil","Tower"),
                     values=c("darkcyan","deepskyblue","black")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(NEE~(kg~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("figures/NEE_oldnewsoil_flux.pdf", plot=flux2,
       width = 169, height = 140, units = 'mm')

