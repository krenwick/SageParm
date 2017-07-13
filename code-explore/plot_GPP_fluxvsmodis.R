################################################################################
# Plot monthly GPP from flux vs. modis
################################################################################
library(tidyverse); theme_set(theme_bw(base_size=10))
library(data.table)
library(zoo)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm")

################################################################################
# Get baseline data on GPP, NEE, and LAI
################################################################################
# Read in GPP flux data
GPP <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="GPP") %>%
  rename(Value=Tower) %>%
  mutate(Source="Tower")

# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
  dplyr::mutate(Latitude=round(Latitude,2)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  dplyr::rename(Year=year) %>%
  dplyr::mutate(Month=month.abb[month]) %>%
  tidyr::gather(Variable, Value, LAI:GPP) %>%
  mutate(Source="MODIS") %>%
  dplyr::select(Year,Month,Variable,Site,Value,Source)  

# Read in GPP data from Bill Smith
bs1 <- read.csv("data/ReynoldsC/BSmith/H08EC_cleaned_daily.csv") %>%
  mutate(Site="h08ec") %>%
  rename(Shrub.GPP..old.=Shrub.GPP..new..1)
bs2 <- read.csv("data/ReynoldsC/BSmith/LOSEC_cleaned_daily.csv") %>%
  mutate(Site="losec") %>%
  rename(Shrub.GPP..old.=Shrub.GPP..new..1)
bs3 <- read.csv("data/ReynoldsC/BSmith/WBSEC_cleaned_daily.csv") %>%
  mutate(Site="wbsec")
bs4 <- read.csv("data/ReynoldsC/BSmith/MBSEC_cleaned_daily.csv") %>%
  mutate(Site="mbsec")
bs <- rbind.data.frame(bs1,bs2,bs3,bs4) %>%
  mutate(Date=as.Date(date)) %>%
  mutate(Month=format(Date, format="%b")) %>%
  group_by(Month,year,Site) %>%
  summarise_at(vars(Grass.GPP..new.:Shrub.GPP..old.), sum) %>%
  mutate(SmithNew=(Shrub.GPP..new.+Grass.GPP..new.)/1000) %>%
  mutate(SmithOld=(Shrub.GPP..old.+Grass.GPP..old.)/1000) %>%
  gather(Source,Value, SmithNew:SmithOld) %>%
  rename(Year=year) %>%
  mutate(Variable="GPP") %>%
  select(Year,Month,Variable,Site,Value,Source)
  

# Merge flux with MODIS 
df4 <- rbind.data.frame(mod,GPP,bs) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2014-09-01") %>%
  filter(Variable=="GPP") 

# Plot MODIS and Tower GPP
df5 <- filter(df4, Year<2016)
p1 <- ggplot(data=df5, aes(x=Date, y=Value, color=Source)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}~month^{-1}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p1

ggsave("figures/GPP_EC_vs_MODIS.pdf", plot=p1,
       width = 180, height = 130, units = 'mm')

ann_text <- data.frame(Date = as.Date(c("2014-11-01","2014-11-01","2014-11-01",
                                        "2014-11-01")),
                       GPP= c(.3,.3,.3,.3),
                       Site = c("h08ec","losec","mbsec", "wbsec"),
                       lab=c("a) burn","b) los","c) mbs", "d) wbs"))

# Make clean plot for ppt and paper: first, bw for paper
df5 <- filter(df4, Year<2016, Source!="SmithOld")
p1 <- ggplot(data=df5, aes(x=Date, y=Value)) +
  #geom_point(aes(color=Source)) +
  geom_line(aes(color=Source,linetype=Source)) +
  scale_linetype_manual(name="Source",
                        labels=c("Modis-MERRA2","MODIS-Local","Tower"),
                        breaks=c("MODIS","SmithNew","Tower"),
                        values=c("dashed","dashed","solid"))+
  scale_color_manual(name="Source",
                     labels=c("Modis-MERRA2","MODIS-Local","Tower"),
                     breaks=c("MODIS","SmithNew","Tower"),
                     values=c("darkcyan","deepskyblue","black")) +
  facet_wrap(~Site) +
  geom_text(data = ann_text,aes(x=Date, y=GPP,label=lab)) +
  xlab("Date") +
  scale_x_date(date_breaks = "2 months",date_labels = "%b %y") +
  ylab(expression(GPP~(kgC~m^{-2}~month^{-1}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        #panel.border = element_rect(colour = "black"),
        legend.position = "top")

p1
p1 <- ggplot(data=df5, aes(x=Date, y=Value, color=Source)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}~month^{-1}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p1

ggsave("figures/GPP_EC_vs_MODIS.pdf", plot=p1,
       width = 180, height = 130, units = 'mm')

