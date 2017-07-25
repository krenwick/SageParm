################################################################################
# Examine output from hyalite optimization runs
# NOTE: make sure model on git is MASTER and compiled with old phenology!
# New phenology model is in LPJfiles
################################################################################
library(DEoptim)
library(tidyverse)
library(data.table)
library(zoo)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm")
outname1 <- "monthlygpplaioldgrassll.51"
outname2 <- "monthlygpplaioldgrassll.52"
object1 <- "HyaliteOutput/DE1parimage_monthlygpplaioldgrassll.5.RData"
object2 <- "HyaliteOutput/NewPhenImage_monthlygpplaioldgrassll.5.RData"

################################################################################
# Get baseline data on GPP, NEE, and LAI
################################################################################
# Read in GPP flux data
GPP <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="GPP")

# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
  dplyr::mutate(Latitude=round(Latitude,2)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  dplyr::rename(Year=year) %>%
  dplyr::mutate(Month=month.abb[month]) %>%
  tidyr::gather(Variable, Tower, LAI:GPP) %>%
  dplyr::filter(Variable=="LAI") %>%
  dplyr::select(Year,Month,Variable,Site,Tower)  

# Merge flux with MODIS LAI
df4 <- rbind.data.frame(mod,GPP)

# Read in monthly NEE data
NEE <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="NEE")

################################################################################
# Pull GPP output from optim1 back in to R:
gpp1 <- fread(paste("Output_localruns/mgpp_",outname1,".txt",sep=""), header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  gather(Month,Model, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="GPP") %>%
  dplyr::select(Year, Month,Site,Variable,Model) %>%
  spread(Variable,Model) %>%
  dplyr::select(Year,Month,Site,GPP) %>%
  gather(Variable, Model, GPP)

# Pull LAI output from optim1 back in to R:
lai1 <- fread(paste("Output_localruns/mlai_",outname1,".txt",sep=""), header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  gather(Month,Model, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="LAI") %>%
  dplyr::select(Year, Month,Site,Variable,Model) %>%
  spread(Variable,Model) %>%
  dplyr::select(Year,Month,Site,LAI) %>%
  gather(Variable, Model, LAI)

################################################################################
# Pull GPP output from optim2 back in to R:
gpp2 <- fread(paste("Output_localruns/mgpp_",outname2,".txt",sep=""), header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  gather(Month,Model2, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="GPP") %>%
  dplyr::select(Year, Month,Site,Variable,Model2) %>%
  spread(Variable,Model2) %>%
  dplyr::select(Year,Month,Site,GPP) %>%
  gather(Variable, Model2, GPP)

# Pull LAI output from optim2 back in to R:
lai2 <- fread(paste("Output_localruns/mlai_",outname2,".txt",sep=""), header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  gather(Month,Model2, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="LAI") %>%
  dplyr::select(Year, Month,Site,Variable,Model2) %>%
  spread(Variable,Model2) %>%
  dplyr::select(Year,Month,Site,LAI) %>%
  gather(Variable, Model2, LAI)

################################################################################
# Merge model output with flux/MODIS data
################################################################################
# Merge with flux data
GPPall <- merge(gpp1,df4, by=c("Year","Month","Site","Variable")) %>%
  merge(.,gpp2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="GPP") %>%
  gather(Source, GPP, c(Tower,Model,Model2)) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  mutate(Source=factor(Source, levels=c("Tower","Model","Model2"), ordered=T))

# Merge with modis data
LAIall <- merge(lai1,df4, by=c("Year","Month","Site","Variable")) %>%
  merge(.,lai2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="LAI") %>%
  gather(Source, LAI, c(Tower,Model,Model2)) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  mutate(Source=factor(Source, levels=c("Tower","Model","Model2"), ordered=T))

################################################################################
# Plot the result
################################################################################
# Plot GPP:
g1 <- ggplot(data=GPPall, aes(x=Date, y=GPP, color=Source, linetype=Source)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(name="Source",
                        breaks=c("Tower","Model","Model2"),
                        labels=c("Tower","Optimal Parameters","New Phenology"),
                        values=c("solid", "dashed","dashed"))+
  scale_color_manual(name="Source",
                     breaks=c("Tower","Model","Model2"),
                     labels=c("Tower","Optimal Parameters","New Phenology"),
                     values=c("black","dodgerblue4","forestgreen")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.minor=element_blank()) +
  theme_bw()
g1

ggsave("figures/GPP_optim_newgrass_GPPannualLAInoweight.pdf", plot=g1,
       width = 200, height = 130, units = 'mm')

# Plot LAI:
l1 <- ggplot(data=LAIall, aes(x=Date, y=LAI, color=Source, linetype=Source)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(name="Source",
                        breaks=c("Tower","Model","Model2"),
                        labels=c("Tower","Optimal Parameters","New Phenology"),
                        values=c("solid", "dashed","dashed"))+
  scale_color_manual(name="Source",
                     breaks=c("Tower","Model","Model2"),
                     labels=c("Tower","Optimal Parameters","New Phenology"),
                     values=c("black","dodgerblue4","forestgreen")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab("LAI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()

l1

ggsave("figures/LAI_optim_newgrass_GPPannualLAInoweight.pdf", plot=l1,
       width = 169, height = 140, units = 'mm')