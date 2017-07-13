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

# Get object from DEparoptim_summergreen1:

load("HyaliteOutput/DE1parimage.RData")
summary(DE1)
DE1$optim
pars1 <- DE1$optim$bestmem

########################
# Extract parameter values and plot results
DE1$optim$bestmem[1]

# Make new ins file with these parameter values:
insfile <- "optim_hyalite_bundle/summergreen_optim1_LMpar.ins" # name of ins file to use
# gsub: replaces all occurences of a pattern
ins  <- readLines(insfile)
tx  <- gsub(pattern = "slaval", replace = DE1$optim$bestmem[1], x = ins)
tx  <- gsub(pattern = "k_latosaval", replace = DE1$optim$bestmem[2], x = tx)
tx  <- gsub(pattern = "ltor_maxval", replace = DE1$optim$bestmem[3], x = tx)
tx  <- gsub(pattern = "rootdistval", replace = DE1$optim$bestmem[4], x = tx)
tx  <- gsub(pattern = "rootdist2", replace = (1-DE1$optim$bestmem[4]), x = tx)
#tx  <- gsub(pattern = "pstemp_minval", replace = DE1$optim$bestmem[5], x = tx)
tx  <- gsub(pattern = "est_maxval", replace = DE1$optim$bestmem[5], x = tx)
tx  <- gsub(pattern = "pstemp_lowval", replace = DE1$optim$bestmem[6], x = tx)
tx  <- gsub(pattern = "pstemp_maxval", replace = DE1$optim$bestmem[7], x = tx)
tx  <- gsub(pattern = "pstemp_hival", replace = DE1$optim$bestmem[8], x = tx)
tx  <- gsub(pattern = "phengdd5rampval", replace = DE1$optim$bestmem[9], x = tx)
tx  <- gsub(pattern = "npatch 1", replace = "npatch 100", x = tx)
insname <- "summergreen_selectedparms"
tx  <- gsub(pattern = "randomval", replace = insname, x = tx)
tx  <- gsub(pattern = "\\./", replace = "Output_localruns/", x = tx)
writeLines(tx, con=paste("NewIns/",insname,".ins", sep=""))

# Run model with new ins then compare output to flux data:
#system("/Users/poulterlab1/version-control/LPJ-GUESS/ModelFiles/modules/./guess /Users/poulterlab1/Documents/SageParm/NewIns/summergreen_selectedparms.ins")

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
gpp1 <- fread(paste("Output_localruns/mgpp/",insname,".out",sep=""), header=T) %>%
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
lai1 <- fread(paste("Output_localruns/mlai/",insname,".out",sep=""), header=T) %>%
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
# Get model object from optim2 model run (New Phen)
# Run new phenology LPJ-GUESS with optimized parameters (new mod location!)
# Get output from model runs
################################################################################
load("HyaliteOutput/NewPhenImage.RData")
summary(DE1)
DE1$optim
pars2 <- DE1$optim$bestmem
########################
# Make new ins file with these parameter values:
insfile2 <- "optim_hyalite_bundle/optim2_newphen.ins" # name of ins file to use
# gsub: replaces all occurences of a pattern
ins2  <- readLines(insfile2)
tx  <- gsub(pattern = "slaval", replace = DE1$optim$bestmem[1], x = ins2)
tx  <- gsub(pattern = "k_latosaval", replace = DE1$optim$bestmem[2], x = tx)
tx  <- gsub(pattern = "ltor_maxval", replace = DE1$optim$bestmem[3], x = tx)
tx  <- gsub(pattern = "rootdistval", replace = DE1$optim$bestmem[4], x = tx)
tx  <- gsub(pattern = "rootdist2", replace = (1-DE1$optim$bestmem[4]), x = tx)
tx  <- gsub(pattern = "phen_winterval", replace = DE1$optim$bestmem[5], x = tx)
tx  <- gsub(pattern = "pstemp_lowval", replace = DE1$optim$bestmem[6], x = tx)
tx  <- gsub(pattern = "pstemp_maxval", replace = DE1$optim$bestmem[7], x = tx)
tx  <- gsub(pattern = "pstemp_hival", replace = DE1$optim$bestmem[8], x = tx)
tx  <- gsub(pattern = "aphenval", replace = DE1$optim$bestmem[9], x = tx)
#tx  <- gsub(pattern = "pstemp_minval", replace = DE1$optim$bestmem[10], x = tx)
tx  <- gsub(pattern = "est_maxval", replace = DE1$optim$bestmem[10], x = tx)
tx  <- gsub(pattern = "npatch 1", replace = "npatch 100", x = tx)

insname2 <- "optim2_newphen"
tx  <- gsub(pattern = "randomval", replace = insname2, x = tx)
tx  <- gsub(pattern = "\\./", replace = "Output_localruns/", x = tx)
writeLines(tx, con=paste("NewIns/",insname2,".ins", sep=""))

# Run model with new ins then compare output to flux data:
#system("/Users/poulterlab1/Documents/SageParm/LPJfiles/./guess /Users/poulterlab1/Documents/SageParm/NewIns/optim2_newphen.ins")

################################################################################
# Pull GPP output from optim2 back in to R:
gpp2 <- fread(paste("Output_localruns/mgpp_",insname2,".txt",sep=""), header=T) %>%
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
lai2 <- fread(paste("Output_localruns/mlai_",insname2,".txt",sep=""), header=T) %>%
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
  gather(Source, GPP, Model:Model2) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  mutate(Source=factor(Source, levels=c("Tower","Model","Model2"), ordered=T))

# Merge with modis data
LAIall <- merge(lai1,df4, by=c("Year","Month","Site","Variable")) %>%
  merge(.,lai2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="LAI") %>%
  gather(Source, LAI, Model:Model2) %>%
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

ggsave("figures/GPP_optim_newgrass_justGPP.pdf", plot=g1,
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

# Lai is still absurdly high

# Calculate R2 for each:
R2gpp1 <- GPPall %>% spread(Source,GPP)
summary(lm(Tower~Model, data=R2gpp1)) # R2 = .70, newgrass= .72
summary(lm(Tower~Model2, data=R2gpp1)) # R2 = .72, newgrass=.83

R2lai <- LAIall %>% spread(Source,LAI)
summary(lm(Tower~Model, data=R2lai)) # R2 = .14, newgrass=.32
summary(lm(Tower~Model2, data=R2lai)) # R2 = .32, newgrass=.58

# calculate SSR:
R2gpp1 %>% mutate(M1=(Model-Tower)^2, M2=(Model2-Tower)^2) %>%
  summarise_at(vars(M1:M2),sum)
# M1 = .104, M2 = .160. Yup.


ggsave("figures/LAI_optim_newgrass_justGPP.pdf", plot=l1,
       width = 169, height = 140, units = 'mm')

################################################################################
# Look at LAI- annual. Is it sage or grass?
a1 <- fread(paste("Output_localruns/lai/",insname,".out",sep=""), header=T) %>%
  mutate(Source="Model")
a2 <- fread(paste("Output_localruns/lai_",insname2,".txt",sep=""), header=T) %>%
  mutate(Source="Model2")
a3 <- rbind.data.frame(a1,a2) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(percSage=ARTR/Total) %>%
  select(Year,Source,Site,percSage) %>%
  spread(Source,percSage)
  #select(Year,Source,Site,ARTR) %>%
  #spread(Source,ARTR)
a3

################################################################################
# Look at FPC. Is grass to sage to bare ground correct?
f1 <- fread(paste("Output_localruns/fpc/",insname,".out",sep=""), header=T) %>%
  mutate(Source="Model")
f2 <- fread(paste("Output_localruns/fpc_",insname2,".txt",sep=""), header=T) %>%
  mutate(Source="Model2")
f3 <- rbind.data.frame(f1,f2) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) 
f3

# Pull in Pat Clark's canopy cover data:
cover <- read.csv("data/ReynoldsC/veg_data/2016_canopycover_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 
cover2 <- select(cover, -firsthits) %>%
  spread(growthform,canopycover) %>%
  mutate(C3=forb+grass) %>%
  select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  select(-replicate) %>%
  summarise_each(funs(mean,sd,min,max)) 
cover2
cover2 %>% mutate(percSage=shrub_mean/(shrub_mean+C3_mean))

################################################################################
# Pull NEE output back in to R:
nee <- fread(paste("Output_localruns/mnee/",insname,".out",sep=""), header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  gather(Month,Model, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="NEE") %>%
  dplyr::select(Year, Month,Site,Variable,Model) %>%
  spread(Variable,Model) %>%
  dplyr::select(Year,Month,Site,NEE) %>%
  gather(Variable, Model, NEE)

nee2 <- fread(paste("Output_localruns/mnee_",insname2,".txt",sep=""), header=T) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  gather(Month,Model2, Jan:Dec) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="NEE") %>%
  dplyr::select(Year, Month,Site,Variable,Model2) %>%
  spread(Variable,Model2) %>%
  dplyr::select(Year,Month,Site,NEE) %>%
  gather(Variable, Model2, NEE)

# Merge with flux data
Newnee <- merge(nee,NEE, by=c("Year","Month","Site","Variable")) %>%
  merge(.,nee2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="NEE") %>%
  gather(Source, NEE, Model:Model2) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  mutate(Source=factor(Source, levels=c("Tower","Model","Model2"), ordered=T))

# Plot the result
ggplot(data=Newnee, aes(x=Date, y=NEE, color=Source, linetype=Source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab("LAI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept=0) +
  theme_bw()

################################################################################
# Try scatter plots with a 1-1 line
GPPall2 <- GPPall %>% spread(Source, GPP)
ggplot(data=GPPall2, aes(x=Tower, y=Model)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", color="blue") +
  geom_point(aes(x=Tower,y=Model2), color="green") +
  geom_smooth(aes(x=Tower,y=Model2),method="lm", color="green") +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~Site)



