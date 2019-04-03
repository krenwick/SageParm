################################################################################
# Look at impact of parameters on LAI and GPP
################################################################################
rm(list=ls()) 
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)
library(grid)
library(gridExtra)
library(xtable)

# Set working directory
setwd("~/Documents/SageParm/ModOut")
outname1 <- "mgl_disturb_summergrass1"
outname2 <- "mgl_disturb_summergrass2"

################################################################################
# Look at total LAI, % grass, % sagebrush
################################################################################
b <- fread("Output_LHC_newphen/lai.txt",header=T)
names(b)[7] <- "file"
b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","ltor_max","root_up","latosa",
                          "pstemp_lo","pstemp_min",
                          "pstemp_max", "aphenmax_grass", "GDD5_sage","phen_winter", 
                          "aphenmax","downramp","downg", "GDD5_grass"),
             sep = "_",extra="drop") %>%
    separate(GDD5_grass, into=c("GDD5_grass","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:GDD5_grass) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name, -id) %>%
    mutate(value=as.numeric(value)) %>%
  mutate(Lat=round(Lat,2)) %>%
  mutate(Site=ifelse(Lat==43.06, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Lat==43.12, "h08ec",Site)) %>%
  mutate(Site=ifelse(Lat==43.14, "losec",Site)) %>%
  mutate(Site=ifelse(Lat==43.17, "wbsec",Site)) %>%
  group_by(Site,parameter, ARTR, C3,Total) %>%
  summarise(Value=mean(value))

################################################################################
# Plot sage LAI as a function of SLA
sla <- filter(b1, parameter=="sla") %>%
  gather(Species, Output, ARTR:Total)
p1 <- ggplot(data=sla, aes(x=Value, y=Output, color=Species, fill=Species)) +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  scale_fill_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  theme(legend.title=element_blank()) +
  xlab("Parameter Value for Shrub SLA") +
  ylab("Modeled LAI") 
p1

# Plot sage LAI as a function of ltor_max
ltor <- filter(b1, parameter=="ltor_max") %>%
  gather(Species, Output, ARTR:Total)
p2 <- ggplot(data=ltor, aes(x=Value, y=Output, color=Species, fill=Species)) +
  #geom_smooth(method="loess") +
  geom_smooth(method="lm") +
  #geom_smooth() +
  scale_color_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  scale_fill_manual(values=c("burlywood4","darkseagreen3","azure4"),
                    labels=c("Shrub","Grass","Total")) +
  theme(legend.title=element_blank()) +
  xlab("Parameter Value for ltor_max") +
  ylab("Modeled LAI")
p2

# Plot sage LAI as a function of root_up
rootup <- filter(b1, parameter=="root_up") %>%
  gather(Species, Output, ARTR:Total)
p3 <- ggplot(data=rootup, aes(x=Value, y=Output, color=Species)) +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  scale_fill_manual(values=c("burlywood4","darkseagreen3","azure4"),
                    labels=c("Shrub","Grass","Total")) +
  theme(legend.title=element_blank()) +
  xlab("Parameter Value for root_up") +
  ylab("Modeled LAI") 
p3

# Plot for aphenmax_sage
pmaxs <- filter(b1, parameter=="aphenmax") %>%
  gather(Species, Output, ARTR:Total)
p4 <- ggplot(data=pmaxs, aes(x=Value, y=Output, color=Species, fill=Species)) +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  scale_fill_manual(values=c("burlywood4","darkseagreen3","azure4"),
                    labels=c("Shrub","Grass","Total")) +
  theme(legend.title=element_blank()) +
  xlab("Parameter Value for aphenmax_shrub") +
  ylab("Modeled LAI") 
p4

# Plot for aphenmax_grass
pmaxg <- filter(b1, parameter=="apheng") %>%
  gather(Species, Output, ARTR:Total)
p5 <- ggplot(data=pmaxg, aes(x=Value, y=Output, color=Species, fill=Species)) +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  scale_fill_manual(values=c("burlywood4","darkseagreen3","azure4"),
                    labels=c("Shrub","Grass","Total")) +
  theme(legend.title=element_blank()) +
  xlab("Parameter Value for aphenmax_grass") +
  ylab("Modeled LAI") 
p5

# Plot for phen_winter
pwint <- filter(b1, parameter=="phen_winter") %>%
  gather(Species, Output, ARTR:Total)
p6 <- ggplot(data=pwint, aes(x=Value, y=Output, color=Species, fill=Species)) +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  scale_fill_manual(values=c("burlywood4","darkseagreen3","azure4"),
                    labels=c("Shrub","Grass","Total")) +
  theme(legend.title=element_blank()) +
  xlab("Parameter Value for phen_winter") +
  ylab("Modeled LAI") 
p6

# Let's make some facets!
fac <- filter(b1, parameter=="phen_winter"|parameter=="aphenmax_grass"|
                  parameter=="sla"|parameter=="GDD5_grass"|parameter=="root_up"|
                parameter=="ltor_max") %>%
  gather(Species, Output, ARTR:Total)
p7 <- ggplot(data=fac, aes(x=Value, y=Output, color=Species, fill=Species)) +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  scale_fill_manual(values=c("burlywood4","darkseagreen3","azure4"),
                    labels=c("Shrub","Grass","Total")) +
  theme(legend.title=element_blank()) +
  xlab("Parameter Value") +
  ylab("Modeled LAI") +
  facet_wrap(~parameter, scales="free_x")
p7

ggsave("../figures/parm_sensitivity_newphen.pdf", plot=p7,
       width = 6.5, height = 4, units = 'in')

################################################################################
# Pull in data from original model 
################################################################################
a <- fread("Output_LHC/lai.txt",header=T)
names(a)[7] <- "file"
a1 <- a %>% mutate(Year=Year+860) %>% 
    filter(Year>=1986) %>%
    mutate(id1=file) %>%
    separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
    dplyr::select(-cut) %>%
    separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                          "root_up","turnover_sap","pstemp_min",
                          "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                          "k_chillb","GDD5_sage"),
             sep = "_",extra="drop") %>%
    separate(GDD5_sage, into=c("GDD5_sage","del"), sep=-5) %>%
    dplyr::select(-del) %>%
    mutate(row=seq(1:nrow(.))) %>%
    gather(parameter,value, sla:GDD5_sage) %>%
    separate(value, into=c("name","value"), sep=5, extra="drop") %>%
    dplyr::select(-name) %>%
    mutate(value=as.numeric(value)) %>%
  mutate(Lat=round(Lat,2)) %>%
  mutate(Site=ifelse(Lat==43.06, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Lat==43.12, "h08ec",Site)) %>%
  mutate(Site=ifelse(Lat==43.14, "losec",Site)) %>%
  mutate(Site=ifelse(Lat==43.17, "wbsec",Site)) %>%
  group_by(Site,parameter, ARTR, C3,Total) %>%
  summarise(Value=mean(value))
################################################################################
# Plot sage LAI as a function of SLA
sla <- filter(a1, parameter=="sla") %>%
  gather(Species, Output, ARTR:Total)
ggplot(data=sla, aes(x=Value, y=Output, color=Species)) +
  geom_smooth() +
  xlab("SLA") +
  ylab("LAI") +
  facet_wrap(~Site)

# Plot sage LAI as a function of ltor_max
ltor <- filter(a1, parameter=="ltor_max") %>%
  gather(Species, Output, ARTR:Total)
ggplot(data=ltor, aes(x=Value, y=Output, color=Species)) +
  #geom_smooth(method="loess") +
  #geom_smooth(method="lm") +
  geom_smooth() +
  xlab("ltor_max") +
  ylab("LAI") +
  facet_wrap(~Site)

# Plot sage LAI as a function of root_up
rootup <- filter(a1, parameter=="root_up") %>%
  gather(Species, Output, ARTR:Total)
ggplot(data=rootup, aes(x=Value, y=Output, color=Species)) +
  geom_smooth(method="lm") +
  xlab("root_up") +
  ylab("LAI") +
  facet_wrap(~Site)

# Let's make some facets!
fac <- filter(a1, parameter=="latosa"|parameter=="pstemp_max"|
                parameter=="sla"|parameter=="GDD5_sage"|parameter=="root_up"|
                parameter=="ltor_max") %>%
  gather(Species, Output, ARTR:Total)
#
o1 <- ggplot(data=fac, aes(x=Value, y=Output, color=Species, fill=Species)) +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("burlywood4","darkseagreen3","azure4"),
                     labels=c("Shrub","Grass","Total")) +
  scale_fill_manual(values=c("burlywood4","darkseagreen3","azure4"),
                    labels=c("Shrub","Grass","Total")) +
  theme(legend.title=element_blank()) +
  xlab("Parameter Value") +
  ylab("Modeled LAI") +
  facet_wrap(~parameter, scales="free_x")
o1
ggsave("../figures/parm_sensitivity_orig.pdf", plot=o1,
       width = 6.5, height = 4, units = 'in')

################################################################################
# Look at NPP
################################################################################
npp <- fread("Output_LHC/anpp.txt",header=T)
names(npp)[7] <- "file"
npp1 <- npp %>% mutate(Year=Year+860) %>% 
  filter(Year>=1986) %>%
  mutate(id1=file) %>%
  separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
  dplyr::select(-cut) %>%
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                        "k_chillb","GDD5_sage"),
           sep = "_",extra="drop") %>%
  separate(GDD5_sage, into=c("GDD5_sage","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  mutate(row=seq(1:nrow(.))) %>%
  gather(parameter,value, sla:GDD5_sage) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name) %>%
  mutate(value=as.numeric(value)) %>%
  mutate(Lat=round(Lat,2)) %>%
  mutate(Site=ifelse(Lat==43.06, "mbsec", "FIX")) %>%
  mutate(Site=ifelse(Lat==43.12, "h08ec",Site)) %>%
  mutate(Site=ifelse(Lat==43.14, "losec",Site)) %>%
  mutate(Site=ifelse(Lat==43.17, "wbsec",Site)) %>%
  group_by(Site,parameter, ARTR, C3,Total) %>%
  summarise(Value=mean(value))

# Plot sage LAI as a function of SLA
sla <- filter(npp1, parameter=="sla") %>%
  gather(Species, Output, ARTR:Total)
ggplot(data=sla, aes(x=Value, y=Output, color=Species)) +
  geom_smooth() +
  xlab("SLA") +
  ylab("NPP") +
  facet_wrap(~Site)

# Plot sage LAI as a function of ltor_max
ltor <- filter(npp1, parameter=="ltor_max") %>%
  gather(Species, Output, ARTR:Total)
ggplot(data=ltor, aes(x=Value, y=Output, color=Species)) +
  #geom_smooth(method="loess") +
  #geom_smooth(method="lm") +
  geom_smooth() +
  xlab("ltor_max") +
  ylab("NPP") +
  facet_wrap(~Site)

# Plot sage LAI as a function of root_up
rootup <- filter(npp1, parameter=="root_up") %>%
  gather(Species, Output, ARTR:Total)
ggplot(data=rootup, aes(x=Value, y=Output, color=Species)) +
  geom_smooth(method="lm") +
  xlab("root_up") +
  ylab("NPP") +
  facet_wrap(~Site)


