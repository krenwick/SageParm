################################################################################
# Calculate RMSE for GPP, NEE, and Resp for each parameter set
# Do separately for each site (but use all available months)
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(data.table)

#---- Set working directory ----
setwd("~/Documents/SageParm")

# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv")

# Pull in data from the LHC model runs------------------------------------------
mod <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet")
for(var in vars) {
  data=paste("ModOut/LHC/",var,".txt", sep="")
  b <- fread(data, header=T)
  names(b)[16] <- "file"
  # select appropriate years (2014-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,Model, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Site,Variable,Model,file)
  
  mod <- rbind.data.frame(mod,b1)
}

# Get variable names from model to match up with tower data
mod2 <- mod %>%
  spread(Variable,Model) %>%
  rename(NEE=mnee, GPP=mgpp) %>%
  mutate(Rsp=mra+mrh) %>%
  mutate(ET=maet+mevap) %>%
  select(Year,Month,Site,file,GPP,NEE,Rsp,ET) %>%
  gather(Variable, Model, GPP:ET)

# Merge with flux data
b <- merge(mod2,df3, by=c("Year","Month","Site","Variable"))

# Caluclate RMSE for each file, just GPP, separate sites
# Use GPP because no parm affects NEE (RPCC analysis)
# pull out best 5% (top 32 runs) for each site
nruns <- -16 # number runs to select. Negative takes from bottom (min RMSE)
bb2 <- b %>%
  filter(Variable=="GPP") %>%
  mutate(SE=(Tower-Model)^2) %>%
  group_by(file,Site) %>%
  summarise(RMSE=sqrt(mean(SE))) %>%
  mutate(id1=file) %>%
  separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
  dplyr::select(-cut) %>%
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                        "k_chillb","phengdd5","leaflong"),
           sep = "_",extra="drop") %>%
  separate(leaflong, into=c("leaflong","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(Site,parameter) 

b2 <- bb2 %>%
  top_n(n = nruns, wt = RMSE) %>%
  summarise(min=min(value),mean=mean(value),max=max(value), SD=sd(value),
            Q1=quantile(value,probs=.25), Q3=quantile(value,probs=.75)) %>%
  gather(stat, value,min:Q3) %>%
  spread(Site,value)

# Re-do that but don't summarise so can add points to plot
n1 <- bb2 %>%
  top_n(n = nruns, wt = RMSE) %>%
  select(-RMSE,-id)

best2 <- bb2 %>%
  top_n(n = -1, wt = RMSE) %>%
  select(parameter,Site,value) %>%
  spread(Site,value)

# Calculate sames stats but grouping the 4 sites together
bb3 <- b %>%
  filter(Variable=="GPP") %>%
  mutate(SE=(Tower-Model)^2) %>%
  group_by(file) %>%
  summarise(RMSE=sqrt(mean(SE))) %>%
  mutate(id1=file) %>%
  separate(id1, into=c("id","cut"), sep=27, extra="drop") %>%
  dplyr::select(-cut) %>%
  separate(file, into=c("sla","latosa","gmin","ltor_max","greff_min",
                        "root_up","turnover_sap","pstemp_min",
                        "pstemp_lo","est_max","pstemp_max", "pstemp_hi",
                        "k_chillb","phengdd5","leaflong"),
           sep = "_",extra="drop") %>%
  separate(leaflong, into=c("leaflong","del"), sep=-5) %>%
  dplyr::select(-del) %>%
  gather(parameter,value, sla:pstemp_hi) %>%
  separate(value, into=c("name","value"), sep=5, extra="drop") %>%
  dplyr::select(-name) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(parameter) 
b3 <- bb3 %>%
  top_n(n = nruns, wt = RMSE) %>%
  summarise(min=min(value),mean=mean(value),max=max(value),SD=sd(value),
            Q1=quantile(value,probs=.25), Q3=quantile(value,probs=.75)) %>%
  gather(stat, All,min:Q3)

# Re-do that but don't summarise so can add points to plot
n2 <- bb3 %>%
  top_n(n = nruns, wt = RMSE) %>%
  mutate(Site="All") %>%
  select(Site,parameter,value)

best3 <- bb3 %>%
  top_n(n = -1, wt = RMSE) %>%
  select(parameter,value) %>%
  rename(All=value)

# Order parameters from most to least important based on mean RPCC for GPP for 4 sites
GPPorder <- c("sla","root_up","ltor_max","pstemp_min","est_max","turnover_sap",
              "latosa","pstemp_hi","pstemp_max","gmin","greff_min","pstemp_lo")
################################################################################
# Table is ugly and complicated. Try figure, panel for each parm.
b4 <- merge(b2,b3,by=c("parameter","stat")) %>%
  gather(Site,Value,h08ec:All) %>%
  spread(stat,Value)
b4$Site <- factor(b4$Site, levels=c("wbsec","losec","h08ec","mbsec","All"))
head(b4)

best4 <- merge(best2,best3,by=c("parameter")) %>%
  gather(Site,Value,h08ec:All)

# Make data frame with min, max, and original values
sla <- c(6, 21,30) # SLA, full range from lit
latosa <- c(1350, 5220, 6000) # k_latosa, min/max from Ganskopp 1986
gmin <- c(.35, .65, .5) # gmin, standard +/- 30%
ltor_max <- c(.5, 1, 1) # ltor_max, exact range in global pft vals
greff_min <- c(.056, .104, .08) # greff_min, standard +/- 30%
root_up <- c(0.6, 1, .6) # root_upper
turnover_sap <- c(.01, .2, .1) # turnover_sap, took values from Ben's paper
pstemp_min <- c(-5.2, -2.8, -4) # pstemp_min, boreal +/- 30%
pstemp_lo <- c(7, 13, 10) # pstemp_low, boreal +/- 30%
est_max <- c(.05, .2, .2) # est_max, exact range in global vals
pstemp_max <- c(26.6, 49.4, 38) #pstemp_max, bor/temp +/-30%
pstemp_hi <- c(17.5, 32.5, 25) #pstemp_hi, bor/temp +/-30%
test <- cbind.data.frame(sla,latosa,gmin,ltor_max,greff_min,root_up,turnover_sap,
                         pstemp_min,pstemp_lo,est_max,pstemp_max,pstemp_hi)
test2 <- t(test)
test3 <- data.frame(test2) %>%
  rownames_to_column()
names(test3) <- c("parameter","min","max","standard")

gg1 <- ggplot(data=b4, aes(x=Site,y=mean)) +
  geom_point() +
  geom_errorbar(width=0,aes(ymin=min,ymax=max)) +
  geom_hline(data=test3,aes(yintercept=min), linetype="dashed") +
  geom_hline(data=test3,aes(yintercept=max), linetype="dashed") +
  geom_hline(data=test3,aes(yintercept=standard), linetype="dotted", 
             color="blue", size=1) +
  geom_point(data=best4,aes(x=Site,y=Value), color="red") +
  coord_flip() +
  facet_wrap(~parameter, scales ="free_x") +
  ylab("Parameter Value") +
  theme(strip.background = element_rect(colour="black", fill="white"))

ggsave("figures/LHC_parm_estimates1.pdf", plot=gg1,
       width = 169, height = 150, units = 'mm')
################################################################################
# Try plotting 16 actual points instead of the range (Show distribution)
################################################################################
n3 <- rbind.data.frame(n1,n2)
n3$Site <- factor(b4$Site, levels=c("wbsec","losec","h08ec","mbsec","All"))

ggplot(data=b4, aes(x=Site,y=mean)) +
  geom_point() +
  geom_point(data=n3, aes(x=Site, y=value), size=.5) +
  geom_jitter(data=n3, aes(x=Site, y=value), size=.5, 
              position = position_jitter(width = 0.1, height = 0)) +
  geom_hline(data=test3,aes(yintercept=min), linetype="dashed") +
  geom_hline(data=test3,aes(yintercept=max), linetype="dashed") +
  geom_hline(data=test3,aes(yintercept=standard), linetype="dotted", 
             color="blue", size=1) +
  geom_point(data=best4,aes(x=Site,y=Value), color="red") +
  coord_flip() +
  facet_wrap(~parameter, scales ="free_x") +
  ylab("Parameter Value") +
  theme(strip.background = element_rect(colour="black", fill="white"))
# THIS LOOKS BAD- CONFUSING.

################################################################################
# Try box or violin plots to show distribution
################################################################################
n3 <- rbind.data.frame(n1,n2)
n3$Site <- factor(n3$Site, levels=c("wbsec","losec","h08ec","mbsec","All"))
porder <- c("sla","root_up","ltor_max","latosa", "turnover_sap","est_max",
            "greff_min","gmin","pstemp_min","pstemp_lo","pstemp_hi","pstemp_max")
n3$parameter <- factor(n3$parameter, levels=porder)
test3$parameter <- factor(test3$parameter, levels=porder)
best4$parameter <- factor(best4$parameter, levels=porder)

gg2 <- ggplot() +
  geom_violin(data=n3, aes(x=Site, y=value),fill=NA) +
  geom_hline(data=test3,aes(yintercept=min), linetype="dashed") +
  geom_hline(data=test3,aes(yintercept=max), linetype="dashed") +
  geom_hline(data=test3,aes(yintercept=standard), linetype="dotted", 
             color="blue", size=1) +
  geom_point(data=best4,aes(x=Site,y=Value), color="red") +
  coord_flip() +
  facet_wrap(~parameter, scales ="free_x") +
  ylab("Parameter Value") +
  theme(strip.background = element_rect(colour="black", fill="white"))
# Distributions are skewed- mean is not informative
# Violin is more helpful than box plot

ggsave("figures/LHC_parm_estimates.pdf", plot=gg2,
       width = 169, height = 150, units = 'mm')

########################################

