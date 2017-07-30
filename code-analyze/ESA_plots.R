################################################################################
# Plots for 2017 ESA talk
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=18))
library(zoo)
library(data.table)
library(grid)
library(gridExtra)

# Set working directory
setwd("~/Documents/SageParm")
outname1 <- "mgl_disturb_summergrass1"
outname2 <- "mgl_disturb_summergrass2"

################################################################################
# Plot baseline comparison of phenology (ever and summer)- call deciduous and evergreen
################################################################################
# Read in flux data from 4 RC sites
df3 <- read.csv("data/RCflux_15_16.csv")

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

# Merge flux with MODIS LAI
df4 <- merge(df3,mod,by=c("Year","Month","Variable","Site"), all=T) %>%
  mutate(Date=as.Date(Date)) %>%
  filter(Date>="2014-10-01")
table(df4$Variable)

# Pull in data from the SUMMERGREEN model runs
summer <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai")
for(var in vars) {
  data=paste("RCout/orig_summer_",var,".out", sep="")
  b <- fread(data, header=T)
  # select appropriate years (2014-2016)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,Summergreen, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Site,Variable,Summergreen)
  
  summer <- rbind.data.frame(summer,b1)
}

# Pull in data from the EVERGREEN model runs
ever <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai")
for(var in vars) {
  data=paste("RCout/orig_ever_",var,".out", sep="")
  b <- fread(data, header=T)
  # select appropriate years (2014-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,Evergreen, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Site,Variable,Evergreen)
  
  ever <- rbind.data.frame(ever,b1)
}

# Pull in data from the RAINGREEN model runs
rain <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai")
for(var in vars) {
  data=paste("RCout/orig_rain_",var,".out", sep="")
  b <- fread(data, header=T)
  # select appropriate years (2014-2015)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=2014) %>%
    gather(Month,Raingreen, Jan:Dec) %>%
    mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
    mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
    mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Site,Variable,Raingreen)
  
  rain <- rbind.data.frame(rain,b1)
}

all <- merge(ever,summer, by=c("Year","Month","Variable","Site")) %>%
  merge(.,rain,by=c("Year","Month","Variable","Site")) %>%
  mutate(Variable=replace(Variable,Variable=="mgpp","GPP")) %>%
  mutate(Variable=replace(Variable,Variable=="mnee","NEE")) %>%
  mutate(Variable=replace(Variable,Variable=="mlai","LAI")) %>%
  merge(.,df4, by=c("Year","Month","Variable","Site")) %>%
  select(-Raingreen) %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))

all$D <- as.yearmon(paste(all$Year, all$Month), "%Y %b")
all$Date <- as.Date(all$D)

# Make a plot for GPP
GPP <- filter(all, Variable=="GPP", Site=="mbsec"|Site=="wbsec", Year==2015) %>%
  gather(Source, GPP, Evergreen:Tower) %>%
  mutate(Source=factor(Source,levels=c("MODIS","Tower","Summergreen",
                                       "Evergreen"), ordered=T)) 

flux <- ggplot(data=GPP, aes(x=Date, y=GPP)) +
  geom_line(aes(color=Source,linetype=Source), size=1) +
  scale_linetype_manual(name="Source",
                        breaks=c("Tower","Summergreen","Evergreen"),
                        labels=c("Flux Tower","Deciduous","Evergreen"),
                        values=c("solid","dashed", "dashed")) +
  scale_color_manual(name="Source",
                     breaks=c("Tower","Summergreen","Evergreen"),
                     labels=c("Flux Tower","Deciduous","Evergreen"),
                     values=c("black","purple","darkcyan")) +
  facet_wrap(~Site) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  ylab(expression(GPP~(kgC~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        legend.justification=c(0,1), legend.position=c(0.01,.99),
        legend.title=element_blank(),
        legend.background = element_rect(colour = NA, fill="white"),
        legend.key = element_rect(colour = NA, fill = "white", size=.5),
        legend.text=element_text(size=14),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  guides(color = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3)) +
  theme(#legend.direction = 'horizontal', 
    legend.key = element_rect(size = 3),
    legend.key.size = unit(1, 'lines'))
flux

# Make a plot for LAI:
LAI <- filter(all, Variable=="LAI", Site=="mbsec"|Site=="wbsec", Year==2015) %>%
  gather(Source, LAI, Evergreen:MODIS) %>%
  filter(Source!="Tower") %>%
  mutate(Source=factor(Source,levels=c("MODIS","Summergreen","Evergreen"), ordered=T)) %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))
lai <- ggplot(data=LAI, aes(x=Date, y=LAI)) +
  geom_line(aes( color=Source, linetype=Source), size=1) +
  scale_linetype_manual(name="Source",
                        breaks=c("MODIS","Summergreen","Evergreen"),
                        labels=c("MODIS","Deciduous","Evergreen"),
                        values=c("solid","dashed", "dashed")) +
  scale_color_manual(name="Source",
                     breaks=c("MODIS","Summergreen","Evergreen"),
                     labels=c("MODIS","Deciduous","Evergreen"),
                     values=c("black","purple","darkcyan")) +
  facet_wrap(~Site) +
  xlab("Month") +
  ylab("Leaf Area Index") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black"),
        legend.justification=c(0,1), legend.position=c(0.01,.99),
        legend.title=element_blank(),
        legend.background = element_rect(colour = NA, fill="white"),
        legend.key = element_rect(colour = NA, fill = "white", size=.5),
        legend.text=element_text(size=14),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  guides(color = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3)) +
  theme(#legend.direction = 'horizontal', 
    legend.key = element_rect(size = 3),
    legend.key.size = unit(1, 'lines'))
lai

################################################################################
# Make pub-level plot including GPP + LAI
################################################################################
gpp2 <- flux +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
lai2 <- lai +
  theme(axis.title.x = element_blank())
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(gpp2))
gp2<- ggplot_gtable(ggplot_build(lai2))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
both <- grid.arrange(arrangeGrob(gp1,gp2, ncol=1,
                                 heights = unit(c(2.6,3.0), "in")))

ggsave("figures/ESA_GPP_LAI_origpheno.pdf", plot=both,
       width = 8, height = 5.6, units = 'in')

################################################################################
# Make plot showing just mbsec and evergreen to demonstrate pattern
################################################################################ 

################################################################################
# Make plot showing tower, orig summergreen, and optim summergreen
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

# Merge with flux data
GPPall <- merge(gpp1,all, by=c("Year","Month","Site","Variable")) %>%
  merge(.,gpp2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="GPP") %>%
  gather(Source, GPP, c(Tower,Model,Model2,Evergreen,Summergreen)) %>%
  mutate(Source=factor(Source, levels=c("Tower","Summergreen","Evergreen",
  "Model","Model2"), ordered=T)) %>%
  filter(Source!="Evergreen") %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))
head(GPPall)

# Merge with modis data
LAIall <- merge(lai1,all, by=c("Year","Month","Site","Variable")) %>%
  merge(.,lai2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="LAI") %>%
  gather(Source, LAI, c(MODIS,Model,Model2,Evergreen,Summergreen)) %>%
  mutate(Source=factor(Source, levels=c("MODIS","Summergreen","Evergreen",
                                        "Model","Model2"), ordered=T)) %>%
  filter(Source!="Evergreen") %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))

################################################################################
# Plot GPP:
opt1 <- filter(GPPall, Source!="Model2", Year==2015, Site=="mbsec"|Site=="wbsec")
opt1gpp <- ggplot(data=opt1, aes(x=Date, y=GPP)) +
  geom_line(aes(color=Source,linetype=Source), size=1) +
  scale_linetype_manual(name="Source",
                        breaks=c("Tower","Summergreen","Model"),
                        labels=c("Flux Tower","Original Model","New Parameters"),
                        values=c("solid","dashed", "dashed")) +
  scale_color_manual(name="Source",
                     breaks=c("Tower","Summergreen","Model"),
                     labels=c("Flux Tower","Original Model","New Parameters"),
                     values=c("black","purple","deeppink3")) +
  facet_wrap(~Site) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  ylab(expression(GPP~(kgC~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        legend.justification=c(0,1), legend.position=c(0.01,.99),
        legend.title=element_blank(),
        legend.background = element_rect(colour = NA, fill="white"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.text=element_text(size=14),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme(#legend.direction = 'horizontal', 
    legend.key = element_rect(size = 3),
    legend.key.size = unit(1, 'lines'))
opt1gpp

# Make a plot for LAI:
opt1l <- filter(LAIall, Source!="Model2", Year==2015, Site=="mbsec"|Site=="wbsec")
opt1lai <- ggplot(data=opt1l, aes(x=Date, y=LAI)) +
  geom_line(aes( color=Source, linetype=Source), size=1) +
  scale_linetype_manual(name="Source",
                        breaks=c("MODIS","Summergreen","Model"),
                        labels=c("MODIS","Original Model","New Parameters"),
                        values=c("solid","dashed", "dashed")) +
  scale_color_manual(name="Source",
                     breaks=c("MODIS","Summergreen","Model"),
                     labels=c("MODIS","Original Model","New Parameters"),
                     values=c("black","purple","deeppink3")) +
  facet_wrap(~Site) +
  xlab("Month") +
  ylab("Leaf Area Index") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification=c(0,1), legend.position=c(0.01,.99),
        legend.title=element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.text=element_text(size=14),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme(#legend.direction = 'horizontal', 
    legend.key = element_rect(size = 3),
    legend.key.size = unit(1, 'lines'))


# Combine both plots into 1:
gpp2 <- opt1gpp +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
lai2 <- opt1lai +
  theme(axis.title.x = element_blank())
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(gpp2))
gp2<- ggplot_gtable(ggplot_build(lai2))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
both <- grid.arrange(arrangeGrob(gp1,gp2, ncol=1,
                                 heights = unit(c(2.6,3.0), "in")))

ggsave("figures/ESA_GPP_LAI_newparm.pdf", plot=both,
       width = 8, height = 5.6, units = 'in')

################################################################################
# Make similar figure but including new phenology run
################################################################################
# Plot GPP:
opt12 <- filter(GPPall, Year==2015, Site=="mbsec"|Site=="wbsec")
opt1gpp2 <- ggplot(data=opt12, aes(x=Date, y=GPP)) +
  geom_line(aes(color=Source,linetype=Source), size=1) +
  scale_linetype_manual(name="Source",
                        breaks=c("Tower","Summergreen","Model","Model2"),
                        labels=c("Flux Tower","Original Model","New Parameters","New Phenology"),
                        values=c("solid","dashed", "dashed","dashed")) +
  scale_color_manual(name="Source",
                     breaks=c("Tower","Summergreen","Model","Model2"),
                     labels=c("Flux Tower","Original Model","New Parameters","New Phenology"),
                     values=c("black","purple","deeppink3","cyan3")) +
  facet_wrap(~Site) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  ylab(expression(GPP~(kgC~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        legend.justification=c(0,1), legend.position=c(0.01,.99),
        legend.title=element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.text=element_text(size=14),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme(#legend.direction = 'horizontal', 
    legend.key = element_rect(size = 3),
    legend.key.size = unit(1, 'lines'))
opt1gpp2

# Make a plot for LAI:
opt1l2 <- filter(LAIall, Year==2015, Site=="mbsec"|Site=="wbsec")
opt1lai2 <- ggplot(data=opt1l2, aes(x=Date, y=LAI)) +
  geom_line(aes( color=Source, linetype=Source), size=1) +
  scale_linetype_manual(name="Source",
                        breaks=c("MODIS","Summergreen","Model","Model2"),
                        labels=c("MODIS","Original Model","New Parameters","New Phenology"),
                        values=c("solid","dashed", "dashed","dashed")) +
  scale_color_manual(name="Source",
                     breaks=c("MODIS","Summergreen","Model","Model2"),
                     labels=c("MODIS","Original Model","New Parameters","New Phenology"),
                     values=c("black","purple","deeppink3","cyan3")) +
  facet_wrap(~Site) +
  xlab("Month") +
  ylab("Leaf Area Index") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification=c(0,1), legend.position=c(0.01,.99),
        legend.title=element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.text=element_text(size=14),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme(#legend.direction = 'horizontal', 
        legend.key = element_rect(size = 3),
        legend.key.size = unit(1, 'lines'))
opt1lai2

# Combine both plots into 1:
gpp2 <- opt1gpp2 +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
lai2 <- opt1lai2 +
  theme(axis.title.x = element_blank())
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(gpp2))
gp2<- ggplot_gtable(ggplot_build(lai2))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
both <- grid.arrange(arrangeGrob(gp1,gp2, ncol=1,
                                 heights = unit(c(2.6,3.0), "in")))

ggsave("figures/ESA_GPP_LAI_newphen.pdf", plot=both,
       width = 8, height = 5.6, units = 'in')

################################################################################
# Calculate summary stats for the 4 runs: R2, RMSE, % improvement- by site + overall
################################################################################
# GPP------------------------------------------
g1 <- GPPall %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  select(-Variable,-MODIS) %>%
  spread(Source,GPP)
head(g1)

# ACROSS ALL 4 SITES:
# Get R2:
Model <- c("Original","NewParm","NewPhen")
summergreen <- summary(lm(data=g1, Tower~Summergreen))$adj.r.squared #.66
NewParm <- summary(lm(data=g1, Tower~Model))$adj.r.squared #.72
NewPhen <- summary(lm(data=g1, Tower~Model2))$adj.r.squared #.84
Value <- c(summergreen,NewParm,NewPhen)
stats1 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="R2", Variable="GPP")

# Get RMSE:
Value <- g1 %>% mutate_at(vars(Summergreen:Model2), funs(.-Tower)) %>%
  na.omit() %>%
  #summarise(Tower=mean(Tower))
  summarise_at(vars(Tower:Model2), funs(mean(.^2))) %>%
  mutate_at(vars(Tower:Model2),funs(sqrt(.))) %>% 
  #mutate_at(vars(Summergreen:Model2),funs(./Tower)) %>%
  select(-Tower)
  # .035, .032, .039
  #mutate_at(vars(Model:Model2), funs((.-Summergreen)/Summergreen))
# New Parameters: RMSE went down by 8%
# New Phenology: RMSE is 12% worse (went up by 12%)
# With new phenolgy: better match to pattern (R2), but worse match to magnitude
Value <- as.numeric(Value)
stats2 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="RMSE", Variable="GPP")

# AT INDIVIDUAL SITES:
# WBS:
summary(lm(data=g1[g1$Site=="wbsec",], Tower~Summergreen))$adj.r.squared #.51
summary(lm(data=g1[g1$Site=="wbsec",], Tower~Model))$adj.r.squared #.50
summary(lm(data=g1[g1$Site=="wbsec",], Tower~Model2))$adj.r.squared #.75

# MBS:
summary(lm(data=g1[g1$Site=="mbsec",], Tower~Summergreen))$adj.r.squared #.83
summary(lm(data=g1[g1$Site=="mbsec",], Tower~Model))$adj.r.squared #.84
summary(lm(data=g1[g1$Site=="mbsec",], Tower~Model2))$adj.r.squared #.89

# LOS:
summary(lm(data=g1[g1$Site=="losec",], Tower~Summergreen))$adj.r.squared #.47
summary(lm(data=g1[g1$Site=="losec",], Tower~Model))$adj.r.squared #.60
summary(lm(data=g1[g1$Site=="losec",], Tower~Model2))$adj.r.squared #.82

# PFS:
summary(lm(data=g1[g1$Site=="h08ec",], Tower~Summergreen))$adj.r.squared #.74
summary(lm(data=g1[g1$Site=="h08ec",], Tower~Model))$adj.r.squared #.79
summary(lm(data=g1[g1$Site=="h08ec",], Tower~Model2))$adj.r.squared #.91

#RMSE:
g1 %>% mutate_at(vars(Summergreen:Model2), funs(.-Tower)) %>%
  na.omit() %>%
  group_by(Site) %>%
  summarise_at(vars(Summergreen:Model2), funs(mean(.^2),)) %>%
  mutate_at(vars(Summergreen:Model2),funs(sqrt(.))) %>% 
  mutate_at(vars(Model:Model2), funs((.-Summergreen)/Summergreen))
# WBS: down 2%, down 39%
# MBS: down 4%, up 53%

# LAI------------------------------------------
l1 <- LAIall %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  select(-Variable,-Tower) %>%
  spread(Source,LAI)
head(l1)

# ACROSS ALL 4 SITES:
# Get R2:
summergreen <- summary(lm(data=l1, MODIS~Summergreen))$adj.r.squared #.29
mod1 <- summary(lm(data=l1, MODIS~Model))$adj.r.squared #.32
mod2 <- summary(lm(data=l1, MODIS~Model2))$adj.r.squared #.75
Value <- c(summergreen,mod1,mod2)
stats3 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="R2",Variable="LAI")

# Get RMSE:
Value <- l1 %>% mutate_at(vars(Summergreen:Model2), funs(.-MODIS)) %>%
  na.omit() %>%
  summarise_at(vars(MODIS:Model2), funs(mean(.^2))) %>%
  mutate_at(vars(MODIS:Model2),funs(sqrt(.))) %>% 
  #mutate_at(vars(Summergreen:Model2),funs(./MODIS)) %>%
  select(-MODIS)
 #%>% # 1.49, 1.59, .17
  #mutate_at(vars(Model:Model2), funs((.-Summergreen)/Summergreen))
# New Parameters: RMSE went UP by 7%
# New Phenology: RMSE went DOWN by 89%
# With new phenolgy: better match to pattern AND magnitude!
Value <- as.numeric(Value)
stats4 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="RMSE",Variable="LAI")

# AT INDIVIDUAL SITES:
# WBS:
summary(lm(data=l1[l1$Site=="wbsec",], MODIS~Summergreen))$adj.r.squared #.01
summary(lm(data=l1[l1$Site=="wbsec",], MODIS~Model))$adj.r.squared #.02
summary(lm(data=l1[l1$Site=="wbsec",], MODIS~Model2))$adj.r.squared #.68

# MBS:
summary(lm(data=l1[l1$Site=="mbsec",], MODIS~Summergreen))$adj.r.squared #.46
summary(lm(data=l1[l1$Site=="mbsec",], MODIS~Model))$adj.r.squared #.54
summary(lm(data=l1[l1$Site=="mbsec",], MODIS~Model2))$adj.r.squared #.85

# LOS:
summary(lm(data=l1[l1$Site=="losec",], MODIS~Summergreen))$adj.r.squared #.05
summary(lm(data=l1[l1$Site=="losec",], MODIS~Model))$adj.r.squared #.15
summary(lm(data=l1[l1$Site=="losec",], MODIS~Model2))$adj.r.squared #.69

# PFS:
summary(lm(data=l1[l1$Site=="h08ec",], MODIS~Summergreen))$adj.r.squared #.27
summary(lm(data=l1[l1$Site=="h08ec",], MODIS~Model))$adj.r.squared #.37
summary(lm(data=l1[l1$Site=="h08ec",], MODIS~Model2))$adj.r.squared #.83

#RMSE:
l1 %>% mutate_at(vars(Summergreen:Model2), funs(.-MODIS)) %>%
  na.omit() %>%
  group_by(Site) %>%
  summarise_at(vars(Summergreen:Model2), funs(mean(.^2),)) %>%
  mutate_at(vars(Summergreen:Model2),funs(sqrt(.))) %>% 
  mutate_at(vars(Model:Model2), funs((.-Summergreen)/Summergreen))
# WBS: down 6%, down 85%
# MBS: down 17%, down 91%

################################################################################
# Plot R2 and RMSE for LAI and GPP
################################################################################
alle <- rbind.data.frame(stats1,stats2,stats3,stats4) %>%
  mutate(Model=factor(Model, levels=c("Original","NewParm","NewPhen"),
                      labels=c("Original Model","New Parameters","New Phenology"),
                         ordered=T))

a1 <- filter(alle, Metric=="R2")
r2 <- ggplot(data=a1,aes(x=Model,y=Value, fill=Model)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(Value,2)), vjust=-.2, size=5) +
  scale_fill_manual(name="Source",
                     #breaks=c("MODIS","Summergreen","Model","Model2"),
                     values=c("purple","deeppink3","cyan3")) +
  ylim(c(0,1)) +
  facet_wrap(~Variable, scales="free") +
  ylab(expression(Adjusted ~R^{2})) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="none",
        axis.title.x=element_blank())
ggsave("figures/ESA_R2.pdf", plot=r2,
       width = 5, height = 4, units = 'in')

a2 <- filter(alle, Metric=="RMSE")
Perc <- c("","-8%","+12%","","+7%","-89%")
a3<- cbind.data.frame(a2,Perc)
# Dummy data to make y axes longer:
dummy <- data.frame(Value = c(.041,1.65),
                    Model = "Original Model", 
                    Variable=c("GPP","LAI"),stringsAsFactors=FALSE)
RMSE <- ggplot(data=a2,aes(x=Model,y=Value, fill=Model)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(Value,2)), vjust=-.2, size=5) +
  geom_blank(data=dummy) +
  scale_fill_manual(name="Source",
                    values=c("purple","deeppink3","cyan3")) +
  facet_wrap(~Variable, scales="free") +
  ylab("RMSE") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="none",
        axis.title.x=element_blank())
RMSE
ggsave("figures/ESA_RMSE.pdf", plot=RMSE,
       width = 5, height = 4, units = 'in')

################################################################################
# Look at total LAI, % grass, % sagebrush
################################################################################
# Look at LAI- annual. Is it sage or grass?
a1 <- fread("Output_localruns/lai_mgl_disturb_summergrass1.txt", header=T) %>%
  mutate(Source="NewParms")
a2 <- fread("Output_localruns/lai_mgl_disturb_summergrass2.txt", header=T) %>%
  mutate(Source="NewPhen")
a <- fread("RCout/orig_summer_lai.out") %>%
  mutate(Source="Deciduous")
a3 <- rbind.data.frame(a1,a2,a) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2016) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(percSage=ARTR/Total) %>%
  select(Year,Source,Site,percSage) %>%
  spread(Source,percSage) %>%
  mutate_if(is.numeric,funs(round(.,2)))
a3

# From email Gerald sent:
#138h08: Total = 1.70; shrub = 0.43 (shrub is 25%), I get 90,98,77
#Wbsec: Total = 1.13; shrub = 0.45 (shrub is 40%, 69% from Pat), I get 1,1,20
#Losec: Total = 0.85; shrub = 0.38 (shrub is 45%, 69% from Pat), I get 95,97,79
#Mbsec: Total = 1.54; shrub = 0.95 (shrub is 62%, 55% from Pat), I get 90,92,65

Site <- c("wbs","mbs","wbs","mbs","wbs","mbs","wbs","mbs")
Shrub <- c(40,62,1,90,1,92,20,65)
Source <-c("Field","Field","Original","Original","NewParm","NewParm","NewPhen","NewPhen")
aa<- cbind.data.frame(Site,Source,Shrub) %>%
  mutate(Grass=100-Shrub) %>%
  gather(Species, Percent, Shrub:Grass) %>%
  mutate(Source=factor(Source, levels=c("Field","Original","NewParm","NewPhen",
                                        order=T)))
aa

ggplot(data=aa, aes(x=rev(Source),y=Percent,fill=Species)) +
  geom_bar(stat="identity") +
  facet_wrap(~rev(Site)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        legend.position="top",
        legend.title=element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.text=element_text(size=14),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"))
################################################################################
# Look at FPC. Is grass to sage to bare ground correct?
f1 <- fread("Output_localruns/fpc_mgl_disturb_summergrass1.txt", header=T) %>%
  mutate(Source="NewParms")
f2 <- fread("Output_localruns/fpc_mgl_disturb_summergrass2.txt", header=T) %>%
  mutate(Source="NewPhen")
f <- fread("RCout/orig_summer_fpc.out") %>%
  mutate(Source="Deciduous")
f3 <- rbind.data.frame(f1,f2,f) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>2015) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbs", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "los", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbs", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08", Site)) 
f3

# Pull in Pat Clark's canopy cover data:
cover <- read.csv("data/ReynoldsC/veg_data/2016_canopycover_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 
cover2 <- select(cover, -firsthits) %>%
  spread(growthform,canopycover) %>%
  mutate(C3=forb+grass) %>%
  select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  select(-replicate)
cover2 %>%
  summarise_each(funs(mean,sd,min,max)) 
cover2 %>% mutate(percSage=shrub_mean/(shrub_mean+C3_mean))

# Pull in Pat's LAI data
lai <- read.csv("data/ReynoldsC/veg_data/2016_LAI_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 
table(lai$Site)
table(lai$Site, lai$Frame)
# Drop green hits, spread, and lump forbs + grass
# This kinda doesn't matter since LPJ lumps all PFTs in monthly
lai2 <- select(lai, -greenhits) %>%
  spread(growthform,lai) %>%
  mutate(C3=forb+grass, Total=shrub+C3) %>%
  select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  select(-replicate) %>%
  summarise_each(funs(mean,sd,min,max)) 
lai2

# what percent sagebrush at each site?
lai2 %>% mutate(percSage=shrub_mean/(shrub_mean+C3_mean))
# wbs: .69%, .25 absolute. Grass is 31%
# los: .69%, .30 absolute. Grass is 31%
# mbs: .55%, .67 absolute. Grass is 45%

# get % sagebrush first then average
lai3 <- select(lai, -greenhits) %>%
  spread(growthform,lai) %>%
  mutate(C3=forb+grass, Total=shrub+C3, percC3=C3/Total) %>%
  select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  select(-replicate) %>%
  na.omit() %>%
  summarise_each(funs(mean,sd)) 
lai3
lai3$Total_mean
# Plots to visualize total LAI:
lai3 <- lai %>% group_by(Site,replicate,Frame) %>%
  summarise(lai=sum(lai))
ggplot(data=lai3, aes(x=Site,y=lai)) +
  geom_boxplot(notch=F) 

# group by rep:
lai4 <- lai %>% group_by(Site,replicate,Frame) %>%
  summarise(lai=sum(lai)) %>%
  group_by(Site,replicate) %>%
  summarise(lai=mean(lai))
ggplot(data=lai4,aes(x=Site, y=lai)) +
  geom_point()

# Try to replicate Gerald's numbers:
table(lai$Frame)
table(lai$Site,lai$Frame)
lai %>% group_by(Site,replicate,Frame) %>%
  summarise(lai=sum(lai)) %>%
  ungroup() %>%
  group_by(Site) %>%
  summarise(lai=mean(lai))
lai %>% 
  filter(growthform=="shrub") %>%
  group_by(Site) %>%
  summarise(lai=mean(lai))

# Is it just the first 13 frames?
lai %>% 
  filter(growthform=="shrub", Frame<=13) %>%
  group_by(Site) %>%
  summarise(lai=mean(lai))


