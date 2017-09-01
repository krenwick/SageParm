################################################################################
# Plots showing original, new parameters, and new model
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
        #legend.text=element_text(size=10),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme(#legend.direction = 'horizontal', 
    legend.key = element_rect(size = 3),
    legend.key.size = unit(1, 'lines'),
    axis.title.x = element_blank())

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
       #legend.text=element_text(size=10),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme(#legend.direction = 'horizontal', 
    legend.key = element_rect(size = 3),
    legend.key.size = unit(1, 'lines'),
    axis.title.x = element_blank())

# Combine both plots into 1:
gpp2 <- opt1gpp2 +
  theme(axis.title.x = element_blank())
lai2 <- opt1lai2 +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(gpp2))
gp2<- ggplot_gtable(ggplot_build(lai2))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
both <- grid.arrange(arrangeGrob(gp2,gp1, ncol=1,
                                 heights = unit(c(50,50), "mm")))

ggsave("figures/GPP_LAI_newphen.pdf", plot=both,
       width = col1.5, height = 100, units = 'mm')

################################################################################
# Make an 8-panel figure showing both years and all sites
################################################################################
# Make a plot for GPP
GPP <- GPPall %>% filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  mutate(Source=factor(Source,levels=c("Summergreen","Model","Model2",
                                       "Tower"), ordered=T))
ann_text <- data.frame(Date = as.Date(c("2014-11-01","2014-11-01","2014-11-01",
                                        "2014-11-01")),
                       GPP= c(.24,.24,.24,.24),
                       Site = c("WBS","LOS","PFS", "MBS"),
                       lab=c("a) WBS","b) LOS","c) PFS", "d) MBS"))

flux <- ggplot(data=GPP, aes(x=Date, y=GPP)) +
  geom_line(aes(color=Source,linetype=Source)) +
  scale_linetype_manual(name="Source",
                        breaks=c("Summergreen","Model","Model2","Tower"),
                        labels=c("Original Model","Optimal Parameters","New Phenology","Reference Data"),
                        values=c("dashed", "dashed","dashed","solid")) +
  scale_color_manual(name="Source",
                     breaks=c("Summergreen","Model","Model2","Tower"),
                     labels=c("Original Model","Optimal Parameters","New Phenology","Reference Data"),
                     values=c("darkcyan","deepskyblue","purple","black")) +
  facet_wrap(~Site, ncol=4) +
  #geom_text(data = ann_text,aes(x=Date, y=GPP,label=lab)) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}~month^{-1}))) +
  scale_x_date(date_breaks = "3 months",date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        #panel.border = element_rect(colour = "black"),
        legend.position = "top",
        panel.grid.major = element_line(size=0.2))
flux

# Make a plot for LAI:
LAI <- LAIall %>% filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  mutate(Source=factor(Source,levels=c("Summergreen","Model","Model2",
                                       "MODIS"), ordered=T))
ann_text <- data.frame(Date = as.Date(c("2014-11-01","2014-11-01","2014-11-01",
                                        "2014-11-01")),
                       LAI = c(5,5,5,5),
                       Site = c("WBS","LOS","PFS", "MBS"),
                       lab=c("a) WBS","b) LOS","c) PFS", "d) MBS"))

lai <- ggplot(data=LAI, aes(x=Date, y=LAI)) +
  #geom_point(aes( color=Source)) +
  geom_line(aes( color=Source, linetype=Source)) +
  scale_linetype_manual(name="Source",
                        breaks=c("Summergreen","Model","Model2","MODIS"),
                        labels=c("Original Model","Optimal Parameters","New Phenology","Reference Data"),
                        values=c("dashed", "dashed","dashed","solid"))+
  scale_color_manual(name="Source",
                     breaks=c("Summergreen","Model","Model2","MODIS"),
                     labels=c("Original Model","Optimal Parameters","New Phenology","Reference Data"),
                     values=c("darkcyan","deepskyblue","purple","black")) +
  #geom_text(data = ann_text,aes(x=Date, y=LAI,label=lab)) +
  facet_wrap(~Site, ncol=4) +
  xlab("Month") +
  ylab("Leaf Area Index") +
  scale_x_date(date_breaks = "3 months",date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black"),
        legend.position = "top",
        legend.margin=margin(t=0, r=0, b=-.3, l=0, unit="cm"),
        panel.grid.major = element_line(size=0.2))
lai

################################################################################
# Make pub-level plot including GPP + LAI
################################################################################
gpp2 <- flux +
  theme(legend.position = "none")
lai2 <- lai +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(gpp2))
gp2<- ggplot_gtable(ggplot_build(lai2))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
both <- grid.arrange(arrangeGrob(gp2,gp1, ncol=1,heights = unit(c(60,60), "mm")))

ggsave("figures/GPP_LAI_newmods.pdf", plot=both,
       width = col2, height = 120, units = 'mm')

################################################################################
# Re-do this plot in black and white for print
################################################################################
printsafe <- c('#a6cee3','#1f78b4','#b2df8a','black')
gpp3 <- gpp2 + scale_color_manual(values=printsafe)
lai3 <- lai2 + scale_color_manual(name="Source",
  breaks=c("Summergreen","Model","Model2","MODIS"),
  labels=c("Original Model","Optimal Parameters","New Phenology","Reference Data"),
  values=printsafe)

# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(gpp3))
gp2<- ggplot_gtable(ggplot_build(lai3))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
both <- grid.arrange(arrangeGrob(gp2,gp1, ncol=1,heights = unit(c(60,60), "mm")))

ggsave("figures/GPP_LAI_newmods_printsafe.pdf", plot=both,
       width = col2, height = 120, units = 'mm')
