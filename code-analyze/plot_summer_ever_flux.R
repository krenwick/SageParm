################################################################################
# Plot standard parameter runs (evergreen, summergreen, raingreen) against flux dat
# This also looks at MODIS and field LAI in comparison to mod runs
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)
library(grid)
library(gridExtra)

# Set working directory
setwd("~/Documents/SageParm")

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
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"),
                     labels=c("WBS","LOS","PFS","MBS"), ordered=T))

all$D <- as.yearmon(paste(all$Year, all$Month), "%Y %b")
all$Date <- as.Date(all$D)

# Make a plot for GPP
GPP <- filter(all, Variable=="GPP") %>%
  dplyr::filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  gather(Source, GPP, Evergreen:Tower)
ann_text <- data.frame(Date = as.Date(c("2014-11-01","2014-11-01","2014-11-01",
                                        "2014-11-01")),
                       GPP= c(.24,.24,.24,.24),
                       Site = c("WBS","LOS","PFS", "MBS"),
                       lab=c("a) WBS","b) LOS","c) PFS", "d) MBS"))
  
flux <- ggplot(data=GPP, aes(x=Date, y=GPP)) +
  #geom_point(aes(color=Source)) +
  geom_line(aes(color=Source,linetype=Source)) +
  scale_linetype_manual(name="Source",
                        breaks=c("Evergreen","Raingreen","Summergreen","Tower"),
                        labels=c("Evergreen","Raingreen","Summergreen","Reference Data"),
                        values=c("dashed", "dashed","dashed","solid"))+
  scale_color_manual(name="Source",
                     breaks=c("Evergreen","Raingreen","Summergreen","Tower"),
                     labels=c("Evergreen","Raingreen","Summergreen","Reference Data"),
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

ggsave("figures/GPP_ever_summer_flux.pdf", plot=flux,
       width = col2, height = col1, units = 'mm')

# Make a plot for LAI:
LAI <- filter(all, Variable=="LAI") %>%
  gather(Source, LAI, Evergreen:MODIS) %>%
  filter(Source!="Tower") %>%
  dplyr::filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  mutate(Source=factor(Source,levels=c("Evergreen","Raingreen","Summergreen",
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
                        breaks=c("Evergreen","Raingreen","Summergreen","MODIS"),
                        labels=c("Evergreen","Raingreen","Summergreen","Reference Data"),
                        values=c("dashed", "dashed","dashed","solid"))+
  scale_color_manual(name="Source",
                     breaks=c("Evergreen","Raingreen","Summergreen","MODIS"),
                     labels=c("Evergreen","Raingreen","Summergreen","Reference Data"),
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

ggsave("figures/LAI_ever_summer_modis.pdf", plot=lai,
       width = col2, height = col2, units = 'mm')

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

ggsave("figures/GPP_LAI_origpheno.pdf", plot=both,
       width = col2, height = 120, units = 'mm')

################################################################################
# Re-do this plot in black and white for print
################################################################################
printsafe <- c('#a6cee3','#1f78b4','#b2df8a','black')
printsafe2 <- c('#1b9e77', '#d95f02', '#7570b3', 'black')
gpp3 <- gpp2 + scale_color_manual(values=printsafe2)
lai3 <- lai2 + scale_color_manual(name="Source",
                breaks=c("Evergreen","Raingreen","Summergreen","MODIS"),
                labels=c("Evergreen","Raingreen","Summergreen","Reference Data"),
                values=printsafe2)

# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(gpp3))
gp2<- ggplot_gtable(ggplot_build(lai3))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
both <- grid.arrange(arrangeGrob(gp2,gp1, ncol=1,heights = unit(c(60,60), "mm")))

ggsave("figures/GPP_LAI_origpheno_printsafe.pdf", plot=both,
       width = col2, height = 120, units = 'mm')

################################################################################
# Calculate RMSE for GPP and LAI for each model run.
# Do it two ways: by site vs. all together
################################################################################
# RMSE for GPP:
GPP2 <- filter(all, Variable=="GPP") %>%
  filter(Date>="2014-10-01") %>%
  gather(Model, GPP, Evergreen:Raingreen) %>%
  select(-MODIS,-D) %>%
  na.omit() %>%
  mutate(SE=(Tower-GPP)^2) %>%
  group_by(Model,Site) %>%
  summarise(RMSE=sqrt(mean(SE))) %>%
  group_by(Site) %>%
  filter(RMSE==min(RMSE))
GPP2 # it's summergreen except evergreen at wbsec

# Same for LAI:
LAI2 <- filter(all, Variable=="LAI") %>%
  filter(Date>="2014-10-01") %>%
  gather(Model, LAI, Evergreen:Raingreen) %>%
  select(-Tower,-D) %>%
  na.omit() %>%
  mutate(SE=(MODIS-LAI)^2) %>%
  group_by(Model,Site) %>%
  summarise(RMSE=sqrt(mean(SE))) %>%
  group_by(Site) %>%
  filter(RMSE==min(RMSE))
LAI2 # same... summergreen except at wbsec

################################################################################
# Calculate phenology metrics based on GPP:
# month of max GPP, month of min, 20%, 80%, slopes?
# actual max and min GPP (magnitude vs. seasonality)
################################################################################
# Month of max GPP:
GPP %>% group_by(Site, Source,Year) %>%
  filter(GPP==max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  select(Source,Site, Month) %>%
  spread(Site,Month) 
# same peak month in 2015 and 2016 (tower)
# summergreen later in 2015 at mbs

# Actual value of max GPP:
GPP %>% group_by(Site, Source,Year) %>%
  filter(GPP==max(GPP,na.rm=T)) %>%
  select(Source,Site, GPP) %>%
  spread(Site,GPP) 
# phenology routine doesn't make much of a difference- controlled by other parameters
# should look at sensitivity of max, not yearly total.

# Month where hit 20% of max GPP:
GPP %>% group_by(Site, Source,Year) %>%
  filter(Year!=2014) %>%
  mutate(percGPP=GPP/max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  filter(Month<=6) %>%
  filter(percGPP>=.2) %>%
  filter(percGPP==min(percGPP)) %>%
  select(Source,Site, Month) %>%
  spread(Site,Month) 
# Same in both years for tower
# Same in both years for summergreen
# Summergreen late at the two low-elev sites

# Month where GPP drops to 20% of max:
GPP %>% group_by(Site, Source,Year) %>%
  filter(Year!=2014) %>%
  mutate(percGPP=GPP/max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  filter(Month>=6) %>%
  filter(percGPP>=.2) %>%
  filter(percGPP==min(percGPP)) %>%
  select(Source,Site, Month,Year) %>%
  spread(Site,Month) 
# changes: earlier in 2016
# summergreen mimics year to year change, but is late both years

# What if I compare quantiles?
m <- GPP %>% group_by(Site, Source,Year) %>%
  filter(Year>=2015) %>%
  mutate(totalGPP=sum(GPP)) %>%
  mutate(P25=totalGPP*.10,
         P50=totalGPP*.5,
         P75=totalGPP*.75) %>%
  arrange(Date) %>%
  mutate(cumsum=cumsum(GPP)) %>%
  mutate(Month=match(Month, month.abb)) 
  
# Hits 25% of annual total:
m %>% filter(cumsum>=P25) %>%
  filter(cumsum==min(cumsum)) %>%
  select(Source,Site, Month) %>%
  spread(Site,Month) 
# summergreen is late

# Hit 50% of annual total:
m %>% filter(cumsum>=P50) %>%
  filter(cumsum==min(cumsum)) %>%
  select(Source,Site, Month) %>%
  spread(Site,Month) 
# in general: cumulative sums don't show pattern b/c tower picks up on
# GPP in winter and models don't.
m %>% filter(Site=="mbsec",Source=="Tower")

# What I really want is a change point...
# I could find the sharpest increase and sharpest decrease in slope...

# Try difference from previous month: get largest change in change
m <- GPP %>% group_by(Site, Source,Year) %>%
  arrange(Date) %>%
  mutate(change=GPP-lag(GPP),
         slopechange=change-lag(change)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  filter(Month<=6) %>%
  filter(slopechange==max(slopechange,na.rm=T)) %>%
  
  select(Source,Site, Month) %>%
  spread(Site,Month) 
m



################################################################
# Pull in LAI data from Pat Clark (USDA ARS)
################################################################
lai <- read.csv("data/ReynoldsC/veg_data/2016_LAI_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 
head(lai)
table(lai$Site)
table(lai$Site,lai$Frame)
dim(lai)
813/3 # 271 frames

# Drop green hits, spread, and lump forbs + grass
# This kinda doesn't matter since LPJ lumps all in monthly
lai2 <- select(lai, -greenhits) %>%
  spread(growthform,lai) %>%
  mutate(C3=forb+grass) %>%
  select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  select(-replicate) %>%
  summarise_each(funs(mean,sd,min,max)) 
lai2

# take a look at the distribution of values for each site
lai3 <- lai %>% group_by(Site,replicate,Frame) %>%
  summarise(lai=sum(lai))
ggplot(data=lai3, aes(x=Site,y=lai)) +
  geom_boxplot(notch=T) 
mean(lai3$lai)
lai3 %>% group_by(Site) %>%
  summarise(mean=mean(lai))

# Sites were sampled may-july 2016. What are the LPJ-GUESS values?
out <- merge(ever,summer, by=c("Year","Month","Variable","Site")) %>%
  mutate(D=as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Variable=="mlai") %>%
  gather(Model,LAI, Evergreen:Summergreen) %>%
  filter(Year==2015)

ggplot(data=out, aes(x=Date,y=LAI,color=Model)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site)

# Is annual LAI an average?
out %>% group_by(Year,Site,Model) %>%
  summarise(LAI=mean(LAI))

out %>% group_by(Year,Site,Model) %>%
  summarise(LAI=max(LAI))

out %>% filter(Month=="Jun") %>%
  group_by(Site) %>%
  summarise(mean=mean(LAI))

# My LAI is obsurdly high! ~5 compared to means ~.2-1.25.
################################################################################
# Look at proportion of grass:shrub at each site
################################################################################
# Look at LAI- annual. Is it sage or grass?
a1 <- fread("RCout/orig_ever_lai.out", header=T) %>%
  mutate(Source="Evergreen")
a2 <- fread("RCout/orig_summer_lai.out", header=T) %>%
  mutate(Source="Summergreen")
a3 <- fread("RCout/orig_rain_lai.out", header=T) %>%
  mutate(Source="Raingreen")
a4 <- rbind.data.frame(a1,a2,a3) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>2015) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(percSage=ARTR/Total) %>%
  select(Year,Source,Site,percSage) %>%
  spread(Source,percSage)
#select(Year,Source,Site,ARTR) %>%
#spread(Source,ARTR)
a4

###############################################
# Make a plot for NEE
NEE <- filter(all, Variable=="NEE") %>%
  filter(Date>="2014-10-01") %>%
  gather(Source, NEE, Evergreen:Tower)

flux2 <- ggplot(data=NEE, aes(x=Date, y=NEE, color=Source, linetype=Source)) +
  #geom_point() +
  geom_line() +
  scale_linetype_manual(name="Source",
                        breaks=c("Evergreen","Raingreen","Summergreen","Tower"),
                        labels=c("Evergreen","Raingreen","Summergreen","Tower"),
                        values=c("dashed","dashed", "dashed","solid"))+
  scale_color_manual(name="Source",
                     breaks=c("Evergreen","Raingreen","Summergreen","Tower"),
                     labels=c("Evergreen","Raingreen","Summergreen","Tower"),
                     values=c("darkcyan","deepskyblue","purple","black")) +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(NEE~(kg~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.minor=element_blank()) +
  geom_hline(yintercept = 0)
flux2

ggsave("figures/NEE_ever_summer_flux.pdf", plot=flux2,
       width = 169, height = 140, units = 'mm')



