################################################################################
# Makes table showing percent shrub LAI for each model run (Table 4)
# Includes a bunch of other stuff I was playing with
################################################################################
rm(list=ls()) 
library(tidyverse); theme_set(theme_bw(base_size=18))
library(zoo)
library(data.table)
library(grid)
library(gridExtra)
library(xtable)

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

# LAI------------------------------------------
l1 <- LAIall %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  select(-Variable,-Tower) %>%
  spread(Source,LAI)
head(l1)
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
  filter(Year==2015) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(percSage=ARTR/Total) %>%
  select(Year,Source,Site,percSage) %>%
  spread(Source,percSage) %>%
  mutate_if(is.numeric,funs(round(.,2)))
a3

# From email Gerald sent: (2015)
#138h08: Total = 1.70; shrub = 0.43 (shrub is 25%), I get 90,98,77
#Wbsec: Total = 1.13; shrub = 0.45 (shrub is 40%, 69% from Pat), I get 1,1,20
#Losec: Total = 0.85; shrub = 0.38 (shrub is 45%, 69% from Pat), I get 95,97,79
#Mbsec: Total = 1.54; shrub = 0.95 (shrub is 62%, 55% from Pat), I get 90,92,65

################################################################################
# Make a pretty table of shrub percentage
################################################################################
Field <- c(.16,.78,.60,.69)
Source <- c("Field Data","Original Model","Optimal Parameters","New Phenology")
a4 <- cbind(a3,Field) %>%
  select(-Year)
rownames(a4) <- a4$Site
a5 <- a4 %>% select(Field,Deciduous,NewParms,NewPhen) 
a5
a6 <- t(a5)
a7 <- cbind.data.frame(Source,a6) %>%
  select(Source,wbsec,losec,h08ec,mbsec)
names(a7) <- c("Source","WBS","LOS","PFS","MBS")
a7

# Make into a pretty table
t <- xtable(a7)
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      file="figures/perclai.tex")

################################################################################

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
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08", Site)) %>%
  mutate(percSage=ARTR/Total)
f3

# Pull in Pat Clark's canopy cover data:
cover <- read.csv("data/ReynoldsC/veg_data/2016_canopycover_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 
cover2 <- select(cover, -firsthits) %>%
  spread(growthform,canopycover) %>%
  mutate(C3=forb+grass) %>%
  dplyr::select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  dplyr::select(-replicate)
cover2 %>%
  summarise_each(funs(mean,sd,min,max)) %>%
  mutate(percSage=shrub_mean/(shrub_mean+C3_mean))

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

################################################################################
# Calculate and Save Statistics: Summergreen, original parameters
################################################################################
GPPall <-filter(all, Variable=="GPP")
LAIall <- filter(all, Variable=="LAI")
# Calculate R2 for each:
gppR2_1 <- summary(lm(Tower~Summergreen, data=GPPall))$adj.r.squared # R2 = .70, newgrass= .82
gppR2_2 <- summary(lm(Tower~Evergreen, data=GPPall))$adj.r.squared # R2 = .72, newgrass=.85

laiR2_1 <- summary(lm(MODIS~Summergreen, data=LAIall))$adj.r.squared # R2 = .14, newgrass=.64
laiR2_2 <- summary(lm(MODIS~Evergreen, data=LAIall))$adj.r.squared # R2 = .32, newgrass=.67

# calculate SSR:
SSRgpp <- GPPall %>% mutate(M1=(Summergreen-Tower)^2, M2=(Evergreen-Tower)^2) %>%
  na.omit() %>%
  summarise_at(vars(M1:M2),sum)
SSRlai <- LAIall %>% mutate(M1=(Summergreen-MODIS)^2, M2=(Evergreen-MODIS)^2) %>%
  summarise_at(vars(M1:M2),sum)

# Month where hit 20% of max GPP:
spring <- GPPall %>% filter(Year==2015) %>% 
  gather(Source,GPP, Evergreen:Summergreen) %>%
  group_by(Site, Source) %>%
  mutate(percGPP=GPP/max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  filter(Month<=6) %>%
  filter(percGPP>=.2) %>%
  filter(percGPP==min(percGPP)) %>%
  dplyr::select(Source,Site, Month) %>%
  spread(Site,Month) 

# Month of max GPP (2015):
maxGPP <- GPPall %>% filter(Year==2015) %>%
  gather(Source,GPP, Evergreen:Summergreen) %>%
  group_by(Site, Source) %>%
  filter(GPP==max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  select(Source,Site, Month) %>%
  spread(Site,Month) 

# Month where GPP drops to 20% of max:
fall <- GPPall %>% filter(Year==2015) %>%
  gather(Source,GPP, Evergreen:Summergreen) %>%
  group_by(Site, Source) %>%
  mutate(percGPP=GPP/max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  filter(Month>=6) %>%
  filter(percGPP<=.2) %>%
  filter(percGPP==max(percGPP)) %>%
  group_by(Site,Source) %>%
  filter(Month==min(Month)) %>% # tiebreaker
  select(Source,Site, Month) %>%
  spread(Site,Month) 

# Calculate difference between max GPP in 2015 vs 2016
diff <- GPPall %>% 
  gather(Source,GPP, Evergreen:Summergreen) %>%
  group_by(Site, Source,Year) %>%
  filter(Year>2014) %>%
  filter(GPP==max(GPP,na.rm=T)) %>%
  select(Source,Site, GPP) %>%
  spread(Year,GPP) %>%
  mutate(diff=(`2016`-`2015`)/`2015`) %>%
  select(Source, Site, diff) %>%
  spread(Site,diff) 

# Combine all in form where can output to spreadsheet
# NOTE: must change to col.names=T to initialize file!!!!
# THEN: change back to F so don't keep printing them
row1 <- c("Summergreen", "OriginalParm",gppR2_1,laiR2_1,as.numeric(SSRgpp[1]),
          as.numeric(SSRlai[1]), as.numeric(maxGPP[2,2:5]), as.numeric(spring[2,2:5]), 
          as.numeric(fall[2,2:5]),as.numeric(diff[2,2:5]))
trow1 <- as.matrix(t(row1))
write.table(trow1, file = "figures/SumStatsOptim2.csv", sep = ",", 
            col.names = F, row.names = F, append=2)

row2 <- c("Evergreen", "OriginalParms",gppR2_2,laiR2_2,as.numeric(SSRgpp[2]),
          as.numeric(SSRlai[2]), as.numeric(maxGPP[1,2:5]), as.numeric(spring[1,2:5]), 
          as.numeric(fall[1,2:5]),as.numeric(diff[1,2:5]))
trow2 <- as.matrix(t(row2))
write.table(trow2, file = "figures/SumStatsOptim2.csv", sep = ",", 
            col.names = F, row.names = F, append=TRUE)

################################################################################

