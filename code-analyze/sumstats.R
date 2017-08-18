################################################################################
# Calculate summary stats for site x model combinations and compare to field dat
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
# Get GPP and LAI output from all model runs + orig data
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
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))

all$D <- as.yearmon(paste(all$Year, all$Month), "%Y %b")
all$Date <- as.Date(all$D)

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
  gather(Source, GPP, c(Tower,Model,Model2,Evergreen,Summergreen, Raingreen)) %>%
  mutate(Source=factor(Source, levels=c("Tower","Summergreen","Evergreen","Raingreen",
                                        "Model","Model2"), ordered=T)) %>%

  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))

# Merge with modis data
LAIall <- merge(lai1,all, by=c("Year","Month","Site","Variable")) %>%
  merge(.,lai2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="LAI") %>%
  gather(Source, LAI, c(MODIS,Model,Model2,Evergreen,Summergreen,Raingreen)) %>%
  mutate(Source=factor(Source, levels=c("MODIS","Summergreen","Evergreen","Raingreen",
                                        "Model","Model2"), ordered=T)) %>%
  filter(Source!="Evergreen") %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))

################################################################################
# Calculate total water year GPP for each year x site x model
################################################################################
head(GPPall)
GPPall2 <- GPPall %>% mutate(WY=ifelse(Date>="2014-10-01"&Date<"2015-10-01",2015,0)) %>%
  mutate(WY=ifelse(Date>="2015-10-01"&Date<"2016-10-01",2016,WY)) %>%
  filter(WY>0) %>%
  group_by(Site,Source,WY) %>%
  summarize(TotalGPP=sum(GPP))

# What is order of sites by GPP in 2015 vs. 2016?
GPPall2 %>% filter(Source=="Tower") %>%
  arrange(desc(TotalGPP))
# ANSWER: YES. mbsec> h08ec > losec > wbsec

# Do models get the order of sites correct?
GPPall2 %>% filter(Source=="Summergreen") %>%
  arrange(desc(TotalGPP)) # mbs > los > h08 > wbs, same both yrs

GPPall2 %>% filter(Source=="Evergreen") %>%
  arrange(desc(TotalGPP)) # h08 > mbs >los > wbs, same both yrs

GPPall2 %>% filter(Source=="Raingreen") %>%
  arrange(desc(TotalGPP)) # h08 > mbs >los > wbs, same both yrs

GPPall2 %>% filter(Source=="Model") %>%
  arrange(desc(TotalGPP)) # h08 > mbs >los > wbs, same both yrs

GPPall2 %>% filter(Source=="Model2") %>%
  arrange(desc(TotalGPP)) # mbs > h08 >los > wbs, same both yrs

# Woohoo! NewPhen is the only model that gets site order correct
################################################################################
# Do all sites move in same direction 2015-2016?
GPPall2 %>% filter(Source=="Tower") %>%
  spread(WY, TotalGPP) %>%
  mutate(change=`2016`-`2015`)
# NO: down: h08 & mbs, up: wyo 
GPPall2 %>% filter(Source=="Evergreen") %>%
  spread(WY, TotalGPP) %>%
  mutate(change=`2016`-`2015`) # Down: wbs, h08, mbs; up: los # 2 right
GPPall2 %>% filter(Source=="Raingreen") %>%
  spread(WY, TotalGPP) %>%
  mutate(change=`2016`-`2015`) # all down # 2 right
GPPall2 %>% filter(Source=="Summergreen") %>%
  spread(WY, TotalGPP) %>%
  mutate(change=`2016`-`2015`) # down: wbs & h08; up: los & mbs # 1 right (h08)
GPPall2 %>% filter(Source=="Model") %>%
  spread(WY, TotalGPP) %>%
  mutate(change=`2016`-`2015`) # down: wbs & los; up: h08 & mbs # nada
GPPall2 %>% filter(Source=="Model2") %>%
  spread(WY, TotalGPP) %>%
  mutate(change=`2016`-`2015`) # down: wbs & los; up: h08 & mbs # nada

# Conclusion: all models show wyo go down, all have errors
# Probably not a good metric- diffs are really small, and reality + model both stochastic

################################################################################
# Look at total GPP for WY2016: at each site? combined?
################################################################################
GPPall2 %>%
  #filter(WY=="2016") %>%
  group_by(Site,Source) %>%
  summarise(TotalGPP=sum(TotalGPP)) %>%
  spread(Source, TotalGPP) %>%
  mutate_at(vars(Summergreen:Model2), funs(.-Tower))
# NewPhen is consistently too low 
# Summergreen is probably the best overall
################################################################################
#Look at change in value of max LAI across sites and years- is pattern correct?
################################################################################
LAIall2 %>% filter(Source=="MODIS") %>%
  group_by(Site,Source,Year) %>%
  filter(LAI==max(LAI,na.rm=T)) %>%
