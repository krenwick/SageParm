################################################################################
# Calculate R2 and MAE and put in table
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
# Calculate summary stats for the 4 runs: R2, MAE, % improvement- by site + overall
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
raingreen <- summary(lm(data=g1, Tower~Raingreen))$adj.r.squared #.71
evergreen <- summary(lm(data=g1, Tower~Evergreen))$adj.r.squared #.69
summergreen <- summary(lm(data=g1, Tower~Summergreen))$adj.r.squared #.66
NewParm <- summary(lm(data=g1, Tower~Model))$adj.r.squared #.72
NewPhen <- summary(lm(data=g1, Tower~Model2))$adj.r.squared #.84
Value <- c(summergreen,NewParm,NewPhen)
stats1 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="R2", Variable="GPP")

# Get MAE:
Value <- g1 %>% mutate_at(vars(Summergreen:Model2), funs(.-Tower)) %>%
  na.omit() %>%
  mutate_at(vars(Summergreen:Model2), funs(abs(.))) %>%
  group_by(Site) %>%
  summarise_at(vars(Summergreen:Model2), funs(sum(.))) %>%
  ungroup() %>%
  summarise_at(vars(Summergreen:Model2), funs(mean(.))) %>%
  select(-Evergreen, -Raingreen)
Value
Value <- as.numeric(Value)
stats2 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="MAE", Variable="GPP")

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

# Get MAE:
Value <- l1 %>% mutate_at(vars(Summergreen:Model2), funs(.-MODIS)) %>%
  na.omit() %>%
  mutate_at(vars(Summergreen:Model2), funs(abs(.))) %>%
  group_by(Site) %>%
  summarise_at(vars(Summergreen:Model2),funs(sum(.))) %>% 
  ungroup() %>%
  summarise_at(vars(Summergreen:Model2),funs(mean(.))) %>%
  select(-Raingreen)
Value <- as.numeric(Value)
stats4 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="MAE",Variable="LAI")

################################################################################
# Combine GPP and LAI stats and format as pretty table
################################################################################
R2 <- rbind.data.frame(stats1,stats3) %>%
  mutate_if(is.numeric, funs(round(.,2))) %>%
  mutate(Model=factor(Model, levels=c("Original","NewParm","NewPhen"), ordered=T,
                      labels=c("Original","Optimal Parameters","New Phenology")))
MAE <- rbind.data.frame(stats2,stats4) %>%
  mutate_if(is.numeric, funs(round(.,2))) %>%
  mutate(Model=factor(Model, levels=c("Original","NewParm","NewPhen"), ordered=T,
                      labels=c("Original","Optimal Parameters","New Phenology")))

R2b <- R2 %>% spread(Variable,Value) %>% select(-Metric)
MAEb <- MAE %>% spread(Variable,Value) %>% select(-Metric,-Model)
both <- cbind(R2b,MAEb) 

#########################
# Make both into a pretty table
t <- xtable(both)
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      #digits(tbl) <- c(0,1,1,1,1,1),
      file="figures/R2MAE.tex")
  