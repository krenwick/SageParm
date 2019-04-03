################################################################################
# Calculate R2 and MAE and put in table (Table 2 in manuscript)
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
outnamegpp1 <- "monthg_summergrass1"
outnamegpp2 <- "monthg_summergrass2"
outnamelai1 <- "ml_summergrass1"
outnamelai2 <- "ml_summergrass2"

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

################################################################################
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

# Pull GPP from optimization model runs into R:
gpp1 <- fread(paste("Output_localruns/mgpp_",outname1,".txt",sep=""), header=T) %>%
  gather(Month,Model, Jan:Dec)
gpp2 <- fread(paste("Output_localruns/mgpp_",outname2,".txt",sep=""), header=T) %>%
  gather(Month,Model2, Jan:Dec) 
gpp3 <- fread(paste("Output_localruns/mgpp_",outnamegpp1,".txt",sep=""), header=T) %>%
  gather(Month,GPP1, Jan:Dec)
gpp4 <- fread(paste("Output_localruns/mgpp_",outnamegpp2,".txt",sep=""), header=T) %>%
  gather(Month,GPP2, Jan:Dec)
gpp5 <- fread(paste("Output_localruns/mgpp_",outnamelai1,".txt",sep=""), header=T) %>%
  gather(Month,LAI1, Jan:Dec)
gpp6 <- fread(paste("Output_localruns/mgpp_",outnamelai2,".txt",sep=""), header=T) %>%
  gather(Month,LAI2, Jan:Dec)

# Pull LAI from optimization model runs into R:
lai1 <- fread(paste("Output_localruns/mlai_",outname1,".txt",sep=""), header=T) %>%
  gather(Month,Model, Jan:Dec)
lai2 <- fread(paste("Output_localruns/mlai_",outname2,".txt",sep=""), header=T) %>%
  gather(Month,Model2, Jan:Dec)
lai3 <- fread(paste("Output_localruns/mlai_",outnamegpp1,".txt",sep=""), header=T) %>%
  gather(Month,GPP1, Jan:Dec)
lai4 <- fread(paste("Output_localruns/mlai_",outnamegpp2,".txt",sep=""), header=T) %>%
  gather(Month,GPP2, Jan:Dec)
lai5 <- fread(paste("Output_localruns/mlai_",outnamelai1,".txt",sep=""), header=T) %>%
  gather(Month,LAI1, Jan:Dec)
lai6 <- fread(paste("Output_localruns/mlai_",outnamelai2,".txt",sep=""), header=T) %>%
  gather(Month,LAI2, Jan:Dec)

gpps <- Reduce(function(...) merge(..., all=TRUE), list(gpp1,gpp2,gpp3,gpp4,gpp5,gpp6)) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="GPP") %>%
  dplyr::select(-Lon,-Lat) 

lais <- Reduce(function(...) merge(..., all=TRUE), list(lai1,lai2,lai3,lai4,lai5,lai6)) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2014) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(Variable="LAI") %>%
  dplyr::select(-Lon,-Lat) 

# Merge with flux data
GPPall <- merge(gpps,all, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="GPP") %>%
  select(-MODIS) %>%
  gather(Source, GPP, Model:Tower) %>%
  mutate(Source=factor(Source, levels=c("Tower","Summergreen","Evergreen","Raingreen",
                                        "Model","Model2","GPP1","GPP2",
                                        "LAI1","LAI2"), ordered=T)) %>%
   mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))

# Merge with modis data
LAIall <- merge(lais,all, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="LAI") %>%
  select(-Tower) %>%
  gather(Source, LAI, Model:MODIS) %>%
  mutate(Source=factor(Source, levels=c("MODIS","Summergreen","Evergreen","Raingreen",
                                        "Model","Model2","GPP1","GPP2",
                                        "LAI1","LAI2"), ordered=T)) %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"), ordered=T))

################################################################################
# Calculate summary stats for the 4 runs: R2, MAE, % improvement- by site + overall
################################################################################
# GPP------------------------------------------
g1 <- GPPall %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  select(-Variable) %>%
  spread(Source,GPP)
head(g1)

# ACROSS ALL 4 SITES:
# Get R2:
Model <- c("Original","NewParm","NewPhen","Mod1GPP","Mod2GPP","Mod1LAI","Mod2LAI")
raingreen <- summary(lm(data=g1, Tower~Raingreen))$adj.r.squared #.71
evergreen <- summary(lm(data=g1, Tower~Evergreen))$adj.r.squared #.69
summergreen <- summary(lm(data=g1, Tower~Summergreen))$adj.r.squared #.66
NewParm <- summary(lm(data=g1, Tower~Model))$adj.r.squared #.72
NewPhen <- summary(lm(data=g1, Tower~Model2))$adj.r.squared #.84
NewParmGPP <- summary(lm(data=g1, Tower~GPP1))$adj.r.squared #.72
NewPhenGPP <- summary(lm(data=g1, Tower~GPP2))$adj.r.squared #.84
NewParmLAI <- summary(lm(data=g1, Tower~LAI1))$adj.r.squared #.72
NewPhenLAI <- summary(lm(data=g1, Tower~LAI2))$adj.r.squared #.84
Value <- c(summergreen,NewParm,NewPhen,NewParmGPP,NewPhenGPP,NewParmLAI,NewPhenLAI)
stats1 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="R2", Variable="GPP")

# Get MAE:
Value <- g1 %>% select(Date,Site,Tower:LAI2) %>%
  mutate_at(vars(Summergreen:LAI2), funs(.-Tower)) %>%
  na.omit() %>%
  mutate_at(vars(Summergreen:LAI2), funs(abs(.))) %>%
  #group_by(Site) %>%
  #summarise_at(vars(Summergreen:LAI2), funs(sum(.))) %>%
  ungroup() %>%
  summarise_at(vars(Summergreen:LAI2), funs(mean(.))) %>%
  select(-Evergreen, -Raingreen)
Value
Value <- as.numeric(Value)
stats2 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="MAE", Variable="GPP")

# examine MAE by site
# VbysiteGPP <- g1 %>% select(Date,Site,Tower:LAI2) %>%
#   mutate_at(vars(Summergreen:LAI2), funs(.-Tower)) %>%
#   na.omit() %>%
#   mutate_at(vars(Summergreen:LAI2), funs(abs(.))) %>%
#   group_by(Site) %>%
#   summarise_at(vars(Summergreen:LAI2), funs(sum(.))) %>%
#   #ungroup() %>%
#   #summarise_at(vars(Summergreen:LAI2), funs(mean(.))) %>%
#   select(-Evergreen, -Raingreen) %>%
#   mutate_at(vars(Summergreen:LAI2), funs(round(.,3)))



# LAI------------------------------------------
l1 <- LAIall %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  select(-Variable) %>%
  spread(Source,LAI)
head(l1)

# ACROSS ALL 4 SITES:
# Get R2:
summergreen <- summary(lm(data=l1, MODIS~Summergreen))$adj.r.squared #.29
mod1 <- summary(lm(data=l1, MODIS~Model))$adj.r.squared #.32
mod2 <- summary(lm(data=l1, MODIS~Model2))$adj.r.squared #.75
NewParmGPP <- summary(lm(data=l1, MODIS~GPP1))$adj.r.squared #.72
NewPhenGPP <- summary(lm(data=l1, MODIS~GPP2))$adj.r.squared #.84
NewParmLAI <- summary(lm(data=l1, MODIS~LAI1))$adj.r.squared #.72
NewPhenLAI <- summary(lm(data=l1, MODIS~LAI2))$adj.r.squared #.84
Value <- c(summergreen,mod1,mod2,NewParmGPP,NewPhenGPP,NewParmLAI,NewPhenLAI)
stats3 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="R2",Variable="LAI")

# Get MAE:
Value <- l1 %>% select(Date,Site,MODIS:LAI2) %>%
  mutate_at(vars(Summergreen:LAI2), funs(.-MODIS)) %>%
  na.omit() %>%
  mutate_at(vars(Summergreen:LAI2), funs(abs(.))) %>%
  #group_by(Site) %>%
  #summarise_at(vars(Summergreen:LAI2), funs(sum(.))) %>%
  ungroup() %>%
  summarise_at(vars(Summergreen:LAI2), funs(mean(.))) %>%
  select(-Raingreen,-Evergreen)
Value <- as.numeric(Value)
stats4 <- cbind.data.frame(Model,Value) %>%
  mutate(Metric="MAE",Variable="LAI")

# ***NOTE: Commented out so can't overwrite manual changes in tex file
# Get MAE by site
# LAIMEAbysite <- l1 %>% select(Date,Site,MODIS:LAI2) %>%
#   mutate_at(vars(Summergreen:LAI2), funs(.-MODIS)) %>%
#   na.omit() %>%
#   mutate_at(vars(Summergreen:LAI2), funs(abs(.))) %>%
#   group_by(Site) %>%
#   summarise_at(vars(Summergreen:LAI2), funs(sum(.))) %>%
#   select(-Raingreen,-Evergreen) %>%
#   mutate_at(vars(Summergreen:LAI2), funs(round(.,2)))


################################################################################
# Combine GPP and LAI stats and format as pretty table
################################################################################
R2 <- rbind.data.frame(stats1,stats3) %>%
  mutate_if(is.numeric, funs(round(.,2))) %>%
  spread(Variable,Value) %>% select(-Metric) 
MAE <- rbind.data.frame(stats2,stats4) %>%
  mutate_if(is.numeric, funs(round(.,3))) %>%
  spread(Variable,Value) %>% select(-Metric) %>%
  rename(GPP2=GPP, LAI2=LAI)

# Original table:
both <- merge(R2,MAE, by="Model") %>%
  filter(Model=="Original"|Model=="NewParm"|Model=="NewPhen") %>%
  mutate(Model=factor(Model, levels=c("Original","NewParm","NewPhen"), ordered=T,
  labels=c("Original","Optimal Parameters","New Phenology"))) %>%
  arrange(Model)
 
#names(both) <- c("Optimization Data","Model","GPP","LAI","GPP","LAI")
# Make both into a pretty table
t <- xtable(both)
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      file="figures/R2MAE.tex")
########################################################################
# Attempt at bigger more complicated table
########################################################################
both2 <- merge(R2,MAE, by="Model") %>%
 mutate(Model=factor(Model, levels=c("Original","NewParm","NewPhen","Mod1GPP",
                                      "Mod2GPP","Mod1LAI","Mod2LAI"), ordered=T,
labels=c("Original","Optimal Parameters","New Phenology",
         "Optimal Parameters2","New Phenology2",
         "Optimal Parameters3","New Phenology3"))) %>%
  arrange(Model)
# **NOTE: not sure what changed, but in 2019 wasn't sorting correctly***********
# **MUST go in to LaTex file and delete numbers in variable names
# **Must also add trailing zeros to some numbers

optim <- c(NA,"GPP and LAI",NA,"Just GPP",NA,"Just LAI",NA)
b3 <- cbind(optim,both2) %>%
  rename(`Optimization Data`=optim) %>%
  mutate(Model=as.character(Model)) # must change from factor to bold
b3
names(b3) <- c("Optimization Data","Model","GPP","LAI","GPP","LAI")
# Make a pretty table
b3[1,2:6] <- paste0("BOLD", b3[1,2:6])
# Gah! lose trailing zero. I'm manually adding in latex file

bold.somerows <- 
  function(x) gsub('BOLD(.*)',paste('\\\\textbf{\\1}'),x)
t2 <- xtable(b3)
print(t2,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      hline.after=c(0,1,3,5),
      sanitize.text.function = bold.somerows,
      file="figures/LatexTables/R2MAE2.tex")

################################################################################
# Make some plots to see if hit peak
################################################################################
g2 <- GPPall %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01")
g3 <- filter(g2, Source=="Tower"|Source=="Model2"|Source=="GPP2"|Source=="LAI2")
# Plot GPP:
g1 <- ggplot(data=g3, aes(x=Date, y=GPP)) +
  #geom_point() +
  geom_line(aes(color=Source, linetype=Source)) +
  # scale_linetype_manual(name="Source",
  #                       breaks=c("Tower","Model","Model2"),
  #                       labels=c("Tower","Optimal Parameters","New Phenology"),
  #                       values=c("solid", "dashed","dashed"))+
  # scale_color_manual(name="Source",
  #                    breaks=c("Tower","Model","Model2"),
  #                    labels=c("Tower","Optimal Parameters","New Phenology"),
  #                    values=c("black","cyan3","deeppink3")) +
  facet_wrap(~Site) +
  scale_x_date(date_breaks = "2 months",date_labels = "%b %y") +
  #geom_text(data = ann_text_gpp,aes(x=Date, y=GPP,label=lab)) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black"),
        legend.position = "top") 
g1


# Plot LAI:
l1 <- ggplot(data=LAIall, aes(x=Date, y=LAI)) +
  #geom_point() +
  geom_line(aes(color=Source, linetype=Source)) +
  scale_linetype_manual(name="Source",
                        breaks=c("Tower","Model","Model2"),
                        labels=c("Tower","Optimal Parameters","New Phenology"),
                        values=c("solid", "dashed","dashed"))+
  scale_color_manual(name="Source",
                     breaks=c("Tower","Model","Model2"),
                     labels=c("Tower","Optimal Parameters","New Phenology"),
                     values=c("black","cyan3","deeppink3")) +
  facet_wrap(~Site) +
  scale_x_date(date_breaks = "2 months",date_labels = "%b %y") +
  geom_text(data = ann_text_lai,aes(x=Date, y=LAI,label=lab)) +
  xlab("Date") +
  ylab("LAI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black"),
        legend.position = "top") 

l1



  