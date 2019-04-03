################################################################################
# Calculate annual GPP and error for each site x year x model combo
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
# Calculate summary stats for the 4 runs: annual GPP, MAE, % improvement- by site + overall
################################################################################
# GPP------------------------------------------
g1 <- GPPall %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  select(-Variable) %>%
  spread(Source,GPP)
head(g1)

# ACROSS ALL 4 SITES:

# Get annual GPP and MAE:
Value <- g1 %>% select(Date,Site,Tower:LAI2) %>%
  mutate(WY=ifelse((Date>="2014-10-01"&Date<="2015-09-01"),2015,NA)) %>%
  mutate(WY=ifelse((Date>="2015-10-01"&Date<="2016-09-01"),2016,WY)) %>%
  group_by(Site, WY) %>%
  summarize_at(vars(Tower:LAI2), funs(sum(.))) %>%
  #mutate_at(vars(Summergreen:LAI2), funs(.-Tower)) %>%
  #mutate_at(vars(Summergreen:LAI2), funs(round(.,2))) %>%
  select(-Evergreen, -Raingreen)

# Get average annual GPP by site:
Value2 <- Value %>% 
  na.omit() %>%
  group_by(Site) %>%
  summarize_at(vars(Tower:LAI2), funs(mean(.)))
  #summarize_at(vars(Summergreen:LAI2), funs(mean(.))) %>%
  ungroup() %>%
 # bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) 
  bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(abs(.)) else "Total Absolute")))

# Plot this summary
Value3 <- Value2 %>% gather(Source, GPP, Tower:LAI2) %>%
  mutate(Model=ifelse(Source=="Tower","Tower", NA)) %>%
  mutate(Model=ifelse(Source=="GPP1"|Source=="LAI1"|Source=="Model","Optimized", Model)) %>%
  mutate(Model=ifelse(Source=="Summergreen", "Original", Model)) %>%
  mutate(Model=ifelse(Source=="GPP2"|Source=="LAI2"|Source=="Model2","NewPhen",Model)) %>%
  mutate(Data=ifelse(Source=="GPP1"|Source=="GPP2","GPPdata", "None")) %>%
  mutate(Data=ifelse(Source=="LAI1"|Source=="LAI2", "LAIdata", Data)) %>%
  mutate(Data=ifelse(Source=="Model"|Source=="Model2", "Both", Data)) %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"),
                     labels=c("WBS","LOS","PFS","MBS"), ordered=T))
Tower <- Value3 %>% filter(Source=="Tower") %>%
  mutate(D1="GPPdata", D2="LAIdata",D3="Both") %>%
  select(-Data) %>%
  gather(D, Data, D1:D3) %>%
  select(-D)
Value4 <- rbind.data.frame(Value3,Tower) %>%
  filter(Data!="None") %>%
  mutate(Model=factor(Model, levels=c("Tower","Optimized","NewPhen"), ordered=T))

ggplot(data=Value4, aes(x=Site, y=GPP, fill=Model)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~Data) +
  scale_fill_manual(name="Source",
                     breaks=c("Tower","Optimized","NewPhen"),
                     labels=c("Tower","Optimal Parameters","New Phenology"),
                     values=c("black","deepskyblue","purple")) +
  theme(panel.grid.major.x = element_blank())


################################################################################
# Make a pretty table
################################################################################
tdat1 <- Value3 %>% spread(Site,GPP) %>%
  select(-Source) %>%
  mutate(Model=factor(Model, levels=c("Tower","Original","Optimized","NewPhen"), ordered=T,
                      labels=c("Flux Tower","Original Model","Optimal Parameters","New Phenology"))) %>%
  mutate(Data=factor(Data, levels=c("None","Both","GPPdata","LAIdata"), ordered=T)) %>%
  arrange(Model) %>%
  arrange(Data) %>%
  mutate_if(is.numeric, funs(round(.,2))) %>%
  select(-Data)

# Make into a pretty table
t <- xtable(tdat1)
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      file="figures/LatexTables/AnnualGPP.tex")

7########################################################################
# Attempt at bigger more complicated table
########################################################################
optim <- c(NA,NA,"GPP and LAI",NA,"Just GPP",NA,"Just LAI",NA)
tdat2 <- cbind(optim,tdat1) %>%
  rename(`Optimization Data`=optim) %>%
  mutate(Model=as.character(Model)) # must change from factor to bold
tdat2
# Make a pretty table
tdat2[1,2:6] <- paste0("BOLD", tdat2[1,2:6])
# Gah! lose trailing zero. I'm manually adding in latex file

bold.somerows <- 
  function(x) gsub('BOLD(.*)',paste('\\\\textbf{\\1}'),x)
t2 <- xtable(tdat2)
print(t2,
      only.contents=TRUE,
      include.rownames=FALSE,
      type="latex",
      booktabs=T,
      hline.after=c(0,2,4,6),
      sanitize.text.function = bold.somerows,
      file="figures/LatexTables/AnnualGPP2.tex")

################################################################################
# Try table showing the difference rather than absolute- highlight best
################################################################################
Value.b <- g1 %>% select(Date,Site,Tower:LAI2) %>%
  mutate(WY=ifelse((Date>="2014-10-01"&Date<="2015-09-01"),2015,NA)) %>%
  mutate(WY=ifelse((Date>="2015-10-01"&Date<="2016-09-01"),2016,WY)) %>%
  group_by(Site, WY) %>%
  summarize_at(vars(Tower:LAI2), funs(sum(.))) %>%
  mutate_at(vars(Summergreen:LAI2), funs(.-Tower)) %>%
  select(-Evergreen, -Raingreen)

# Get difference in average annual GPP by site:
Value.b2 <- Value.b %>% 
  na.omit() %>%
  group_by(Site) %>%
  summarize_at(vars(Summergreen:LAI2), funs(mean(.))) %>%
  ungroup() %>%
  mutate_at(vars(Summergreen:LAI2), funs(round(.,2))) %>%
  # bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) 
  bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(abs(.)) else "Total Absolute")))
t(Value.b2)
# Interesting- Just LAI/optimal parms is best at 2 wetter sites, since all model results
# are underestimates and these are the highes

# Try averaging across sites
Value.c <- g1 %>% select(Date,Site,Tower:LAI2) %>%
  mutate(WY=ifelse((Date>="2014-10-01"&Date<="2015-09-01"),2015,NA)) %>%
  mutate(WY=ifelse((Date>="2015-10-01"&Date<="2016-09-01"),2016,WY)) %>%
  group_by(Site, WY) %>%
  summarize_at(vars(Tower:LAI2), funs(sum(.))) %>%
  mutate_at(vars(Summergreen:LAI2), funs(.-Tower)) %>%
  na.omit() %>% # get rid of WY15 at losec
  ungroup() %>%
  summarize_at(vars(Summergreen:LAI2), funs(mean(.))) %>%
  select(-Evergreen, -Raingreen) %>%
  mutate_at(vars(Summergreen:LAI2), funs(round(.,2)))
# GPP data, optim parms is the best

# Try doing total error- not average error
Value.d <- g1 %>% select(Date,Site,Tower:LAI2) %>%
  mutate(WY=ifelse((Date>="2014-10-01"&Date<="2015-09-01"),2015,NA)) %>%
  mutate(WY=ifelse((Date>="2015-10-01"&Date<="2016-09-01"),2016,WY)) %>%
  group_by(Site, WY) %>%
  summarize_at(vars(Tower:LAI2), funs(sum(.))) %>%
  mutate_at(vars(Summergreen:LAI2), funs(.-Tower)) %>%
  select(-Evergreen, -Raingreen) %>%
  na.omit() %>%
  ungroup() %>%
  summarize_at(vars(Summergreen:LAI2), funs(sum(abs(.))))
# GPP2 is now the best

# Separate out years- are improvements a coincidence, or do years tell same story?
Value.e <- g1 %>% select(Date,Site,Tower:LAI2) %>%
  mutate(WY=ifelse((Date>="2014-10-01"&Date<="2015-09-01"),2015,NA)) %>%
  mutate(WY=ifelse((Date>="2015-10-01"&Date<="2016-09-01"),2016,WY)) %>%
  group_by(Site, WY) %>%
  summarize_at(vars(Tower:LAI2), funs(sum(.))) %>%
  mutate_at(vars(Summergreen:LAI2), funs(.-Tower)) %>%
  select(-Evergreen, -Raingreen) %>%
  mutate_at(vars(Summergreen:LAI2), funs(round(.,2)))

Value.e2 <- t(Value.e)

################################################################################
# Can I look at percent improvement over original model?
# Don't need this in table form, but it would be good in text
################################################################################

