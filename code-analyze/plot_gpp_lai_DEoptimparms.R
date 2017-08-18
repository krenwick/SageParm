################################################################################
# Examine output from hyalite optimization runs
# NOTE: make sure model on git is MASTER and compiled with old phenology!
# New phenology model is in LPJfiles
################################################################################
library(DEoptim)
library(tidyverse); theme_set(theme_bw(base_size=10))
library(data.table)
library(zoo)
library(grid)
library(gridExtra)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm")
outname1 <- "ml_Mod1_disturb"
outname2 <- "ml_NewPhen_disturb"
object1 <- "HyaliteOutput/DE1parimage_ml_summergrass.RData"
object2 <- "HyaliteOutput/NewPhenImage_ml_summergrass.RData"

#-------------------------------------------------------------------------------
# Journal Specifications for figure size
# Agricultural and Forest Meteorology
col1 <- 90 # 1 column width, in mm
col1.5 <- 140
col2 <- 190 # 2 column width, in mm
#--------------------------------------

# Get object from DEparoptim_summergreen1:

load(object1)
summary(DE1)
DE1$optim
pars1 <- c(outname1,DE1$optim$bestmem)
FF <- as.matrix(t(pars1))
write.table(FF, file = "figures/Optim1Parms2.csv", sep = ",", 
            col.names = F, row.names = F, append=T)


########################
# Extract parameter values and plot results
DE1$optim$bestmem[1]

# Make new ins file with these parameter values:
insfile <- "optim_hyalite_bundle/summergreen_optim1_LMpar.ins" # name of ins file to use
# gsub: replaces all occurences of a pattern
ins  <- readLines(insfile)
tx  <- gsub(pattern = "slaval", replace = DE1$optim$bestmem[1], x = ins)
tx  <- gsub(pattern = "k_latosaval", replace = DE1$optim$bestmem[2], x = tx)
tx  <- gsub(pattern = "ltor_maxval", replace = DE1$optim$bestmem[3], x = tx)
tx  <- gsub(pattern = "rootdistval", replace = DE1$optim$bestmem[4], x = tx)
tx  <- gsub(pattern = "rootdist2", replace = (1-DE1$optim$bestmem[4]), x = tx)
tx  <- gsub(pattern = "pstemp_minval", replace = DE1$optim$bestmem[5], x = tx)
#tx  <- gsub(pattern = "est_maxval", replace = DE1$optim$bestmem[5], x = tx)
tx  <- gsub(pattern = "pstemp_lowval", replace = DE1$optim$bestmem[6], x = tx)
tx  <- gsub(pattern = "pstemp_maxval", replace = DE1$optim$bestmem[7], x = tx)
#tx  <- gsub(pattern = "pstemp_hival", replace = DE1$optim$bestmem[8], x = tx)
tx  <- gsub(pattern = "phengdd5rampval", replace = DE1$optim$bestmem[8], x = tx)
tx  <- gsub(pattern = "npatch 1", replace = "npatch 100", x = tx)
tx  <- gsub(pattern = "ifdisturb 0", replace = "ifdisturb 1", x = tx)
insname <- "summergreen_selectedparms"
tx  <- gsub(pattern = "randomval", replace = outname1, x = tx)
tx  <- gsub(pattern = "\\./", replace = "Output_localruns/", x = tx)
writeLines(tx, con=paste("NewIns/",insname,".ins", sep=""))

# Run model with new ins then compare output to flux data:
system("/Users/poulterlab1/Documents/SageParm/LPJfiles/OriginalModel/./guess /Users/poulterlab1/Documents/SageParm/NewIns/summergreen_selectedparms.ins")

################################################################################
# Get baseline data on GPP, NEE, and LAI
################################################################################
# Read in GPP flux data
GPP <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="GPP")

# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
  dplyr::mutate(Latitude=round(Latitude,2)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  dplyr::rename(Year=year) %>%
  dplyr::mutate(Month=month.abb[month]) %>%
  tidyr::gather(Variable, Tower, LAI:GPP) %>%
  dplyr::filter(Variable=="LAI") %>%
  dplyr::select(Year,Month,Variable,Site,Tower)  

# Merge flux with MODIS LAI
df4 <- rbind.data.frame(mod,GPP)

# Read in monthly NEE data
NEE <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="NEE")

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

################################################################################
# Get model object from optim2 model run (New Phen)
# Run new phenology LPJ-GUESS with optimized parameters (new mod location!)
# Get output from model runs
################################################################################
load(object2)
summary(DE1)
DE1$optim
DE1$member
pars2 <- c(outname2,DE1$optim$bestmem)
FF2 <- as.matrix(t(pars2))
write.table(FF2, file = "figures/NewPhenParms2.csv", sep = ",", 
            col.names = F, row.names = F, append=T)
########################
# Make new ins file with these parameter values:
insfile2 <- "optim_hyalite_bundle/optim2_newphen.ins" # name of ins file to use
# gsub: replaces all occurences of a pattern
ins2  <- readLines(insfile2)
tx  <- gsub(pattern = "slaval", replace = DE1$optim$bestmem[1], x = ins2)
tx  <- gsub(pattern = "k_latosaval", replace = DE1$optim$bestmem[2], x = tx)
tx  <- gsub(pattern = "ltor_maxval", replace = DE1$optim$bestmem[3], x = tx)
tx  <- gsub(pattern = "rootdistval", replace = DE1$optim$bestmem[4], x = tx)
tx  <- gsub(pattern = "rootdist2", replace = (1-DE1$optim$bestmem[4]), x = tx)
tx  <- gsub(pattern = "phen_winterval", replace = DE1$optim$bestmem[5], x = tx)
tx  <- gsub(pattern = "pstemp_lowval", replace = DE1$optim$bestmem[6], x = tx)
tx  <- gsub(pattern = "pstemp_minval", replace = DE1$optim$bestmem[7], x = tx)
tx  <- gsub(pattern = "apheng", replace = DE1$optim$bestmem[8], x = tx)
tx  <- gsub(pattern = "aphenval", replace = DE1$optim$bestmem[9], x = tx)
#tx  <- gsub(pattern = "pstemp_minval", replace = DE1$optim$bestmem[10], x = tx)
tx  <- gsub(pattern = "phengdd5g", replace = DE1$optim$bestmem[10], x = tx)
tx  <- gsub(pattern = "npatch 1", replace = "npatch 100", x = tx)
tx  <- gsub(pattern = "ifdisturb 0", replace = "ifdisturb 1", x = tx)

insname2 <- "optim2_newphen"
tx  <- gsub(pattern = "randomval", replace = outname2, x = tx)
tx  <- gsub(pattern = "\\./", replace = "Output_localruns/", x = tx)
writeLines(tx, con=paste("NewIns/",insname2,".ins", sep=""))

# Run model with new ins then compare output to flux data:
system("/Users/poulterlab1/Documents/SageParm/LPJfiles/./guess /Users/poulterlab1/Documents/SageParm/NewIns/optim2_newphen.ins")

################################################################################
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

################################################################################
# Merge model output with flux/MODIS data
################################################################################
# Merge with flux data
GPPall <- merge(gpp1,df4, by=c("Year","Month","Site","Variable")) %>%
  merge(.,gpp2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="GPP") %>%
  gather(Source, GPP, c(Tower,Model,Model2)) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  mutate(Source=factor(Source, levels=c("Tower","Model","Model2"), ordered=T))

# Merge with modis data
LAIall <- merge(lai1,df4, by=c("Year","Month","Site","Variable")) %>%
  merge(.,lai2, by=c("Year","Month","Site","Variable")) %>%
  filter(Variable=="LAI") %>%
  gather(Source, LAI, c(Tower,Model,Model2)) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  mutate(Source=factor(Source, levels=c("Tower","Model","Model2"), ordered=T))

################################################################################
# Plot the result
################################################################################
ann_text_gpp <- data.frame(Date = as.Date(c("2014-12-01","2014-12-01","2014-12-01",
                                        "2014-12-01")),
                       GPP= c(.24,.24,.24,.24),
                       Site = c("h08ec","losec","mbsec", "wbsec"),
                       lab=c("a) Post-fire","b) Low","c) Mountain", "d) Wyoming"))
ann_text_lai <- data.frame(Date = as.Date(c("2014-12-01","2014-12-01","2014-12-01",
                                        "2014-12-01")),
                       LAI = c(4,4,4,4),
                       Site = c("h08ec","losec","mbsec", "wbsec"),
                       lab=c("e) Post-fire","f) Low","g) Mountain", "h) Wyoming"))
# Plot GPP:
g1 <- ggplot(data=GPPall, aes(x=Date, y=GPP)) +
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
  geom_text(data = ann_text_gpp,aes(x=Date, y=GPP,label=lab)) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black"),
        legend.position = "top") 
g1

ggsave("figures/GPP_optim_newgrass_GPPannualLAInoweight.pdf", plot=g1,
       width = 200, height = 130, units = 'mm')

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

ggsave("figures/LAI_optim_newgrass_GPPannualLAInoweight.pdf", plot=l1,
       width = 169, height = 140, units = 'mm')

# Combine the GPP and LAI figures into 1
gpp2 <- g1 +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
lai2 <- l1 +
  theme(legend.position = "none")
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(gpp2))
gp2<- ggplot_gtable(ggplot_build(lai2))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
both <- grid.arrange(arrangeGrob(gp1,gp2, ncol=1,heights = unit(c(90,90), "mm")))

ggsave(paste("figures/",outname1,".pdf", sep=""), plot=both,
       width = col2, height = col2, units = 'mm')

################################################################################
# Calculate and Save Statistics
################################################################################
# Calculate R2 for each:
R2gpp1 <- GPPall %>% spread(Source,GPP)
gppR2_1 <- summary(lm(Tower~Model, data=R2gpp1))$adj.r.squared # R2 = .70, newgrass= .82
gppR2_2 <- summary(lm(Tower~Model2, data=R2gpp1))$adj.r.squared # R2 = .72, newgrass=.85

R2lai <- LAIall %>% spread(Source,LAI)
laiR2_1 <- summary(lm(Tower~Model, data=R2lai))$adj.r.squared # R2 = .14, newgrass=.64
laiR2_2 <- summary(lm(Tower~Model2, data=R2lai))$adj.r.squared # R2 = .32, newgrass=.67

# calculate SSR:
SSRgpp <- R2gpp1 %>% mutate(M1=(Model-Tower)^2, M2=(Model2-Tower)^2) %>%
  summarise_at(vars(M1:M2),sum)
SSRlai <- R2lai %>% mutate(M1=(Model-Tower)^2, M2=(Model2-Tower)^2) %>%
  summarise_at(vars(M1:M2),sum)

# Month where hit 20% of max GPP:
spring <- GPPall %>% filter(Year==2015) %>% 
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
  group_by(Site, Source) %>%
  filter(GPP==max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  select(Source,Site, Month) %>%
  spread(Site,Month) 

# Month where GPP drops to 20% of max:
fall <- GPPall %>% filter(Year==2015) %>%
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
diff <- GPPall %>% group_by(Site, Source,Year) %>%
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
row1 <- c(outname1, "original",gppR2_1,laiR2_1,as.numeric(SSRgpp[1]),
        as.numeric(SSRlai[1]), as.numeric(maxGPP[2,2:5]), as.numeric(spring[2,2:5]), 
        as.numeric(fall[2,2:5]),as.numeric(diff[2,2:5]))
trow1 <- as.matrix(t(row1))
write.table(trow1, file = "figures/SumStatsOptim2.csv", sep = ",", 
            col.names = F, row.names = F, append=2)

row2 <- c(outname2, "NewPhen",gppR2_2,laiR2_2,as.numeric(SSRgpp[2]),
          as.numeric(SSRlai[2]), as.numeric(maxGPP[3,2:5]), as.numeric(spring[3,2:5]), 
          as.numeric(fall[3,2:5]),as.numeric(diff[3,2:5]))
trow2 <- as.matrix(t(row2))
write.table(trow2, file = "figures/SumStatsOptim2.csv", sep = ",", 
            col.names = F, row.names = F, append=TRUE)

# Check it worked:
head(read.csv("figures/SumStatsOptim2.csv"),30)



################################################################################
# Look at LAI- annual. Is it sage or grass?
a1 <- fread(paste("Output_localruns/lai_",insname,".txt",sep=""), header=T) %>%
  mutate(Source="Model")
a2 <- fread(paste("Output_localruns/lai_",insname2,".txt",sep=""), header=T) %>%
  mutate(Source="Model2")
a3 <- rbind.data.frame(a1,a2) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
  dplyr::mutate(percSage=ARTR/Total) %>%
  select(Year,Source,Site,percSage) %>%
  spread(Source,percSage)
a3

################################################################################
# Look at FPC. Is grass to sage to bare ground correct?
f1 <- fread(paste("Output_localruns/fpc_",insname,".txt",sep=""), header=T) %>%
  mutate(Source="Model")
f2 <- fread(paste("Output_localruns/fpc_",insname2,".txt",sep=""), header=T) %>%
  mutate(Source="Model2")
f3 <- rbind.data.frame(f1,f2) %>%
  dplyr::mutate(Year=Year+860) %>% 
  filter(Year>=2015) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
  dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) 
f3

# Pull in Pat Clark's canopy cover data:
cover <- read.csv("data/ReynoldsC/veg_data/2016_canopycover_by_frame.csv") %>%
  separate(col=Site, into=c("Site","replicate"),sep=-2) 
cover2 <- select(cover, -firsthits) %>%
  spread(growthform,canopycover) %>%
  mutate(C3=forb+grass) %>%
  select(-forb,-grass,-Frame) %>%
  group_by(Site) %>%
  select(-replicate) %>%
  summarise_each(funs(mean,sd,min,max)) 
cover2
cover2 %>% mutate(percSage=shrub_mean/(shrub_mean+C3_mean))

################################################################################
# # Pull NEE output back in to R:
# nee <- fread(paste("Output_localruns/mnee_",insname,".txt",sep=""), header=T) %>%
#   dplyr::mutate(Year=Year+860) %>% 
#   filter(Year>=2015) %>%
#   gather(Month,Model, Jan:Dec) %>%
#   dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
#   dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
#   dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
#   dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
#   dplyr::mutate(Variable="NEE") %>%
#   dplyr::select(Year, Month,Site,Variable,Model) %>%
#   spread(Variable,Model) %>%
#   dplyr::select(Year,Month,Site,NEE) %>%
#   gather(Variable, Model, NEE)
# 
# nee2 <- fread(paste("Output_localruns/mnee_",insname2,".txt",sep=""), header=T) %>%
#   dplyr::mutate(Year=Year+860) %>% 
#   filter(Year>=2015) %>%
#   gather(Month,Model2, Jan:Dec) %>%
#   dplyr::mutate(Site=ifelse(Lon==-116.7486, "mbsec", "FIX")) %>%
#   dplyr::mutate(Site=ifelse(Lon==-116.7356, "losec", Site)) %>%
#   dplyr::mutate(Site=ifelse(Lon==-116.7132, "wbsec", Site)) %>%
#   dplyr::mutate(Site=ifelse(Lon==-116.7231, "h08ec", Site)) %>%
#   dplyr::mutate(Variable="NEE") %>%
#   dplyr::select(Year, Month,Site,Variable,Model2) %>%
#   spread(Variable,Model2) %>%
#   dplyr::select(Year,Month,Site,NEE) %>%
#   gather(Variable, Model2, NEE)
# 
# # Merge with flux data
# Newnee <- merge(nee,NEE, by=c("Year","Month","Site","Variable")) %>%
#   merge(.,nee2, by=c("Year","Month","Site","Variable")) %>%
#   filter(Variable=="NEE") %>%
#   gather(Source, NEE, Model:Model2) %>%
#   mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
#   mutate(Date=as.Date(D)) %>%
#   mutate(Source=factor(Source, levels=c("Tower","Model","Model2"), ordered=T))
# 
# # Plot the result
# ggplot(data=Newnee, aes(x=Date, y=NEE, color=Source, linetype=Source)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~Site) +
#   xlab("Date") +
#   ylab("LAI") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   geom_hline(yintercept=0) +
#   theme_bw()

################################################################################
# Try scatter plots with a 1-1 line
GPPall2 <- GPPall %>% spread(Source, GPP)
ggplot(data=GPPall2, aes(x=Tower, y=Model)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", color="blue") +
  geom_point(aes(x=Tower,y=Model2), color="green") +
  geom_smooth(aes(x=Tower,y=Model2),method="lm", color="green") +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~Site)

#####################################

