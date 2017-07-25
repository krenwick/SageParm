################################################################################
# Plot monthly GPP from flux vs. modis
################################################################################
library(tidyverse); theme_set(theme_bw(base_size=10))
library(data.table)
library(zoo)

# SET WORKING DIRECTORY:
setwd("~/Documents/SageParm")

#-------------------------------------------------------------------------------
# Journal Specifications for figure size
# Agricultural and Forest Meteorology
col1 <- 90 # 1 column width = 80 mm
col1.5 <- 140
col2 <- 190 # 2 column width = 169 mm
#--------------------------------------

################################################################################
# Get baseline data on GPP, NEE, and LAI
################################################################################
# Read in GPP flux data
GPP <- read.csv("data/RCflux_15_16.csv") %>%
  dplyr::filter(Variable=="GPP") %>%
  rename(Value=Tower) %>%
  mutate(Source="Tower")

# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
  dplyr::mutate(Latitude=round(Latitude,2)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.06, "mbsec", "FIX")) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.12, "h08ec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.14, "losec",Site)) %>%
  dplyr::mutate(Site=ifelse(Latitude==43.17, "wbsec",Site)) %>%
  dplyr::rename(Year=year) %>%
  dplyr::mutate(Month=month.abb[month]) %>%
  tidyr::gather(Variable, Value, LAI:GPP) %>%
  mutate(Source="MODIS") %>%
  dplyr::select(Year,Month,Variable,Site,Value,Source)  

# Read in GPP data from Bill Smith
bs1 <- read.csv("data/ReynoldsC/BSmith/H08EC_cleaned_daily.csv") %>%
  mutate(Site="h08ec") %>%
  rename(Shrub.GPP..old.=Shrub.GPP..new..1)
bs2 <- read.csv("data/ReynoldsC/BSmith/LOSEC_cleaned_daily.csv") %>%
  mutate(Site="losec") %>%
  rename(Shrub.GPP..old.=Shrub.GPP..new..1)
bs3 <- read.csv("data/ReynoldsC/BSmith/WBSEC_cleaned_daily.csv") %>%
  mutate(Site="wbsec")
bs4 <- read.csv("data/ReynoldsC/BSmith/MBSEC_cleaned_daily.csv") %>%
  mutate(Site="mbsec")
bs <- rbind.data.frame(bs1,bs2,bs3,bs4) %>%
  mutate(Date=as.Date(date)) %>%
  mutate(Month=format(Date, format="%b")) %>%
  group_by(Month,year,Site) %>%
  summarise_at(vars(Grass.GPP..new.:Shrub.GPP..old.), sum) %>%
  mutate(SmithNew=(Shrub.GPP..new.+Grass.GPP..new.)/1000) %>%
  mutate(SmithOld=(Shrub.GPP..old.+Grass.GPP..old.)/1000) %>%
  gather(Source,Value, SmithNew:SmithOld) %>%
  rename(Year=year) %>%
  mutate(Variable="GPP") %>%
  select(Year,Month,Variable,Site,Value,Source)
  

# Merge flux with MODIS 
df4 <- rbind.data.frame(mod,GPP,bs) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Date>="2014-09-01") %>%
  filter(Variable=="GPP") 

# Plot MODIS and Tower GPP
df5 <- filter(df4, Year<2016)
p1 <- ggplot(data=df5, aes(x=Date, y=Value, color=Source)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~Site) +
  xlab("Date") +
  ylab(expression(GPP~(kgC~m^{-2}~month^{-1}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p1

ggsave("figures/GPP_EC_vs_MODIS.pdf", plot=p1,
       width = 180, height = 130, units = 'mm')

ann_text <- data.frame(Date = as.Date(c("2014-11-01","2014-11-01","2014-11-01",
                                        "2014-11-01")),
                       GPP= c(.3,.3,.3,.3),
                       Site = c("h08ec","losec","mbsec", "wbsec"),
                       lab=c("a) Post-fire","b) Low",
                             "c) Mountain", "d) Wyoming"))

# Make clean plot for ppt and paper: first, bw for paper
df5 <- filter(df4, Year<2016, Source!="SmithOld")
p1 <- ggplot(data=df5, aes(x=Date, y=Value)) +
  #geom_point(aes(color=Source)) +
  geom_line(aes(color=Source,linetype=Source)) +
  scale_linetype_manual(name="Source",
                        labels=c("Modis-MERRA2","MODIS-Local","Tower"),
                        breaks=c("MODIS","SmithNew","Tower"),
                        values=c("dashed","dashed","solid"))+
  scale_color_manual(name="Source",
                     labels=c("Modis-MERRA2","MODIS-Local","Tower"),
                     breaks=c("MODIS","SmithNew","Tower"),
                     values=c("darkcyan","deepskyblue","black")) +
  facet_wrap(~Site) +
  geom_text(data = ann_text,aes(x=Date, y=GPP,label=lab)) +
  xlab("Date") +
  scale_x_date(date_breaks = "2 months",date_labels = "%b %y") +
  ylab(expression(GPP~(kgC~m^{-2}~month^{-1}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        #panel.border = element_rect(colour = "black"),
        legend.position = "top")

p1


ggsave("figures/GPP_EC_vs_MODIS.pdf", plot=p1,
       width = col1.5, height = 120, units = 'mm')

# Re-do in black and white for print:

p1 <- ggplot(data=df5, aes(x=Date, y=Value)) +
  #geom_point(aes(color=Source)) +
  geom_line(aes(color=Source,linetype=Source)) +
  scale_linetype_manual(name="Source",
                        labels=c("Modis-MERRA2","MODIS-Local","Tower"),
                        breaks=c("MODIS","SmithNew","Tower"),
                        values=c("dashed","dashed","solid"))+
  scale_color_manual(name="Source",
                     labels=c("Modis-MERRA2","MODIS-Local","Tower"),
                     breaks=c("MODIS","SmithNew","Tower"),
                     values=c("gray73","gray43","black")) +
  facet_wrap(~Site) +
  geom_text(data = ann_text,aes(x=Date, y=GPP,label=lab)) +
  xlab("Date") +
  scale_x_date(date_breaks = "2 months",date_labels = "%b %y") +
  ylab(expression(GPP~(kgC~m^{-2}~month^{-1}))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        #panel.border = element_rect(colour = "black"),
        legend.position = "top")

p1
ggsave("figures/GPP_EC_vs_MODIS_bw.pdf", plot=p1,
       width = col1.5, height = 120, units = 'mm')

################################################################################
# Calculate and Save Statistics
################################################################################
head(df4)
df5 <- filter(df4, Year==2015, Variable=="GPP")
# Calculate R2 for each:
R2gpp1 <- df5 %>% spread(Source,Value)
gppR2_1 <- summary(lm(Tower~MODIS, data=R2gpp1))$adj.r.squared # R2 = .84
gppR2_2 <- summary(lm(Tower~SmithNew, data=R2gpp1))$adj.r.squared # R2 = .82

# calculate SSR:
SSRgpp <- R2gpp1 %>% mutate(M1=(MODIS-Tower)^2, M2=(SmithNew-Tower)^2) %>%
  summarise_at(vars(M1:M2),sum)

# Month where hit 20% of max GPP:
spring <- GPPall %>% filter(Year==2016) %>% 
  group_by(Site, Source) %>%
  mutate(percGPP=GPP/max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  filter(Month<=6) %>%
  filter(percGPP>=.2) %>%
  filter(percGPP==min(percGPP)) %>%
  dplyr::select(Source,Site, Month) %>%
  spread(Site,Month) 

# Month of max GPP (2016):
maxGPP <- GPPall %>% filter(Year==2016) %>%
  group_by(Site, Source) %>%
  filter(GPP==max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  select(Source,Site, Month) %>%
  spread(Site,Month) 

# Month where GPP drops to 20% of max:
fall <- GPPall %>% filter(Year==2016) %>%
  group_by(Site, Source) %>%
  mutate(percGPP=GPP/max(GPP,na.rm=T)) %>%
  mutate(Month=match(Month, month.abb)) %>%
  filter(Month>=6) %>%
  filter(percGPP>=.2) %>%
  filter(percGPP==min(percGPP)) %>%
  group_by(Site,Source) %>%
  filter(Month==max(Month)) %>%
  select(Source,Site, Month) %>%
  spread(Site,Month) 

# Calculate difference between max GPP in 2015 vs 2016
diff <- GPPall %>% group_by(Site, Source,Year) %>%
  filter(Year>2014) %>%
  filter(GPP==max(GPP,na.rm=T)) %>%
  select(Source,Site, GPP) %>%
  spread(Year,GPP) %>%
  mutate(diff=`2016`-`2015`) %>%
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
            col.names = F, row.names = F, append=T)

row2 <- c(outname2, "NewPhen",gppR2_2,laiR2_2,as.numeric(SSRgpp[2]),
          as.numeric(SSRlai[2]), as.numeric(maxGPP[3,2:5]), as.numeric(spring[3,2:5]), 
          as.numeric(fall[3,2:5]),as.numeric(diff[3,2:5]))
trow2 <- as.matrix(t(row2))
write.table(trow2, file = "figures/SumStatsOptim2.csv", sep = ",", 
            col.names = F, row.names = F, append=TRUE)

# Check it worked:
head(read.csv("figures/SumStatsOptim2.csv"),30)



