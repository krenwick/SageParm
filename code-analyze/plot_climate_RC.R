################################################################################
# Read in and merge 2015-2016 weather station data
# Make plot
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(rgdal)
library(raster)
library(gridExtra)
library(grid)

#----- SET WORKING DIRECTORY ----- 
setwd("~/Documents/SageParm")
# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Documents/SageParm/data/"
cpath <- "Daymet3_ID/"

############ Get station data and put in propper format
# Get station data for 2014- sent via email by Gerald Flerchinger, ARS
RC176 <- read.csv(paste(dpath, "ReynoldsC/176.wea.30min.2015.csv", sep="")) %>%
  mutate(Source="Station", Site="mbsec")
RC098c <- read.csv(paste(dpath, "ReynoldsC/098c.wea.30min.2015.csv", sep="")) %>%
  mutate(Source="Station", Site="wbsec")
RC127 <- read.csv(paste(dpath, "ReynoldsC/127.wea.30min.2015.csv", sep="")) %>%
  mutate(Source="Station", Site="losec")
RC138j10 <- read.csv(paste(dpath, "ReynoldsC/138j10.wea.30min.2015.csv", sep="")) %>%
  mutate(Source="Station", Site="h08ec")
RCweather <- rbind(RC176,RC098c,RC127,RC138j10)

# Convert 30-min temp to daily max/min
RC2 <- RCweather %>% mutate(date=as.Date(datetimec)) %>%
  group_by(date,Site) %>% # group by day to calculate days with precip
  summarise(dayppt=sum(ppta), daytemp=mean(tmp3), daysol=mean(sol)) %>%
  mutate(yearmon=format(date, format="%y-%m")) %>%
  mutate(year=format(date, format="%y")) %>%
  mutate(month=format(date, format="%m")) %>%
  group_by(year, month,Site) %>%
  summarise(temp=mean(daytemp), ppt=sum(dayppt), meansol=mean(daysol),
            wet=sum(dayppt>0)) %>%
  ungroup() %>%
  #mutate_if(is.character, as.numeric) %>%
  mutate(Month=month.abb[as.numeric(month)]) %>%
  mutate(Year=as.numeric(year)+2000) %>%
  mutate(D = as.yearmon(paste(Year, Month), "%Y %b")) %>%
  mutate(Date=as.Date(D)) %>%
  filter(Year==2015)
head(RC2)
str(RC2)

# Plot temperature
ggplot(data=RC2, aes(x=Date,y=temp, color=Site)) +
  geom_point() +
  geom_line() +
  ylab(expression("Temperature " ( degree*C))) +
  theme_bw(base_size=12) +
  scale_x_date(date_breaks = "1 month", date_labels="%Y %m") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.minor=element_blank()) 

# Plot precipitation
ggplot(data=RC2, aes(x=Date,y=ppt, color=Site)) +
  geom_point() +
  geom_line() +
  ylab("Precipitation (cm)") +
  theme_bw(base_size=12) +
  scale_x_date(date_breaks = "1 month", date_labels="%Y %m") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.minor=element_blank()) 
# this plot is pretty uninformative due to noise. Need to plot normals,
# or just boxplots for normals

################################################################################
# Pull in Daymet temperature and precip data and calculate monthly normals
################################################################################
# coordinates of flux sites:
sites <- read.csv(paste(dpath, "ReynoldsC/EC-CoreSites-coords.csv", sep=""))
ll <- dplyr::select(sites, Lon,Lat)
Site <- dplyr::select(sites, Site)

# Pull in daymet3 data, extract values for points
#Loop through the months and sample the climate data
nmonths <- 12*30
ClimateD3 <- NULL
TS <- seq(as.Date("1980-01-15"), as.Date("2009-12-15"), by="month")

for(m in 1:nmonths){
  print(paste(round((m/nmonths)*100,2),"% finished"))
  #Sample tmean
  tmean.month.m <- raster(paste(cpath, "tmean_1980-2016.nc4", sep=""), 
                          band=(m), varname="tmean")    
  temp <- raster::extract(tmean.month.m, ll)
  ppt.month.m <- raster(paste(cpath, "prcp_1980-2016.nc4", sep=""), 
                        band=(m), varname="prcp")    
  ppt <- raster::extract(ppt.month.m, ll)
  
  # Get dates
  yearmon <- rep(TS[m], 4)
  all <- cbind(yearmon,Site,temp,ppt)
  ClimateD3 <- rbind(ClimateD3,all)
} 
ClimateD3 <- as_tibble(ClimateD3)
ClimateD32 <- ClimateD3 %>%
  mutate(Year=format(yearmon, "%Y")) %>%
  mutate(Month=format(yearmon, "%m")) %>%
  group_by(Month,Site) %>%
  summarise(temp=mean(temp), ppt=mean(ppt)) %>%
  ungroup() %>%
  mutate(Month=as.numeric(Month)) %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"),
    labels=c("Wyoming big sage","Low sage","Post-fire sage","Mountain big sage"),
    ordered=T))

head(ClimateD32)
#colors <- rev(c('#ffffd4','#fed98e','#fe9929','#cc4c02'))
colors <- c('#a6611a','#dfc27d','#80cdc1','#018571')

# Plot temperature
ggplot(data=ClimateD32, aes(x=Month,y=temp, color=Site)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=colors) +
  ylab(expression("Temperature " ( degree*C))) +
  theme_bw(base_size=12) +
  #scale_x_date(date_breaks = "1 month", date_labels="%Y %m") +
  scale_x_continuous(breaks=seq(1:12)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.minor=element_blank()) 

# Plot precipitation
ggplot(data=ClimateD32, aes(x=Month,y=ppt, color=Site)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=colors) +
  ylab("Precipitation (mm)") +
  theme_bw(base_size=12) +
  scale_x_continuous(breaks=seq(1:12)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.minor=element_blank()) 

###########################################
# Try boxplots instead
###########################################
c2 <- ClimateD3 %>%
  mutate(Year=format(yearmon, "%Y")) %>%
  mutate(Month=format(yearmon, "%m")) %>%
  group_by(Year,Site) %>%
  summarise(temp=mean(temp), ppt=mean(ppt)) %>%
  ungroup() %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(Site=factor(Site, levels=c("wbsec","losec","h08ec","mbsec"),
                     #labels=c("Wyoming big sage","Low sage","Post-fire sage","Mountain big sage"),
                     labels=c("Wyoming","Low","Post-fire","Mountain"),
                     ordered=T))
# Temp:
t <- ggplot(data=c2, aes(x=Site,y=temp, color=Site)) +
  geom_boxplot() +
  ylab(expression("Mean Annual Temperature " ( degree*C))) +
  scale_color_manual(values=colors) +
  theme(legend.position="none", legend.title=element_blank(),
        panel.background=element_blank(),plot.background=element_blank(),
        #panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        legend.text.align = 0,
        plot.margin=unit(c(.1,.1,.1,.1), "cm"),
        axis.title.y = element_text(size = rel(1)),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) 
# Pre:
p<- ggplot(data=c2, aes(x=Site,y=ppt, color=Site)) +
  geom_boxplot() +
  scale_color_manual(values=colors) +
  ylab("Mean Annual Precipitation (mm)") +
  theme(legend.position="none", legend.title=element_blank(),
        panel.background=element_blank(),plot.background=element_blank(),
        panel.grid.minor=element_blank(),
        legend.text.align = 0,
        plot.margin=unit(c(.1,.1,.1,.1), "cm"),
        axis.title.y = element_text(size = rel(1))) 

# Combine these into one figure:
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(t))
gp2<- ggplot_gtable(ggplot_build(p))

maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
#quartz() # deal with RStudio plot crash issue that sometimes happens
both <- grid.arrange(gp1,gp2,ncol=1)

#eps doesn't support transparency, use pdf for line with CI.
ggsave("figures/TempPrecip.pdf", plot=both,
       width = 80, height = 120, units = 'mm')


