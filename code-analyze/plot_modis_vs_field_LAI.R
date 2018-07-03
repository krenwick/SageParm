################################################################################
# Pull in veg data and re-format in long form, make plots to examine
# Output data in format comprable to LPJ-GUESS
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(zoo)
library(data.table)

# Set working directory
setwd("~/Documents/SageParm")

# Pull in LAI data from MODIS
#mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv")

# Merge MODIS lai with field measurements to compare

# To properly compare MODIS, should read in the bi-weekly data:
m1 <- read.csv("data/ReynoldsC/MODIS/mbsage-MCD15A2-005-results.csv") %>%
  select(Date, Latitude,MCD15A2_005_Lai_1km) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  rename(LAI=MCD15A2_005_Lai_1km) %>%
  spread(Latitude, LAI) # must be short form for merge

# OR: new 4-day data?? For 2017
#m1 <- read.csv("data/ReynoldsC/MODIS/RC2017-MCD15A3H-006-results.csv") %>%
 # select(Date, Latitude,MCD15A3H_006_Lai_500m) %>%
 # mutate(Date=as.Date(as.character(Date))) %>%
 # rename(LAI=MCD15A3H_006_Lai_500m) %>%
  #spread(Latitude, LAI) # must be short form for merge


# Get dates to match the sampling dates: (must use quotes to filter by date!)
#m2 <- m1 %>% filter(Date > "2016-05-01") %>%
 # filter(Date=="2016-06-01" |Date=="2016-06-25" | Date=="2016-07-19")
m3 <- m1 %>% filter(Date>="2016-01-01") %>%
  mutate(Source="MODIS") %>%
  rename(mbs=`43.064483`,wbs=`43.167545`,los=`43.1438884`, pfs=`43.120711`) %>%
  gather(Site, LAI, mbs:wbs) %>%
  filter(Date > "2016-01-01" & Date<"2017-09-01") 

# Get Pat's LAI data in the right format:
Site <- c("wbs","los","mbs","pfs")
Date <- c("2016-05-17","2016-06-29","2016-07-20", "2017-06-28")
LAI <- c(.58,.38,1.19,1.67)
field <- cbind.data.frame(Site,Date, LAI) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  mutate(Source="Field")
field

# merge field with MODIS
both <- merge(m3,field,by=c("Date","Source","Site","LAI"),all=T) %>%
  filter(Site!="pfs") %>%
  mutate(Site=ifelse(Site=="wbs", "WBS", Site)) %>%
  mutate(Site=ifelse(Site=="los", "LOS", Site)) %>%
  mutate(Site=ifelse(Site=="mbs", "MBS", Site))
both$Site <- factor(both$Site, levels=c("WBS","LOS","MBS"), ordered=T)

# Plot them both together to verify quality of MODIS:
mod <- ggplot(data=both, aes(x=Date,y=LAI, color=Source)) +
  geom_point() +
  #geom_line() +
  #geom_smooth() +
  facet_wrap(~Site) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        legend.justification=c(0,1), legend.position=c(0.01,.99),
        legend.title=element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        #legend.text=element_text(size=10),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm")) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  xlab("Month (2016)")
mod
# YES! I feel good about using MODIS

ggsave("figures/LAI_field_vs_MODIS.pdf", plot=mod,
       width = 200, height = 110, units = 'mm')

