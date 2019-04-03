################################################################################
# Re-do supplemental fig 1 showing MODIS vs field LAI
# Include colors for quality flag
# add line showing monthly values used in analysis
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=12))
library(zoo)
library(data.table)

# Set working directory
setwd("~/Documents/SageParm")

# Pull in LAI data from MODIS
mod <- read.csv("data/ReynoldsC/MODIS/lai_gpp.csv") %>%
  mutate(Latitude=round(Latitude,2)) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  mutate(Site=ifelse(Latitude==43.06, "MBS", "FIX")) %>%
  mutate(Site=ifelse(Latitude==43.12, "PFS",Site)) %>%
  mutate(Site=ifelse(Latitude==43.14, "LOS",Site)) %>%
  mutate(Site=ifelse(Latitude==43.17, "WBS",Site)) %>%
  mutate(Site=factor(Site, levels=c("WBS","LOS","PFS","MBS"), ordered=T)) %>%
  mutate(C="black")

# Pull in raw bi-weekly data:
m1 <- read.csv("data/ReynoldsC/MODIS/mbsage-MCD15A2-005-results.csv") %>%
  select(Date, Latitude,MCD15A2_005_Lai_1km,MCD15A2_005_FparLai_QC_MODLAND_Description) %>%
  mutate(Date=as.Date(as.character(Date))) %>%
  filter(Date>="2014-10-01"&Date<="2016-09-01") %>%
  rename(LAI=MCD15A2_005_Lai_1km,QC=MCD15A2_005_FparLai_QC_MODLAND_Description) %>%
  mutate(Latitude=round(Latitude,2)) %>%
  mutate(Site=ifelse(Latitude==43.06, "MBS", "FIX")) %>%
  mutate(Site=ifelse(Latitude==43.12, "PFS",Site)) %>%
  mutate(Site=ifelse(Latitude==43.14, "LOS",Site)) %>%
  mutate(Site=ifelse(Latitude==43.17, "WBS",Site)) %>%
  mutate(Quality=ifelse(QC=="Good quality (main algorithm with or without saturation)",
                        "Good","Other")) %>%
  mutate(Site=factor(Site, levels=c("WBS","LOS","PFS","MBS"), ordered=T))

# Merge raw with monthly
b <- merge(mod, m1, by=c("Date", "Latitude"), all=T)

# Plot, color-coding bad data
# Add line showing monthly values
QC <- ggplot(data=m1, aes(x=Date, y=LAI)) +
  geom_point(aes(color=Quality)) +
  scale_color_manual(name="Data Quality",
                   breaks=c("Good","Other"),
                   values=c("darkcyan","darkgoldenrod1")) +
  geom_point(data=mod, color="black") +
  geom_line(data=mod, color="black") +
  facet_wrap(~Site) +
  scale_x_date(date_breaks = "1 month",date_labels = "%y-%b") +
  ylab("MODIS LAI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        panel.grid.minor=element_blank(),
        legend.justification=c(0,1), legend.position=c(0.01,.99),
        #legend.title=element_text(),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        #legend.text=element_text(size=10),
        legend.margin=margin(t=.2, r=0.05, b=0, l=0.05, unit="cm")) +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme(#legend.direction = 'horizontal', 
    legend.key = element_rect(size = 3),
    legend.key.size = unit(1, 'lines')) +
  theme(plot.margin=unit(c(2,4,2,1),"mm"))


ggsave("figures/MODIS_LAI_QC.pdf", plot=QC,
       width = 240, height = 190, units = 'mm')

