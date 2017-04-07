################################################################################
# Compare Reynold's Creek weather data to Daymet
# 1. Compare station data from site 176
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=12)) # sized for print
library(rgdal)
library(raster)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Documents/SageParm/data/"
fpath <- "/Users/poulterlab1/Documents/SageParm/figures/"
cpath <- "/Users/poulterlab1/Documents/LPJGUESS/Climate/Daymet/" # climate data

############ Get station data and put in propper format
# Get station data for 2014- sent via email by Gerald Flerchinger, ARS
RC176 <- read.csv(paste(dpath, "ReynoldsC/176.wea.30min.csv", sep=""))

# coordinates of mt big sage site nearby:
ll <- c(43.064483, -116.74862)

# Convert 30-min temp and precip to monthly 
head(RC176)
str(RC176)
RC2 <- RC176 %>% mutate(date=as.Date(datetimec)) %>%
  group_by(date) %>% # group by day to calculate days with precip
  summarise(dayppt=sum(ppta), daytemp=mean(tmp3), daysol=mean(sol)) %>%
  mutate(yearmon=format(date, format="%y-%m")) %>%
  group_by(yearmon) %>%
  summarise(temp=mean(daytemp), ppt=sum(dayppt), meansol=mean(daysol),
            wet=sum(dayppt>0))

head(RC2)
RC2$source <- "station176"
RC3 <- filter(RC2, yearmon<"15-01")

# Pull in daymet data, extract values for points
#Loop through the months and sample the climate data
nmonths <- 12
climate <- matrix(nrow=nmonths,ncol=6)

for(m in 1:nmonths){
  print(paste(round((m/nmonths)*100,2),"% finished"))
  #Sample tmean
  tmean.month.m <- raster(paste(cpath, "tmean_wusa.nc", sep=""), 
                          band=(m+408), varname="tmean")    
  climate[m,2] <- raster::extract(tmean.month.m, ll)[1]
  ppt.month.m <- raster(paste(cpath, "prcp_wusa.nc", sep=""), 
                          band=(m+408), varname="prcp")    
  climate[m,3] <- raster::extract(ppt.month.m, ll)[1]
  wet.month.m <- raster(paste(cpath, "wetd_wusa.nc", sep=""), 
                          band=(m+408), varname="wetdays")    
  climate[m,5] <- raster::extract(wet.month.m, ll)[1]
  srad.month.m <- raster(paste(cpath, "srad24_wusa.nc", sep=""), 
                          band=(m+408), varname="srad")    
  climate[m,4] <- raster::extract(srad.month.m, ll)[1]
} 

climate2 <- data.frame(climate) # weird number issues with matrix
names(climate2) <- c("yearmon","temp","ppt","meansol","wet","source")
climate2$yearmon <- RC3$yearmon
climate2$source <- "Daymet"

all <- rbind(RC3,climate2)
head(all)

################################################################################
# Plot tmean
png(paste(fpath, "tmean_station_daymet.png", sep=""),
    width=320, height=160, units='mm', res=300)
  ggplot(data=all, aes(x=yearmon,y=temp, group=source,color=source)) +
  geom_point() +
  geom_line() +
  scale_color_discrete(name="Data Source") +
  ylab(expression("Temperature ("*~degree*"C)")) +
  xlab("Date (year-month)")
dev.off()

# Plot ppt
png(paste(fpath, "ppt_station_daymet.png", sep=""),
    width=320, height=160, units='mm', res=300)
  ggplot(data=all, aes(x=yearmon,y=ppt, group=source,color=source)) +
  geom_point() +
  geom_line() +
  scale_color_discrete(name="Data Source") +
  ylab("Precipitation (mm/month)") +
  xlab("Date (year-month)")
dev.off()

# Plot wetdays
png(paste(fpath, "wetd_station_daymet.png", sep=""),
    width=320, height=160, units='mm', res=300)
  ggplot(data=all, aes(x=yearmon,y=wet, group=source,color=source)) +
  geom_point() +
  geom_line() +
  scale_color_discrete(name="Data Source") +
  ylab("Wetdays (#/month)") +
  xlab("Date (year-month)")
dev.off()

# Plot srad
png(paste(fpath, "srad_station_daymet.png", sep=""),
    width=320, height=160, units='mm', res=300)
  ggplot(data=all, aes(x=yearmon,y=meansol, group=source,color=source)) +
  geom_point() +
  geom_line() +
  scale_color_discrete(name="Data Source") +
  ylab("Solar Radiation (w/m2)") +
  xlab("Date (year-month)")
dev.off()

# Look at 2-year mean:
all2 <- gather(all, Variable, value, temp:wet)
head(all2)

png(paste(fpath, "means-2yr.png", sep=""),
    width=320, height=160, units='mm', res=300)
  ggplot(data=all2, aes(x=source, y=value, color=source)) +
  geom_boxplot(notch=T) +
  facet_wrap(~Variable, scale="free")
dev.off()

  
  