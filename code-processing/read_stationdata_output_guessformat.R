################################################################################
# Take weather station data and output for LPJ-GUESS
# 1. Station data from site 176 at Reynold's Creek
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=12)) # sized for print

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Documents/SageParm/data/"
fpath <- "/Users/poulterlab1/Documents/SageParm/figures/"
opath <- "/Users/poulterlab1/Documents/SageParm/LPJfiles/" # put output here
#cpath <- "/Users/poulterlab1/Documents/LPJGUESS/Climate/Daymet/" # climate data

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
  mutate(year=format(date, format="%y")) %>%
  mutate(month=format(date, format="%m")) %>%
  group_by(year, month) %>%
  summarise(temp=mean(daytemp), ppt=sum(dayppt), meansol=mean(daysol),
            wet=sum(dayppt>0)) 

head(RC2)
RC2$source <- "station176"
RC2$lon <- ll[2]
RC2$lat <-ll[1]

# Function to take data and put into form for LPJ-GUESS read-in
# Data must be long-form, include lat-lon, and have variables as columns
# nyear: number of simulation years desired, ... = variable to use
format_guess <- function(data, nyear, ...){ # use dots since passing text to dplyr problematic
  d1 <- data %>% select(lon, lat, year, month, ...) %>%
    spread(month, ...)
  d2 <- do.call(rbind, replicate(nyear/nrow(d1), d1, simplify=FALSE))
  #d2 <- d2[2:nrow(d2),]
  return(d2)
}

temp <- format_guess(RC2,1156, temp)
ppt <- format_guess(RC2,1156, ppt)
srad <- format_guess(RC2,1156, meansol)
wetd <- format_guess(RC2,1156, wet)

write.table(temp, paste(opath, "temp.txt", sep=""), sep=" ",
            quote=F, row.names=F, col.names=F)
write.table(ppt, paste(opath, "ppt.txt", sep=""), sep=" ",
            quote=F, row.names=F, col.names=F)
write.table(srad, paste(opath, "srad.txt", sep=""), sep=" ",
            quote=F, row.names=F, col.names=F)
write.table(wetd, paste(opath, "wetd.txt", sep=""), sep=" ",
            quote=F, row.names=F, col.names=F)
