#!/bin/bash

############################################################################
# DOES: Downloads climate data for the Reynold's Creek CZO
# Processes netcdfs into monthly data needed for LPJ-GUESS
############################################################################

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J get_data
#SBATCH -o get_data_%j.out
#SBATCH -e get_data_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=END
#SBATCH -p priority
#SBATCH -t 70:00:00

# Temperature data:
# loop through to download all of the years
wget http://data.boisestate.edu/opendap/CZO/air_temperature/ta_wy2014.nc 
cdo ymonmean ta_wy2014.nc ta_wy2014_monthly.nc

# Precip data:
# loop through to download all of the years
# http://data.boisestate.edu/opendap/CZO/precipitation_depth/precip_wy2014.nc