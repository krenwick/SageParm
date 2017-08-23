#!/bin/bash

############################################################################
# Slurm Script to copy input and output directories/files onto compute node
# Run optimization routine
############################################################################

# Running a job managed by Slurm
# -t time limit to batch job
# -a submit job array (32 processors per node to max) ! THIS IS WRONG! splits to different compute nodes
# -N number of nodes
# -n number of tasks, requests number of processor cores per subjob
# -J name of job
# --mail-user email address to send notifications
# --mail-type=ALL send user email at start and end of job
# --exclusive do not share compute node
# -p queue to run job
# --no-requeue do not resubmit job if fails
# -o create an out file of log
# -e create an error file
# -x exclude these compute nodes
# --mem 256000 request bigger node

# These are default settings
#SBATCH -t 14:30:00
#SBATCH -N 1
#SBATCH -n 32

#SBATCH -p defq
#SBATCH --no-requeue

# These settings get changed by Subset script
#SBATCH -J optim_newphen
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=ALL

#SBATCH --mem-per-cpu 2000



###############
# Copy files to /local on compute node
# Create temporary directory

jobdir="/local/job/$SLURM_JOB_ID"
jobname=optim_newphen
modelDir=/home/katie.renwick/scripts/LPJ-GUESS
glAbrv=grid
scriptsDir=/home/katie.renwick/scripts
outputDir=/mnt/lustrefs/work/katie.renwick


# Copy Climate, CO2, Soils data and Gridlists to compute node
for dir in /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3_ID /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/CO2 /mnt/lustrefs/store/katie.renwick/Soil_LPJGUESS/STATSGO /mnt/lustrefs/store/katie.renwick/Gridlists/RC4; do
	rsync -avhP $dir $jobdir/
done

# Copy LPJGUESS executable
rsync -avhP $modelDir/phenmodules/guess /$jobdir 

# Copy job-specific files (CHECK!)
for fil in $scriptsDir/Slurm_scripts/Optimization/optim2_newphen.ins $scriptsDir/Slurm_scripts/Optimization/lai_gpp.csv $scriptsDir/Slurm_scripts/Optimization/RCflux_15_16.csv $scriptsDir/Slurm_scripts/Optimization/DE_paroptim_newphen.R $scriptsDir/Slurm_scripts/Optimization/FieldLaiCover.csv; do
	rsync -avhP $fil $jobdir/
done

rsync -avhP $scriptsDir/Slurm_scripts/Optimization/runRnewphen.sh /$jobdir 

# Make or copy output directories
mkdir $jobdir/Output_$jobname

###############

# Run R script
./runRnewphen.sh
wait


############################---------------------------------
outdir="./Output_$jobname/"
mv $jobdir/*.RData $jobdir/Output_$jobname
############################

# Copy output data to work directory
rsync -avhP $jobdir/Output_$jobname $outputDir
