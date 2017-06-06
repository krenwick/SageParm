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
#SBATCH -t 72:00:00
#SBATCH -N 1
#SBATCH -n 1

#SBATCH -p priority
#SBATCH --no-requeue

# These settings get changed by Subset script
#SBATCH -J SCEoptim_summergreen1
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=ALL

#SBATCH --mem-per-cpu 8000



###############
# Copy files to /local on compute node
# Create temporary directory

jobdir="/local/job/$SLURM_JOB_ID"
jobname=SCEoptim_summergreen1
modelDir=/home/katie.renwick/scripts/LPJ-GUESS
glAbrv=grid
scriptsDir=/home/katie.renwick/scripts
outputDir=/mnt/lustrefs/work/katie.renwick


# Copy Climate, CO2, Soils data and Gridlists to compute node
for dir in /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3_ID /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/CO2 /mnt/lustrefs/store/katie.renwick/Soil_LPJGUESS/STATSGO /mnt/lustrefs/store/katie.renwick/Gridlists/RC4; do
	rsync -avhP $dir $jobdir/
done

# Copy LPJGUESS executable
rsync -avhP $modelDir/ModelFiles/modules/guess /$jobdir 

# Copy instruction files
for fil in $scriptsDir/Slurm_scripts/Optimization/summergreen_optim1_LM.ins $scriptsDir/Slurm_scripts/Optimization/lai_gpp.csv $scriptsDir/Slurm_scripts/Optimization/RCflux_15_16.csv $scriptsDir/Slurm_scripts/Optimization/SCE_optim.R $scriptsDir/Slurm_scripts/Optimization/runjob.sh; do
	rsync -avhP $fil $jobdir/
done

rsync -avhP $scriptsDir/Slurm_scripts/Optimization/runR.sh /$jobdir 

# Make or copy output directories
mkdir $jobdir/Output_$jobname


###############

# Run R script
./runR.sh
#Rscript /local/job/$SLURM_JOB_ID/SCE_optim.R

wait


############################---------------------------------
outdir="./Output_$jobname/"
mv $jobdir/SCE1.RData $jobdir/Output_$jobname
mv $jobdir/tempins2.ins $jobdir/Output_$jobname
mv $jobdir/mgpp.txt $jobdir/Output_$jobname
mv $jobdir/mlai.txt $jobdir/Output_$jobname
############################

# Copy output data to work directory
rsync -avhP $jobdir/Output_$jobname $outputDir
