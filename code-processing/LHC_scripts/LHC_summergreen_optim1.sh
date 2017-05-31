#!/bin/bash

############################################################################
# Slurm Script to copy input and output directories/files onto compute node
#/local folder and to submit LPJ-GUESS simulations on the Hyalite cluster
#
# June 2016 modified by kristen.emmett#gmail.com
# May 2016 modified by jerad.hoy#msu.montana.edu
# June 2015 created by kristen.emmett#gmail.com
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
#SBATCH -t 00:30:00
#SBATCH -N 1
#SBATCH -n 32

#SBATCH -p express
#SBATCH --no-requeue

# These settings get changed by Subset script
#SBATCH -J LHC_summergreen_optim1
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=ALL

#SBATCH --mem-per-cpu 2000



###############
# Copy files to /local on compute node
# Create temporary directory

jobdir="/local/job/$SLURM_JOB_ID"
jobname=LHC_summergreen_optim1
modelDir=/home/katie.renwick/scripts/LPJ-GUESS
glAbrv=grid
scriptsDir=/home/katie.renwick/scripts
outputDir=/mnt/lustrefs/work/katie.renwick
nprocs=32
nfolder=9 # number of folders with ins files


# Copy Climate, CO2, Soils data and Gridlists to compute node
for dir in /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3_ID /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/CO2 /mnt/lustrefs/store/katie.renwick/Soil_LPJGUESS/STATSGO /mnt/lustrefs/store/katie.renwick/Gridlists/RC4; do
	rsync -avhP $dir $jobdir/
done

# Copy LPJGUESS executable
rsync -avhP $modelDir/ModelFiles/modules/guess /$jobdir 

# Copy instruction files
rsync -avhP $scriptsDir/Ins_files/$jobname/ /$jobdir

# Make or copy output directories
mkdir $jobdir/Output_$jobname
for var in fpc anpp cmass lai cflux dens tot_runoff cpool firert mnpp mlai mrh mgpp mra mnee maet mpet mevap mintercept mrunoff mwcont_upper mwcont_lower; do
	mkdir $jobdir/Output_$jobname/$var
done

###############

# submit each ins file
cd $jobdir
counter=1
let c2=1
let number=1
while (($counter < nfolder+1)); do
	for f in $jobdir/set_$number/*.ins; do
f=ls set_$number/*.ins | head -n 1
		echo "starting file $f"
		b=$(basename $f)
		echo "FILE = $b"
		srun -n 1 ./guess $f & 
		done
		wait

# Submit the job to each node and each proccessor core
#ls $jobdir/set_$number/*.ins|xargs -I% srun -n 1 ./guess % &
#/mnt/lustrefs/work/katie.renwick/out_%j_$number.txt -e /mnt/lustrefs/work/katie.renwick/err_%j_$number.txt /$jobdir/./guess %

let number=$number+1
let counter=$counter+1
done
#wait

############################---------------------------------
mkdir -p $jobdir/Output_$jobname/Output_$jobname
outdir="./Output_$jobname"
############################

# First: append file name to each row in every file
for folder in anpp cmass dens fpc lai maet mevap mgpp mlai mnee mnpp mpet mra mrh
do
	cd $jobdir/Output_$jobname/$folder
	echo "appending to files in $folder"
	# NOTE: different from mac version! Linux = sed -i -e
	# On mac = sed -i ''
	ls *.out|xargs -I% sed -i -e 's/$/ %/g' %
	cd ..
done

# Merge all output- comment out variables not needed --------------------------------
# Because each file has header- can't just cat. 
# Extract header first then add files starting at line 2

# list variables
for var in anpp cmass dens fpc lai maet mevap mgpp mlai mnee mnpp mpet mra mrh
do
	echo "merging files in $var"
	first=$(ls $var/*.out | head -n 1)
	head -1 $first > $outdir/$var".txt" # > overwrites
	for file in $var/*.out; do
		tail -n +2 -q $file >> $outdir/$var".txt" # >> appends
	done
done

###############
# Copy output data back to home directory
#rsync -avhP $jobdir/Output_$jobname $outputDir
# Only copy the merged data:
rsync -avhP $jobdir/Output_$jobname/Output_$jobname $outputDir
