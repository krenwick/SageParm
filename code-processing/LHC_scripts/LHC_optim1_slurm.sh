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
#SBATCH -t 00:20:00
#SBATCH -N 1
#SBATCH -n 32

#SBATCH -p express
#SBATCH --no-requeue

# These settings get changed by Subset script
#SBATCH -J LHC_optim1
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=ALL



###############
# Copy files to /local on compute node
# Create temporary directory

jobdir="/local/job/$SLURM_JOB_ID"
jobname=LHC_optim1
modelDir=/home/katie.renwick/scripts/LPJ-GUESS
glAbrv=grid
scriptsDir=/home/katie.renwick/scripts
outputDir=/mnt/lustrefs/work/katie.renwick
nprocs=32


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
while (($counter < 20+1)); do
	for f in $jobdir/set_$number/*.ins; do
f=ls set_$number/*.ins | head -n 1
		echo "starting file $f"
		b=$(basename $f)
		echo "FILE = $b"
		srun -n 1 ./guess $f & # didn't work
		# Problem ins:
		#srun -n 1 ./guess set_1/slaa.10_lasa.1986_gmin.0.65_ltor.1_gref.0.06_root.0.9_tsap.0.19_psmi.-3.2_pslo.7.7_estm.0.08_psma.29.1_pshi.19.1.ins
		# Working Ins:
		#srun -n 1 ./guess set_1/slaa.10_lasa.1388_gmin.0.48_ltor.0.9_gref.0.06_root.0.7_tsap.0.02_psmi.-2.8_pslo.9.4_estm.0.18_psma.35.9_pshi.23.6.ins
		#srun -n 1 -o $outputDir/$jobname/out_$f -e $outputDir/$jobname/err_$f ./guess $f & # doesn't work
		#srun -n 1 -o $outputDir/$jobname/out_$b.txt -e $outputDir/$jobname/err_$b.txt ./guess $f & 
		#srun -n 1 -o $outputDir/$jobname/out_%j_$number_$f.txt -e $outputDir/$jobname/err_%j_$number_$f.txt ./guess $f &
		done
		wait

# Submit the job to each node and each proccessor core
#ls $jobdir/set_$number/*.ins|xargs -I% srun -n 1 ./guess % &
#/mnt/lustrefs/work/katie.renwick/out_%j_$number.txt -e /mnt/lustrefs/work/katie.renwick/err_%j_$number.txt /$jobdir/./guess %

let number=$number+1
let counter=$counter+1
done
#wait

###############
# Copy output data back to home directory
rsync -avhP $jobdir/Output_$jobname $outputDir
