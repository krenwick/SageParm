#!/bin/bash

cd /local/job/$SLURM_JOB_ID
Rscript /local/job/$SLURM_JOB_ID/DE_paroptim_newphen.R