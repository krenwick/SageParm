#!/bin/bash

cd /local/job/$SLURM_JOB_ID
srun -n 1 ./guess tempins2.ins