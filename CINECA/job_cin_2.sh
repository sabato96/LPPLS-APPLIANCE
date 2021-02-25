#!/bin/bash
#SBATCH --nodes=1                   # 1 node
#SBATCH --ntasks-per-node=18         # 36 tasks per node
#SBATCH --mem=30000                  # Memory
#SBATCH --time=0:20:00               # time limits: 1 hour
#SBATCH --error=myJob2.err            # standard error file
#SBATCH --output=myJob2.out           # standard output file
#SBATCH --account=try21_gargiulo           # account name
#SBATCH --partition=gll_usr_prod     # partition name

cd $WORK
cd progettoR
Rscript fitting_2.R


