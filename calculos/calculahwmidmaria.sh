#!/bin/bash
#SBATCH -p all # partition
#SBATCH -N 1 # number of nodes 
#SBATCH -n 1 # number of cores 
#SBATCH --mem 100 # memory pool for all cores 
#SBATCH -o err/slurm.%N.%j.out # STDOUT 
#SBATCH -e err/slurm.%N.%j.err # STDERR 
#SBATCH --job-name="hwmid"
#SBATCH -w nodo-7-14

date
Rscript hwmidmaria.R $1 $2
date
