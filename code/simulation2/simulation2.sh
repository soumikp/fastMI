#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=0-00:30
#SBATCH --job-name=sim1
#SBATCH --mem-per-cpu=1000MB
#SBATCH --array=1-126
#SBATCH --cpus-per-task=1
#SBATCH --output=/home/soumikp/2022_jmva/log/simulation2/slurm-%x_%A_%a.out
#SBATCH --account=pxsong1
#SBATCH --partition=standard

module load gcc
module load gsl
module load R

Rscript /home/soumikp/2022_jmva/code/simulation2/simulation2.R
