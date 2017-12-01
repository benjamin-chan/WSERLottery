#!/usr/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=12G
#SBATCH --partition=exacloud
#SBATCH --time=360
srun /usr/bin/Rscript make.r

