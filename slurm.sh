#!/usr/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --partition=exacloud
#SBATCH --time=60
srun /usr/bin/Rscript make.r

