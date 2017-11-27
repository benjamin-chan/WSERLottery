#!/usr/bin/bash
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=16G
#SBATCH --partition=exacloud
#SBATCH --time=120
srun /usr/bin/Rscript make.r

