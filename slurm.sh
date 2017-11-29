#!/usr/bin/bash
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=4G
#SBATCH --partition=exacloud
#SBATCH --time=240
srun /usr/bin/Rscript make.r

