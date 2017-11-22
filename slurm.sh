#!/usr/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=1G
#SBATCH --partition=exacloud
srun /usr/bin/Rscript make.r

