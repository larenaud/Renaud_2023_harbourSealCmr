#!/bin/bash
#SBATCH --account=def-pelleti2
#SBATCH --time=0-48:05          # time (DD-HH:MM)
#SBATCH --ntasks=2              # 12 core(CPU)
#SBATCH --job-name=cormackJollySeber   # sensible name for the job
#SBATCH --mem-per-cpu=20G                 # Default memory per CPU is 3GB.

# If you would like to use more please adjust this.

## Below you can put your scripts
# If you want to load module
module load nixpkgs/16.09
module load StdEnv/2020
module load gcc/9.3.0
module load r/4.2.2

# module list # List loaded modules

# Other commands can be included below
cd ~/projects/def-pelleti2/renl2702/phoques

R CMD BATCH --no-save --no-restore cjs_m4.R
#Rscript --verbose jsms1_metis.R


# salloc --time=1:0:0 --ntasks=1 --account=def-pelleti2
