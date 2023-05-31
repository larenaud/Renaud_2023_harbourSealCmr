#!/bin/bash
#SBATCH --account=def-pelleti2
#SBATCH --time=0-00:35           # time (DD-HH:MM)
#SBATCH --job-name=simReproCost   # sensible name for the job
#SBATCH --mem=5G      # memory; default unit is megabytes
# If you would like to use more please adjust this.

## Below you can put your scripts
# If you want to load module
module load nixpkgs/16.09
module load gcc/7.3.0
module load r/4.0.2
module load jags/4.3.0
module list # List loaded modules

# Other commands can be included below


Rscript R/GenSimDat.R
