https://docs.computecanada.ca/wiki/Running_jobs


salloc --time=1:0:0 --ntasks=2 --account=def-pelleti2
module load nixpkgs/16.09
module load gcc/7.3.0
module load r/4.0.2



other way to laucnh scripts
R CMD BATCH --no-save --no-restore Script.R  job_out.Rout  > job_file.job  ## REPLACE "Script.R" BY THE
