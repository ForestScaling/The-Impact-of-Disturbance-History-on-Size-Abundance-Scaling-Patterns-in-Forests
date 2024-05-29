#!/bin/bash
#SBATCH -J Rjob   #job name
#SBATCH --time=03-00:00:00  #requested time 
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=10
#SBATCH --account=REDACTED
#SBATCH --output=baysian.%j.out  #saving standard output to file
#SBATCH --error=bayesian.%j.err   #saving standard error to file
#SBATCH --mail-type=ALL    #email optitions
#SBATCH --mail-user=YOUR-EMAIL-HERE

#load R module
module load gnu/11.2.0
module load mkl/2021.3.0
module load R/4.3.0
#run your R script with "Rscript", make sure you save all output/data generated from your script to file inside your R script. 
Rscript --no-save 7_Bayesian_Model.R 

# place this script at where your R script is and modify the amount of resources you need, the R script name, job name, .etc
# submit batch job with "$ sbatch R_script.sh" from command line on any node (compute or login) on the cluster
