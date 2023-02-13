#!/bin/bash

# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
# ATTENTION: REPLACE THIS WITH YOUR EMAIL ADDRESS!
#SBATCH --mail-user=sky.qiu@berkeley.edu

#SBATCH --partition=low                                # high/low/gpu, default if empty is low
#SBATCH --nodes=1                                      # only use 1 node, MPI option
#SBATCH --ntasks=1                                     # how many tasks to start
#SBATCH --cpus-per-task=16                             # number of cores to use, multi-core/mu$
#SBATCH --job-name=fit_day100_cases_model_task_02_04   # job name fore queue, default may be u$
#SBATCH --error=fit_day100_cases_model_task_02_04.err  # error file, default if empty is slurm$
#SBATCH --output=fit_day100_cases_model_task_02_04.out # standard out file, no default

mkdir logs;
R CMD BATCH --no-save 02_task_VarImpRisk.R logs/02_task_VarImpRisk.Rout;
R CMD BATCH --no-save 04_task_IntxnRisk.R logs/04_task_IntxnRisk.Rout