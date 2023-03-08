#!/bin/bash

for i in {001..008}
do 
  sbatch submit_$i.sh
done