#!/usr/bin/Rscript
## do_exp.r DIR-NAME USE-BOMB USE-PRC
## Example:
##   do_exp.r "b3-2" T T 
source ('sourceall.r')
args <- commandArgs (TRUE)
results <- asn_experiment (shape = args [1],
                           use.bombing = as.logical (args [2]),
                           use.prc = as.logical (args [3]))
