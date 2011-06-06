#!/usr/bin/Rscript
## do_raster_exp.r DIR-NAME
## Example:
##   do_raster_exp.r "b3-2"
source ('test.r')
args <- commandArgs (TRUE)
results <- raster_experiment (shape = args [1])