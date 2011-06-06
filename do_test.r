#!/usr/bin/Rscript
source ('test.r')
args <- commandArgs (TRUE)
stopifnot (length (args) == 2)
print (args)
run.tests1 (b = as.integer (args [1]), i = as.integer (args [2]))
