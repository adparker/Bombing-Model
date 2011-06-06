#!/usr/bin/Rscript
source ('figures.r')
args <- commandArgs (TRUE)
print (args)
fix.mean.mat (asn.results.fname = args[1])
