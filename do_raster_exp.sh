#!/bin/bash

for b in {9..1} 
do 
    for i in {1..3} 
    do 
	echo "./do_raster_exp.r b${b}-${i}"
    done
done
