#!/bin/bash

for b in {9..1} 
do 
    for i in {1..3} 
    do 
	echo "./do_exp.r b${b}-${i} F F"
    done
done
