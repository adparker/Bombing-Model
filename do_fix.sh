#!/bin/bash -x
for f in ~/experiments/2009.12.07/asn_bombing*
do
    echo "Processing $f..."
    ./do_exp.r $f
done
