#!/bin/bash

for b in {9..1}
do
    for i in {1..3}
    do
	echo "do_test.r $b $i"
	./do_test.r $b $i
    done
done
