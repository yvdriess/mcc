#!/bin/bash
for I in `seq  2 16`
  do 
    ./mcc < qft/qft$I.mc
    make
    mv mccompiled qft/qft$I
done
