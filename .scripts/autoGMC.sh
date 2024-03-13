#! /bin/bash

# Heinz-Georg Stangl <heinz-georg.stangl@ims.co.at>, 2023-10-04
# conforms to Optics Manual 2023Q3 ch.8

# example usage 
# bash autoGMC.sh se07 mb017

tool=$1
mb=$2

for j in {type1,type2}; do ( 
    find Ref_*.dat -exec optimpresets.sh -i {} -t $tool -p gmc_$j \;;
    cd optim_outputs;
    files=`ls -1q *$j.dat | wc -l`; 
    weight=`bc -l <<< "scale=4; 1/$files"`;
    # cmdstr='';
    for i in `ls -1q *$j.dat`; do
        cmdstr+=$i' '$weight' ';
     done 
     dcp-linear-combination.py $cmdstr -o GMC_$mb\_$j\_AVG.dat 
     DistSave.py GMC_$mb\_$j\_AVG.dat -sf 2
    ); done
