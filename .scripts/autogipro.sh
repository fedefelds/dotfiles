#!/bin/bash
DistSave.py *posR001_R002*.dat && \
DistSave.py *complete.dat && \
dcp-linear-combination.py *posR001_R002*.dat 1 *complete.dat -1 -o difference.dat && \
DistSave.py difference.dat && \
mkdir angular spatial && \
mv *_ang.png angular/ && \
mv *.png spatial/ && \
echo done


