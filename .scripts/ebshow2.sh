#!/bin/bash
#Gottfried Hochleitner 2015-06-11
#FeFe & Heinz also did some work


path="$1"
echo $path

param=''
layer=''


for nfiles in `cat $1 | sed -n 's:.*<maly>\(.*\)</maly>.*:\1:p'`
do
    echo $nfiles
    layer=`grep "BEGIN MASK " $nfiles | awk '{printf("%s",$3)}'`
    param="$param +layer $layer  $nfiles"
done

for nfiles in `find ./$1 -name "*.ejb"`
do
    echo $nfiles
    layer=`grep "BEGIN MASK " $nfiles | awk '{printf("%s",$3)}'`
    param="$param +layer 1 $nfiles"
done

echo $param

ebview $param
