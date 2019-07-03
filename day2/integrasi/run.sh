#!/bin/sh

#compile
gfortran -o integrasi.out integrasi.f95

#run compilation file
./test.out

#remove compilation file
rm -rf test.out
