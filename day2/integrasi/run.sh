#!/bin/sh

#compile
gfortran -o integrasi.out integrasi.f95

#run compilation file
./integrasi.out

#remove compilation file
rm -rf *.out
