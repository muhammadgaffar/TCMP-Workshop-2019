#!bin/sh

#compile program file
gfortran particlebox.f95

#run output file
./a.out

#remove output file
rm -rf *.out
