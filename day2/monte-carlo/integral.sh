#!bin/sh

#compile program to output file "integration.o"
gfortran -o integration.o integral.f95

#run output file
./integration.o

#remove output file
rm -rf *.o
