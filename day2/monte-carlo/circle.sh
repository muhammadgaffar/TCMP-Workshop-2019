#!bin/sh

# compile fortran program
gfortran circle.f95

# run ouput file
./a.out

# remove output file
rm -rf *.out
