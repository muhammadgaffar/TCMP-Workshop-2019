program matriks
    implicit none
    integer, parameter :: n = 3, m = 4
    real, dimension(n,m) :: a
    integer :: i, j

    open(unit = 10, file ='bayu.dat', status ='old', action = 'read')
    
    do i = 1, n
        read(10,*) (a(i,j), j = 1, m) 
        write(*,*) (a(i,j), j = 1, m)
    end do

end program matriks