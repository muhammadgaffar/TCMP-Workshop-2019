program kalikanmatriks
    implicit none

    integer, parameter :: na = 2, ma = 3
    integer, parameter :: nb = 3, mb = 2
    real, dimension(na, ma) :: matriksA
    real, dimension(nb, mb) :: matriksB
    real, dimension(na, mb) :: matriksC
    integer :: k, i, j

    
    write(*,*)'Calculate matriks A'
    do i = 1, na
        do j = 1, ma
            matriksA(i,j) = i*2 + j
        end do
    end do
    
    do i = 1, na
        write(*,*) (matriksA(i,j), j = 1, ma)
    end do

    write(*,*)'Calculate matriks B'
    do i = 1, nb
        do j = 1, mb
            matriksB(i,j) = i + j
        end do
    end do

    do i = 1, nb
        write(*,*) (matriksB(i,j), j = 1, mb)
    end do


    write(*,*)'Calculate matriks C = AB'
    
    do i = 1, na
        do j = 1, mb
            matriksC(i,j) = 0.0
            do k = 1, nb
                matriksC(i,j) = matriksC(i,j) + matriksA(i,k)*matriksB(k,j)
            end do
        end do
    end do

    do i = 1, na
        write(*,*) (matriksC(i,j), j = 1, mb)
    end do 

end program kalikanmatriks