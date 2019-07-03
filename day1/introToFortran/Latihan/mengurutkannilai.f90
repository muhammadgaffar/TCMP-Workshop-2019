program mengurutkannilai
    implicit none
    integer, parameter :: n = 8
    real, dimension(n) :: kecil, besar
    integer :: i, j
    real :: a

    kecil = [3, 5, -2, -3, 4, 8, 9, 10]
    besar = [-9, 10, 2, 4, 8, -7, -5, 2]

    !Dari kecil ke besar
    do i = 1, n-1
        do j = i+1, n
            if (kecil(j) < kecil(i)) then
                a = kecil(j)
                kecil(j) = kecil(i)
                kecil(i) = a
            end if
        end do
    end do
   
    write(*,*) kecil
    
    do i = 1, n-1
        do j = i+1, n
            if (besar(j) > besar(i)) then
                a = besar(j)
                besar(j) = besar(i)
                besar(i) = a
            end if 
        end do
    end do

    write(*,*) besar

end program mengurutkannilai