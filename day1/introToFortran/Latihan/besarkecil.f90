program kecilbesar
    implicit none
    integer, parameter :: n = 8
    real, dimension(n) :: a
    integer :: i
    real :: terkecil, terbesar

    a = [3, 10, 1, 8, 5,-2, 4, 20]

    terkecil = a(1)
    terbesar = a(1)

    do i = 1, n
        if (a(i) < terkecil) then
            terkecil = a(i)
        else if (a(i) > terbesar) then
            terbesar = a(i)
        end if
    end do

    write(*,*) terbesar, terkecil

end program kecilbesar