program jumlahkali
    implicit none
    real :: sum, func, p
    integer :: i, n
    
    !Sumasi deret sampai ke-n
    sum = 0.0
    n = 5

    do i = 1, n
        sum = sum + func(i)
    end do

    write(*,*) sum

    !Kali deret sampai ke-n
    p = 1.0
    do i = 1, n
        p = p*func(i)
    end do

    write(*,*) p

end program jumlahkali


function func(t) result(z)

    integer,intent(in) :: t

    z = 2*t
    return
end function func