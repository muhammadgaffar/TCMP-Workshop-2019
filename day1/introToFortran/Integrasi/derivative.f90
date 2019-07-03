program derivative
    implicit none
    real*8 :: dx, xn, x0
    real*8, allocatable :: x(:), y(:), df(:)
    double precision :: f
    integer :: N, i

    x0 = 0.d0
    xn = 10.d0
    
    N = 100

    allocate(x(N), y(N), df(N))

    dx = (xn-x0)/(N-1)

    do i = 1, N
        x(i) = x0 + (i-1)*dx
        y(i) = f(x(i))
    end do

    !Three-point finite difference

    do i = 1, N
        if (i == 1) then
            df(i) = (-3*f(x(i))+4*f(x(i+1))-f(x(i+2)))/(2*dx)
        else if (i == N) then
            df(i) = (3*f(x(i))-4*f(x(i-1))+f(x(i-2)))/(2*dx)
        else
            df(i) = (f(x(i+1))-f(x(i-1)))/(2*dx)
        end if
    end do

    open(unit = 10, file = 'deriv.dat', status = 'unknown')
    do i = 1, N
        write(10,*) x(i), y(i), df(i)
    end do

    deallocate(x,y,df)
end program derivative

double precision function f(x)
    implicit none
    real*8 :: x

    f = sin(x)

    return
end function f