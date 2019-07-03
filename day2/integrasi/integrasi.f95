! ================================================================
! PURPOSE	: CONTOH PROGRAM PERHITUNGAN INTEGRASI
! AUTHOR	: MUHAMMAD GAFFAR
! DATE		: 03 JUNI 2019
! ================================================================

program main
  
  implicit none
  integer :: nData,i,nx,iData
  double precision, allocatable :: x(:),y(:),approxIntg(:),error(:)
  double precision :: xmin,xmax,dx,exact
  double precision, external :: polinomial, intgPolinomial

  !plot data polinimial p(x) = 2.5x^2 - x - 10
  nData = 100
  xmin = 0.d0
  xmax = 12.d0
  dx = (xmax - xmin) / (nData-1)
  
  allocate(x(nData))
  allocate(y(nData))

  open(unit = 1, file="polinimial.dat",status="unknown")
  do i = 1,nData
    x(i) = xmin + (i-1)*dx
    y(i) = polinomial(x(i))

    write(1,*) x(i),y(i)
  end do
  close(1)

 deallocate(x)
 deallocate(y)

 !hasil integasi analitik eksak dari p(x) dari xmin -> xmax
 exact = intgPolinomial(xmax) - intgPolinomial(xmin)

 !hasil integrasi numerik dari p(x) dari xmin -> xmax dengan variasi banyak Data
 open(unit = 2, file="compareIntg.dat",status="unknown")
 !variasi banyak data dari 10 -> 100 dg interval 10
 do iData = 10,100,10

    allocate(x(iData))
    allocate(y(iData))
    allocate(approxIntg(iData))
    allocate(error(iData))

    xmin = 0.d0
    xmax = 12.d0
    dx = (xmax - xmin) / (iData-1)

    do i = 1,iData
       x(i) = xmin + (i-1)*dx
       y(i) = polinomial(x(i))    
    end do

    !integrasi numerik
    call integrasi(iData,y,x,approxIntg(iData))

    !error dari nilai eksak = | (numerik - eksak) / eksak |
    error(iData) = abs( ( approxIntg(iData) - exact ) / exact )

    write(2,*) iData, exact, approxIntg(iData), error(iData)

    deallocate(x)
    deallocate(y)
    deallocate(approxIntg)
    deallocate(error)
    
 end do
 close(2)

end program main

!fungsi polinomial
function polinomial(x) result(y)
  double precision, intent(in) :: x
  double precision :: y

  y = 2.5*x**3 - x - 10
  
end function polinomial

!fungsi integrasi Polinomial
function intgPolinomial(x) result(y)
  double precision, intent(in) :: x
  double precision :: y

  y = 2.5*x**4 / 4 - x**2 / 2 - 10*x
  
end function intgPolinomial

!integrasi numerik Trapezoid
subroutine integrasi(nx,fn,x,trapzSum)
  implicit none
  !input variabel
  integer :: nx
  double precision:: fn(nx),x(nx)
  !internal variabel
  integer :: i
  double precision :: xmin,xmax,dx
  !output variabel
  double precision :: trapzSum 

  xmin = x(1)
  xmax = x(nx)
  dx = (xmax - xmin) / (nx - 1)

  trapzSum = fn(1) / 2
  do i = 2,nx-1
     trapzSum = trapzSum + fn(i)
  end do
  trapzSum = trapzSum + fn(nx) / 2
  trapzSum = trapzSum * dx
  
end subroutine integrasi

