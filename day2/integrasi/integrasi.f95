! ================================================================
! PURPOSE	: CONTOH PROGRAM PERHITUNGAN INTEGRASI
! AUTHOR	: MUHAMMAD GAFFAR
! DATE		: 03 JUNI 2019
! ================================================================

program main
  
  implicit none
  integer 			:: nData,i,nx,iData
  double precision, allocatable :: x(:),y(:),wLgr(:),approxIntg(:),error(:)
  double precision 		:: xmin,xmax,dx,exact
  double precision, external 	:: polinomial, intgPolinomial

  !plot data polinimial p(x) = x^4  + 5x^3 - 25x
  nData	= 100
  xmin 	= -3.d0
  xmax 	= 2.d0
  dx 	= (xmax - xmin) / (nData-1)
  
  allocate(x(nData))
  allocate(y(nData))

  open(unit=1,file="polinimial.dat",status="unknown")
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

 !hasil integrasi numerik trapezoid
 open(unit=2,file="compareTrapz.dat",status="unknown")
 !variasi banyak data dari 10 -> 100 dg interval 10
 do iData = 5,55,10

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
    call trapz(iData,y,x,approxIntg(iData))

    !error dari nilai eksak = | (numerik - eksak) / eksak |
    error(iData) = abs( ( approxIntg(iData) - exact ) / exact )

    write(2,*) iData, exact, approxIntg(iData), error(iData)

    deallocate(x)
    deallocate(y)
    deallocate(approxIntg)
    deallocate(error)
    
 end do
 close(2)

 !hasil integrasi numerik simpson
 open(unit=3,file="compareSimspson.dat",status="unknown")
 !variasi banyak data dari 10 -> 100 dg interval 10
 do iData = 5,55,10

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
    call simpson(iData,y,x,approxIntg(iData))

    !error dari nilai eksak = | (numerik - eksak) / eksak |
    error(iData) = abs( ( approxIntg(iData) - exact ) / exact )

    write(3,*) iData, exact, approxIntg(iData), error(iData)

    deallocate(x)
    deallocate(y)
    deallocate(approxIntg)
    deallocate(error)
    
 end do
 close(3)

end program main

!fungsi polinomial
function polinomial(x) result(y)
  double precision, intent(in) :: x
  double precision :: y

  y = x**4 + 5*x**3 - 25*x
  
end function polinomial

!fungsi integrasi Polinomial
function intgPolinomial(x) result(y)
  double precision, intent(in) :: x
  double precision :: y

  y = x**5 / 5 + 5*x**4 / 4 - 12.5*x**2
  
end function intgPolinomial

!integrasi numerik Trapezoid
subroutine trapz(nx,fn,x,trapzSum)
  implicit none
  !input variabel
  integer :: nx
  double precision:: fn(nx),x(nx)
  !internal dan outputvariabel
  integer :: i
  double precision :: xmin,xmax,dx
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
  
end subroutine trapz

subroutine simpson(nx,fn,x,simpsonSum)
  implicit none
  !input variabel
  integer :: nx
  double precision:: fn(nx),x(nx)
  !internal dan outputvariabel
  integer :: i
  double precision :: xmin,xmax,dx
  double precision :: simpsonSum

  if (mod(nx,2).ne.1) then
     write(*,*) "number of mesh data of x must be odd"
  else
     xmin = x(1)
     xmax = x(nx)
     dx = (xmax - xmin) / (nx - 1)
  
     simpsonSum = fn(1)
     do i = 2,nx-3,2
        simpsonSum = simpsonSum + fn(i) * 4.0 + fn(i+1) * 2.0
     end do
     simpsonSum = simpsonSum + fn(nx-1) * 4.0 + fn(nx)
     simpsonSum = simpsonSum * dx / 3.d0
  end if

end subroutine simpson
