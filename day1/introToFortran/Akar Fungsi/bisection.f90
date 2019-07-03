program bisect
  implicit none
  real :: x1, x2, roots
  real :: error
  CHARACTER (len = 10) :: Akar1 = 'Batas Kiri' !Batas kiri adalah X1
  CHARACTER (len = 11) :: Akar2 = 'Batas Kanan' !Batas kanan adalah X2
  CHARACTER (len = 13) :: Akar3 = 'Tengah-tengah' !Tengah-tengah adalah (X1+X2)/2
  
  write(*,*)
  write(*,*) '====================================='
  write(*,*) 'This code is written by Rasyid Sulaeman'
  write(*,*) 'Last updated in 12 Maret 2019'
  write(*,*) '====================================='
  write(*,*)

  write(*,*) 'Batas akar kiri (x1)'
  read(*,*) x1
  write(*,*) 'Batas akar kanan (x2)'
  read(*,*) x2

  write(*,*) '================================================='
  error = 1.0e-06

  write(*,10) Akar1, Akar2, Akar3
  10 FORMAT(2X, A10, 7X, A11, 6X, A13)
  write(*,*)

  call bisection(x1,x2,error, roots)

  write(*,*) 
  write(*,*) 'Akar fungsinya'
  write(*,*) roots

end program bisect

subroutine bisection(x1,x2,eps,root)
  
implicit none
REAL :: f, x1, x2, eps, root
REAL :: a, b, c
INTEGER :: i
INTEGER, PARAMETER :: iter = 200

if (f(x1)*f(x2) > 0.0) then
  write(*,*) 'Tidak punya solusi akar2 pada interval [x1,x2]'
  return
end if

a = x1
b = x2


DO i = 1, iter
  c = (a+b)/2 
  if (f(a)*f(c) < 0.0) then
    b = c
  else 
    a = c
  end if 
  
  if (abs(b-a) < eps) exit 
  
  write(*,*) a, b, c
end DO

root = (a+b)/2

end subroutine bisection

real function f(t)
implicit none
real:: t

f = cos(t) - t

end function
