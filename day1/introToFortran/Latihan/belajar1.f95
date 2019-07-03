program belajar

implicit none 
REAL :: a, b, c, D
REAL :: x1, x2
REAL :: Discriminant


write(*,*) 'Enter number a, b, c ='
read (*,*) a, b, c 


D = Discriminant(a,b,c)
write(*,*) 'Discriminant = ', D

if (D .ge. 0.0) then
        call roots(a, b, c, x1, x2)
        write(*,*) 'Roots x1 = ', x1, 'x2 =', x2
        
    else if (D == 0.0) then
        call roots(a, b, c, x1, x2)
        write(*,*) 'Equal roots = x1', x1
        
    else 
        call roots(a, b, c, x1, x2)
        write(*,*) 'Doesnt have a roots'
end if

end program belajar

real function Discriminant(a,b,c)
implicit none
real :: a, b, c 

Discriminant = b**2 - 4*a*c

end function Discriminant


subroutine roots(a, b, c, x1, x2)
implicit none

real :: a, b, c
real :: x1, x2
real :: D, Discriminant

if (a == 0.0) then
    stop 'roots : a = 0'
end if 

D = discriminant(a,b,c)
if (D .ge. 0.0) then
    D = sqrt(D)
else 
    write(*,*) 'Roots : sorry cannot compute roots , D<0', D
    stop
end if

x1 = (-b + D)/(2.0*a)
x2 = (-b - D)/(2.0*a)
 
end subroutine roots
