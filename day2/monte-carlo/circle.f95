program main
  implicit none
  integer		:: N,nSample,nCircle,i
  double precision	:: x,y,circle,pi_approx

  write(*,*) "exact pi = ", 2.d0 * asin(1.d0)
  write(*,*)
  do N = 1,8
     nSample = 10**N

     ncircle = 0
     do i = 1,nSample
        x = rand()-0.5
        y = rand()-0.5

        circle = 0.25 - x**2
        if (y**2.le.circle) then
           ncircle = ncircle + 1
        end if

     end do
     pi_approx = 4 * dfloat(ncircle) / dfloat(nSample)

     write(*,*) nSample, pi_approx

  end do

end program main
