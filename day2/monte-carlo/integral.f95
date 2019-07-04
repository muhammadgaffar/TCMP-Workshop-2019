program main
  implicit none
  integer			:: iSample, nSample,i
  double precision 		:: x,sumfx,sumfx2,variance,fx
  double precision, external 	:: func


  write(*,*) "exact integration = ", 2*asin(1.d0)
  write(*,*) "exact variance = ", 0.413581
  write(*,*)
  do iSample = 1,6
     nSample = 10**iSample

     sumfx = 0.d0
     sumfx2 = 0.d0
     do i = 1,nSample
        x = rand()
        fx = func(x)
        sumfx = sumfx + fx
        sumfx2 = sumfx2 + fx*fx
     end do
     sumfx = sumfx / nSample
     sumfx2 = sumfx2 / nSample
     variance = sumfx2 - sumfx**2

     write(*,*) nSample, sumfx, variance
  end do
  
end program main

function func(x) result(y)
  implicit none
  double precision, intent(in) 	:: x
  double precision		:: y

  y = 4 / (1 + x**2)
  
end function func
