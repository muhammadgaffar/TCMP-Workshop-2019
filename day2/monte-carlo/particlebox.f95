program main
  implicit none
  integer	:: nparticle, nt, nkiri, it
  double precision	:: tmin, tmax, time, dt, prob_gen, prob_part


  nparticle = 50
  
  tmin = 0.d0
  tmax = 1.d0
  nt = 100 * nparticle

  dt = (tmax - tmin) / (nt - 1)

  nkiri = nparticle

  open(unit=1, file="particlebox.dat")
  do it = 1,nt
     prob_gen  = rand()
     prob_part = dfloat(nkiri) / dfloat(nparticle) 
     if (prob_gen.le.prob_part) then
        nkiri = nkiri - 1
     else
        nkiri = nkiri + 1
     end if

     time = tmin + dt * (it-1)
     
     write(1,*) time, nkiri
  end do
  close(1)
  
end program main
