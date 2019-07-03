program test_bisection

    implicit none
    real*8 :: a, b, hasil
    real*8, external :: func

    a = 0
    b = 3

    call bisection(func, a, b, hasil)
    write (*,*) "Hasil akar penyelesaian dari bisection : "
    write (*,10) hasil
    10 format (F10.7)

end program test_bisection

double precision function func(x)
    implicit none
    real, intent(in) :: x
    !func = sin(x)
    func = x**2 - 1
end function func

subroutine bisection(func_dp, batas_bawah_dp, batas_atas_dp, x_dp)
            implicit none
            real(8) :: func_dp                          ! fungsi yang akan dikerjakan
            real(8), intent(in) :: batas_atas_dp        ! batas nilai atas
            real(8), intent(in) :: batas_bawah_dp       ! batas nilai bawah
            real(8), intent(out) :: x_dp                ! hasil x saat f(x) ~ 0
            real(8) :: tol_i                            ! nilai toleransi iterasi ke - i
            real(8) :: a_temp, b_temp                   ! batas dummy
            real(8) :: x_old                            ! nilai x untuk iterasi sebelumnya
            integer :: i                                ! indeks iterasi looping
            !call header(1)                              ! 1 merupakan code Bisection
            real*8,parameter :: tolerance_double = 1e-6

            ! Proses memasukkan nilai dan batas awal
            a_temp = batas_bawah_dp
            b_temp = batas_atas_dp
            x_old = (b_temp + a_temp)/2.0
            
            ! Iterasi ke - 0
            i = 0
            write (*,'(A10, 5(A25))') "Iterasi", "Titik_Kiri(A)", "Titik_Kanan(B)", "Titik_tengah(x)", "Fungsi di x", "Toleransi"
            write (*,'(I10, 4(F25.15))') i, a_temp, b_temp, x_old, func_dp(x_old)

            ! Kondisi apabila tidak terdapat solusi atau lebih dari satu solusi di batas a dan b
            if (func_dp(a_temp)*func_dp(b_temp) > 0) then
                do
                    write (*,"(A,F10.3, A, F10.3)") "tidak ada solusi atau lebih dari 1 solusi di daerah ", a_temp," hingga", b_temp
                    write (*,"(A)") "Masukkan batas fungsi yang baru (format : batas_kiri, batas_kanan)"
                    read (*,*) a_temp, b_temp
                    
                    if (func_dp(a_temp)*func_dp(b_temp) < 0) then
                        i = 0
                        write (*,"(A10, 4(A25))") "Iterasi", "Titik_Kiri(A)", "Titik_Kanan(B)", "Titik_tengah(x)", "Fungsi di x"
                        write (*,'(I10, 4(F25.15))') i, a_temp, b_temp, x_old, func_dp(x_old)
                        exit
                    end if
                
                end do
            end if
            
            ! Kondisi apabila terdapat 1 solusi di antara a dan b
            do
                i = i + 1

                ! Kondisi saat nilai x iterasi ke-i sudah merupakan nilai exact
                if (func_dp(x_old) == 0.0) then
                    write (*,*) "solusi ", x_old
                    exit
                end if
                
                ! UPDATE NILAI BATAS
                ! Kondisi saat solusi : a < solusi < x-iterasi-sebelumnya
                if (func_dp(a_temp)*func_dp(x_old) < 0) then
                    b_temp = x_old
                ! Kondisi saat solusi : x-iterasi-sebelumnya < solusi < b
                else
                    a_temp = x_old
                end if

                ! Update nilai x dari batas yang baru
                x_dp = (a_temp + b_temp)/2.0
                
                ! Nilai kesalahan relatif semu untuk iterasi ke-i
                tol_i = abs((x_old - x_dp) / x_dp)
                write (*,'(I10, 5(F25.15))') i, a_temp, b_temp, x_dp, func_dp(x_dp), tol_i

                ! Kondisi saat kesalahan relatif semu < toleransi yang ditentukan
                if (tol_i < tolerance_double) then
                    write (*,*) "Sudah mencapai batas toleransi"
                    exit
                end if

                ! Update nilai x dari iterasi ke-i
                x_old = x_dp
            end do
end subroutine bisection