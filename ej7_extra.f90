program Ej7_extra
use iso_fortran_env, only: wp => real64
use f95_lapack, only: la_gels

implicit none

real(wp), allocatable :: a(:,:),f(:)
integer i

!gfortran -Wall -o Tpfinal ej7_extra.f90 -I../practica_3/lapack/modules 
!-L../practica_3/lapack/lib -llapack95 -llapack -lblas

allocate(f(23),a(23,3))

f=[5.0291, 6.5099, 5.3666, 4.1272, 4.2948, 6.1261, 10.0502, 9.1614, 7.5677, 7.292, & 
    11.0708, 13.4045, 12.8415, 11.9666, 11.0765, 11.7774, 14.5701, 17.044, 17.0398, &
    15.9069, 15.485, 15.5112, 17.6572]

do i=1,23
    if (6<i.and.i<11) then
        a(i,1)=1
        a(i,2)=i+1
        a(i,3)=sin(real(i+1))
    else if(i<=6) then
        a(i,1)=1
        a(i,2)=i
        a(i,3)=sin(real(i))
    else
        a(i,1)=1
        a(i,2)=i+2
        a(i,3)=sin(real(i+2))
    endif
enddo

call la_gels(a,f)

write(*,*)"ajuste 3:"
write(*,*)""
do i=1,3
    write(*,*) f(i)
enddo


end program