program Ej5_6
use iso_fortran_env, only: wp => real64
use f95_lapack, only: la_gels

implicit none

real(wp), allocatable :: a(:,:),f(:)
integer i

!compilador
!gfortran -Wall -o Tpfinal ej5-6.f90 -I./modules -L./lib -llapack95 -llapack -lblas

allocate(f(25),a(25,2))

!defino la tabla dada 

f=[5.0291, 6.5099, 5.3666, 4.1272, 4.2948, 6.1261, 12.514, 10.0502, 9.1614, 7.5677, 7.292, & 
    10.0357,11.0708, 13.4045, 12.8415, 11.9666, 11.0765, 11.7774, 14.5701, 17.044, 17.0398, &
    15.9069, 15.485, 15.5112, 17.6572]

!Armo matriz A de Ax=b con A la matriz diseño de una recta evaluada cada 1 seg

!asigno los valores a la matriz A
do i=1,25
    a(i,1)=1
    a(i,2)=i
enddo

call la_gels(a,f)

!imprimo los coeficientes del ajuste 0
write(*,*)"ajuste0:"
write(*,*)""
do i=1,2
    write(*,*) f(i)
enddo

!veo en python que el valor atipico se da a t=7
!lo elimino y lo vuelvo a ajustar

deallocate(f,a)
allocate(f(24),a(24,2))

f=[5.0291, 6.5099, 5.3666, 4.1272, 4.2948, 6.1261, 10.0502, 9.1614, 7.5677, 7.292, & 
    10.0357,11.0708, 13.4045, 12.8415, 11.9666, 11.0765, 11.7774, 14.5701, 17.044, 17.0398, &
    15.9069, 15.485, 15.5112, 17.6572]

do i=1,24
    if (i>6) then !al borrar el t=7s hay un salto de fila  
        a(i,1)=1
        a(i,2)=i+1
    else
        a(i,1)=1
        a(i,2)=i
    endif
enddo

call la_gels(a,f)

!imprimo los coeficientes del ajuste 1
write(*,*)"ajuste 1:"
write(*,*)""
do i=1,2
    write(*,*) f(i)
enddo


end program