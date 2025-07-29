Program Ejercicio_2
use iso_fortran_env, only: wp => real64
use roots, only: biseccion

implicit none

real(wp) :: a,b, tol, raiz
integer :: n, clave

!compilador 
!gfortran -Wall ej2.f90 roots.o -o Tpfinal


!primero resuelvo donde estan los extremos locales de mi funcion hallando
!donde f'(T)=0, luego aplico bisección en la funcion original para 
!comprobar si corta al eje x entre los extremos y hallar la raiz con un metodo sucesivo

a=1_wp
b=(3480.3_wp*log(10.0_wp))/5.081_wp
n=100
tol=0.5e-3_wp

call biseccion(f, a, b, n, tol, raiz, clave)

if (clave == 0) then
write(*,*) "1º Raíz de f= ", raiz
write(*,*) "Iteraciones realizadas =", n
else
write(*,*) "Error =", clave
endif

a=b
b=b**2_wp
n=100
tol=0.5e-2_wp

call biseccion(f, a, b, n, tol, raiz, clave)

if (clave == 0) then
write(*,*) "2º Raíz de f= ", raiz
write(*,*) "Iteraciones realizadas =", n
else
write(*,*) "Error =", clave
endif

contains

    real(wp) function f(T)
    real(wp), intent(in) :: T
        f= 21.1306_wp - (3480.3_wp)/T - (5.081_wp)*log10(T)
    end function f

end program