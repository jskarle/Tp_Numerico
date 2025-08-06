Program Ejercicio_4
use iso_fortran_env, only: wp => real64
use f95_lapack, only: la_syev
!también se puede resolver con la_gbsv para matrices banda 
!la cual tiene un costo computacional menor o como una matriz simetrica

implicit none

real(wp), allocatable :: a(:,:),C(:),k(:),m(:),L(:)
integer i,j,n
character yn

!compilador
!gfortran -Wall -o Tpfinal ej4.f90 -I./modules -L./lib -llapack95 -llapack -lblas

yn="Y"
do while (yn=="Y") !condicion para luego ejecutar el programa de 
!nuevo con distintos numeros de particula (n)

open(10,file="matriz_A.dat")

write(*,*)"cantidad de particulas"
read(*,*)n

allocate(a(n,n),C(n),k(n+1),m(n),L(n))

!defino los valores de k_i
do i=1,n+1
    k(i)=1
enddo

!defino los valores de m_i
do i=1,n
    m(i)=1
enddo

do i=1,n
!declaro un if para los extremos de la matriz
if (i==1) then
    a(1,1)=(k(1)+k(2))/m(1)
    a(1,2)=(-k(2))/m(1)
else if (i==n) then
    a(n,n-1)=(-k(n))/m(n)
    a(n,n)=(k(n)+k(n+1))/m(n)
else
    a(i,i-1)= (-k(i))/m(i)
    a(i,i)= (k(i)+k(i+1))/m(i)
    a(i,i+1)= (-k(i+2))/m(i)
endif !aquellos no definidos seran ceros
enddo

!escribo la matriz A en el archivo nuevo que abri con el indice 10
! IMPORTANTE: cada vez que cambie el n se reescribira
do i=1,n
    write(10,*) (a(i,j),j=1,n)
enddo
close(10)

call la_syev(a,L)

!a me devuelve la factorizacion LU
!L me devuelve los autosvalores

write(*,*)"autovalores"
do i=1,n
    write(*,*)L(i)
enddo

!los autovalores son igual a la frecuencia al cuadrado
write(*,*)"frecuencias"
do i=1,n
    write(*,*) sqrt(L(i))
enddo

!borro los tamaños de las matrices si deseo cambiar el n
write(*,*)"queres hacer otro ejercicio? (indicar Y/N)"
read(*,*)yn
if (yn=="Y") then
    deallocate(a)
    deallocate(c)
    deallocate(k)
    deallocate(m)
    deallocate(L)
else if(yn=="y") then
    deallocate(a)
    deallocate(c)
    deallocate(k)
    deallocate(m)
    deallocate(L)
    yn="Y"
endif
enddo

end program
