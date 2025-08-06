Program Ejercicio_3
use iso_fortran_env, only: wp => real64
use f95_lapack, only: la_gesv
!también se puede resolver con la_gbsv para matrices banda 
!la cual tiene un costo computacional menor o como una matriz simetrica

implicit none

real(wp), allocatable :: a(:,:),f(:),k(:),x(:),A1(:,:)
integer i,j,n

!compilador
!gfortran -Wall -o Tpfinal ej3.f90 -I./modules -L./lib -llapack95 -llapack -lblas

open(10,file="matriz_K.dat")
!matriz_K.dat es un archivo para escribir la matriz A y poder visualizarla mejor que en la terminal

n=20 !numero de particulas y cantidad de filas y columnas

allocate(a(n,n),f(n),k(n+1),x(n),A1(n,n))

!defino las constantes segun lo indicado en el ejercicio
do i=1,n+1
    k(i)=1
enddo

do i=1,n
!declaro un if para los extremos de la matriz
if (i==1) then
    a(1,1)=k(1)+k(2)
    a(1,2)=-k(2)
else if (i==n) then
    a(n,n-1)=-k(n)
    a(n,n)=k(n)+k(n+1)
else
    a(i,i-1)= -k(i)
    a(i,i)= k(i)+k(i+1)
    a(i,i+1)= -k(i+1)
endif !aquellos no definidos seran ceros
enddo

do i=1,n
A1(i,:)=a(i,:) !guardo los datos para verificar
enddo

!defino las fuerzas segun lo indicado en el ejercicio
f(5)=1
f(16)=-1

!escribo la matriz A en el archivo nuevo que abri con el indice 10
do i=1,n
    write(10,*) (a(i,j),j=1,n)
enddo
close(10)

call la_gesv(a,f)

!a me devuelve la factorizacion LU
!f me devuelve la solucion

do i=1,n
    write(*,*) "la nueva posicion x",i,"es: ",f(i)
enddo

write(*,*)"verificacion"
!solucion original f 
do i=1,n
do j=1,n
    x(i)=x(i)+A1(i,j)*f(j)
enddo
write(*,*)"la fuerza",i,"es: ",x(i)
enddo

end program
