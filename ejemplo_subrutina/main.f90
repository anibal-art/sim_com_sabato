! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino
program ej_subr
implicit none
integer :: a=5
real :: b, c=10

call rutina1(a,b,c)
print*,"a= ",a
print*,"b= ",b
print*,"c= ",c

end program
