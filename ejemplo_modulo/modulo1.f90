module mod1
implicit none
real(kind=8) :: x=1.0,y=1.0 !Aca defino variables globales a las que tengo
integer ::i,j,i_loop,j_loop !acceso siempre que ponga "use mod1" al principio

contains  !Luego de 'contains' puedo definir ruitnas y funciones
!!!Define Function f_1
real(kind=8) function f_1(a,b)
real(kind=8),intent(in) :: a,b !Notar que estoy definiendo variables
                               !internas en la funcion. a y b solo 
f_1=1/tanh(a)-b/a              !existen dentro de la funcion
end function

!Define Subrutina lectura
subroutine lectura    !Esta rutina no tiene variables internas
open(unit=3,file="input.dat",status='unknown') !modifica las variables globales
read(3,*) i_loop,j_loop ! i_loop,j_loop. No hace falta llamarla con argumentos
close(3)
end subroutine


end module
