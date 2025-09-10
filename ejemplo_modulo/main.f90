! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino

program ejemplo_modulo
use mod1 !Uso el modulo mod1, no necesito definir variables globales.
         !Tambien puedo usar las rutinas y funciones definidas en mod1
implicit none

call lectura !llamo a la rutina lecura, definida en modulo1.f90 
             !Aunque no la llame con argumentos, esta rutina modifica las
             !variables globales 'i_loop' y 'j_loop'
do i =1,i_loop
    do j=1,j_loop
        print*, "f(",x,",",y,") = ",f_1(x,y) !llamo a la funcion f_1
        x=x+1.                               !que esta definida en el modulo
    end do
    y=y+1.
end do
end program
