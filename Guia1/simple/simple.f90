! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino
! Alumno: Anibal Varela

!Problema 1: El objetivo de este ejercicio es aprender a leer y escribir archivos desde fortran,
!utilzando las herramientas vistas en clase. Use como punto de partida el paquete simple.tar.gz
!que se da en la página del curso.

program simple 


    use ziggurat
    implicit none
    logical :: es
    integer :: seed,i ,j,k, n
    real (kind=8) :: x(10),a(10,10),b(10,10)
    real (kind=8), allocatable  :: y(:),c(:,:)
    real(kind=8) :: d, e
    real(kind=8), allocatable :: Xv(:), Yv(:)

![NO TOCAR] Inicializa generador de número random

    inquire(file='seed.dat',exist=es)
    if(es) then
        open(unit=10,file='seed.dat',status='old')
        read(10,*) seed
        close(10)
        print *,"  * Leyendo semilla de archivo seed.dat"
    else
        seed = 24583490
    end if

    call zigset(seed)
![FIN NO TOCAR]    




!    do i = 1, 500
!       print *,i,uni()
!  end do

!! 
!! EDITAR AQUI 
!! 
                        
!   a=0
!   b(:,:) = 1.

! Alocar variables

!    allocate(c(10,10),y(10))


!    do i=1,10
!        do j=1,10
!            c(i,j)=a(i,j)+b(i,j)
!        end do
!    end do

    
!    if(i>5)  then 
!        a(1,1) =1.
!        b(2,2)= 0.
!    end if
!b) Utilizando los comandos open, read y close, logre que el programa lea de un archivo externo
!(’input.dat’) la cantidad de puntos aleatorios N que se van a generar. No olvide crear el
!archivo ’input.dat’ con su editor preferido1y escribir un número entero en él.
    
    inquire(file='input.dat',exist=es)
    open(unit=10,file='input.dat',status='old')
    read(10,*) n
    close(10)
    print *,"  * La cantidad de numeros leidas de input.dat es: ", n

!b->a) Escriba un programa que genere N = 100 pares de numeros aleatorios (X, Y ) a partir de
!la función uni() de ziggurat.f90, y sólo escriba a pantalla los que cumplen X < Y .



!c) Agregando el comando write genere un archivo de salida (’output.dat’) con todos los núme-
!ros que cumplen la condición descripta en el ítem 1. Grafique los resultados para visualizar
!los puntos y verificar que el programa esté haciendo lo esperado. Considerando que la fun-
!ción uni() da números reales entre 0 y 1, ¿Qué porcentaje de los números N espera que
!sean escritos en el archivo de salida?

    allocate(Xv(n), Yv(n))

    open(unit=10,file='output.dat',status='unknown')
    
	do i = 1, n
        Xv(i) = uni()
        Yv(i) = uni()

		if (Xv(i) < Yv(i) .and. Yv(i) > 0.5d0) then
		    print *, Xv(i), Yv(i)
            write(10,*) Xv(i), Yv(i)

		end if
	end do
    close(10)

!d) Modifique el programa para que guarde todos los números aleatorios en un vector (array)
!y sólo escriba al archivo de salida los que cumplen la condición X < Y y también Y > 0,5.
!Ahora, ¿Qué porcentaje de los números N espera que sean escritos al archivo de salida?



!! 
!! FIN FIN edicion
!! 
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
         write(10,*) seed
        close(10)
![FIN no Tocar]        


end program simple
