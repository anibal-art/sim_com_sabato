! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino

!Problema 1: El objetivo de este ejercicio es aprender a leer y escribir archivos desde fortran,
!utilzando las herramientas vistas en clase. Use como punto de partida el paquete simple.tar.gz
!que se da en la página del curso.

!a) Escriba un programa que genere N = 100 pares de numeros aleatorios (X, Y ) a partir de
!la función uni() de ziggurat.f90, y sólo escriba a pantalla los que cumplen X < Y .

!b) Utilizando los comandos open, read y close, logre que el programa lea de un archivo externo
!(’input.dat’) la cantidad de puntos aleatorios N que se van a generar. No olvide crear el
!archivo ’input.dat’ con su editor preferido1y escribir un número entero en él.

!c) Agregando el comando write genere un archivo de salida (’output.dat’) con todos los núme-
!ros que cumplen la condición descripta en el ítem 1. Grafique los resultados para visualizar
!los puntos y verificar que el programa esté haciendo lo esperado. Considerando que la fun-
!ción uni() da números reales entre 0 y 1, ¿Qué porcentaje de los números N espera que
!sean escritos en el archivo de salida?

!d) Modifique el programa para que guarde todos los números aleatorios en un vector (array)
!y sólo escriba al archivo de salida los que cumplen la condición X < Y y también Y > 0,5.
!Ahora, ¿Qué porcentaje de los números N espera que sean escritos al archivo de salida?

program problema1a 


    use ziggurat
    implicit none
    logical :: es
    integer :: seed,i ,j,k
    real (kind=8) :: x(10),a(10,10),b(10,10)
    real (kind=8), allocatable  :: y(:),c(:,:)


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

! Ej: Número random en [0,1]: uni()


    do i = 1, 100
	x = uni()
	y = uni()
	if (x < y) then
	    print x, y
	end if
    end do




!! 
!! F:IN FIN edicion
!! 
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
         write(10,*) seed
        close(10)
![FIN no Tocar]        


end program problema1a
