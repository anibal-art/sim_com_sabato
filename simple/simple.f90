! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino

program simple 


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


        do i = 1, 500
            print *,i,uni()
        end do

!! 
!! EDITAR AQUI 
!! 
                        
    a=0
    b(:,:) = 1.

! Alocar variables

    allocate(c(10,10),y(10))


    do i=1,10
        do j=1,10
            c(i,j)=a(i,j)+b(i,j)
        end do
    end do

    
    if(i>5)  then 
        a(1,1) =1.
        b(2,2)= 0.
    end if




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


end program simple
