! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino

program simple 


    use ziggurat
    implicit none
    logical :: es
    integer :: seed,N,L, i, j, sigma, eps, cont
    real (kind=8) :: aux, Vpot, d, fx, fy, fz, f_abs
    real (kind=8), allocatable  :: y(:), c(:,:), r(:,:),v(:,:),f(:,:)


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


    N=10
    L=1


    allocate(r(N,3),v(N,3),f(N,3))
    !print *,r
! Ej: Número random en [0,1]: uni()

    do i = 1, N
        do j = 1, 3
            aux=uni()*L
            !print *,i,j, aux
            r(i,j) = aux
        end do
    end do
    !print *,r(:,:)

    Vpot=0.0
    eps=1
    sigma=1
    !cont=0
    do i = 1, N
        do j = 1, i-1
            !cont=cont+1
            d = sqrt((r(i,1)-r(j,1))**2+(r(i,2)-r(j,2))**2+(r(i,3)-r(j,3))**2)
            !print *,4*eps*(-(sigma/d)**6 + (sigma/d)**12)   
            Vpot=Vpot+4*eps*(-(sigma/d)**6 + (sigma/d)**12) 
            !print *,i,j, d, Vpot
        end do
    end do
    !print *, cont

    do i = 1, N
        do j = 1, i-1
            fx = 0
            fy = 0
            fz = 0
            if (i /= j) then
                d = sqrt((r(i,1)-r(j,1))**2+(r(i,2)-r(j,2))**2+(r(i,3)-r(j,3))**2)
                f_abs = 24*eps*(-(sigma/d)**6 + 2 * (sigma/d)**12)/(d*d)
                
                fx = f_abs * (r(i,1)-r(j,1))   
                fy = f_abs * (r(i,2)-r(j,2))   
                fz = f_abs * (r(i,3)-r(j,3))   

                f(i,1) = f(i,1) + fx
                f(i,2) = f(i,2) + fy
                f(i,3) = f(i,3) + fz

                f(j,1) = f(j,1) - fx
                f(j,2) = f(j,2) - fy
                f(j,3) = f(j,3) - fz

            !f(i,:) = [fx,fy,fz]
            end if
        end do
    end do
    print *, f
            






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
