! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino

program simple 


    use ziggurat
    implicit none
    logical :: es
    integer :: seed,N,L, i, j, sigma, eps, cont
    real (kind=8) :: aux, Vpot, d, fx, fy, fz
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
        fx = 0
        fy = 0
        fz = 0
        do j = 1, N
            if (i /= j) then
                d = sqrt((r(i,1)-r(j,1))**2+(r(i,2)-r(j,2))**2+(r(i,3)-r(j,3))**2)
                !print *, r(i,:), r(j,:)
                !print *, r(i,:)- r(j,:)
                fx = fx +4*eps*(6*sigma**6 * d**-7 - 12 * sigma**12 * d**-13) * (r(i,1)-r(j,1))/d   
                fy = fy +4*eps*(6*sigma**6 * d**-7 - 12 * sigma**12 * d**-13) * (r(i,2)-r(j,2))/d   
                fz = fz +4*eps*(6*sigma**6 * d**-7 - 12 * sigma**12 * d**-13) * (r(i,3)-r(j,3))/d   
                !print *,i,j, f(i,j)
            f(i,:) = [fx,fy,fz]
            end if
        end do
    end do
    print *, f
            



!! 
!! EDITAR AQUI 
!! 
                        
!    a=0
!    b(:,:) = 1.

! Alocar variables

!    allocate(c(10,10),y(10))


!    do i=1,10
!        do j=1,10
!            c(i,j)=a(i,j)+b(i,j)
!        end do
!    end do

    
!    if(i>5)  then 
 !       a(1,1) =1.
  !      b(2,2)= 0.
   ! end if




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
