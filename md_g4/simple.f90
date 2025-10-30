! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino

program simple 


    use ziggurat
    implicit none
    logical :: es
    integer :: seed,N,L, i, j, sigma, eps, cont
    real (kind=8) :: aux, Vpot, d, fx, fy, fz, f_abs, dx, dy, dz 
    real (kind=8) :: rc, rc2, vcut
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


    N=100
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

    rc = 2.5*sigma
    rc2 = rc*rc
    vcut = 4*eps*((sigma/rc)**12 - (sigma/rc)**6)

    do i = 1, N
        do j = 1, i-1

            dx = r(i,1) - r(j,1)
            dy = r(i,2) - r(j,2)
            dz = r(i,3) - r(j,3)

            if (dx >  L/2) dx = dx - L
            if (dx < -L/2) dx = dx + L
            if (dy >  L/2) dy = dy - L
            if (dy < -L/2) dy = dy + L
            if (dz >  L/2) dz = dz - L
            if (dz < -L/2) dz = dz + L

            d = sqrt((dx)**2+(dy)**2+(dz)**2)

            if (d > rc) cycle

            Vpot = Vpot+4*eps*(-(sigma/d)**6 + (sigma/d)**12) - vcut

        end do
    end do

    f=0.0
    do i = 1, N
        do j = 1, i-1
            fx = 0
            fy = 0
            fz = 0
            if (i /= j) then

                dx = r(i,1) - r(j,1)
                dy = r(i,2) - r(j,2)
                dz = r(i,3) - r(j,3)

                if (dx >  L/2) dx = dx - L
                if (dx < -L/2) dx = dx + L
                if (dy >  L/2) dy = dy - L
                if (dy < -L/2) dy = dy + L
                if (dz >  L/2) dz = dz - L
                if (dz < -L/2) dz = dz + L
    
                d = sqrt((dx)**2+(dy)**2+(dz)**2)

                if (d > rc) cycle

                f_abs = 24*eps*(-(sigma/d)**6 + 2 * (sigma/d)**12)/(d*d)
                
                fx = f_abs * dx  
                fy = f_abs * dy   
                fz = f_abs * dz   

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
