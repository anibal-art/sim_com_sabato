! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino

program simple 


    use ziggurat
    implicit none
    logical :: es
    integer :: seed,N, i, j 
    real (kind=8) :: sigma, eps, cont, L
    real (kind=8) :: aux, Vpot, d, fx, fy, fz, f_abs, dx, dy, dz 
    real (kind=8) :: rc, rc2, vcut
    real (kind=8), allocatable  :: y(:), c(:,:), r(:,:),v(:,:),f(:,:)
    integer :: step, nsteps
    real(kind=8) :: m, dt
    integer :: uE, uX

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
    L=3

    allocate(r(N,3),v(N,3),f(N,3))

    ! Estado inicial aleatorio con una distribucion uniforme
    do i = 1, N
        do j = 1, 3
            aux=uni()*L
            r(i,j) = aux
        end do
    end do


    Vpot = 0.0
    eps = 1
    sigma = 1

    rc = 2.5*sigma
    rc2 = rc*rc
    vcut = 4*eps*((sigma/rc)**12 - (sigma/rc)**6)


!! 
!! FIN FIN edicion
!! 
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

!        open(unit=10,file='seed.dat',status='unknown')
!        seed = shr3() 
!         write(10,*) seed
!        close(10)
![FIN no Tocar]        

    m        = 2        ! masa efectiva
    dt       = 0.1   ! “paso” del descenso
    nsteps   = 10000     ! cantidad de pasos

    ! Archivos de salida
    uE = 20; uX = 30
    open(uE, file='energia.dat', status='replace')   ! paso  E_pot
    open(uX, file='traj.xyz',   status='replace')

    ! Escribir estado inicial
    write(uE,'(i8,1x,es18.10)') 0, Vpot
    call dump_xyz(uX, r, N, L, 0)

    !  trayectorias en la direccion de la fuerza con las condiciones de contorno 
    do step = 1, nsteps
        ! (1) r <- r + 1/2 (f/m) dt^2  + PBC
        do i = 1, N
            r(i,1) = r(i,1) + 0.5 * (f(i,1)/m) * dt*dt
            r(i,2) = r(i,2) + 0.5 * (f(i,2)/m) * dt*dt
            r(i,3) = r(i,3) + 0.5 * (f(i,3)/m) * dt*dt
            if (r(i,1) >= L) r(i,1) = r(i,1) - L
            if (r(i,1) <  0) r(i,1) = r(i,1) + L
            if (r(i,2) >= L) r(i,2) = r(i,2) - L
            if (r(i,2) <  0) r(i,2) = r(i,2) + L
            if (r(i,3) >= L) r(i,3) = r(i,3) - L
            if (r(i,3) <  0) r(i,3) = r(i,3) + L
        end do

        ! (2) Recalcular fuerzas y energía en r(t+dt)
        call compute_forces_energy(r, f, Vpot, N, L, eps, sigma, rc, vcut)


        ! (3) Log de energía y frame para VMD
        write(uE,'(i8,1x,es18.10)') step, Vpot
        call dump_xyz(uX, r, N, L, step)
    end do

    close(uE); close(uX)
    

    print *, 'Listo. Archivos: energia.dat y traj.xyz'

!================ [NO TOCAR] Guardar semilla ===============
    open(unit=10,file='seed.dat',status='unknown')
    seed = shr3()
    write(10,*) seed
    close(10)
!===========================================================

contains

    subroutine compute_forces_energy(r, f, Vpot, N, L, eps, sigma, rc, vcut)
        implicit none
        integer, intent(in) :: N
        real(kind=8) :: L
        real(kind=8), intent(in) :: eps, sigma, rc, vcut
        real(kind=8), intent(inout) :: r(N,3)
        real(kind=8), intent(out) :: f(N,3)
        real(kind=8), intent(out) :: Vpot
        integer :: i, j
        real(kind=8) :: dx, dy, dz, d, f_abs

        f = 0
        Vpot = 0

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

                d = sqrt(dx*dx + dy*dy + dz*dz)
                if (d > rc) cycle
                if (d > 1e-8) then
                    Vpot = Vpot + 4*eps*(-(sigma/d)**6 + (sigma/d)**12) - vcut

                    f_abs = 24*eps*(-(sigma/d)**6 + 2*(sigma/d)**12)/(d*d)
                    fx = f_abs*dx;  fy = f_abs*dy;  fz = f_abs*dz

                    f(i,1)=f(i,1)+fx; f(i,2) = f(i,2)+fy; f(i,3) = f(i,3)+fz
                    f(j,1)=f(j,1)-fx; f(j,2) = f(j,2)-fy; f(j,3) = f(j,3)-fz
                end if
            end do
        end do
    end subroutine compute_forces_energy

    ! Escribe un frame XYZ para VMD
    subroutine dump_xyz(u, r, N, L, step)
        implicit none
        integer, intent(in) :: u, N, step
        real(kind=8), intent(in) :: r(N,3)
        real(kind=8) :: L
        integer :: i
        write(u,'(i0)') N
        write(u,'(a,i0,a,es12.4)') 'step ', step, '  L=', L

        do i = 1, N
            write(u,'(a,3(1x,f12.6))') 'Ar', r(i,1), r(i,2), r(i,3)
        end do
    end subroutine dump_xyz

end program simple
