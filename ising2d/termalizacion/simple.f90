! Introducción a la Simulación Computacional
! Edición: 2025
! Docentes: Joaquín Torres y Claudio Pastorino
! Alumno: Anibal Varela

program simple

    use ziggurat
    implicit none

    ! Varialbes

    logical :: es, input_exists
    integer :: seed, gridsize

    ! Parámetros de la red y el hamiltoniano
    integer, parameter :: L = 30                 ! lado de la red
    integer, parameter :: N = L*L
    real(8), parameter :: J = 1.0              ! J=1 (unidades reducidas)
    real(8), parameter :: kB = 1.0             ! kB=1
    integer :: i 
    !real(8), parameter :: T = 1.0           
    real(8) :: T
    integer  :: k, k_min, k_max, dk

    real(8) :: mmedia, emedia, chi, c
    character(len=128) :: fname
    character(len=128) :: fnameM



    ! Estado Ising
    
    !integer :: S(L,L)     ! espines en {+1,-1}
    integer, allocatable :: S(:,:)

    ! Salida
    integer :: ures

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

! Estado incial de la red
    allocate(S(1:L,1:L))
    


    k_min = 1
    k_max = 10
    dk    = 1

    do k = k_min, k_max, dk
        S = reshape( (/ (merge(1,-1,uni()>0.5), i=1 ,L*L ) /) , shape(s) )
        T = 0.5 * dble(k)         ! 0.5, 1.0, 1.5, ..., 5.0
        call termalizacion(T, S)    ! tu rutina que termaliza/guarda E vs paso
    end do
    print *, "  * Resultados escritos en E_thermalize.dat"
    
!! 
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
         write(10,*) seed
        close(10)
![FIN no Tocar]        


    deallocate(S)

contains
    !character(len=128) :: fname, fnameM  

    subroutine termalizacion(T, S)
        ! Termaliza a temperatura T
        real(8), intent(in)    :: T
        integer, intent(inout) :: S(:,:)

        real(8) :: beta
        integer :: nequilibrio
        real(8) :: prob(2)
        integer :: n

        ! Acumuladores (promedios de trayectoria)
        real(8) :: m_curr, e_curr, dm, de, pp
        real(8) :: sum_abs_m, sum_e, sum_e2, sum_m2

        beta = 1.0 / T
        nequilibrio = 1000000
        prob(1) = exp(-4.0*beta)
        prob(2) = exp(-8.0*beta)
        pp = 1.0 / dble(N)


        ! --- Termalización ---
        ! --- Termalización ---

        ! Archivos de salida por temperatura
        write(fname,  '("E_thermalize_",F4.2,".dat")') T
        open(99, file=trim(adjustl(fname)), status='replace', action='write')

        write(fnameM, '("M_thermalize_",F4.2,".dat")') T
        open(98, file=trim(adjustl(fnameM)), status='replace', action='write')

        ! n = 0: estado inicial antes de cualquier sweep
        e_curr = h(S)
        m_curr = mean_spin(S)
        write(99,'(i8,1x,f14.8)') 0, e_curr
        write(98,'(i8,1x,f14.8)') 0, m_curr

        do n = 1, nequilibrio
            call metropolis(S, prob, dm, de)  ! dm, de son POR PARTÍCULA (ΔM/N, ΔE/N) en un sweep
            m_curr = m_curr + dm              ! actualiza magnetización por sitio
            e_curr = e_curr + de              ! (más barato que recalcular h(S) cada vez)
            write(99,'(i8,1x,f14.8)') n, e_curr
            write(98,'(i8,1x,f14.8)') n, m_curr
        end do

        close(99)
        close(98)

        !write(fname, '("E_thermalize_",F4.2, ".dat")') T  
        !open(99, file=trim(adjustl(fname)), status='replace', action='write')
        !open(99, file='E_thermalize.dat', status='replace', action='write')
        !do n = 1, nequilibrio
        !    call metropolis(S, prob, dm, de)   ! L^2 propuestas
        !    e_curr = h(S)                      ! energía por partícula tras este sweep
        !    write(99,'(i8,1x,f14.8)') n, e_curr
        !end do
        !close(99)

    end subroutine termalizacion

    subroutine metropolis(S, prob, dm_sum, de_sum)
        ! Aplica L^2 propuestas (un sweep) y suma dm y de
        integer, intent(inout) :: S(:,:)
        real(8), intent(in)    :: prob(2)
        real(8), intent(out)   :: dm_sum, de_sum
        integer :: k
        real(8) :: dm, de

        dm_sum = 0.0
        de_sum = 0.0
        do k = 1, size(S,1)*size(S,2)
            call metropolis_pre(S, prob, dm, de)
            dm_sum = dm_sum + dm
            de_sum = de_sum + de
        end do
    end subroutine metropolis

    
    subroutine metropolis_pre(S, prob, dm, de)
        
        integer, intent(inout) :: S(:,:)
        real(8), intent(in)    :: prob(2)        ! [exp(-4 beta), exp(-8 beta)]
        real(8), intent(out)   :: dm, de         ! por partícula

        integer :: a, b, ip, im, jp, jm, nn, dEint, L1, L2
        real(8) :: r, pp

        L1 = size(S,1);  L2 = size(S,2)
        pp = 1.0 / dble(L1*L2)

        a = 1 + int( uni() * L1 )
        b = 1 + int( uni() * L2 )

        im = a - 1;  if (im < 1)  im = L1
        ip = a + 1;  if (ip > L1) ip = 1
        jm = b - 1;  if (jm < 1)  jm = L2
        jp = b + 1;  if (jp > L2) jp = 1

        nn    = S(ip,b) + S(im,b) + S(a,jp) + S(a,jm)
        dEint = 2 * S(a,b) * nn             

        dm = -2.0 * dble(S(a,b)) * pp     

        select case (dEint)
        case (4)
            r = uni()
            if (r < prob(1)) then
                S(a,b) = -S(a,b)
                de = 4.0 * pp
            else
                dm = 0.0
                de = 0.0
            end if
        case (8)
            r = uni()
            if (r < prob(2)) then
                S(a,b) = -S(a,b)
                de = 8.0 * pp
            else
                dm = 0.0
                de = 0.0
            end if
        case default
            ! ΔE <= 0: aceptar siempre
            S(a,b) = -S(a,b)
            de = dble(dEint) * pp
        end select
    end subroutine metropolis_pre

    ! la energia de la red por spin
    function h(S) result(e_per_particle)
        integer, intent(in) :: S(:,:)
        real(8) :: e_per_particle
        integer :: i, j, im, jm, L
        integer :: sumE

        L = size(S,1) !lado de la red
        sumE = 0
        do i = 1, L
            do j = 1, L
                im = i - 1; if (im < 1) im = L   !filas con contorno periódico
                jm = j - 1; if (jm < 1) jm = L   !columnas con contorno periódico
                sumE = sumE - S(i,j) * ( S(im,j) + S(i,jm) )
            end do
        end do
        e_per_particle = dble(sumE) / dble(L*L)
    end function h

    function mean_spin(S) result(m)
        integer, intent(in) :: S(:,:)
        real(8) :: m
        integer :: i, j, Lloc, ssum
        Lloc = size(S,1)
        ssum = 0
        do i = 1, Lloc
            do j = 1, Lloc
                ssum = ssum + S(i,j)
            end do
        end do
        m = dble(ssum) / dble(Lloc*Lloc)
    end function mean_spin


end program simple