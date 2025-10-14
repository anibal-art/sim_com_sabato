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
    real(8), parameter :: J = 1.0D0              ! J=1 (unidades reducidas)
    real(8), parameter :: kB = 1.0D0             ! kB=1
    integer :: i 
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
    !S = 1
    S = reshape( (/ (merge(1,-1,uni()>0.5D0), i=1 ,L*L ) /) , shape(s) )

    call run_temperature_scan(S)   ! genera results_vs_T.dat

    print *, "  * Resultados escritos en results_vs_T.dat"

!! 
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

    subroutine run_temperature_scan(S)
        integer, intent(inout) :: S(:,:)
        ! Construye temp1, temp2, temp3 y ejecuta promedios con npr específicos.
        real(8), allocatable :: temp(:)
        integer :: i, idx, n1, n2, n3
        real(8) :: T, mmedia, emedia, chi, c

        ! Definimos tres tramos para la temperatura 
        n1 = 200
        n2 = 200
        n3 = 200

        allocate(temp(n1+n2+n3))

        call linspace(1.55D0, 2.15D0, n1, temp(1:n1))
        call linspace(2.15D0, 2.45D0, n2, temp(n1+1:n1+n2))
        call linspace(2.45D0, 3.05D0, n3, temp(n1+n2+1:n1+n2+n3))

        open(newunit=ures, file='results_vs_T.dat', status='replace', action='write')
        write(ures,'(a)') "# T        <|M|>         <E>           chi            C"

        do i = 1, n1
            T = temp(i)
            call promedios(T, 3000, S, mmedia, emedia, chi, c)
            write(ures,'(f8.4,1x,f14.8,1x,f14.8,1x,f14.8,1x,f14.8)') T, mmedia, emedia, chi, c
        end do
        !
        do i = 1, n2
            idx = n1 + i
            T = temp(idx)
            call promedios(T, 20000, S, mmedia, emedia, chi, c)
            write(ures,'(f8.4,1x,f14.8,1x,f14.8,1x,f14.8,1x,f14.8)') T, mmedia, emedia, chi, c
        end do
        
        do i = 1, n3
            idx = n1 + n2 + i
            T = temp(idx)
            call promedios(T, 3000, S, mmedia, emedia, chi, c)
            write(ures,'(f8.4,1x,f14.8,1x,f14.8,1x,f14.8,1x,f14.8)') T, mmedia, emedia, chi, c
        end do

        close(ures)
        deallocate(temp)
    end subroutine run_temperature_scan

    subroutine promedios(T, npromedio, S, mmedia, emedia, chi, c)
        ! Termaliza y calcula promedios/observables a temperatura T
        real(8), intent(in)    :: T
        integer, intent(in)    :: npromedio
        integer, intent(inout) :: S(:,:)
        real(8), intent(out)   :: mmedia, emedia, chi, c

        real(8) :: beta
        integer :: nequilibrio
        real(8) :: prob(2)
        integer :: n

        ! Acumuladores (promedios de trayectoria)
        real(8) :: m_curr, e_curr, dm, de, pp
        real(8) :: sum_abs_m, sum_e, sum_e2, sum_m2

        beta = 1.0D0 / T
        nequilibrio = 1000
        prob(1) = exp(-4.0D0*beta)
        prob(2) = exp(-8.0D0*beta)
        pp = 1.0D0 / dble(N)

        ! --- Termalización ---
        do n = 1, nequilibrio
            call metropolis(S, prob, dm, de)  ! L^2 intentos
        end do

        ! Estado inicial de la medición
        m_curr = mean_spin(S)                 ! promedio de S
        e_curr = h(S)                         ! energía por partícula

        sum_abs_m = abs(m_curr)
        sum_e     = e_curr
        sum_e2    = e_curr*e_curr
        sum_m2    = m_curr*m_curr

        ! --- Trayectoria de longitud npromedio ---
        do n = 2, npromedio
            call metropolis(S, prob, dm, de)  ! dm, de ya están por partícula
            m_curr = m_curr + dm
            e_curr = e_curr + de
            sum_abs_m = sum_abs_m + abs(m_curr)
            sum_e     = sum_e     + e_curr
            sum_e2    = sum_e2    + e_curr*e_curr
            sum_m2    = sum_m2    + m_curr*m_curr
        end do

        mmedia = sum_abs_m / dble(npromedio)       ! <M>
        emedia = sum_e     / dble(npromedio)       ! <E>
        chi    = dble(N) * beta     * ( (sum_m2/dble(npromedio)) - mmedia*mmedia )
        c      = dble(N) * beta*beta * ( (sum_e2/dble(npromedio)) - emedia*emedia )
    end subroutine promedios

    subroutine metropolis(S, prob, dm_sum, de_sum)
        ! Aplica L^2 propuestas (un sweep) y suma dm y de
        integer, intent(inout) :: S(:,:)
        real(8), intent(in)    :: prob(2)
        real(8), intent(out)   :: dm_sum, de_sum
        integer :: k
        real(8) :: dm, de

        dm_sum = 0.0D0
        de_sum = 0.0D0
        do k = 1, size(S,1)*size(S,2)
            call metropolis_pre(S, prob, dm, de)
            dm_sum = dm_sum + dm
            de_sum = de_sum + de
        end do
    end subroutine metropolis

    
    subroutine metropolis_pre(S, prob, dm, de)
        ! Un intento de flipear en un lugar aleatorio y aplicar Metropolis
        integer, intent(inout) :: S(:,:)
        real(8), intent(in)    :: prob(2)        ! [exp(-4 beta), exp(-8 beta)]
        real(8), intent(out)   :: dm, de         ! por partícula

        integer :: a, b, ip, im, jp, jm, nn, dEint
        real(8) :: r, pp

        pp = 1.0D0 / dble(size(S,1)*size(S,2))

        ! Sitio aleatorio uniforme 1..L
        a = 1 + int( uni() * size(S,1) )
        b = 1 + int( uni() * size(S,2) )


        ! Vecinos (contorno periódico con MODULO)
        ip = 1 + modulo((a-1)+1, size(S,1))
        im = 1 + modulo((a-1)-1, size(S,1))
        jp = 1 + modulo((b-1)+1, size(S,2))
        jm = 1 + modulo((b-1)-1, size(S,2))

        nn   = S(ip,b) + S(im,b) + S(a,jp) + S(a,jm)
        dEint = 2 * S(a,b) * nn            ! ΔE (entero), J=1

        dm = -2.0D0 * dble(S(a,b)) * pp    ! ΔM/N

        select case (dEint)
        case (4)
            r = uni()
            if (r < prob(1)) then
                S(a,b) = -S(a,b)
                de = 4.0D0 * pp
            else
                dm = 0.0D0
                de = 0.0D0
            end if
        case (8)
            r = uni()
            if (r < prob(2)) then
                S(a,b) = -S(a,b)
                de = 8.0D0 * pp
            else
                dm = 0.0D0
                de = 0.0D0
            end if
        case default
            ! DE <= 0: aceptar siempre
            S(a,b) = -S(a,b)
            de = dble(dEint) * pp
        end select
    end subroutine metropolis_pre

    ! la energia de la red por spin
    function h(S) result(e_per_particle)
        ! Energía por partícula: hamiltoniano con enlaces (i-1,j) y (i,j-1)
        integer, intent(in) :: S(:,:)
        real(8) :: e_per_particle
        integer :: i, j, im, jm, Lloc
        integer :: sumE

        Lloc = size(S,1)
        sumE = 0
        do i = 1, Lloc
            do j = 1, Lloc
                im = 1 + modulo((i-1)-1, Lloc)   ! i-1 con PBC
                jm = 1 + modulo((j-1)-1, Lloc)   ! j-1 con PBC
                sumE = sumE - S(i,j) * ( S(im,j) + S(i,jm) )
            end do
        end do
        e_per_particle = dble(sumE) / dble(Lloc*Lloc)
    end function h

    ! funcion para calcular el promedio de S
    function mean_spin(S) result(m)
        ! Promedio (magnetización por espín) = mean(S)
        integer, intent(in) :: S(:,:)
        real(8) :: m
        integer :: i, j, Lloc
        integer :: ssum
        Lloc = size(S,1)
        ssum = 0
        do i = 1, Lloc
            do j = 1, Lloc
                ssum = ssum + S(i,j)
            end do
        end do
        m = dble(ssum) / dble(Lloc*Lloc)
    end function mean_spin

    ! el linspace de python, masomenos
    subroutine linspace(a, b, n, out)
        real(8), intent(in)  :: a, b
        integer, intent(in)  :: n
        real(8), intent(out) :: out(:)
        integer :: i
        if (n <= 1) then
            if (size(out) >= 1) out(1) = a
            return
        end if
        do i = 1, n
            out(i) = a + (b - a) * dble(i-1) / dble(n-1)
        end do
    end subroutine linspace

    


end program simple