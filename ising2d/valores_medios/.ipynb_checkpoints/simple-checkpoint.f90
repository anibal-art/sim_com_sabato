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
    real(8), parameter :: kB = 1.0            ! kB=1
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

    S = reshape( (/ (merge(1,-1,uni()>0.5), i=1 ,L*L ) /) , shape(s) )

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
        real(8) :: T, Tmin, Tmax, dT, mmedia, emedia, chi, c, frac_acept
        integer :: i, nsteps, npromedio

        Tmin   = 1.55
        Tmax   = 3.05
        dT     = 0.001
        nsteps = nint( (Tmax - Tmin)/dT ) 

        open(newunit=ures, file='results_vs_T.dat', status='replace', action='write')
        write(ures,'(a)') "# T        <M>         <E>           chi            C            frac_acept"

        do i = 0, nsteps
            T = Tmin + dT*i

            if (T <= 2.15) then
                npromedio = 10000
            else if (T <= 2.45) then
                npromedio = 40000
            else
                npromedio = 10000
            end if

            call promedios(T, npromedio, S, mmedia, emedia, chi, c, frac_acept)
            write(ures,'(f8.4,1x,f14.8,1x,f14.8,1x,f14.8,1x,f14.8,1x,f14.8)') &
                T, mmedia, emedia, chi, c, frac_acept
        end do

        close(ures)
    end subroutine run_temperature_scan

    subroutine promedios(T, npromedio, S, mmedia, emedia, chi, c, frac_acept)
        ! Termaliza y calcula promedios a temperatura T
        real(8), intent(in)    :: T
        integer, intent(in)    :: npromedio
        integer, intent(inout) :: S(:,:)
        real(8), intent(out)   :: mmedia, emedia, chi, c, frac_acept

        real(8) :: beta
        integer :: nequilibrio
        real(8) :: prob(2)
        integer :: n

        real(8) :: m_curr, e_curr, dm, de, pp
        real(8) :: sum_abs_m, sum_e, sum_e2, sum_m2
        real(8) :: sum_facc

        sum_facc = 0.0

        beta = 1.0 / T
        nequilibrio = 10000
        prob(1) = exp(-4.0*beta)
        prob(2) = exp(-8.0*beta)
        pp = 1.0 / N

        ! --- Termalización ---
        do n = 1, nequilibrio
            call metropolis(S, prob, dm, de,frac_acept)  ! L^2 intentos
        end do

        ! Estado inicial de la medición
        m_curr = mean_spin(S)                 ! promedio de S
        e_curr = h(S)                         ! energía por partícula

        sum_abs_m = abs(m_curr)
        sum_e     = e_curr
        sum_e2    = e_curr*e_curr
        sum_m2    = m_curr*m_curr

        do n = 2, npromedio
            call metropolis(S, prob, dm, de, frac_acept)
            m_curr = m_curr + dm
            e_curr = e_curr + de
            sum_abs_m = sum_abs_m + abs(m_curr)
            sum_e     = sum_e     + e_curr
            sum_e2    = sum_e2    + e_curr*e_curr
            sum_m2    = sum_m2    + m_curr*m_curr
            sum_facc  = sum_facc  + frac_acept
        end do

        mmedia = sum_abs_m / npromedio       ! <M>
        emedia = sum_e     / npromedio       ! <E>
        chi    = N * beta     * ( (sum_m2/npromedio) - mmedia*mmedia )
        c      = N * beta*beta * ( (sum_e2/npromedio) - emedia*emedia )
        frac_acept = sum_facc / (npromedio-1)
    end subroutine promedios

    subroutine metropolis(S, prob, dm_sum, de_sum, frac_acept)
        integer, intent(inout) :: S(:,:)
        real(8), intent(in)    :: prob(2)        ! casos posibles[exp(-4*beta), exp(-8*beta)]
        real(8), intent(out)   :: dm_sum, de_sum, frac_acept

        integer :: k, a, b, ip, im, jp, jm, nn, dEint, L1, L2, nacc
        real(8) :: r, pp, dm, de

        L1 = size(S,1)
        L2 = size(S,2)
        pp = 1.0 / (L1*L2)

        dm_sum = 0.0
        de_sum = 0.0
        nacc   = 0

        do k = 1, L1*L2
            ! sitio aleatorio 1..L
            a = 1 + int( uni() * L1 )
            b = 1 + int( uni() * L2 )

            im = a - 1
            if (im < 1) then
                im = L1
            end if

            ip = a + 1
            if (ip > L1) then
                ip = 1
            end if

            jm = b - 1
            if (jm < 1) then
                jm = L2
            end if

            jp = b + 1
            if (jp > L2) then
                jp = 1
            end if

            nn    = S(ip,b) + S(im,b) + S(a,jp) + S(a,jm)
            dEint = 2 * S(a,b) * nn

            dm = -2.0 * S(a,b) * pp

            if (dEint == 4) then
                r = uni()
                if (r < prob(1)) then
                    S(a,b) = -S(a,b)
                    de     = 4.0 * pp
                    nacc   = nacc + 1
                else
                    dm = 0.0
                    de = 0.0
                end if

            else if (dEint == 8) then
                r = uni()
                if (r < prob(2)) then
                    S(a,b) = -S(a,b)
                    de     = 8.0 * pp
                    nacc   = nacc + 1
                else
                    dm = 0.0
                    de = 0.0
                end if

            else
                S(a,b) = -S(a,b)
                de     = dEint * pp
                nacc   = nacc + 1
            end if

            dm_sum = dm_sum + dm
            de_sum = de_sum + de
        end do

        frac_acept = nacc / (L1*L2*1.0)
    end subroutine metropolis



    ! la energia de la red por spin
    function h(S) result(e_per_particle)
        integer, intent(in) :: S(:,:)
        real(8) :: e_per_particle
        integer :: i, j, im, jm, L
        integer :: sumE
    
        L = size(S,1) 
        sumE = 0
        do i = 1, L
            do j = 1, L
                im = i - 1
                if (im < 1) then
                    im = L
                end if
    
                jm = j - 1
                if (jm < 1) then
                    jm = L
                end if
    
                sumE = sumE - S(i,j) * ( S(im,j) + S(i,jm) )
            end do
        end do
    
        e_per_particle = real(sumE) / (L*L)  


    ! funcion para calcular el promedio de S
    function mean_spin(S) result(m)
        ! Promedio (magnetización por espín) = mean(S)
        integer, intent(in) :: S(:,:)
        real(8) :: m
        integer :: i, j, L
        integer :: ssum
        L = size(S,1)
        ssum = 0
        do i = 1, L
            do j = 1, L
                ssum = ssum + S(i,j)
            end do
        end do
        m = dble(ssum) / dble(L*L)
    end function mean_spin


end program simple
