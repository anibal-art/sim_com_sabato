program problema1a
    use ziggurat
    implicit none
    logical :: es
    integer :: seed,i ,j,k
    real (kind=8) :: x(10),a(10,10),b(10,10)
    real (kind=8), allocatable  :: y(:),c(:,:)

! [NO TOCAR] Inicializa generador de número random
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
! [FIN NO TOCAR]    

    ! N=100 pares de números aleatorios
    do i = 1, 100
        x = uni()
        y = uni()
        if (x < y) then
            write(*,'(I4,2(1X,F10.6))') i, x, y
        end if
    end do

! [NO TOCAR] Guardar semilla
    open(unit=10,file='seed.dat',status='unknown')
    seed = shr3() 
    write(10,*) seed
    close(10)
! [FIN NO TOCAR]        

end program problema1a

