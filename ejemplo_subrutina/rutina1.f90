subroutine rutina1(x,y,z)
implicit none
integer :: i
integer,intent(in) :: x
real,intent(inout) :: y

real,intent(out) :: z
do i=1,x
    z=z+1.0
end do

y=z**3

end subroutine
