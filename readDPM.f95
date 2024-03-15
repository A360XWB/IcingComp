program readDPM
  implicit none
  character(len = 50) :: filename = "TestData.dpm" ! Specify your input file name
  integer, parameter :: num_col = 13, num_row = 10000000
  integer :: iostat, i, len_str
  character(len = 500) :: string
  character(len = 20) :: properties_C

  ! Store properties from xyz, uvw, diameter, t, parcel-mass, mass, n-in-parcel, time, to flow time
  real*8 properties   ! 13 properties in total
  dimension properties (0 : num_col, 0 : num_row)

  ! Open the file for reading
  open(unit = 10, file = filename, status = 'old', action = 'read', iostat = iostat)
  if (iostat /= 0) then
    write(*, *) "Error opening file ", trim(filename)
    stop
  end if
  ! Neglect first two lines
  read (10, *)
  read (10, *)
  ! Read the space-separated strings
  do
    read(10, '(A)', iostat=iostat) string
    if (iostat /= 0) then 
      exit  ! Exit loop at the end of the file
    elseif (trim(string) == ')') then 
      exit
    end if
    write(*, *) string
!go to 777
    len_str = len_trim(string)
    do i = 0, len_str
      if (string(i:i) == ")") then 
        exit
      else if (string(i:i) >= '0' .and. string(i:i) <= '9') then
        properties_C = string(i : i + 9)
        call my_incr(i, 9)
      else if (string(i:i) == '-') then
        properties_C = string(i : i + 10)
        call my_incr(i, 10)
      end if
      write(*, *) properties_C
    end do
  end do
!777 continue
  ! Close the file
  close(10)
end program readDPM

subroutine my_incr(var, incr)
  integer,intent(inout) :: var
  integer,intent(in)    :: incr
  var = var + incr
end subroutine