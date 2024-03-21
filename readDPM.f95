program readDPM
  implicit none
  character(len = 50) :: filename = "800plane-11=4.5-test.dpm" ! Specify your input file name
  integer, parameter :: num_col = 12, num_row = 10000000
  integer :: iostat, i, j, k, len_str
  character(len = 500) :: string
  character(len = 20) :: properties_C
<<<<<<< HEAD
  
  real :: properties 
  dimension properties (0 : num_row, 0 : num_col)
=======
  real*8 :: properties 
  !dimension properties (0 : num_col, 0 : num_row)
>>>>>>> e0001016d8e5dd7c17143fb3386e8c8038ee0098
  
  ! Store properties from xyz, uvw, diameter, t, parcel-mass, mass, n-in-parcel, time, to flow time
  ! 13 properties in total


  ! Open the file for reading
  open(unit = 10, file = filename, status = 'old', action = 'read', iostat = iostat)
  if (iostat /= 0) then
    write(*, *) "Error opening file ", trim(filename)
    stop
  end if
  ! Neglect first two lines
  read (10, *)
  read (10, *)
  
  j = 0
  do
    write(*, *) 'j = ', j
    read(10, '(A)', iostat=iostat) string   ! Read the space-separated strings
    if (iostat /= 0) then 
      exit  ! Exit loop at the end of the file
    elseif (trim(string) == ')') then 
      exit
    end if
    !write(*, *) string ! string is being read
!go to 777
    len_str = len_trim(string) ! extract data from the string
    k = 0
    do i = 0, len_str
      if (k == num_col + 1) then
        exit
      else if (string(i:i) >= '0' .and. string(i:i) <= '9') then
        properties_C = string(i : i + 9)
<<<<<<< HEAD
        read(properties_C, *) properties (j, k)
=======
        read(properties_C, *) properties
        write(*, *) properties
        !write(*, *) properties_C
>>>>>>> e0001016d8e5dd7c17143fb3386e8c8038ee0098
        call my_incr(i, 9)
        call my_incr(k, 1)
      else if (string(i:i) == '-') then
        properties_C = string(i : i + 10)
<<<<<<< HEAD
        read(properties_C, *) properties (j, k)
=======
        read(properties_C, *) properties
        write(*, *) properties
        !write(*, *) properties_C
>>>>>>> e0001016d8e5dd7c17143fb3386e8c8038ee0098
        call my_incr(i, 10)
        call my_incr(k, 1)
      end if
<<<<<<< HEAD
      !write(*, *) i, j, k, ' element: ', properties_C 
=======
>>>>>>> e0001016d8e5dd7c17143fb3386e8c8038ee0098
    end do
    call my_incr(j, 1)
  end do
!777 continue
  ! Close the file
  close(10)

<<<<<<< HEAD
do i = 0, 30
  do k = 0, num_col
    write(*, *) 'P(', i, ', ', k, ') = ', properties(i, k)    
  end do
end do

!write(*, *) 'P(0, 5) * P(0, 6) = ', properties(0, 5) * properties(0, 6)
=======
>>>>>>> e0001016d8e5dd7c17143fb3386e8c8038ee0098

end program readDPM

subroutine my_incr(var, incr)
  integer,intent(inout) :: var
  integer,intent(in)    :: incr
  var = var + incr
end subroutine