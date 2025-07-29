program bmi_main

  use bmiprosumf
  use bmif_2_0
  use, intrinsic :: iso_fortran_env, only : file_unit => input_unit
  implicit none

  character (len=*), parameter :: output_file = "bmiprosumf.out"
  character (len=BMI_MAX_VAR_NAME), parameter :: var2 = "vegetation__nitrogen_availability"
  integer, parameter :: ndims = 1

  type (bmi_prosum) :: model
  integer :: arg_count = 0, s, j  
  integer :: grid2, grid_size2, grid_shape(ndims)
  double precision :: current_time, end_time
  double precision, allocatable :: buf2(:)
  character(len=256) :: arg  

  ! Fix: proper command line argument handling
  arg_count = command_argument_count()
  if (arg_count >= 1) then
    call get_command_argument(1, arg)
  else
    arg = ""
  end if
  
  if (len_trim(arg) == 0) then
     write(*,"(a)") "Usage: run_bmiprosumf CONFIGURATION_FILE"
     stop
  end if

  open(file_unit, file=output_file)

  write(file_unit,"(a)") "Initialize model."
  s = model%initialize("")

  s = model%get_current_time(current_time)
  s = model%get_end_time(end_time)

  ! Simplified version - only test one output variable
  s = model%get_var_grid(var2, grid2)
  s = model%get_grid_size(grid2, grid_size2)
  s = model%get_grid_shape(grid2, grid_shape)
  allocate(buf2(grid_size2))

  do while (current_time <= end_time)
     write(file_unit,"(a, f6.1)") "Model values at time = ", current_time
     s = model%get_value(var2, buf2)
     do j = 1, grid_shape(1)
       write (file_unit,"(a, f10.4)") trim(var2)//":", buf2(j)
     end do
     s = model%update()
     s = model%get_current_time(current_time)
  end do

  deallocate(buf2)
  s = model%finalize()
  write(file_unit,"(a)") "Finalize model."
  close(file_unit)

end program bmi_main
