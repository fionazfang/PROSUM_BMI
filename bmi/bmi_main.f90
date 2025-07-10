program bmi_main

  use bmiprosumf
  use bmif_2_0
  use, intrinsic :: iso_fortran_env, only : file_unit => input_unit
  implicit none

  character (len=*), parameter :: output_file = "bmiprosumf.out"
  character (len=BMI_MAX_VAR_NAME), parameter :: var1 = "climate__temperature"
  character (len=BMI_MAX_VAR_NAME), parameter :: var2 = "vegetation__nitrogen_availability"
  integer, parameter :: ndims = 1

  type (bmi_prosum) :: model
  integer :: arg_count = 0
  character (len=80) :: arg
  integer :: i, j, s
  integer :: grid1, grid2, grid_size1, grid_size2, grid_shape(ndims)
  double precision :: current_time, end_time
  double precision, allocatable :: buf1(:), buf2(:)

  do while (arg_count <= 1)
    call get_command_argument(arg_count, arg)
    arg_count = arg_count + 1
  end do
  if (len_trim(arg) == 0) then
     write(*,"(a)") "Usage: run_bmiprosumf CONFIGURATION_FILE"
     stop
  end if

  open(file_unit, file=output_file)

  write(file_unit,"(a)") "Initialize model."
  s = model%initialize(arg)

  s = model%get_current_time(current_time)
  s = model%get_end_time(end_time)

  s = model%get_var_grid(var1, grid1)
  s = model%get_grid_size(grid1, grid_size1)
  s = model%get_grid_shape(grid1, grid_shape)
  allocate(buf1(grid_size1))

  s = model%get_var_grid(var2, grid2)
  s = model%get_grid_size(grid2, grid_size2)
  allocate(buf2(grid_size2))

  do while (current_time <= end_time)
     write(file_unit,"(a, f6.1)") "Model values at time = ", current_time
     s = model%get_value(var1, buf1)
     do j = 1, grid_shape(1)
       write (file_unit,"(a, f10.4)") trim(var1)//":", buf1(j)
     end do
     s = model%get_value(var2, buf2)
     do j = 1, grid_shape(1)
       write (file_unit,"(a, f10.4)") trim(var2)//":", buf2(j)
     end do
     s = model%update()
     s = model%get_current_time(current_time)
  end do

  deallocate(buf1, buf2)
  s = model%finalize()
  write(file_unit,"(a)") "Finalize model."
  close(file_unit)

end program bmi_main
