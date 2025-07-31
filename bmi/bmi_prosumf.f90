module bmiprosumf

  use bmif_2_0
  use PROSUM_module
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  
  private
  public :: bmi_prosum

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "PROSUM (Plant Productivity and Succession Model)"

  ! Exchange items
  integer, parameter :: input_item_count = 0      ! In the first version I will not expose any input variable
  integer, parameter :: output_item_count = 5
  character (len=BMI_MAX_VAR_NAME), target, dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, dimension(output_item_count) :: output_items
  

  type, extends (bmi) :: bmi_prosum
     private
   contains
     procedure :: get_component_name => prosum_component_name
     procedure :: get_input_item_count => prosum_input_item_count
     procedure :: get_output_item_count => prosum_output_item_count
     procedure :: get_input_var_names => prosum_input_var_names
     procedure :: get_output_var_names => prosum_output_var_names
     procedure :: initialize => prosum_initialize
     procedure :: finalize => prosum_finalize
     procedure :: get_start_time => prosum_start_time
     procedure :: get_end_time => prosum_end_time
     procedure :: get_current_time => prosum_current_time
     procedure :: get_time_step => prosum_time_step
     procedure :: get_time_units => prosum_time_units
     procedure :: update => prosum_update
     procedure :: update_until => prosum_update_until
     procedure :: get_var_grid => prosum_var_grid
     procedure :: get_grid_type => prosum_grid_type
     procedure :: get_grid_rank => prosum_grid_rank
     procedure :: get_grid_shape => prosum_grid_shape
     procedure :: get_grid_size => prosum_grid_size
     procedure :: get_grid_spacing => prosum_grid_spacing
     procedure :: get_grid_origin => prosum_grid_origin
     procedure :: get_grid_x => prosum_grid_x
     procedure :: get_grid_y => prosum_grid_y
     procedure :: get_grid_z => prosum_grid_z
     procedure :: get_grid_node_count => prosum_grid_node_count
     procedure :: get_grid_edge_count => prosum_grid_edge_count
     procedure :: get_grid_face_count => prosum_grid_face_count
     procedure :: get_grid_edge_nodes => prosum_grid_edge_nodes
     procedure :: get_grid_face_edges => prosum_grid_face_edges
     procedure :: get_grid_face_nodes => prosum_grid_face_nodes
     procedure :: get_grid_nodes_per_face => prosum_grid_nodes_per_face
     procedure :: get_var_type => prosum_var_type
     procedure :: get_var_units => prosum_var_units
     procedure :: get_var_itemsize => prosum_var_itemsize
     procedure :: get_var_nbytes => prosum_var_nbytes
     procedure :: get_var_location => prosum_var_location
     procedure :: get_value_int => prosum_get_int
     procedure :: get_value_float => prosum_get_float
     procedure :: get_value_double => prosum_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
     procedure :: get_value_ptr_int => prosum_get_ptr_int
     procedure :: get_value_ptr_float => prosum_get_ptr_float
     procedure :: get_value_ptr_double => prosum_get_ptr_double
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_float, &
          get_value_ptr_double
     procedure :: get_value_at_indices_int => prosum_get_at_indices_int
     procedure :: get_value_at_indices_float => prosum_get_at_indices_float
     procedure :: get_value_at_indices_double => prosum_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => prosum_set_int
     procedure :: set_value_float => prosum_set_float
     procedure :: set_value_double => prosum_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
     procedure :: set_value_at_indices_int => prosum_set_at_indices_int
     procedure :: set_value_at_indices_float => prosum_set_at_indices_float
     procedure :: set_value_at_indices_double => prosum_set_at_indices_double
     generic :: set_value_at_indices => &
          set_value_at_indices_int, &
          set_value_at_indices_float, &
          set_value_at_indices_double
    ! procedure :: print_model_info
  end type bmi_prosum

contains

  ! Get the name of the model.
  function prosum_component_name(this, name) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function prosum_component_name

  ! Count the input variables.
  function prosum_input_item_count(this, count) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = input_item_count
    bmi_status = BMI_SUCCESS
  end function prosum_input_item_count

  ! Count the output variables.
  function prosum_output_item_count(this, count) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = output_item_count
    bmi_status = BMI_SUCCESS
  end function prosum_output_item_count

  ! List input variables.
  function prosum_input_var_names(this, names) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    !input_items(1)  = 'climate__temperature'
    !input_items(2)  = 'climate__photosynthetic_photon_flux_density'
    !input_items(3)  = 'atmosphere__carbon_dioxide_concentration'
    !input_items(4)  = 'ecology__herbivore_biomass_density'
    !input_items(5)  = 'management__tillage_flag'
    !input_items(6)  = 'management__harvest_flag'
    !input_items(7)  = 'plant__functional_type_index'

    !names => input_items
    !bmi_status = BMI_SUCCESS

    bmi_status = BMI_FAILURE
  end function prosum_input_var_names

  ! List output variables.
  function prosum_output_var_names(this, names) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    
    output_items(1) = 'vegetation__nitrogen_availability'
    output_items(2) = 'vegetation__magnesium_availability'
    output_items(3) = 'vegetation__potassium_availability'
    output_items(4) = 'vegetation__carbon_availability'
    output_items(5) = 'vegetation__calcium_availability'

    names => output_items
    bmi_status = BMI_SUCCESS
  end function prosum_output_var_names
 
  
  ! BMI initializer.
  function prosum_initialize(this, config_file) result (bmi_status)
    use PROSUM_module
    implicit none
    class (bmi_prosum), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    real :: temp, par, herbivores
    double precision :: co2
    integer :: tillage, harvest, planttype
    logical :: plantcover

    integer :: nlayer = 4                   
    integer :: nnutrient = 6                 
    integer :: nplantbits = 5               
    integer :: nplanttypes = 6               
    integer :: Num_months_of_parameters 
    character(len=120) :: TempFilePath

    TempFilePath = "PROSUM_parameters.csv"
    open(10, file=TempFilePath)
    read(10,*) 
    read(10,*) Num_months_of_parameters 
    close(10)
    
    write(*,*) 'Simulation length:', Num_months_of_parameters, 'months'

    call SoilTrECProsum_allocate(nlayer, nnutrient, nplantbits, nplanttypes, Num_months_of_parameters)
    call FillArrays(Num_months_of_parameters, StandAlone=1)

    ThisMonth = 1
    
    temp = Monthly_pars(ThisMonth, 2)       
    par = Monthly_pars(ThisMonth, 3)        
    co2 = Monthly_pars(ThisMonth, 4)        
    herbivores = Monthly_pars(ThisMonth, 5) 
    tillage = int(Monthly_pars(ThisMonth, 6))
    harvest = int(Monthly_pars(ThisMonth, 7))  
    planttype = int(Monthly_pars(ThisMonth, 8))
    plantcover = .false.

    call PROSUM(1, 1, ThisMonth, temp, par, co2, & 
                herbivores, tillage, harvest, planttype, plantcover, &
                nlayer, nnutrient, nplantbits, nplanttypes)

    bmi_status = BMI_SUCCESS
  end function prosum_initialize

  ! BMI finalizer.
  function prosum_finalize(this) result (bmi_status)
    class (bmi_prosum), intent(inout) :: this
    integer :: bmi_status

    call SoilTrECProsum_deallocate()
    bmi_status = BMI_SUCCESS
  end function prosum_finalize

  ! Model start time.
  function prosum_start_time(this, time) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 1.0d0
    bmi_status = BMI_SUCCESS
  end function prosum_start_time

  ! Model end time.
  function prosum_end_time(this, time) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(Month_end)
    bmi_status = BMI_SUCCESS
  end function prosum_end_time

  ! Model current time.
  function prosum_current_time(this, time) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(ThisMonth)
    bmi_status = BMI_SUCCESS
  end function prosum_current_time

  ! Model time step.
  function prosum_time_step(this, time_step) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = 1.0
    bmi_status = BMI_SUCCESS
  end function prosum_time_step

  ! Model time units.
  function prosum_time_units(this, units) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "month"
    bmi_status = BMI_SUCCESS
  end function prosum_time_units

  ! Advance model by one time step.
  function prosum_update(this) result (bmi_status)
    use PROSUM_module
    implicit none
    class (bmi_prosum), intent(inout) :: this
    integer :: bmi_status
    
    real :: temp, par, herbivores
    double precision :: co2
    integer :: tillage, harvest, planttype
    logical :: plantcover
    integer :: nlayer = 4, nnutrient = 6, nplantbits = 5, nplanttypes = 6
    
    temp = Monthly_pars(ThisMonth, 2)       
    par = Monthly_pars(ThisMonth, 3)        
    co2 = Monthly_pars(ThisMonth, 4)        
    herbivores = Monthly_pars(ThisMonth, 5) 
    tillage = int(Monthly_pars(ThisMonth, 6))    
    harvest = int(Monthly_pars(ThisMonth, 7))   
    planttype = int(Monthly_pars(ThisMonth, 8))  
    plantcover = .false.  
    
    ! 31/07/2005 DEBUG: Print values before PROSUM call
    write(*,*) '=== BMI UPDATE DEBUG ==='
    write(*,*) 'ThisMonth =', ThisMonth
    write(*,*) 'Array bounds: Monthly_pars =', shape(Monthly_pars)
    write(*,*) 'temp =', temp
    write(*,*) 'par =', par
    write(*,*) 'co2 =', co2
    write(*,*) 'About to call PROSUM...'
    
    call PROSUM(2, 1, ThisMonth, temp, par, co2, &
                herbivores, tillage, harvest, planttype, plantcover, &
                nlayer, nnutrient, nplantbits, nplanttypes)
    
    write(*,*) 'PROSUM call completed successfully'
    ThisMonth = ThisMonth + 1
    bmi_status = BMI_SUCCESS
end function prosum_update

  ! Advance the model until the given time.
  function prosum_update_until(this, time) result (bmi_status)
    class (bmi_prosum), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    if (time < dble(ThisMonth)) then
       bmi_status = BMI_FAILURE
       return
    end if

	do while (dble(ThisMonth) < floor(time))
       s = prosum_update(this)
       if (s /= BMI_SUCCESS) then
           bmi_status = BMI_FAILURE
           return
       endif
    end do

    bmi_status = BMI_SUCCESS
  end function prosum_update_until


  ! Get the grid id for a particular variable.
  function prosum_var_grid(this, name, grid) result(status)
	class(bmi_prosum), intent(in)    :: this
	character(len=*), intent(in)     :: name
	integer, intent(out)             :: grid
	integer                           :: status

	select case (name)
	case ( &
        !"climate__temperature", &
		  !"climate__photosynthetic_photon_flux_density", &
		  !"atmosphere__carbon_dioxide_concentration", &
		  !"ecology__herbivore_biomass_density", &
		  !"management__tillage_flag", &
		  !"management__harvest_flag", &
		  !"plant__functional_type_index", &
		  "vegetation__nitrogen_availability", &
		  "vegetation__magnesium_availability", &
		  "vegetation__potassium_availability", &
		  "vegetation__carbon_availability", &
		  "vegetation__calcium_availability" )
	  grid  = 0
	  status = BMI_SUCCESS
	  
	case default
	  grid  = -1
	  status = BMI_FAILURE
	end select
  end function prosum_var_grid


  ! The type of a variable's grid.
  function prosum_grid_type(this, grid, type) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
       type = "scalar"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function prosum_grid_type

  ! The number of dimensions of a grid.
  function prosum_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
       rank = 0
       bmi_status = BMI_SUCCESS
    case default
       rank = -1
       bmi_status = BMI_FAILURE
    end select
  end function prosum_grid_rank

  ! The dimensions of a grid.
  function prosum_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status

    select case(grid)
    case (0)
       shape(1) = 1
       bmi_status = BMI_SUCCESS
    case default
       shape(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function prosum_grid_shape

  ! The total number of elements in a grid.
  function prosum_grid_size(this, grid, size) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
       size = 1
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function prosum_grid_size

  ! The distance between nodes of a grid. 
  function prosum_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_spacing

  ! Coordinates of grid origin.
  function prosum_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_origin

  ! X-coordinates of grid nodes.
  function prosum_grid_x(this, grid, x) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_x

  ! Y-coordinates of grid nodes.
  function prosum_grid_y(this, grid, y) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_y

  ! Z-coordinates of grid nodes.
  function prosum_grid_z(this, grid, z) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_z

  ! Get the number of nodes in an unstructured grid.
  function prosum_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function prosum_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function prosum_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_face_count

  ! Get the edge-node connectivity.
  function prosum_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_edge_nodes

  ! Get the face-edge connectivity.
  function prosum_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_face_edges

  ! Get the face-node connectivity.
  function prosum_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_face_nodes

  ! Get the number of nodes for each face.
  function prosum_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_prosum), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status

	bmi_status = BMI_FAILURE
  end function prosum_grid_nodes_per_face


  ! The data type of the variable, as a string.
  function prosum_var_type(this, name, type) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    !case("climate__temperature")
    !   type = "double precision"
    !   bmi_status = BMI_SUCCESS
    !case("climate__photosynthetic_photon_flux_density")
    !   type = "double precision"
    !   bmi_status = BMI_SUCCESS
    !case("atmosphere__carbon_dioxide_concentration")
    !   type = "double precision"
    !   bmi_status = BMI_SUCCESS
    !case("ecology__herbivore_biomass_density")
    !   type = "double precision"
    !   bmi_status = BMI_SUCCESS
    !case("management__tillage_flag")
    !   type = "integer"
    !   bmi_status = BMI_SUCCESS
    !case("management__harvest_flag")
    !   type = "integer"
    !   bmi_status = BMI_SUCCESS
    !case("plant__functional_type_index")
    !   type = "integer"
    !   bmi_status = BMI_SUCCESS
    case("vegetation__nitrogen_availability")
       type = "double precision"
       bmi_status = BMI_SUCCESS
    case("vegetation__magnesium_availability")
       type = "double precision"
       bmi_status = BMI_SUCCESS
    case("vegetation__potassium_availability")
       type = "double precision"
       bmi_status = BMI_SUCCESS
    case("vegetation__carbon_availability")
       type = "double precision"
       bmi_status = BMI_SUCCESS
    case("vegetation__calcium_availability")
       type = "double precision"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function prosum_var_type

  ! The units of the given variable.
  function prosum_var_units(this, name, units) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status


    select case(name)
    !case("climate__temperature")
    !   units = "°C"
    !   bmi_status = BMI_SUCCESS
    !case("climate__photosynthetic_photon_flux_density")
    !   units = "µmol m⁻² s⁻¹"
    !   bmi_status = BMI_SUCCESS
    !case("atmosphere__carbon_dioxide_concentration")
    !   units = "µL L⁻¹"
    !   bmi_status = BMI_SUCCESS
    !case("ecology__herbivore_biomass_density")
    !   units = ""
    !   bmi_status = BMI_SUCCESS
    !case("management__tillage_flag")
    !   units = "1"
    !   bmi_status = BMI_SUCCESS
    !case("management__harvest_flag")
    !   units = "1"
    !   bmi_status = BMI_SUCCESS
    !case("plant__functional_type_index")
    !   units = "1"
    !   bmi_status = BMI_SUCCESS
    case("vegetation__nitrogen_availability")
       units = "mol m⁻²"
       bmi_status = BMI_SUCCESS
    case("vegetation__magnesium_availability")
       units = "mol m⁻²"
       bmi_status = BMI_SUCCESS
    case("vegetation__potassium_availability")
       units = "mol m⁻²"
       bmi_status = BMI_SUCCESS
    case("vegetation__carbon_availability")
       units = "mol m⁻²"
       bmi_status = BMI_SUCCESS
    case("vegetation__calcium_availability")
       units = "mol m⁻²"
       bmi_status = BMI_SUCCESS
    case default
       units = "-"
       bmi_status = BMI_FAILURE
    end select
  end function prosum_var_units

  ! Memory use per array element.
  function prosum_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status
       
    select case (trim(name))
    case ( &
    !    "climate__temperature",                            &
    !    "climate__photosynthetic_photon_flux_density",     &
    !    "atmosphere__carbon_dioxide_concentration",        &
    !    "ecology__herbivore_biomass_density",              &
        "vegetation__nitrogen_availability",               &
        "vegetation__magnesium_availability",              &
        "vegetation__potassium_availability",              &
        "vegetation__carbon_availability",                 &
        "vegetation__calcium_availability"                 &
      )
        size        = 8
        bmi_status  = BMI_SUCCESS
    !case ( &
    !    "management__tillage_flag",                      &
    !    "management__harvest_flag",                      &
    !    "plant__functional_type_index"                   &
    !  )
    !    size        = 4
    !    bmi_status  = BMI_SUCCESS
    case default
        size        = -1
        bmi_status  = BMI_FAILURE
    end select
  end function prosum_var_itemsize

  ! The size of the given variable.
  function prosum_var_nbytes(this, name, nbytes) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: nbytes
    integer :: bmi_status
    integer :: s1, s2, s3, grid, grid_size, item_size

    s1 = this%get_var_grid(name, grid)
    s2 = this%get_grid_size(grid, grid_size)
    s3 = this%get_var_itemsize(name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       nbytes = -1
       bmi_status = BMI_FAILURE
    end if
  end function prosum_var_nbytes

  ! The location (node, face, edge) of the given variable.
  function prosum_var_location(this, name, location) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status

    select case(name)
    case ( &
    !  "climate__temperature",                            &
    !  "climate__photosynthetic_photon_flux_density",     &
    !  "atmosphere__carbon_dioxide_concentration",        &
    !  "ecology__herbivore_biomass_density",              &
      "vegetation__nitrogen_availability",               &
      "vegetation__magnesium_availability",              &
      "vegetation__potassium_availability",              &
      "vegetation__carbon_availability",                 &
      "vegetation__calcium_availability"                 &
    !  "management__tillage_flag",                        &
    !  "management__harvest_flag",                        &
    !  "plant__functional_type_index"                     &
    )
      location   = "none"
      bmi_status = BMI_SUCCESS
    case default
      bmi_status = BMI_FAILURE
    end select
  end function prosum_var_location



  function prosum_get_float(this, name, dest) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case default
       dest(:) = -1.0
       bmi_status = BMI_FAILURE
    end select
  end function prosum_get_float



  function prosum_get_int(this, name, dest) result (bmi_status)
    use PROSUM_module               
    implicit none
    class (bmi_prosum), intent(in)    :: this
    character(len=*), intent(in)      :: name
    integer,         intent(inout)    :: dest(:)
    integer                         :: bmi_status

    select case (name)
    !case ("management__tillage_flag")
    !   dest(1)    = Tillage_TF(1)
    !   bmi_status = BMI_SUCCESS
    !case ("management__harvest_flag")
    !  dest(1)    = Harvest_TF(1)
    !   bmi_status = BMI_SUCCESS
    !case ("plant__functional_type_index")
    !   dest(1)    = PlantType(1)
    !   bmi_status = BMI_SUCCESS
    case default
       dest(1) = -1
       bmi_status = BMI_FAILURE

    end select
  end function prosum_get_int

  function prosum_get_double(this, name, dest) result (bmi_status)
    implicit none
    class (bmi_prosum), intent(in)    :: this
    character(len=*),   intent(in)    :: name
    double precision,   intent(inout) :: dest(:)
    integer                           :: bmi_status

    select case (name)
    !case ("climate__temperature")
    !   dest(1)    = Temp_oC(1)
    !   bmi_status = BMI_SUCCESS
    !case ("climate__photosynthetic_photon_flux_density")
    !   dest(1)    = PAR_uMpm2s(1)
    !   bmi_status = BMI_SUCCESS
    !case ("atmosphere__carbon_dioxide_concentration")
    !   dest(1)    = AtmosphCO2_uLpL(1)
    !   bmi_status = BMI_SUCCESS
    !case ("ecology__herbivore_biomass_density")
    !   dest(1)    = Herbivores_kgLivepha(1)
    !   bmi_status = BMI_SUCCESS
    ! (suffix = _e): 1=C, 2=N, 3=P, 4=Ca, 5=Mg, 6=K
    case ("vegetation__carbon_availability")
       dest(1)    = NutAvail_e(1)
       bmi_status = BMI_SUCCESS
    case ("vegetation__nitrogen_availability")
       dest(1)    = NutAvail_e(2)
       bmi_status = BMI_SUCCESS
    case ("vegetation__magnesium_availability")
       dest(1)    = NutAvail_e(5)
       bmi_status = BMI_SUCCESS
    case ("vegetation__potassium_availability")
       dest(1)    = NutAvail_e(6)
       bmi_status = BMI_SUCCESS
    case ("vegetation__calcium_availability")
       dest(1)    = NutAvail_e(4)
       bmi_status = BMI_SUCCESS
    case default
       dest(1)    = -1.0d0
       bmi_status = BMI_FAILURE
    end select
  end function prosum_get_double

  ! Get a reference to an integer-valued variable, flattened.
  function prosum_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status

    select case(name)
    !case ("management__tillage_flag")
    !   bmi_status = BMI_FAILURE
    !case ("management__harvest_flag")
    !   bmi_status = BMI_FAILURE
    !case ("plant__functional_type_index")
    !   bmi_status = BMI_FAILURE
    case default
       bmi_status = BMI_FAILURE
    end select
  end function prosum_get_ptr_int

  ! Get a reference to a real-valued variable, flattened.
  function prosum_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function prosum_get_ptr_float

  ! Get a reference to an double-valued variable, flattened.
  function prosum_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status

    select case(name)
    case ( &
          !"climate__temperature",                            &
          !"climate__photosynthetic_photon_flux_density",     &
          !"atmosphere__carbon_dioxide_concentration",        &
          !"ecology__herbivore_biomass_density",              &
          "vegetation__nitrogen_availability",               &
          "vegetation__magnesium_availability",              &
          "vegetation__potassium_availability",              &
          "vegetation__carbon_availability",                 &
          "vegetation__calcium_availability")
       bmi_status = BMI_FAILURE
    case default
       bmi_status = BMI_FAILURE
    end select
  end function prosum_get_ptr_double

  ! Get values of an integer variable at the given locations.
  function prosum_get_at_indices_int(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status

    dest(:) = -1
    bmi_status = BMI_FAILURE
  end function prosum_get_at_indices_int

  ! Get values of a real variable at the given locations.
  function prosum_get_at_indices_float(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status

    dest(:) = -1.0
    bmi_status = BMI_FAILURE
  end function prosum_get_at_indices_float

  ! Get values of a double variable at the given locations.
  function prosum_get_at_indices_double(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_prosum), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status

    dest(:) = -1.0d0
    bmi_status = BMI_FAILURE
  end function prosum_get_at_indices_double

  ! Set new integer values.
  function prosum_set_int(this, name, src) result (bmi_status)
    use PROSUM_module
    implicit none
    class (bmi_prosum), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    !case ("management__tillage_flag")
    !   Tillage_TF(1) = src(1)
    !   bmi_status    = BMI_SUCCESS
    !case ("management__harvest_flag")
    !   Harvest_TF(1) = src(1)
    !   bmi_status    = BMI_SUCCESS
    !case ("plant__functional_type_index")
    !   PlantType(1)  = src(1)
    !   bmi_status    = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function prosum_set_int

  ! Set new real values.
  function prosum_set_float(this, name, src) result (bmi_status)
    implicit none
    class (bmi_prosum), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    bmi_status = BMI_FAILURE
  end function prosum_set_float

  ! Set new double values.
  function prosum_set_double(this, name, src) result (bmi_status)
    use PROSUM_module
    class (bmi_prosum), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    select case (name)
    !case ("climate__temperature")
    !   Temp_oC(1) = src(1)
    !   bmi_status = BMI_SUCCESS
    !case ("climate__photosynthetic_photon_flux_density")
    !   PAR_uMpm2s(1) = src(1)
    !   bmi_status = BMI_SUCCESS
    !case ("atmosphere__carbon_dioxide_concentration")
    !   AtmosphCO2_uLpL(1) = src(1)
    !   bmi_status = BMI_SUCCESS
    !case ("ecology__herbivore_biomass_density")
    !   Herbivores_kgLivepha(1) = src(1)
    !   bmi_status = BMI_SUCCESS
    case ("vegetation__carbon_availability")
       NutAvail_e(1) = src(1)
       bmi_status    = BMI_SUCCESS
    case ("vegetation__nitrogen_availability")
       NutAvail_e(2) = src(1)
       bmi_status    = BMI_SUCCESS
    case ("vegetation__magnesium_availability")
       NutAvail_e(5) = src(1)
       bmi_status    = BMI_SUCCESS
    case ("vegetation__potassium_availability")
       NutAvail_e(6) = src(1)
       bmi_status    = BMI_SUCCESS
    case ("vegetation__calcium_availability")
       NutAvail_e(4) = src(1)
       bmi_status    = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function prosum_set_double

  ! Set integer values at particular locations.
  function prosum_set_at_indices_int(this, name, inds, src) &
       result (bmi_status)
    class (bmi_prosum), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: bmi_status

    bmi_status = BMI_FAILURE
  end function prosum_set_at_indices_int

  ! Set real values at particular locations.
  function prosum_set_at_indices_float(this, name, inds, src) &
       result (bmi_status)
    class (bmi_prosum), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: bmi_status


    bmi_status = BMI_FAILURE    
  end function prosum_set_at_indices_float

  ! Set double values at particular locations.
  function prosum_set_at_indices_double(this, name, inds, src) &
       result (bmi_status)
    class (bmi_prosum), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    bmi_status = BMI_FAILURE
  end function prosum_set_at_indices_double

  ! A non-BMI helper routine to advance the model by a fractional time step.
!  subroutine update_frac(this, time_frac)
!    class (bmi_prosum), intent(inout) :: this
!    double precision, intent(in) :: time_frac
!    integer                      ::
!    if (time_frac > 0.0) then
!       time_step = this%model%dt
!       this%model%dt = time_step*real(time_frac)
!       call advance_in_time(this%model)
!       this%model%dt = time_step
!    end if
!  end subroutine update_frac

  ! A non-BMI procedure for model introspection.
!   subroutine print_model_info(this)
!     class (bmi_prosum), intent(in) :: this
!     call print_info()
!   end subroutine print_model_info

end module bmiprosumf
