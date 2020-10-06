module dssat_output

  use iso_c_binding

  implicit none

  type nc_output
     logical :: yes
     character(len=120) :: filename
     type(c_ptr) :: ptr
  end type nc_output

  type(nc_output) nc_out

  interface add_var_seasonal
     procedure add_real_seasonal
     procedure add_integer_seasonal
!     procedure add_character_seasonal
  end interface add_var_seasonal

  interface
     type(c_ptr) function create_output() bind(C,name='create_output')
       use iso_c_binding
       implicit none
     end function create_output
     subroutine add_seasonal_lat(out_ptr,valptr) bind(C,name='add_lat_seasonal')
       use iso_c_binding
       implicit none
       type(c_ptr),value :: out_ptr
       real              :: valptr
     end subroutine add_seasonal_lat
     subroutine add_seasonal_lon(out_ptr,valptr) bind(C,name='add_lon_seasonal')
       use iso_c_binding
       implicit none
       type(c_ptr),value :: out_ptr
       real              :: valptr
     end subroutine add_seasonal_lon
     subroutine add_seasonal_seas(out_ptr,valptr) bind(C,name='add_seas_seasonal')
       use iso_c_binding
       implicit none
       type(c_ptr),value :: out_ptr
       integer              :: valptr
     end subroutine add_seasonal_seas
     subroutine add_fvar_seasonal(out_ptr,name,valptr) bind(C,name='add_fvar_seasonal')
       use iso_c_binding
       implicit none
       type(c_ptr),value :: out_ptr
       character(len=1)  :: name
       real              :: valptr
     end subroutine add_fvar_seasonal
     subroutine add_ivar_seasonal(out_ptr,name,valptr) bind(C,name='add_ivar_seasonal')
       use iso_c_binding
       implicit none
       type(c_ptr),value :: out_ptr
       character(len=1)  :: name
       integer           :: valptr
     end subroutine add_ivar_seasonal
     subroutine collect_seasonal_values(out_ptr) bind(C,name='collect_seasonal_values')
       use iso_c_binding
       implicit none
       type(c_ptr),value :: out_ptr
     end subroutine collect_seasonal_values
     subroutine write_output(out_ptr,filename) bind(C,name='write_output')
       use iso_c_binding
       implicit none
       type(c_ptr),value :: out_ptr
       character(len=1)  :: filename
     end subroutine write_output
  end interface

contains

  subroutine instantiate_output()

    implicit none

    if(.not.c_associated(nc_out%ptr)) &
         nc_out%ptr = create_output()

  end subroutine instantiate_output

  subroutine set_output_file_name(filename)

    implicit none

    character(len=*) filename

    nc_out%filename = trim(filename)//c_null_char

  end subroutine set_output_file_name

  subroutine add_real_seasonal(name,val)
    character(len=*)  :: name
    real              :: val
    call add_fvar_seasonal(nc_out%ptr,name//C_NULL_CHAR,val)
  end subroutine add_real_seasonal

  subroutine add_integer_seasonal(name,val)
    character(len=*)  :: name
    integer           :: val
    call add_ivar_seasonal(nc_out%ptr,name//C_NULL_CHAR,val)
  end subroutine add_integer_seasonal

  subroutine add_lat_seasonal(val)
    real              :: val
    call add_seasonal_lat(nc_out%ptr,val)
  end subroutine add_lat_seasonal

  subroutine add_lon_seasonal(val)
    real              :: val
    call add_seasonal_lon(nc_out%ptr,val)
  end subroutine add_lon_seasonal

  subroutine add_seas_seasonal(val)
    integer              :: val
    call add_seasonal_seas(nc_out%ptr,val)
  end subroutine add_seas_seasonal

  subroutine collect_seasonal()
    implicit none
    call collect_seasonal_values(nc_out%ptr)
  end subroutine collect_seasonal

  subroutine write_seasonal()
    implicit none
    call write_output(nc_out%ptr,nc_out%filename)
  end subroutine write_seasonal

end module dssat_output
