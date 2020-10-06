module nf90_file_module

  use netcdf
  use dssat_cmd_arg

  implicit none

  real             :: nf90_missing_real = -99.
  integer          :: nf90_missing_integer = -99
  character(len=1) :: nf90_missing_character = ' '

  type nf90_variable
     character(len=:),allocatable :: name
     integer varid
  end type nf90_variable

  type nf90_dimension
     character(len=:),allocatable :: name
     integer dimid
     integer dim_size
  end type nf90_dimension

  type nf90_file
     integer ncid,lat_i,lon_i,z_i
     logical :: define_mode = .false. ,yes = .false., is_open = .false.
     character(len=:),allocatable :: file_name
     type(nf90_dimension),dimension(:),allocatable :: dims
     type(nf90_variable),dimension(:),allocatable :: vars
   contains
     procedure :: create => create_nf90_file
     procedure :: close => close_nf90_file
     procedure :: add_dim => add_nf90_dimension
     procedure :: add_var => add_nf90_variable
     procedure :: end_def => end_define_nf90_file
     procedure :: redef => redefine_nf90_file
     procedure :: find_dimid
     procedure :: write_variable_real_name
     procedure :: write_variable_real_varid
     procedure :: write_variable_integer_name
     procedure :: write_variable_integer_varid
     procedure :: write_variable_real_array_name
     procedure :: write_variable_real_array_varid
     procedure :: write_variable_integer_array_name
     procedure :: write_variable_integer_array_varid
     generic   :: write_variable => &
          write_variable_real_name,&
          write_variable_real_varid,&
          write_variable_integer_name,&
          write_variable_integer_varid,&
          write_variable_real_array_name,&
          write_variable_real_array_varid,&
          write_variable_integer_array_name,&
          write_variable_integer_array_varid
     procedure :: write_netcdf_mpi_registries
     generic :: write_netcdf => &
          write_netcdf_mpi_registries
     procedure :: set_file_name => set_nf90_file_name
     procedure :: set_file_from_cmd_arg => set_nf90_file_name_from_cmd_arg
     procedure :: open_nf90_file
     procedure :: open_nf90_file_no_file_name
     generic   :: open => &
          open_nf90_file,&
          open_nf90_file_no_file_name
     procedure :: read_netcdf_real_nf90_file
     procedure :: read_netcdf_rvec_nf90_file
     procedure :: read_netcdf_integer_nf90_file
     procedure :: read_netcdf_ivec_nf90_file
     procedure :: read_netcdf_character_nf90_file
     procedure :: read_netcdf_cvec_nf90_file
     generic :: read => &
          read_netcdf_real_nf90_file,&
          read_netcdf_rvec_nf90_file,&
          read_netcdf_integer_nf90_file,&
          read_netcdf_ivec_nf90_file,&
          read_netcdf_character_nf90_file,&
          read_netcdf_cvec_nf90_file
     procedure :: get_ind_nvals => get_ind_nvals_nf90_file
     procedure :: get_dim_size => get_nf90_dim_size
     procedure :: set_lat_lon => set_nf90_lat_lon
     procedure :: set_date => set_nf90_date_index
  end type nf90_file

  interface assignment (=)
     module procedure nf90_dimension_assignment
     module procedure nf90_variable_assignment
  end interface assignment (=)

  interface deallocate
     module procedure deallocate_nf90_dimension_array
     module procedure deallocate_nf90_variable_array
  end interface deallocate

  interface reallocate
     module procedure reallocate_nf90_dimension_array
     module procedure reallocate_nf90_variable_array
  end interface reallocate

  interface set_missing
     module procedure set_missing_nf90_real
     module procedure set_missing_nf90_integer
     module procedure set_missing_nf90_character
     module procedure set_missing_nf90_rvec
     module procedure set_missing_nf90_ivec
     module procedure set_missing_nf90_cvec
  end interface set_missing

  interface is_missing
     module procedure is_missing_nf90_real
     module procedure is_missing_nf90_integer
  end interface is_missing

contains

  subroutine set_nf90_file_name(self,file_name)

    implicit none

    class(nf90_file) :: self

    character(len=*) :: file_name

    if(allocated(self%file_name))then
       if(len(self%file_name) /= len(file_name)) then
          deallocate(self%file_name)
          allocate(character(len=len(file_name))::self%file_name)
       end if
    end if

    self%file_name = file_name

  end subroutine set_nf90_file_name

  subroutine set_nf90_file_name_from_cmd_arg(self,key)

    implicit none

    class(nf90_file) :: self

    integer             :: i

    character(len=*)    :: key

    character(len=1000) :: file_name

    call get_dssat_arg(key,file_name)

    file_name = adjustl(file_name)

    i = len(trim(file_name))

    self%yes = i > 0

    if(self%yes) call self%set_file_name(file_name(1:i))

  end subroutine set_nf90_file_name_from_cmd_arg

    subroutine create_nf90_file(self,file_name,overwrite)

      implicit none

      class(nf90_file) self

      integer f_unit
      real    r_unit
      character(len=*) file_name

      logical,optional :: overwrite

      logical :: file_exist

      if(present(overwrite))then
         if(overwrite)then
            inquire(file=file_name,exist=file_exist)
            if(file_exist)then
               call random_seed()
               call random_number(r_unit)
               f_unit = ceiling(r_unit*huge(f_unit))
               open(unit=f_unit,file=file_name,status='old')
               close(f_unit,status='delete')
            end if
         end if
      end if

      self%file_name = file_name

      call nc_err_check(nf90_create(file_name,nf90_netcdf4,self%ncid))

      self%define_mode = .TRUE.

    end subroutine create_nf90_file

    subroutine open_nf90_file(self,file_name)

      use iso_fortran_env, only: error_unit

      implicit none

      class(nf90_file)             :: self

      character(len=*)             :: file_name

      logical :: exists

      if(self%is_open) return

      inquire(file = file_name, exist = exists)

      if(.not. exists) then

         write(error_unit,'(a)') &
              'File "'//trim(adjustl(file_name))//'" does not exist.'
         stop 'Error'

      else

         self%file_name = file_name

         call nc_err_check(nf90_open(file_name,nf90_nowrite,self%ncid))

         self%is_open = .true.

      end if

    end subroutine open_nf90_file

    subroutine open_nf90_file_no_file_name(self)

      use iso_fortran_env, only: error_unit

      implicit none

      class(nf90_file) :: self

      logical :: exists

      if(self%is_open) return

      if(allocated(self%file_name))then

         inquire(file = self%file_name, exist = exists)

         if(.not. exists) then

            write(error_unit,'(a)') &
                 'File "'//trim(adjustl(self%file_name))//'" does not exist.'
            stop 'Error'

         else

            call nc_err_check(nf90_open(self%file_name,nf90_nowrite,self%ncid))

            self%is_open = .true.

         end if

      else

         stop 'File name not allocated.'

      end if

    end subroutine open_nf90_file_no_file_name

    function find_dimid(self,name) result(dimid)

      implicit none

      integer dimid,i
      character(len=*) :: name

      class(nf90_file) :: self

      do i=1,size(self%dims)
         if(self%dims(i)%name == name)then
            dimid = i
            exit
         end if
      end do

  end function find_dimid

  subroutine add_nf90_dimension(self,dim_name,dim_size)

    implicit none

    class(nf90_file) :: self

    character(len=*) :: dim_name
    integer          :: dim_size,i,new_dim
    logical          :: add

    logical  end_define_on_return

    end_define_on_return = .not.self%define_mode

    if(.not.self%define_mode)then
       call self%redef()
    end if

    add = .TRUE.
    if(allocated(self%dims))then
       do i=1,size(self%dims)
          if(dim_name == self%dims(i)%name)then
             add = .FALSE.
             exit
          end if
       end do
    end if

    if(add)then
       call reallocate(self%dims)
       new_dim = size(self%dims)
       self%dims(new_dim)%name = trim(dim_name)
       self%dims(new_dim)%dim_size = dim_size
       call nc_err_check(nf90_def_dim(self%ncid,trim(dim_name),&
            dim_size,self%dims(new_dim)%dimid))
    end if

    if(end_define_on_return)then
       call self%end_def()
    end if

  end subroutine add_nf90_dimension

  subroutine add_nf90_variable(self,var_name,dims,vtype)

    implicit none

    class(nf90_file) :: self

    character(len=*) :: var_name
    character(len=*),dimension(:) :: dims
    integer,dimension(:),allocatable :: dimids
    integer          :: i,j,new_var,vtype
    logical          :: add

    logical  end_define_on_return

    end_define_on_return = .not.self%define_mode

    allocate(dimids(size(dims)))

    i_loop: do i=1,size(dims)
       j_loop: do j=1,size(self%dims)
          if(self%dims(j)%name == dims(i))then
             dimids(i) = self%dims(j)%dimid
             exit j_loop
          end if
       end do j_loop
    end do i_loop

    add = .TRUE.
    if(allocated(self%vars))then
       do i=1,size(self%vars)
          if(var_name == self%vars(i)%name)then
             new_var = i
             add = .FALSE.
             exit
          end if
       end do
    end if

    if(add)then
       call reallocate(self%vars)
       new_var = size(self%vars)
       self%vars(new_var)%name = trim(var_name)
       call nc_err_check(nf90_def_var(self%ncid,trim(var_name),vtype,&
            dimids,self%vars(new_var)%varid))
       select case(vtype)
       case(nf90_float)
           call nc_err_check(nf90_put_att(self%ncid,self%vars(new_var)%varid,'_FillValue',nf90_missing_real))
       case(nf90_int)
           call nc_err_check(nf90_put_att(self%ncid,self%vars(new_var)%varid,'_FillValue',nf90_missing_integer))
       end select
    end if

    if(end_define_on_return)then
       call self%end_def()
    end if

  end subroutine add_nf90_variable

  subroutine end_define_nf90_file(self)

    implicit none

    class(nf90_file) :: self

    call nc_err_check(nf90_enddef(self%ncid))

    self%define_mode = .FALSE.

  end subroutine end_define_nf90_file

  subroutine redefine_nf90_file(self)

    implicit none

    class(nf90_file) :: self

    call nc_err_check(nf90_redef(self%ncid))

    self%define_mode = .TRUE.

  end subroutine redefine_nf90_file

  subroutine close_nf90_file(self)

    implicit none

    class(nf90_file) :: self

    call nc_err_check(nf90_close(self%ncid))

  end subroutine close_nf90_file

  subroutine nc_err_check(status,key,nostop)

    implicit none

    integer, intent (in)                 :: status
    character(len=*),intent(in),optional :: key
    logical,optional                     :: nostop

    if(status /= nf90_noerr) then
       if(present(key)) print *, 'Error key: ',trim(adjustl(key))
       print *, trim(nf90_strerror(status))
       if(.not.present(nostop)) stop "Program stopped due to error."
    end if

  end subroutine nc_err_check

  subroutine nf90_dimension_assignment(lhs,rhs)

    implicit none

    type(nf90_dimension),intent(out) :: lhs
    type(nf90_dimension),intent(in)  :: rhs

    lhs%name = rhs%name
    lhs%dimid = rhs%dimid
    lhs%dim_size = rhs%dim_size

  end subroutine nf90_dimension_assignment

  subroutine nf90_variable_assignment(lhs,rhs)

    implicit none

    type(nf90_variable),intent(out) :: lhs
    type(nf90_variable),intent(in)  :: rhs

    lhs%name = rhs%name
    lhs%varid = rhs%varid

  end subroutine nf90_variable_assignment

  subroutine deallocate_nf90_dimension_array(dim_array)

    implicit none

    type(nf90_dimension),dimension(:),allocatable :: dim_array

    integer i

    do i=1,size(dim_array)
       deallocate(dim_array(i)%name)
    end do

    deallocate(dim_array)

  end subroutine deallocate_nf90_dimension_array

  subroutine reallocate_nf90_dimension_array(dim_array)

    implicit none

    type(nf90_dimension),dimension(:),allocatable :: dim_array
    type(nf90_dimension),dimension(:),allocatable :: temp_array

    integer i

    if(allocated(dim_array))then

       call move_alloc(dim_array,temp_array)

       allocate(dim_array(size(temp_array)+1))

       do i=1,size(temp_array)
          dim_array(i) = temp_array(i)
       end do

       deallocate(temp_array)

    else

       allocate(dim_array(1))

    end if

  end subroutine reallocate_nf90_dimension_array

  subroutine deallocate_nf90_variable_array(var_array)

    implicit none

    type(nf90_variable),dimension(:),allocatable :: var_array

    integer i

    do i=1,size(var_array)
       deallocate(var_array(i)%name)
    end do

    deallocate(var_array)

  end subroutine deallocate_nf90_variable_array

  subroutine reallocate_nf90_variable_array(var_array)

    implicit none

    type(nf90_variable),dimension(:),allocatable :: var_array
    type(nf90_variable),dimension(:),allocatable :: temp_array

    integer i

    if(allocated(var_array))then

       call move_alloc(var_array,temp_array)

       allocate(var_array(size(temp_array)+1))

       do i=1,size(temp_array)
          var_array(i) = temp_array(i)
       end do

       deallocate(temp_array)

    else

       allocate(var_array(1))

    end if

  end subroutine reallocate_nf90_variable_array

  subroutine write_variable_real_array_name(self,name,start,count,r_values)

    implicit none

    class(nf90_file)  self
    character(len=*)  name
    integer           i
    integer           varid
    integer,dimension(:) :: start,count
    real,dimension(:) :: r_values

    do i=1,size(self%vars)
       if(self%vars(i)%name == name)then
          varid = self%vars(i)%varid
       end if
    end do
    call nc_err_check(nf90_put_var(self%ncid,varid,r_values,start=start,count=count))

  end subroutine write_variable_real_array_name

  subroutine write_variable_real_array_varid(self,varid,start,count,r_values)

    implicit none

    class(nf90_file)  self
    integer           varid
    integer,dimension(:) :: start,count
    real,dimension(:) :: r_values

    call nc_err_check(nf90_put_var(self%ncid,varid,r_values,start=start,count=count))

  end subroutine write_variable_real_array_varid

  subroutine write_variable_integer_array_name(self,name,start,count,i_values)

    implicit none

    class(nf90_file)  self
    character(len=*)  name
    integer           i
    integer           varid
    integer,dimension(:) :: i_values,start,count

    do i=1,size(self%vars)
       if(self%vars(i)%name == name)then
          varid = self%vars(i)%varid
       end if
    end do
    call nc_err_check(nf90_put_var(self%ncid,varid,i_values,start=start,count=count))

  end subroutine write_variable_integer_array_name

  subroutine write_variable_integer_array_varid(self,varid,start,count,i_values)

    implicit none

    class(nf90_file)  self
    integer           varid
    integer,dimension(:) :: i_values,start,count

    call nc_err_check(nf90_put_var(self%ncid,varid,i_values,start=start,count=count))

  end subroutine write_variable_integer_array_varid

  subroutine write_variable_real_name(self,name,r_value)

    implicit none

    class(nf90_file)  self
    character(len=*)  name
    integer           i
    integer           varid
    real              r_value

    do i=1,size(self%vars)
       if(self%vars(i)%name == name)then
          varid = self%vars(i)%varid
       end if
    end do
    call nc_err_check(nf90_put_var(self%ncid,varid,r_value))

  end subroutine write_variable_real_name

  subroutine write_variable_real_varid(self,varid,r_value)

    implicit none

    class(nf90_file)  self
    integer           varid
    real              r_value

    call nc_err_check(nf90_put_var(self%ncid,varid,r_value))

  end subroutine write_variable_real_varid

  subroutine write_variable_integer_name(self,name,i_value)

    implicit none

    class(nf90_file)  self
    character(len=*)  name
    integer           i
    integer           varid
    integer           i_value

    do i=1,size(self%vars)
       if(self%vars(i)%name == name)then
          varid = self%vars(i)%varid
       end if
    end do
    call nc_err_check(nf90_put_var(self%ncid,varid,i_value))

  end subroutine write_variable_integer_name

  subroutine write_variable_integer_varid(self,varid,i_value)

    implicit none

    class(nf90_file)  self
    integer           varid
    integer           i_value

    call nc_err_check(nf90_put_var(self%ncid,varid,i_value))

  end subroutine write_variable_integer_varid

  subroutine write_netcdf_mpi_registries(self,parent,dim_size,xcrd_i,ycrd_i)

    use dssat_mpi

    implicit none

    integer i,j,k,p1,p2,dim_size,count
    integer,dimension(:),allocatable  :: curr_trt
    integer,dimension(:) :: xcrd_i,ycrd_i

    type(mpi_parent_type) parent
    class(nf90_file) :: self

    do i=1,size(parent%seasonal(1)%variables)
       if(allocated(parent%seasonal(1)%variables(i)%r_val))then
          call self%add_var(parent%seasonal(1)%variables(i)%name,&
               (/'lon   ','lat   ','season'/),nf90_float)
       else if(allocated(parent%seasonal(1)%variables(i)%i_val))then
          call self%add_var(parent%seasonal(1)%variables(i)%name,&
               (/'lon   ','lat   ','season'/),nf90_int)
       end if
    end do

    if(self%define_mode) call self%end_def()
    allocate(curr_trt(size(parent%seasonal(1)%variables)))
    curr_trt = 1
    do i=1,size(parent%seasonal)
       do j=1,size(parent%seasonal(i)%variables)
          count = 0
          do k=1,parent%seasonal(i)%variables(j)%curr_ind/dim_size
             p1 = count + 1
             p2 = p1 + dim_size - 1
             if(allocated(parent%seasonal(i)%variables(j)%r_val))then
                call self%write_variable(&
                     parent%seasonal(i)%variables(j)%name,&
                     (/xcrd_i(curr_trt(j)),ycrd_i(curr_trt(j)),1/),&
                     (/1,1,dim_size/),&
                     parent%seasonal(i)%variables(j)%r_val(p1:p2))
             else if(allocated(parent%seasonal(i)%variables(j)%i_val))then
                call self%write_variable(&
                     parent%seasonal(i)%variables(j)%name,&
                     (/xcrd_i(curr_trt(j)),ycrd_i(curr_trt(j)),1/),&
                     (/1,1,dim_size/),&
                     parent%seasonal(i)%variables(j)%i_val(p1:p2))
             end if
             count = count + dim_size
             curr_trt(j) = curr_trt(j) + 1
          end do
       end do
    end do

  end subroutine write_netcdf_mpi_registries

  subroutine get_nf90_dim_size(self,name,dim_size,key)

    implicit none

    class(nf90_file)          :: self
    character(len=*)          :: name
    character(len=*),optional :: key
    integer                   :: dimid,dim_size

    if(present(key))then
       call nc_err_check(nf90_inq_dimid(self%ncid,name,dimid),key=key)
       call nc_err_check(nf90_inquire_dimension(self%ncid,dimid,len=dim_size),key=key)
    else
       call nc_err_check(nf90_inq_dimid(self%ncid,name,dimid))
       call nc_err_check(nf90_inquire_dimension(self%ncid,dimid,len=dim_size))
    end if

  end subroutine get_nf90_dim_size

  subroutine read_netcdf_character_nf90_file(self,name,v_ind,value,dim_var)

    implicit none

    class(nf90_file)                           :: self
    integer                                    :: varid,which_z,err,v_ind
    integer,dimension(:),allocatable           :: start,count
    character(len=*)                           :: name
    character(len=*),intent(out)               :: value
    logical,optional                           :: dim_var

    if(present(dim_var))then
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z, dim_var = dim_var)
    else
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z)
    end if

    if(.not.allocated(start))then
       call set_missing(value)
       return
    end if

    if(which_z > 1)then
       if(count(which_z) > 1)then
          count(which_z) = 1
       end if
    end if

    if(count(1) > len(value))then
       count(1) = len(value)
    end if

    err = nf90_get_var(self%ncid, varid, value(1:count(1)),&
         start = start, count = count)

    if(err /= nf90_noerr)then
       value = ' '
    else if(count(1) < len(value))then
       value((count(1)+1):len(value)) = ' '
    end if

  end subroutine read_netcdf_character_nf90_file

  subroutine read_netcdf_cvec_nf90_file(self,name,v_ind,value,dim_var)

    implicit none

    class(nf90_file)                           :: self
    integer                                    :: i,n_iter
    integer                                    :: varid,which_z,err,v_ind
    integer,dimension(:),allocatable           :: start,count
    character(len=*)                           :: name
    character(len=*),dimension(:),intent(out)  :: value
    logical,optional                           :: dim_var

    if(present(dim_var))then
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z, dim_var = dim_var)
    else
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z)
    end if

    if(.not.allocated(start))then
       call set_missing(value)
       return
    end if

    if(which_z > 1)then
       if(count(which_z) <= size(value))then
          n_iter = count(which_z)
       else
          n_iter = size(value)
       end if
       count(which_z) = 1
    else
       n_iter = 1
    end if

    if(count(1) > len(value))then
       count(1) = len(value)
    end if

    do i=1,n_iter

       if(i > 1) start(which_z) = start(which_z) + 1

       err = nf90_get_var(self%ncid, varid, value(i)(1:count(1)),&
            start = start, count = count)

       if(err /= nf90_noerr)then
          value(i) = ' '
       else if(count(1) < len(value))then
          value(i)((count(1)+1):len(value)) = ' '
       end if

    end do

    if(size(value) > n_iter)then
       value((n_iter+1):size(value)) = ' '
    end if

  end subroutine read_netcdf_cvec_nf90_file

  subroutine read_netcdf_integer_nf90_file(self,name,v_ind,value,dim_var)

    implicit none

    class(nf90_file)                   :: self
    integer                            :: varid,which_z,err,v_ind
    integer,dimension(:),allocatable   :: start,count
    character(len=*)                   :: name
    integer,intent(out)                :: value
    logical,optional                   :: dim_var

    if(present(dim_var))then
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z, dim_var = dim_var)
    else
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z)
    end if

    if(.not.allocated(start))then
       call set_missing(value)
       return
    end if

    err = nf90_get_var(self%ncid, varid, value, start = start)

    if(err /= nf90_noerr)then
       call set_missing(value)
    end if

  end subroutine read_netcdf_integer_nf90_file

  subroutine read_netcdf_ivec_nf90_file(self,name,v_ind,value,dim_var)

    implicit none

    class(nf90_file)                   :: self
    integer                            :: varid,which_z,err,v_ind
    integer,dimension(:),allocatable   :: start,count
    character(len=*)                   :: name
    integer,dimension(:),intent(out)   :: value
    logical,optional                   :: dim_var

    if(present(dim_var))then
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z, dim_var = dim_var)
    else
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z)
    end if

    if(.not.allocated(start))then
       call set_missing(value)
       return
    end if

    if(which_z > 0)then
       if(count(which_z) > size(value))then
          count(which_z) = size(value)
       else if(count(which_z) < size(value))then
          call set_missing(value((count(which_z)+1):size(value)))
       end if
    end if

    err = nf90_get_var(self%ncid,varid,value(1:count(which_z)),&
         start = start, count = count)

    if(err /= nf90_noerr)then
       call set_missing(value)
    end if

  end subroutine read_netcdf_ivec_nf90_file

  subroutine read_netcdf_real_nf90_file(self,name,v_ind,value,dim_var)

    implicit none

    class(nf90_file)                   :: self
    integer                            :: varid,which_z,err,v_ind
    integer,dimension(:),allocatable   :: start,count
    character(len=*)                   :: name
    real,intent(out)                   :: value
    logical,optional                   :: dim_var

    if(present(dim_var))then
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z, dim_var = dim_var)
    else
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z)
    end if

    if(.not.allocated(start))then
       call set_missing(value)
       return
    end if

    err = nf90_get_var(self%ncid, varid, value, start = start)

    if(err /= nf90_noerr)then
       call set_missing(value)
    end if

  end subroutine read_netcdf_real_nf90_file

  subroutine read_netcdf_rvec_nf90_file(self,name,v_ind,value,dim_var)

    implicit none
    class(nf90_file)                   :: self
    integer                            :: varid,which_z,err,v_ind
    integer,dimension(:),allocatable   :: start,count
    character(len=*)                   :: name
    real,dimension(:),intent(out)      :: value
    logical,optional                   :: dim_var

    if(present(dim_var))then
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z, dim_var = dim_var)
    else
       call get_varid_start_count(self, name, v_ind, varid, start, count,&
            which_z)
    end if

    if(.not.allocated(start))then
       call set_missing(value)
       return
    end if

    if(which_z > 0)then
       if(count(which_z) > size(value))then
          count(which_z) = size(value)
       else if(count(which_z) < size(value))then
          call set_missing(value((count(which_z)+1):size(value)))
       end if
    end if

    err = nf90_get_var(self%ncid,varid,value(1:count(which_z)),&
         start = start, count = count)

    if(err /= nf90_noerr)then
       call set_missing(value)
    end if

  end subroutine read_netcdf_rvec_nf90_file

  subroutine get_varid_start_count(self, name, z_i,&
       varid, start, count, which_z, dim_var)

      implicit none

      class(nf90_file)                 :: self
      integer,dimension(:),allocatable :: start,count
      integer,dimension(:),allocatable :: dims
      integer                          :: varid,i,ndims,which_z,dsize,z_i
      integer                          :: vtype,err
      character(len=*)                 :: name
      character(nf90_max_name)         :: dname
      character(len=15)                :: key = 'get_start_count'
      logical,optional                 :: dim_var

      err = nf90_inq_varid(self%ncid,name,varid)

!      call nc_err_check(err, key='Variable Name: '//name, nostop=.true.)

      if(err /= nf90_noerr) return

      call nc_err_check(nf90_inquire_variable(self%ncid,&
                                              varid,&
                                              ndims = ndims,&
                                              xtype = vtype),&
           key='Variable Name: '//name)

      allocate(start(ndims),count(ndims),dims(ndims))

      call nc_err_check(nf90_inquire_variable(self%ncid,&
                                              varid,&
                                              dimids = dims),&
           key='Variable Name: '//name)

      which_z = -99
      do i=1,size(dims)
         call nc_err_check(nf90_inquire_dimension(self%ncid,dims(i),&
              dname),&
           key='Dimension Name: '//dname)
         if(trim(adjustl(dname)) == "latitude" .and. &
            .not. present(dim_var))then
            start(i) = self%lat_i
            count(i) = 1
         else if(trim(adjustl(dname)) == "longitude" .and. &
                .not. present(dim_var))then
            start(i) = self%lon_i
            count(i) = 1
         else
            call nc_err_check(nf90_inquire_dimension(self%ncid,dims(i),&
                 len=dsize),&
           key='Dimension Name: '//dname)
            if(i == 1 .and. vtype == nf90_char)then
               start(i) = 1
            else
               which_z = i
               start(i) = z_i
            end if
            count(i) = dsize - (start(i) - 1)
         end if
      end do

    end subroutine get_varid_start_count

    subroutine set_nf90_lat_lon(self,lat0,lon0)

      implicit none

      class(nf90_file)                :: self
      real                            :: lat0,lon0
      integer                         :: nlat,nlon,dimid,latid,lonid
      integer                         :: ncid,i
      integer                         :: maxiter,iter
      integer,dimension(3,3)          :: lat_ind,lon_ind
      integer,dimension(2)            :: local_search
      integer,dimension(1)            :: start
      real                            :: pi=3.1415927
      character(len=12)               :: key='find_nearest'
      real,allocatable,dimension(:,:) :: &
           nc_lat,nc_lon,clat,clon,slat,slon,delX,delY,delZ,dist_sq

      ! Get latitude and longitude dimension lengths
      call self%get_dim_size('latitude', nlat, key = key)
      call self%get_dim_size('longitude', nlon, key = key)

      allocate(nc_lat(nlon,nlat),nc_lon(nlon,nlat),&
           clat(nlon,nlat),clon(nlon,nlat),&
           slat(nlon,nlat),slon(nlon,nlat),&
           delX(nlon,nlat),delY(nlon,nlat),delZ(nlon,nlat),&
           dist_sq(nlon,nlat))

      call self%read('latitude', 1, nc_lat(1,:), dim_var = .true.)
      call self%read('longitude', 1, nc_lon(:,1), dim_var = .true.)

      do i=2,nlon
         nc_lat(i,:) = nc_lat(1,:)
      end do
      do i=2,nlat
         nc_lon(:,i) = nc_lon(:,1)
      end do

      clat = cos(nc_lat*pi/180.)
      clon = cos(nc_lon*pi/180.)
      slat = sin(nc_lat*pi/180.)
      slon = sin(nc_lon*pi/180.)

      delX = cos(lat0*pi/180)*cos(lon0*pi/180)-clat*clon
      delY = cos(lat0*pi/180)*sin(lon0*pi/180)-clat*slon
      delZ = sin(lat0*pi/180) - slat

      dist_sq = delX**2 + delY**2 + delZ**2

      local_search = minloc(dist_sq)

      self%lon_i = local_search(1)
      self%lat_i = local_search(2)

      deallocate(nc_lat,nc_lon,clat,clon,slat,slon,delX,delY,delZ,dist_sq)

    end subroutine set_nf90_lat_lon

    function calc_days_since(yeardoy, refyeardoy) result(days)

      implicit none

      integer        :: days,yeardoy,refyeardoy,refyr,year,i

      refyr = refyeardoy/1000
      year = yeardoy/1000

      days = 0
      if(year > refyr + 1)then
         do i=year-1,refyr+1,-1
            days = days + days_per_year(i)
         end do
      end if

      days = days + days_per_year(refyr) + refyr*1000 - refyeardoy;

      days = days + days_per_year(year) + yeardoy - year*1000;

    end function calc_days_since

    function days_per_year(year) result(days)

      implicit none

      integer year,days

      if(modulo(year,400) == 0 .or. &
         (modulo(year,100) /= 0 .and. modulo(year,4) == 0))then
         days = 366
      else
         days = 365
      end if

    end function days_per_year

    subroutine set_nf90_date_index(self, yeardoy)

      implicit none

      class(nf90_file)                 :: self

      integer                          :: n_dates,yeardoy,days_since
      integer,dimension(3)             :: time_ind

      integer,dimension(:),allocatable :: date

      character(len=14)                :: key = 'set_date_index'

      call self%get_dim_size('DATE', n_dates, key = key)

      allocate(date(n_dates))

      call self%read('DATE',1,date,dim_var=.true.)

      days_since = yeardoy

      time_ind = 1

      time_ind(3) = n_dates

      do while(date(time_ind(1)) < days_since .and.&
               date(time_ind(3)) > days_since)

         time_ind(2) = (time_ind(3) + time_ind(1))/2

         if(date(time_ind(2)) > days_since)then
            time_ind(3) = time_ind(2)
         else if(date(time_ind(2)) < days_since)then
            time_ind(1) = time_ind(2)
         else
            exit
         end if

      end do

      self%z_i = time_ind(2)

    end subroutine set_nf90_date_index

    subroutine nf90_match_level(temp,level,begin,nvals)

      implicit none

      integer              :: level,begin,nvals,i
      integer,dimension(:) :: temp

      call set_missing(begin)
      call set_missing(nvals)

      do i=1,size(temp)
         if(is_missing(begin) .and. temp(i) == level)then
            begin = i
            nvals = 1
         else if(.not. is_missing(begin) .and. temp(i) /= level)then
            nvals = i - begin
            exit
         end if
      end do

      if(.not. is_missing(begin) .and. &
         is_missing(nvals) .and. &
         temp(size(temp)) == level)then
         nvals = size(temp) - begin + 1
      end if

    end subroutine nf90_match_level

    subroutine read_level_real_nf90_file(self,dname,level,vname,value)

      implicit none

      class(nf90_file)                 :: self
      character(len=*)                 :: dname,vname
      real                             :: value
      integer                          :: level,dimsize,i,varid,which_z
      integer                          :: begin,nvals
      integer,dimension(:),allocatable :: start,count
      integer,dimension(:),allocatable :: temp

      call get_varid_start_count(self, dname, 1, varid, start, count,&
           which_z)

      allocate(temp(count(which_z)))

      call self%read(dname,1,temp)

      call nf90_match_level(temp,level,begin,nvals)

      call self%read(vname,begin,value)

    end subroutine read_level_real_nf90_file

    subroutine read_level_rvec_nf90_file(self,dname,level,vname,value)

      implicit none

      class(nf90_file)                 :: self
      character(len=*)                 :: dname,vname
      real,dimension(:)                :: value
      integer                          :: level,dimsize,i,varid,which_z
      integer                          :: begin,nvals
      integer,dimension(:),allocatable :: start,count
      integer,dimension(:),allocatable :: temp

      call get_varid_start_count(self, dname, 1, varid, start, count,&
           which_z)

      allocate(temp(count(which_z)))

      call self%read(dname,1,temp)

      call nf90_match_level(temp,level,begin,nvals)

      if(size(value) > nvals)then
         call set_missing(value((nvals+1):size(value)))
      else if(size(value) < nvals)then
         nvals = size(value)
      end if

      call self%read(vname,begin,value(1:nvals))

    end subroutine read_level_rvec_nf90_file

    subroutine read_level_integer_nf90_file(self,dname,level,vname,value)

      implicit none

      class(nf90_file)                 :: self
      character(len=*)                 :: dname,vname
      integer                          :: value
      integer                          :: level,dimsize,i,varid,which_z
      integer                          :: begin,nvals
      integer,dimension(:),allocatable :: start,count
      integer,dimension(:),allocatable :: temp

      call get_varid_start_count(self, dname, 1, varid, start, count,&
           which_z)

      allocate(temp(count(which_z)))

      call self%read(dname,1,temp)

      call nf90_match_level(temp,level,begin,nvals)

      call self%read(vname,begin,value)

    end subroutine read_level_integer_nf90_file

    subroutine read_level_ivec_nf90_file(self,dname,level,vname,value)

      implicit none

      class(nf90_file)                 :: self
      character(len=*)                 :: dname,vname
      integer,dimension(:)             :: value
      integer                          :: level,dimsize,i,varid,which_z
      integer                          :: begin,nvals
      integer,dimension(:),allocatable :: start,count
      integer,dimension(:),allocatable :: temp

      call get_varid_start_count(self, dname, 1, varid, start, count,&
           which_z)

      allocate(temp(count(which_z)))

      call self%read(dname,1,temp)

      call nf90_match_level(temp,level,begin,nvals)

      if(size(value) > nvals)then
         call set_missing(value((nvals+1):size(value)))
      else if(size(value) < nvals)then
         nvals = size(value)
      end if

      call self%read(vname,begin,value(1:nvals))

    end subroutine read_level_ivec_nf90_file

    subroutine read_level_character_nf90_file(self,dname,level,vname,value)

      implicit none

      class(nf90_file)                 :: self
      character(len=*)                 :: dname,vname
      character(len=*)                 :: value
      integer                          :: level,dimsize,i,varid,which_z
      integer                          :: begin,nvals
      integer,dimension(:),allocatable :: start,count
      integer,dimension(:),allocatable :: temp

      call get_varid_start_count(self, dname, 1, varid, start, count,&
           which_z)

      allocate(temp(count(which_z)))

      call self%read(dname,1,temp)

      call nf90_match_level(temp,level,begin,nvals)

      call self%read(vname,begin,value)

    end subroutine read_level_character_nf90_file

    subroutine read_level_cvec_nf90_file(self,dname,level,vname,value)

      implicit none

      class(nf90_file)                 :: self
      character(len=*)                 :: dname,vname
      character(len=*),dimension(:)    :: value
      integer                          :: level,dimsize,i,varid,which_z
      integer                          :: begin,nvals
      integer,dimension(:),allocatable :: start,count
      integer,dimension(:),allocatable :: temp

      call get_varid_start_count(self, dname, 1, varid, start, count,&
           which_z)

      allocate(temp(count(which_z)))

      call self%read(dname,1,temp)

      call nf90_match_level(temp,level,begin,nvals)

      if(size(value) > nvals)then
         call set_missing(value((nvals+1):size(value)))
      else if(size(value) < nvals)then
         nvals = size(value)
      end if

      call self%read(vname,begin,value(1:nvals))

    end subroutine read_level_cvec_nf90_file

    subroutine get_ind_nvals_nf90_file(self,dname,level,ind,nvals)

      implicit none

      class(nf90_file)        :: self
      character(len=*)        :: dname
      integer                 :: level,ind,nvals,varid,which_z
      integer,dimension(:),allocatable :: start,count
      integer,dimension(:),allocatable :: temp

      call get_varid_start_count(self, dname, 1, varid, start, count,&
           which_z)

      allocate(temp(count(which_z)))

      call self%read(dname,1,temp)

      call nf90_match_level(temp,level,ind,nvals)

    end subroutine get_ind_nvals_nf90_file

    subroutine set_missing_nf90_real(value)

      implicit none

      real         :: value

      value = nf90_missing_real

    end subroutine set_missing_nf90_real

    subroutine set_missing_nf90_rvec(value)

      implicit none

      real,dimension(:)     :: value

      value = nf90_missing_real

    end subroutine set_missing_nf90_rvec

    subroutine set_missing_nf90_integer(value)

      implicit none

      integer        :: value

      value = nf90_missing_integer

    end subroutine set_missing_nf90_integer

    subroutine set_missing_nf90_ivec(value)

      implicit none

      integer,dimension(:)     :: value

      value = nf90_missing_integer

    end subroutine set_missing_nf90_ivec

    subroutine set_missing_nf90_character(value)

      implicit none

      character(len=*)        :: value

      value = nf90_missing_character

    end subroutine set_missing_nf90_character

    subroutine set_missing_nf90_cvec(value)

      implicit none

      character(len=*),dimension(:)     :: value

      value = nf90_missing_character

    end subroutine set_missing_nf90_cvec

    function is_missing_nf90_real(value) result(test)

      implicit none

      real         :: value
      logical      :: test

      test = value == nf90_missing_real

    end function is_missing_nf90_real

    function is_missing_nf90_integer(value) result(test)

      implicit none

      integer      :: value
      logical      :: test

      test = value == nf90_missing_integer

    end function is_missing_nf90_integer

end module nf90_file_module
