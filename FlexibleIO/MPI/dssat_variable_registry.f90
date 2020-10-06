module dssat_variable_registry

  type registered_variable
     integer                      :: curr_ind
     character(len=:),allocatable :: name
     real,dimension(:),allocatable :: r_val
     integer,dimension(:),allocatable :: i_val
     real,pointer     :: r_ptr
     integer,pointer  :: i_ptr
   contains
     procedure :: print => print_registered_var
     procedure :: init => initialize_registered_variable
     procedure :: reallocate_real
     procedure :: reallocate_integer
     procedure :: reallocate => reallocate_registered_variable
     procedure :: size => registered_variable_size
     procedure :: store => store_value_registered_variable
     procedure :: reset => reset_variable
  end type registered_variable

  type registry_type
     type(registered_variable),dimension(:),allocatable :: variables
   contains
     procedure :: print => print_registry
     procedure :: initialize_registry_scalar
     procedure :: initialize_registry_array
     generic   :: initialize => &
          initialize_registry_scalar,&
          initialize_registry_array
     procedure :: set_target_real
     procedure :: set_target_integer
     generic   :: set_target => &
          set_target_real,&
          set_target_integer
     procedure :: check_registry
     procedure :: add_to_registry_scalar
     procedure :: add_to_registry_array
     generic   :: add_to_registry => &
          add_to_registry_scalar,&
          add_to_registry_array
     procedure :: csv_to_registry
     procedure :: store => store_values_registry
     procedure :: reset => reset_registry
  end type registry_type

  type(registry_type) :: seasonal_registry
  type(registry_type) :: daily_registry

  interface assignment (=)
     module procedure registered_variable_assignment
  end interface assignment (=)

contains

  subroutine reset_variable(self)

    implicit none

    class(registered_variable) self

    self%curr_ind = 0

  end subroutine reset_variable

  subroutine reset_registry(self)

    implicit none

    integer i

    class(registry_type) :: self

    do i=1,size(self%variables)
       call self%variables(i)%reset()
    end do

  end subroutine reset_registry

  subroutine print_registry(self)

    implicit none

    integer i

    class(registry_type) :: self

    do i=1,size(self%variables)
       call self%variables(i)%print()
    end do

  end subroutine print_registry

  subroutine print_registered_var(self)

    implicit none

    integer i

    class(registered_variable) self

    if(allocated(self%r_val))then
          do i=1,self%curr_ind
             print *, self%name,' at time ',i,' = ',self%r_val(i)
          end do
    else if(allocated(self%i_val)) then
          do i=1,self%curr_ind
             print *, self%name,' at time ',i,' = ',self%i_val(i)
          end do
    else
       print *, self%name,' = no target assigned'
    end if


  end subroutine print_registered_var

  subroutine initialize_registry_scalar(self,r_name)

    implicit none

    integer                       :: i
    character(len=*)              :: r_name

    class(registry_type) :: self

    allocate(self%variables(1))

    call self%variables(1)%init(r_name)

  end subroutine initialize_registry_scalar

  subroutine initialize_registry_array(self,r_names)

    implicit none

    integer                       :: i
    character(len=*),dimension(:) :: r_names

    class(registry_type) :: self
    allocate(self%variables(size(r_names)))

    do i=1,size(self%variables)
       call self%variables(i)%init(r_names(i))
    end do

  end subroutine initialize_registry_array

  subroutine check_registry(self,r_names,add,n_new)

    implicit none

    integer i,j,n_new
    logical,dimension(:)          :: add
    character(len=*),dimension(:) :: r_names
    class(registry_type) :: self

    n_new = size(r_names)
    add = .true.
    do i=1,size(r_names)
       check_loop: do j=1,size(self%variables)
          if(self%variables(j)%name == r_names(i))then
             n_new = n_new - 1
             add(i) = .false.
             exit check_loop
          end if
       end do check_loop
    end do

  end subroutine check_registry

  subroutine add_to_registry_scalar(self,r_name)

    implicit none

    character(len=*)                 :: r_name
    class(registry_type)             :: self

    call add_to_registry_array(self,(/r_name/))

  end subroutine add_to_registry_scalar

  subroutine add_to_registry_array(self,r_names)

    implicit none

    integer      :: i,j,n_new

    class(registry_type) :: self
    type(registered_variable),dimension(:),allocatable :: tmp_vars

    character(len=*),dimension(:) :: r_names
    logical,dimension(:),allocatable :: add

    if(.not.allocated(self%variables))then
       call self%initialize(r_names)
    else
       allocate(add(size(r_names)))
       call self%check_registry(r_names,add,n_new)
       if(n_new > 0)then
          call move_alloc(self%variables,tmp_vars)
          allocate(self%variables(size(tmp_vars)+n_new))
          do i=1,size(tmp_vars)
             self%variables(i) = tmp_vars(i)
          end do
          j=1
          do i=1,size(r_names)
             if(add(i))then
                call self%variables(j+size(tmp_vars))%init(r_names(i))
                j = j + 1
             end if
          end do
          deallocate(tmp_vars)
       end if
       deallocate(add)
    end if

  end subroutine add_to_registry_array

  subroutine initialize_registered_variable(self,rname)

    implicit none

    character(len=*)   :: rname
    class(registered_variable) :: self

    self%name = trim(rname)
    self%curr_ind = 0
    nullify(self%r_ptr)
    nullify(self%i_ptr)

  end subroutine initialize_registered_variable

  subroutine set_target_real(self,v_name,variable)

    implicit none

    integer                    :: i
    character(len=*)           :: v_name
    real,target                :: variable
    class(registry_type)       :: self

    if(allocated(self%variables))then
       do i=1,size(self%variables)
          if(trim(adjustl(self%variables(i)%name)) == v_name)then
             self%variables(i)%r_ptr => variable
             nullify(self%variables(i)%i_ptr)
          end if
       end do
    end if

  end subroutine set_target_real

  subroutine set_target_integer(self,v_name,variable)

    implicit none

    integer                    :: i
    character(len=*)           :: v_name
    integer,target             :: variable
    class(registry_type)       :: self

    if(allocated(self%variables))then
       do i=1,size(self%variables)
          if(trim(adjustl(self%variables(i)%name)) == v_name)then
             self%variables(i)%i_ptr => variable
             nullify(self%variables(i)%r_ptr)
          end if
       end do
    end if

  end subroutine set_target_integer

  subroutine registered_variable_assignment(lhs,rhs)

    implicit none

    integer i
    type(registered_variable),intent(out)  :: lhs
    type(registered_variable),intent(in) :: rhs

    lhs%curr_ind = rhs%curr_ind
    lhs%name = rhs%name
    if(associated(rhs%r_ptr))then
       lhs%r_ptr => rhs%r_ptr
       if(allocated(rhs%r_val))then
          allocate(lhs%r_val(size(rhs%r_val)))
          do i=1,rhs%curr_ind
             lhs%r_val(i) = rhs%r_val(i)
          end do
       end if
    else
       nullify(lhs%r_ptr)
    end if
    if(associated(rhs%i_ptr))then
       lhs%i_ptr => rhs%i_ptr
       if(allocated(rhs%i_val))then
          allocate(lhs%i_val(size(rhs%i_val)))
          do i=1,rhs%curr_ind
             lhs%i_val(i) = rhs%i_val(i)
          end do
       end if
    else
       nullify(lhs%i_ptr)
    end if

  end subroutine registered_variable_assignment

  subroutine csv_to_registry(self,vlist)

    implicit none

    integer          p1,p2,spc
    character(len=*) :: vlist
    class(registry_type) :: self

    p1 = 1
    do while(p1 <= len(vlist))
       p2 = p1 + index(vlist(p1:len(vlist)),',') - 2
       if(p2 < p1)then
          spc = index(vlist(p1:len(vlist)),' ')
          if(spc > 2)then
             p2 = p1 + spc - 2
          else
             p2 = len(vlist)
          end if
       end if
       call self%add_to_registry(vlist(p1:p2))
       p1 = p2 + 2
    end do

  end subroutine csv_to_registry

  subroutine reallocate_real(self)

    implicit none

    integer new_size,i
    class(registered_variable)       :: self
    real,dimension(:),allocatable :: r_temp

    if(allocated(self%r_val))then
       new_size = size(self%r_val)*3/2+1
       call move_alloc(self%r_val,r_temp)
       allocate(self%r_val(new_size))
       do i=1,size(r_temp)
          self%r_val(i) = r_temp(i)
       end do
       deallocate(r_temp)
    else
       allocate(self%r_val(1))
    end if

  end subroutine reallocate_real

  subroutine reallocate_integer(self)

    implicit none

    integer new_size,i
    class(registered_variable)       :: self
    integer,dimension(:),allocatable :: i_temp

    if(allocated(self%i_val))then
       new_size = size(self%i_val)*3/2+1
       call move_alloc(self%i_val,i_temp)
       allocate(self%i_val(new_size))
       do i=1,size(i_temp)
          self%i_val(i) = i_temp(i)
       end do
       deallocate(i_temp)
    else
       allocate(self%i_val(1))
    end if

  end subroutine reallocate_integer

  subroutine reallocate_registered_variable(self)

    implicit none

    class(registered_variable) self

    if(associated(self%r_ptr))then
       call self%reallocate_real()
    else if(associated(self%i_ptr))then
       call self%reallocate_integer()
    end if

  end subroutine reallocate_registered_variable

  function registered_variable_size(self) result(v_size)

    implicit none

    integer v_size
    class(registered_variable) self

    if(associated(self%r_ptr))then
       if(allocated(self%r_val))then
          v_size = size(self%r_val)
       else
          v_size = 0
       end if
    else if(associated(self%i_ptr))then
       if(allocated(self%i_val))then
          v_size = size(self%i_val)
       else
          v_size = 0
       end if
    end if

  end function registered_variable_size

  subroutine store_value_registered_variable(self)

    implicit none

    integer  new_size
    class(registered_variable) self

    if(self%curr_ind == self%size()) &
         call self%reallocate()

    self%curr_ind = self%curr_ind + 1

    if(associated(self%r_ptr))then
       self%r_val(self%curr_ind) = self%r_ptr
    else if(associated(self%i_ptr))then
       self%i_val(self%curr_ind) = self%i_ptr
    end if

  end subroutine store_value_registered_variable

  subroutine store_values_registry(self)

    implicit none

    integer i

    class(registry_type) :: self

    do i=1,size(self%variables)
       call self%variables(i)%store()
    end do

  end subroutine store_values_registry

end module dssat_variable_registry
