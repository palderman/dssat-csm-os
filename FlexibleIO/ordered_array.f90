module ordered_array

  type real_ordered_array
     real,dimension(:),allocatable :: values
     integer curr_end
   contains
     procedure :: insert => insert_real_ordered_array
     procedure :: reallocate => reallocate_real_ordered_array
     procedure :: find => find_value_in_real_ordered_array
  end type real_ordered_array

contains

  subroutine reallocate_real_ordered_array(self)

    implicit none

    integer new_size,i
    class(real_ordered_array)       :: self
    real,dimension(:),allocatable :: r_temp

    if(allocated(self%values))then
       new_size = size(self%values)*3/2+1
       call move_alloc(self%values,r_temp)
       allocate(self%values(new_size))
       do i=1,size(r_temp)
          self%values(i) = r_temp(i)
       end do
       deallocate(r_temp)
    else
       allocate(self%values(1))
    end if

  end subroutine reallocate_real_ordered_array

  subroutine insert_real_ordered_array(self,new_value)

    implicit none

    real :: new_value
    integer  ::i,j,insert_loc
    integer,dimension(:),allocatable :: new_index
    logical  :: add
    class(real_ordered_array) :: self

    add = .FALSE.
    if(.not. allocated(self%values))then
       allocate(self%values(1))
       self%curr_end = 0
       self%curr_end = 1
       self%values(1) = new_value
       return
    end if

    do i=1,self%curr_end
       if(new_value == self%values(i)) return
    end do

    if(new_value > self%values(self%curr_end)) then
       add = .TRUE.
       insert_loc = self%curr_end + 1
    else
       if(self%curr_end>1)then
          do i=self%curr_end-1,1,-1
             if(new_value == self%values(i))then
                return
             else
                add = new_value > self%values(i)
                if(add)then
                   insert_loc = i + 1
                   exit
                else if(i == 1)then
                   insert_loc = 1
                   add = .TRUE.
                end if
             end if
          end do
       else
          if(new_value > self%values(1))then
             insert_loc = 2
          else
             insert_loc = 1
          end if
          add = .TRUE.
       end if
    end if
    if(add)then
       if(self%curr_end == size(self%values)) call self%reallocate()
       self%curr_end = self%curr_end + 1
       do j=self%curr_end,insert_loc+1,-1
          self%values(j) = self%values(j-1)
       end do
       self%values(insert_loc) = new_value
    end if

  end subroutine insert_real_ordered_array

  function find_value_in_real_ordered_array(self,value) result(ind)

    implicit none

    integer ind,upper,lower
    real    value
    class(real_ordered_array) self

    upper = self%curr_end
    lower = 1

    ind = -99

    do while(.true.)
       if(value == self%values(1))then
          ind = 1
          return
       else if(value == self%values(upper))then
          ind = upper
          return
       else
          ind = (upper + lower)/2
          if(value == self%values(ind))then
             return
          else if(value > self%values(ind))then
             lower = ind
          else if(value < self%values(ind))then
             upper = ind
          end if
          if(lower + 1 == upper) return
       end if
    end do

  end function find_value_in_real_ordered_array

end module ordered_array
