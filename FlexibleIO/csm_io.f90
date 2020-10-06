module class_alchar

    type alchar
       character(len=1),allocatable,dimension(:)    :: s
    end type alchar

    interface assignment(=)
       module procedure alchar_assign
       module procedure alchar_assign_to_char
       module procedure alchar_assign_to_alchar
       module procedure alchar_assign_to_char_array
       module procedure alchar_array_assign_to_char_array
    end interface assignment(=)

    interface check_names
       module procedure check_names_char
       module procedure check_names_alchar
    end interface check_names

    interface index
       module procedure index_alloc_str
    end interface index

    contains

      function find_name(names,key) result(ind)

        implicit none

        integer :: i,ind

        type(alchar),dimension(:),allocatable :: names
        character(len=*)    :: key
        character(len=len(key)) :: tmp

        ind = 0

        if(allocated(names))then
           do i=1,size(names)
              tmp = as_string(names(i)%s)
              if(tmp==trim(adjustl(key))) then
                 ind = i
                 exit
              end if
           end do
        end if

      end function find_name

      function index_alloc_str(as,sbstr) result(ind)

        implicit none

        integer :: ind

        type(alchar) :: as
        character(len=*)     :: sbstr

        ind = index(as_string(as%s),sbstr)

      end function index_alloc_str

      function as_string(str_vec) result(string)

        implicit none

        integer :: i
        character(len=1),dimension(:)      :: str_vec
        character(len=size(str_vec))     :: string

        do i=1,size(str_vec)
           string(i:i) = str_vec(i)
        end do

      end function as_string

      function as_alchar(string) result(str_vec)

        implicit none

        integer :: i
        character(len=*)                        :: string
        character(len=1),dimension(len(string)) :: str_vec

        do i=1,size(str_vec)
            str_vec(i) = string(i:i)
        end do

      end function as_alchar

      subroutine alchar_assign(str_vec_ac,string)

        implicit none

        integer                        :: i

        type(alchar),intent(out)          :: str_vec_ac
        character(len=*),intent(in)               :: string

        if(.not.allocated(str_vec_ac%s)) then
           allocate(str_vec_ac%s(len(string)))
        else if(size(str_vec_ac%s)<len(string))then
           deallocate(str_vec_ac%s)
           allocate(str_vec_ac%s(len(string)))
        end if

        do i=1,len(string)
           str_vec_ac%s(i) = string(i:i)
        end do

      end subroutine alchar_assign

      subroutine alchar_assign_to_alchar(str_vec2,str_vec1)

        implicit none

        integer                        :: i

        type(alchar),intent(in)          :: str_vec1
        type(alchar),intent(out)          :: str_vec2

        if(allocated(str_vec1%s))then
           if(.not.allocated(str_vec2%s)) then
              allocate(str_vec2%s(size(str_vec1%s)))
           else if(size(str_vec2%s)<size(str_vec1%s))then
              deallocate(str_vec2%s)
              allocate(str_vec2%s(size(str_vec1%s)))
           end if
           do i=1,size(str_vec1%s)
              str_vec2%s(i) = str_vec1%s(i)
           end do
        else
           if(allocated(str_vec2%s)) deallocate(str_vec2%s)
        end if

      end subroutine alchar_assign_to_alchar

      subroutine alchar_assign_to_char(string,str_vec)

        implicit none

        integer                        :: i

        type(alchar),intent(in)                   :: str_vec
        character(len=*),intent(out) :: string

        if(allocated(str_vec%s))then
           do i=1,len(string)
              if(i<=size(str_vec%s))then
                 string(i:i) = str_vec%s(i)
              else
                 string(i:i) = ' '
              end if
           end do
        else
           string = ' '
        end if

      end subroutine alchar_assign_to_char

      subroutine alchar_assign_to_char_array(string,str_vec)

        implicit none

        integer                        :: i

        type(alchar),intent(in)                   :: str_vec
        character(len=*),dimension(:),intent(out) :: string

        do i=1,size(string)
           string(i) = str_vec
        end do

      end subroutine alchar_assign_to_char_array

      subroutine alchar_array_assign_to_char_array(string,str_vec)

        implicit none

        integer                        :: i,j

        type(alchar),dimension(:),intent(in)      :: str_vec
        character(len=*),dimension(:),intent(out) :: string

        do i=1,size(string)
           string(i) = str_vec(i)
        end do

      end subroutine alchar_array_assign_to_char_array

      function check_names_char(curr_names,new_names) result(found)

        implicit none

        integer                              :: i,j

        character(len=*),dimension(:)        :: curr_names
        character(len=*),dimension(:)        :: new_names
        logical,dimension(size(new_names))   :: found

        found = .false.

        do i=1,size(new_names)
           do j=1,size(curr_names)
              if(index(curr_names(j),&
                   trim(adjustl(new_names(i))))/=0) then
                 found(i) = .true.
                 exit
              end if
           end do
        end do

      end function check_names_char

      function check_names_alchar(curr_names,new_names) result(found)

        implicit none
        
        integer                              :: i,j

        type(alchar),dimension(:)        :: curr_names
        character(len=*),dimension(:)        :: new_names
        logical,dimension(size(new_names))   :: found

        found = .false.

        do i=1,size(new_names)
           do j=1,size(curr_names)
              if(index(curr_names(j),&
                   trim(adjustl(new_names(i))))/=0) then
                 found(i) = .true.
                 exit
              end if
           end do
        end do

      end function check_names_alchar

end module class_alchar

module class_io

    use class_alchar

    type io_type
         type(alchar),allocatable,dimension(:)      :: char_name
         type(alchar),allocatable,dimension(:)      :: real_name
         type(alchar),allocatable,dimension(:)      :: int_name
         type(alchar),allocatable,dimension(:,:)    :: char_value
         integer,allocatable,dimension(:,:)         :: int_value
         real,allocatable,dimension(:,:)            :: real_value
    end type io_type

    interface assignment(=)
       module procedure assign_io_type
    end interface assignment(=)

    interface add_row
       module procedure add_real_row
       module procedure add_int_row
       module procedure add_char_row
    end interface add_row

    contains

      subroutine assign_io_type(io_out,io_in)

        implicit none

        integer                       :: i,j

        type(io_type),intent(in)      :: io_in
        type(io_type),intent(out)     :: io_out

        if(allocated(io_in%char_name))then
           if(allocated(io_out%char_name))then
              deallocate(io_out%char_name)
           end if
           allocate(io_out%char_name(size(io_in%char_name)))
           do i=1,size(io_in%char_name)
              io_out%char_name(i) = io_in%char_name(i)
           end do
        else if(allocated(io_out%char_name))then
           deallocate(io_out%char_name)
        end if

        if(allocated(io_in%int_name))then
           if(allocated(io_out%int_name))then
              deallocate(io_out%int_name)
           end if
           allocate(io_out%int_name(size(io_in%int_name)))
           do i=1,size(io_in%int_name)
              io_out%int_name(i) = io_in%int_name(i)
           end do
        else if(allocated(io_out%int_name))then
           deallocate(io_out%int_name)
        end if

        if(allocated(io_in%real_name))then
           if(allocated(io_out%real_name))then
              deallocate(io_out%real_name)
           end if
           allocate(io_out%real_name(size(io_in%real_name)))
           do i=1,size(io_in%real_name)
              io_out%real_name(i) = io_in%real_name(i)
           end do
        else if(allocated(io_out%real_name))then
           deallocate(io_out%real_name)
        end if

        if(allocated(io_in%real_value))then
           if(allocated(io_out%real_value))then
              deallocate(io_out%real_value)
           end if
           allocate(io_out%real_value(size(io_in%real_value,1),&
                size(io_in%real_value,2)))
           do i=1,size(io_in%real_value,1)
              io_out%real_value(i,:) = io_in%real_value(i,:)
           end do
        else if(allocated(io_out%real_value))then
           deallocate(io_out%real_value)
        end if

        if(allocated(io_in%int_value))then
           if(allocated(io_out%int_value))then
              deallocate(io_out%int_value)
           end if
           allocate(io_out%int_value(size(io_in%int_value,1),&
                size(io_in%int_value,2)))
           do i=1,size(io_in%int_value,1)
              io_out%int_value(i,:) = io_in%int_value(i,:)
           end do
        else if(allocated(io_out%int_value))then
           deallocate(io_out%int_value)
        end if

        if(allocated(io_in%char_value))then
           if(allocated(io_out%char_value))then
              deallocate(io_out%char_value)
           end if
           allocate(io_out%char_value(size(io_in%char_value,1),&
                size(io_in%char_value,2)))
           do i=1,size(io_in%char_value,1)
              do j=1,size(io_in%char_value,2)
                 io_out%char_value(i,j) = io_in%char_value(i,j)
              end do
           end do
        else if(allocated(io_out%char_value))then
           deallocate(io_out%char_value)
        end if

      end subroutine assign_io_type

      subroutine add_io_var_name(curr_names,new_names)

        implicit none

        integer                                 :: i,j

        integer,allocatable,dimension(:)        :: tmpint
        logical,allocatable,dimension(:)        :: tmplog

        type(alchar),allocatable,dimension(:)   :: tmpname
        type(alchar),allocatable,dimension(:)   :: curr_names

        character(len=*),dimension(:)           :: new_names

        allocate(tmplog(size(new_names)))
        allocate(tmpint(size(new_names)))

        if(allocated(curr_names))then

           tmplog = check_names(curr_names,new_names)

           where(tmplog)
              tmpint=0
           elsewhere
              tmpint=1
           end where

           call move_alloc(curr_names,tmpname)
           allocate(curr_names(size(tmpname)+sum(tmpint)))
           do i=1,size(tmpname)
              curr_names(i) = tmpname(i)
           end do
           j = size(tmpname)
           deallocate(tmpname)
        else
           tmplog = .false.
           allocate(curr_names(size(new_names)))
           j=0
        end if
        do i=1,size(new_names)
           if(.not.tmplog(i))then
              j = j + 1
              curr_names(j) = trim(adjustl(new_names(i)))
           end if
        end do

        deallocate(tmplog,tmpint)
        
      end subroutine add_io_var_name

      subroutine add_char_col(char_value,ncols)

        implicit none

        type(alchar),allocatable,dimension(:,:)  :: char_value
        type(alchar),allocatable,dimension(:,:)  :: tmp_value
        integer                                  :: ncols,i,j

        call move_alloc(char_value,tmp_value)
        allocate(char_value(size(tmp_value,1),size(tmp_value,2)+ncols))
        do i=1,size(tmp_value,1)
           do j=1,size(tmp_value,2)
              char_value(i,j) = tmp_value(i,j)
           end do
        end do
        deallocate(tmp_value)

      end subroutine add_char_col

      subroutine add_int_col(int_value,ncols)

        implicit none

        integer,allocatable,dimension(:,:)  :: int_value
        integer,allocatable,dimension(:,:)  :: tmp_value
        integer                                :: ncols

        call move_alloc(int_value,tmp_value)
        allocate(int_value(size(tmp_value,1),size(tmp_value,2)+ncols))
        int_value(1:size(tmp_value,1),1:size(tmp_value,2)) = tmp_value
        deallocate(tmp_value)

      end subroutine add_int_col

      subroutine add_real_col(real_value,ncols)

        implicit none

        real,allocatable,dimension(:,:)  :: real_value
        real,allocatable,dimension(:,:)  :: tmp_value
        integer                                :: ncols

        call move_alloc(real_value,tmp_value)
        allocate(real_value(size(tmp_value,1),size(tmp_value,2)+ncols))
        real_value(1:size(tmp_value,1),1:size(tmp_value,2)) = tmp_value
        deallocate(tmp_value)

      end subroutine add_real_col

      subroutine add_char_row(char_value,nrows)

        implicit none

        integer                                  :: i,j
        type(alchar),allocatable,dimension(:,:)  :: char_value
        type(alchar),allocatable,dimension(:,:)  :: tmp_value
        integer                                :: nrows

        call move_alloc(char_value,tmp_value)
        allocate(char_value(size(tmp_value,1)+nrows,size(tmp_value,2)))
        do i=1,size(tmp_value,1)
           do j=1,size(tmp_value,2)
              char_value(i,j) = tmp_value(i,j)
           end do
        end do
        deallocate(tmp_value)

      end subroutine add_char_row

      subroutine add_int_row(int_value,nrows)

        implicit none

        integer,allocatable,dimension(:,:)  :: int_value
        integer,allocatable,dimension(:,:)  :: tmp_value
        integer                                :: nrows

        call move_alloc(int_value,tmp_value)
        allocate(int_value(size(tmp_value,1)+nrows,size(tmp_value,2)))
        int_value(1:size(tmp_value,1),1:size(tmp_value,2)) = tmp_value
        deallocate(tmp_value)

      end subroutine add_int_row

      subroutine add_real_row(real_value,nrows)

        implicit none

        real,allocatable,dimension(:,:)  :: real_value
        real,allocatable,dimension(:,:)  :: tmp_value
        integer                                :: nrows

        call move_alloc(real_value,tmp_value)
        allocate(real_value(size(tmp_value,1)+nrows,size(tmp_value,2)))
        real_value(1:size(tmp_value,1),1:size(tmp_value,2)) = tmp_value
        deallocate(tmp_value)

      end subroutine add_real_row

end module class_io

module class_section

    use class_io

    type io_sec_type
         type(alchar)                               :: header
         type(io_type),allocatable,dimension(:)     :: tier
    end type io_sec_type

    interface assignment(=)
       module procedure assign_section
    end interface assignment(=)

    contains

      subroutine assign_section(sec_out,sec_in)

        implicit none

        integer                                    :: i

        type(io_sec_type),intent(in)  :: sec_in
        type(io_sec_type),intent(out) :: sec_out

        sec_out%header = sec_in%header

        if(allocated(sec_out%tier))then
           if(size(sec_out%tier)/=size(sec_in%tier))then
              deallocate(sec_out%tier)
              allocate(sec_out%tier(size(sec_in%tier)))
           end if
        else
           allocate(sec_out%tier(size(sec_in%tier)))
       end if
        
        do i=1,size(sec_in%tier)
           sec_out%tier(i) = sec_in%tier(i)
        end do

      end subroutine assign_section

end module class_section

module class_ioput
  
    use class_section

    type csm_io_type
         type(io_sec_type),allocatable,dimension(:),private :: section
       contains
           procedure :: not_empty
           procedure :: find => find_sec
           procedure :: add_sec
           procedure :: add_var
           procedure :: destroy => destroy_io
           procedure :: put_io_val_real
           procedure :: put_io_val_int
           procedure :: put_io_val_char
           procedure :: put_io_val_real_array
           procedure :: put_io_val_int_array
           procedure :: put_io_val_char_array
           procedure :: get_io_val_real
           procedure :: get_io_val_int
           procedure :: get_io_val_char
           procedure :: get_io_val_real_array
           procedure :: get_io_val_int_array
           procedure :: get_io_val_char_array
           generic   :: put => put_io_val_real,put_io_val_int,&
                put_io_val_char,put_io_val_real_array,&
                put_io_val_int_array,put_io_val_char_array
           generic   :: get => get_io_val_real,get_io_val_int,&
                get_io_val_char,get_io_val_real_array,&
                get_io_val_int_array,get_io_val_char_array
    end type csm_io_type

  contains

      function not_empty(ioput) result(is_not_empty)

        implicit none

        logical is_not_empty

        class(csm_io_type)   :: ioput

        is_not_empty = allocated(ioput%section)

      end function not_empty

      function find_sec(ioput,key) result(ind)

        implicit none

        integer :: i,ind

        class(csm_io_type)   :: ioput

        character(len=*)    :: key

        ind = 0

        do i=1,size(ioput%section)
           if(index(ioput%section(i)%header,key)/=0) then
              ind = i
              exit
           end if
        end do
        
      end function find_sec

      subroutine add_sec(ioput,header,ntiers)

        implicit none

        integer                                    :: i,tier_size
        integer,optional                           :: ntiers
        character(len=*)                           :: header
        class(csm_io_type)                         :: ioput
        type(io_sec_type),allocatable,dimension(:) :: tmpsec

        if(present(ntiers))then
           tier_size=ntiers
        else
           tier_size = 1
        end if

        if(.not.allocated(ioput%section))then
           allocate(ioput%section(1))
           ioput%section(1)%header = header
           allocate(ioput%section(1)%tier(tier_size))
        else
           if(find_sec(ioput,header)==0)then
              call move_alloc(ioput%section,tmpsec)
              allocate(ioput%section(size(tmpsec)+1))
              do i=1,size(tmpsec)
                 ioput%section(i) = tmpsec(i)
              end do
              ioput%section(size(tmpsec)+1)%header = header
              allocate(ioput%section(size(tmpsec)+1)%tier(tier_size))
              deallocate(tmpsec)
           end if
        end if
      end subroutine add_sec

      subroutine put_io_val_real_array(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s,diff
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        real,dimension(:),intent(in)       :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%real_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%real_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(i+size(val)-1>size(ioput%section(s)%tier(t)%real_value,1))then
           diff = i+size(val)-1 - size(ioput%section(s)%tier(t)%real_value,1)
           call add_row(ioput%section(s)%tier(t)%real_value,diff)
        end if
        ioput%section(s)%tier(t)%real_value(i:(i+size(val)-1),j) = val

      end subroutine put_io_val_real_array

      subroutine put_io_val_int_array(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s,diff
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        integer,dimension(:),intent(in)    :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%int_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%int_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(i+size(val)-1>size(ioput%section(s)%tier(t)%int_value,1))then
           diff = i+size(val)-1 - size(ioput%section(s)%tier(t)%int_value,1)
           call add_row(ioput%section(s)%tier(t)%int_value,diff)
        end if
        ioput%section(s)%tier(t)%int_value(i:(i+size(val)-1),j) = val

      end subroutine put_io_val_int_array

      subroutine put_io_val_char_array(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                                  :: i,j,t,s,diff,k
        character(len=*)                         :: sec_name
        integer,optional,intent(in)              :: ind,tier
        character(len=*),intent(in)              :: vname
        character(len=*),dimension(:),intent(in) :: val
        class(csm_io_type)                        :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%char_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%char_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(i+size(val)-1>size(ioput%section(s)%tier(t)%char_value,1))then
           diff = i+size(val)-1 - size(ioput%section(s)%tier(t)%char_value,1)
           call add_row(ioput%section(s)%tier(t)%char_value,diff)
        end if
        do k=1,size(val)
           ioput%section(s)%tier(t)%char_value(i-1+k,j) = trim(adjustl(val(k)))
        end do

      end subroutine put_io_val_char_array

      subroutine put_io_val_real(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s,diff
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        real,intent(in)                    :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%real_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%real_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(i>size(ioput%section(s)%tier(t)%real_value,1))then
           diff = i - size(ioput%section(s)%tier(t)%real_value,1)
           call add_row(ioput%section(s)%tier(t)%real_value,diff)
        end if
        ioput%section(s)%tier(t)%real_value(i,j) = val

      end subroutine put_io_val_real

      subroutine put_io_val_int(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s,diff
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        integer,intent(in)                 :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%int_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%int_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(i>size(ioput%section(s)%tier(t)%int_value,1))then
           diff = i - size(ioput%section(s)%tier(t)%int_value,1)
           call add_row(ioput%section(s)%tier(t)%int_value,diff)
        end if
        ioput%section(s)%tier(t)%int_value(i,j) = val

      end subroutine put_io_val_int

      subroutine put_io_val_char(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s,diff
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        character(len=*),intent(in)        :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%char_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%char_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(i>size(ioput%section(s)%tier(t)%char_value,1))then
           diff = i - size(ioput%section(s)%tier(t)%char_value,1)
           call add_row(ioput%section(s)%tier(t)%char_value,diff)
        end if
        ioput%section(s)%tier(t)%char_value(i,j) = trim(adjustl(val))

      end subroutine put_io_val_char

      subroutine get_io_val_real(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        real,intent(out)                    :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%real_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%real_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if


        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        val = ioput%section(s)%tier(t)%real_value(i,j)

      end subroutine get_io_val_real

      subroutine get_io_val_int(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        integer,intent(out)                :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%int_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%int_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if


        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        val = ioput%section(s)%tier(t)%int_value(i,j)

      end subroutine get_io_val_int

      subroutine get_io_val_char(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        character(len=*),intent(out)        :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%char_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%char_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        val = ioput%section(s)%tier(t)%char_value(i,j)

      end subroutine get_io_val_char

      subroutine get_io_val_real_array(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,t,s,k
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        real,dimension(:),intent(out)      :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%real_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%real_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(size(val)<=&
             size(ioput%section(s)%tier(t)%real_value,1)-i+1)then
           k = i + size(val) - 1
           val = ioput%section(s)%tier(t)%real_value(i:k,j)
        else
           k = size(ioput%section(s)%tier(t)%real_value,1)
           val(1:(k-i+1)) = ioput%section(s)%tier(t)%real_value(i:k,j)
           val((k-i+2):) = -99.
        end if

      end subroutine get_io_val_real_array

      subroutine get_io_val_int_array(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                            :: i,j,k,t,s
        character(len=*)                   :: sec_name
        integer,optional,intent(in)        :: ind,tier
        character(len=*),intent(in)        :: vname
        integer,dimension(:),intent(out)   :: val
        class(csm_io_type)                  :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%int_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%int_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(size(val)<=&
             size(ioput%section(s)%tier(t)%int_value,1)-i+1)then
           k = i + size(val) - 1
           val = ioput%section(s)%tier(t)%int_value(i:k,j)
        else
           k = size(ioput%section(s)%tier(t)%int_value,1)
           val(1:(k-i+1)) = ioput%section(s)%tier(t)%int_value(i:k,j)
           val((k-i+2):) = -99
        end if

      end subroutine get_io_val_int_array

      subroutine get_io_val_char_array(ioput,sec_name,vname,val,ind,tier)

        implicit none

        integer                                   :: i,j,t,s,k
        character(len=*)                          :: sec_name
        integer,optional,intent(in)               :: ind,tier
        character(len=*),intent(in)               :: vname
        character(len=*),dimension(:),intent(out) :: val
        class(csm_io_type)                         :: ioput

        s = find_sec(ioput,sec_name)

        if(s<=0)then
           write(*,*) 'Section ',sec_name,' not found.'
           stop
        end if

        if(present(tier))then
           t=tier
           j = find_name(ioput%section(s)%tier(t)%char_name,vname)
        else
           do i=1,size(ioput%section(s)%tier)
              j = find_name(ioput%section(s)%tier(i)%char_name,vname)
              if(j>=1)then
                 t = i
                 exit
              end if
           end do
        end if

        if(j<=0)then
           write(*,*) 'Variable ',vname, ' not found in ',sec_name,'.'
           stop
        end if

        if(present(ind))then
           i = ind
        else
           i = 1
        end if

        if(size(val)<=&
             size(ioput%section(s)%tier(t)%char_value,1)-i+1)then
           k = i + size(val) - 1
           val = ioput%section(s)%tier(t)%char_value(i:k,j)
        else
           k = size(ioput%section(s)%tier(t)%char_value,1)
           val(1:(k-i+1)) = ioput%section(s)%tier(t)%char_value(i:k,j)
           val((k-i+2):) = ' '
        end if
      end subroutine get_io_val_char_array

      subroutine add_var(ioput,header,tier,char_name,int_name,real_name,nrow)

        implicit none

        character(len=*)                       :: header
        integer                                :: t,s,r
        integer,optional                       :: tier,nrow
        character(len=*),dimension(:),optional :: char_name,int_name,real_name

        integer,allocatable,dimension(:)       :: tmpint
        logical,allocatable,dimension(:)       :: tmplog

        class(csm_io_type)                     :: ioput

        s = find_sec(ioput,header)

        if(s<=0)then
           write(*,*) 'Section ',header,' not found.'
           stop
        end if

        if(present(tier))then
           t = tier
        else
           t = 1
        end if

        if(present(nrow))then
           r = nrow
        else
           r = 1
        end if

        if(present(char_name))then
           if(.not.allocated(ioput%section(s)%tier(t)%char_value))then
              allocate(ioput%section(s)%tier(t)%char_value(r,size(char_name)))
              call add_io_var_name(ioput%section(s)%tier(t)%char_name,&
                   char_name)
           else
              allocate(tmplog(size(char_name)))
              tmplog = check_names(&
                   ioput%section(s)%tier(t)%char_name,char_name)
              if(any(.not.tmplog))then
                 allocate(tmpint(size(char_name)))
                 where(tmplog)
                    tmpint=0
                 elsewhere
                    tmpint=1
                 end where
                 call add_char_col(ioput%section(s)%tier(t)%char_value,&
                      sum(tmpint))
                 call add_io_var_name(ioput%section(s)%tier(t)%char_name,&
                      char_name)
                 deallocate(tmpint)
              end if
              deallocate(tmplog)
           end if
        end if
        if(present(int_name))then
           if(.not.allocated(ioput%section(s)%tier(t)%int_value))then
              allocate(ioput%section(s)%tier(t)%int_value(r,size(int_name)))
              call add_io_var_name(ioput%section(s)%tier(t)%int_name,&
                   int_name)
           else
              allocate(tmplog(size(int_name)))
              tmplog = check_names(ioput%section(s)%tier(t)%int_name,int_name)
              if(any(.not.tmplog))then
                 allocate(tmpint(size(int_name)))
                 where(tmplog)
                    tmpint=0
                 elsewhere
                    tmpint=1
                 end where
                 call add_int_col(ioput%section(s)%tier(t)%int_value,&
                      sum(tmpint))
                 call add_io_var_name(ioput%section(s)%tier(t)%int_name,&
                      int_name)
                 deallocate(tmpint)
              end if
              deallocate(tmplog)
           end if
        end if
        if(present(real_name))then
           if(.not.allocated(ioput%section(s)%tier(t)%real_value))then
              allocate(ioput%section(s)%tier(t)%real_value(r,size(real_name)))
              call add_io_var_name(ioput%section(s)%tier(t)%real_name,&
                   real_name)
           else
              allocate(tmplog(size(real_name)))
              tmplog = check_names(ioput%section(s)%tier(t)%real_name,real_name)
              if(any(tmplog))then
                 allocate(tmpint(size(real_name)))
                 where(tmplog)
                    tmpint=0
                 elsewhere
                    tmpint=1
                 end where
                 call add_real_col(ioput%section(s)%tier(t)%real_value,&
                      sum(tmpint))
                    call add_io_var_name(ioput%section(s)%tier(t)%real_name,&
                         real_name)
                    deallocate(tmpint)
                 end if
                 deallocate(tmplog)
              end if
           end if

         end subroutine add_var

      subroutine destroy_io(ioput)

        implicit none

        class(csm_io_type)  :: ioput

        if(allocated(ioput%section)) deallocate(ioput%section)

      end subroutine destroy_io

end module class_ioput

module csm_io

    use class_ioput

    implicit none

    type(csm_io_type)  :: csminp,csmout

end module csm_io
