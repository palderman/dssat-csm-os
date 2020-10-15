module dssat_cmd_arg

  interface get_dssat_arg
     module procedure get_cmd_arg_real
     module procedure get_cmd_arg_integer
     module procedure get_cmd_arg_character
  end interface get_dssat_arg

contains

  function cmd_arg_present(string) result(is_present)

    implicit none

    logical is_present
    integer len_tmp
    character(len=*) string
    character(len=:),allocatable :: tmp

    call get_command(length=len_tmp)

    allocate(character(len=len_tmp)::tmp)
    
    call get_command(tmp)

    is_present = index(tmp,string)>0

  end function cmd_arg_present
  
  subroutine find_cmd_arg_alloc(string,arg)

    implicit none

    integer          :: pos1,pos2
    integer          :: len_tmp

    character(len=*) :: string
    character(len=:),allocatable :: tmp,arg

    call get_command(length=len_tmp)

    allocate(character(len=len_tmp)::tmp)

    call get_command(tmp)
    pos1 = index(tmp,string)
    if(pos1>0)then
       pos1 = index(tmp(pos1:),'=') + pos1
       pos2 = pos1 + index(tmp(pos1:),' ') - 2
       if(pos2 < pos1) pos2 = len(tmp)
       len_tmp = pos2 - pos1 + 1
       allocate(character(len=len_tmp)::arg)
       arg = tmp(pos1:pos2)
       return
    else
       allocate(character(len=1)::arg)
       arg = ' '
    end if

  end subroutine find_cmd_arg_alloc
  
  subroutine find_cmd_arg(string,arg)

      implicit none

      integer          :: pos1,pos2
      integer          :: len_tmp

      character(len=*) :: string,arg
      character(len=:),allocatable :: tmp

      call get_command(length=len_tmp)

      allocate(character(len=len_tmp)::tmp)

      call get_command(tmp)
      pos1 = index(tmp,string)
      if(pos1>0)then
         pos1 = index(tmp(pos1:),'=') + pos1
         pos2 = pos1 + index(tmp(pos1:),' ') - 2
         if(pos2 < pos1) pos2 = len(tmp)
         arg = tmp(pos1:pos2)
         return
      else
         arg = ' '
      end if

    end subroutine find_cmd_arg

    subroutine get_cmd_arg_real(string,arg)

      implicit none

      character(len=*)   :: string
      real               :: arg
      character(len=:),allocatable :: afmt
      integer            :: tlen
      integer            :: len_tmp
      character(len=:),allocatable :: tmp

      call get_command(length=len_tmp)

      allocate(character(len=len_tmp)::tmp)

      call find_cmd_arg(string,tmp)

      tlen=len(trim(tmp))

      if(tlen<=0)then
         arg = -99
         return
      end if

      afmt = '(f   .0)'
      write(afmt(3:5),'(i3)') tlen
      read(tmp,afmt) arg

    end subroutine get_cmd_arg_real

    subroutine get_cmd_arg_integer(string,arg)

      implicit none

      character(len=*)   :: string
      integer            :: arg
      character(len=:),allocatable :: afmt
      integer            :: tlen
      integer            :: len_tmp
      character(len=:),allocatable :: tmp

      call get_command(length=len_tmp)

      allocate(character(len=len_tmp)::tmp)

      call find_cmd_arg(string,tmp)

      tlen=len(trim(tmp))

      if(tlen<=0)then
         arg = -99
         return
      end if
      afmt = '(i   )'
      write(afmt(3:5),'(i3)') tlen
      read(tmp,afmt) arg

    end subroutine get_cmd_arg_integer

    subroutine get_cmd_arg_character(string,arg)

      implicit none

      character(len=*)   :: string,arg

      call find_cmd_arg(string,arg)

    end subroutine get_cmd_arg_character

end module dssat_cmd_arg
