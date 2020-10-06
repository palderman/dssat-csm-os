module dssat_mpi

  use mpi
  use dssat_variable_registry

  implicit none

  type mpi_connection_template
     integer rank
     integer,dimension(:),allocatable :: trtno
     character(len=:),allocatable :: varlist
   contains
     procedure,nopass :: close => close_mpi_connection
  end type mpi_connection_template

  type,extends(mpi_connection_template) :: mpi_parent_type
     integer inter_comm
     integer,dimension(:),allocatable :: children
     integer,dimension(:,:),allocatable :: trtno_range
     type(registry_type),dimension(:),allocatable :: seasonal
     type(registry_type),dimension(:),allocatable :: daily
     character(len=50),dimension(:),allocatable :: cmds
     character(len=1000),dimension(:,:),allocatable :: args
     integer,dimension(:,:),allocatable :: requests
     integer,dimension(:),allocatable :: np,info,errors
   contains
     procedure :: init => initialize_parent_connection
     procedure :: spawn_dssat_children
     procedure :: receive_variable => receive_registered_variable
     procedure :: receive_registries
     procedure :: print_registries => print_mpi_parent_registries
  end type mpi_parent_type

  type,extends(mpi_connection_template) :: mpi_child_type
     integer :: parent
     logical use_mpi
     integer curr_trt_index
     character(len=1) :: rnmode
     character(len=2) :: crop
     integer,dimension(:),allocatable :: requests
   contains
     procedure :: connect => connect_to_parent
     procedure :: send_registry => send_registry_from_child
     procedure :: send_variable => send_registered_variable_from_child
  end type mpi_child_type

  type(mpi_parent_type) :: mpi_parent
  type(mpi_child_type) :: mpi_child

  integer,parameter:: DSSAT_MPI_NAME_LEN=0,&
       DSSAT_MPI_NAME=1,&
       DSSAT_MPI_IS_INT=2,&
       DSSAT_MPI_VALUE=3,&
       DSSAT_MPI_VALUE_SIZE=4,&
       DSSAT_MPI_REGISTRY_TRTNO=5,&
       DSSAT_MPI_NTRT=6,&
       DSSAT_MPI_TRTNO_ARRAY=7,&
       DSSAT_MPI_VARLIST_LEN=8,&
       DSSAT_MPI_VARLIST=9,&
       DSSAT_MPI_RNMODE=10,&
       DSSAT_MPI_CROP_CODE=11

contains

  subroutine initialize_parent_connection(self)

    implicit none

    integer ierr
    class(mpi_parent_type) self

    call MPI_Init(ierr);

  end subroutine initialize_parent_connection

  subroutine spawn_dssat_children(self,n_children,trt_start,trt_end,rnmode,crop_code,cmd,dssat_args,work_dir)

    implicit none

    class(mpi_parent_type) self

    integer i,ierr,request
    integer n_children,trt_start,trt_end,total_trts,ntrt,trt_ind

    character(len=1) rnmode
    character(len=2) crop_code
    character(len=3) rank_buff
    character(len=*) cmd
    character(len=*) work_dir
    character(len=*) dssat_args

    total_trts = trt_end - trt_start + 1

    if(n_children > total_trts) n_children = total_trts

    allocate(self%children(n_children),&
         self%trtno_range(2,n_children),&
         self%cmds(n_children),&
         self%args(n_children,1),&
         self%np(n_children),&
         self%info(n_children),&
         self%errors(n_children))

    self%cmds = cmd

    do while(.true.)
       i = index(dssat_args,',')
       if(i==0) exit
       dssat_args(i:i)=' '
    end do
    
    do i=1,n_children
       self%children(i) = i - 1
       write(rank_buff,'(i3)') i-1
       self%args(i,1) = trim(adjustl(dssat_args))//" --work_dir="//&
            trim(adjustl(work_dir))//"/dssat_"//trim(adjustl(rank_buff))
    end do

    self%np = 1

    self%info = MPI_INFO_NULL

    call MPI_Comm_spawn_multiple(n_children, self%cmds, self%args, self%np,&
         self%info, 0, MPI_COMM_WORLD, self%inter_comm, self%errors,ierr)

    ! Construct treatment array
    allocate(self%trtno(total_trts))
    do i=1,total_trts
       self%trtno(i) = trt_start + i - 1
    end do

    allocate(self%requests(4,n_children))

    ! Send arguments to child process:
    trt_ind=1
    do i=1,n_children
       ntrt = total_trts/n_children
       if(i <= modulo(total_trts,n_children)) ntrt = ntrt + 1
       call MPI_Send(ntrt,1,MPI_INTEGER,self%children(i), DSSAT_MPI_NTRT,&
            self%inter_comm,ierr)
       call MPI_Send(self%trtno(trt_ind:(trt_ind+ntrt-1)),ntrt,MPI_INTEGER,&
            self%children(i),DSSAT_MPI_TRTNO_ARRAY,self%inter_comm,&
            ierr)
       call MPI_Send(len(self%varlist),1,MPI_INTEGER,self%children(i),&
            DSSAT_MPI_VARLIST_LEN,self%inter_comm,ierr)
       call MPI_Send(self%varlist,len(self%varlist),MPI_CHARACTER,&
            self%children(i),DSSAT_MPI_VARLIST,self%inter_comm,&
            ierr)
       call MPI_Send(rnmode,1,MPI_CHARACTER,self%children(i), DSSAT_MPI_RNMODE,&
            self%inter_comm,ierr)
       call MPI_Send(crop_code,2,MPI_CHARACTER,self%children(i), DSSAT_MPI_CROP_CODE,&
            self%inter_comm,ierr)
       self%trtno_range(1,i) = self%trtno(trt_ind)
       self%trtno_range(2,i) = self%trtno(trt_ind+ntrt-1)
       trt_ind = trt_ind + ntrt;
    end do

    deallocate(self%requests)

    allocate(self%seasonal(n_children))

    do i=1,n_children
       call self%seasonal(i)%csv_to_registry(self%varlist)
    end do

  end subroutine spawn_dssat_children

  subroutine connect_to_parent(self)

    use mpi
    use dssat_cmd_arg

    implicit none

    integer i,ierr,ntrt
    integer nvars,vlen
    character(len=10) char_buff
    character(len=:),allocatable :: child_path

    class(mpi_child_type) self

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, self%rank, ierr)
    call MPI_Comm_get_parent(self%parent,ierr)

    call MPI_Recv(ntrt, 1, MPI_INTEGER, 0, DSSAT_MPI_NTRT, self%parent,&
         MPI_STATUS_IGNORE,ierr)

    allocate(self%trtno(ntrt))

    call MPI_Recv(self%trtno, ntrt, MPI_INTEGER, 0, DSSAT_MPI_TRTNO_ARRAY,&
         self%parent, MPI_STATUS_IGNORE, ierr)

    call MPI_Recv(vlen, 1, MPI_INTEGER, 0, DSSAT_MPI_VARLIST_LEN, self%parent,&
         MPI_STATUS_IGNORE,ierr)
    allocate(character(vlen)::self%varlist)

    call MPI_Recv(self%varlist, vlen, MPI_CHARACTER, 0, DSSAT_MPI_VARLIST,&
         self%parent, MPI_STATUS_IGNORE, ierr)

    call MPI_Recv(self%rnmode, 1, MPI_CHARACTER, 0, DSSAT_MPI_RNMODE,&
         self%parent, MPI_STATUS_IGNORE, ierr)

    call MPI_Recv(self%crop, 2, MPI_CHARACTER, 0, DSSAT_MPI_CROP_CODE,&
         self%parent, MPI_STATUS_IGNORE, ierr)

    self%use_mpi = .TRUE.
    self%curr_trt_index = 0

    allocate(character(len=1000)::child_path)
    call get_dssat_arg('--work_dir',child_path)
    call system("mkdir -p "//child_path) ! Linux specific
    call chdir(child_path)
    deallocate(child_path)

  end subroutine connect_to_parent

  subroutine send_registered_variable_from_child(self,r_var)

    use mpi

    implicit none

    integer ierr
    class(mpi_child_type) self
    type(registered_variable) r_var

    call MPI_Send(len(r_var%name),1,MPI_INTEGER,0,DSSAT_MPI_NAME_LEN,&
         self%parent,ierr)
    call MPI_Send(r_var%name,len(r_var%name),MPI_CHARACTER,0,DSSAT_MPI_NAME,&
         self%parent,ierr)
    if(associated(r_var%r_ptr))then
         call MPI_Send(0,1,MPI_INTEGER,0,DSSAT_MPI_IS_INT,self%parent,&
              ierr)
         if(allocated(r_var%r_val))then
            call MPI_Send(r_var%curr_ind,1,MPI_INTEGER,0,&
                 DSSAT_MPI_VALUE_SIZE,self%parent,ierr)
            call MPI_Send(r_var%r_val,r_var%curr_ind,MPI_REAL,0,&
                 DSSAT_MPI_VALUE,self%parent,ierr)
         else
            call MPI_Send(0,1,MPI_INTEGER,0,DSSAT_MPI_VALUE_SIZE,self%parent,&
                 ierr)
         end if
    end if
    if(associated(r_var%i_ptr))then
       call MPI_Send(1,1,MPI_INTEGER,0,DSSAT_MPI_IS_INT,self%parent,&
            ierr)
         if(allocated(r_var%i_val))then
            call MPI_Send(r_var%curr_ind,1,MPI_INTEGER,0,&
                 DSSAT_MPI_VALUE_SIZE,self%parent,ierr)
            call MPI_Send(r_var%i_val,r_var%curr_ind,MPI_INTEGER,0,&
                 DSSAT_MPI_VALUE,self%parent,ierr)
         else
            call MPI_Send(0,1,MPI_INTEGER,0,DSSAT_MPI_VALUE_SIZE,self%parent,&
                 ierr)
         end if
      end if

  end subroutine send_registered_variable_from_child

  subroutine send_registry_from_child(self,registry)

    implicit none

    class(mpi_child_type) self
    type(registry_type) registry
    integer i,trtno,request,ierr

    do i=1,size(registry%variables)
       call self%send_variable(registry%variables(i))
    end do

  end subroutine send_registry_from_child

  subroutine receive_registered_variable(self,rank,r_var)

    use mpi

    implicit none

    integer ierr
    integer is_int
    integer name_len
    integer val_size
    integer rank
    integer,dimension(MPI_STATUS_SIZE) :: status
    class(mpi_parent_type) self
    type(registered_variable) r_var

    if(allocated(r_var%name)) deallocate(r_var%name)
    call MPI_Recv(name_len,1,MPI_INTEGER,rank,DSSAT_MPI_NAME_LEN,&
         self%inter_comm,status,ierr)
    allocate(character(len=name_len)::r_var%name)
    call MPI_Recv(r_var%name,name_len,MPI_CHARACTER,rank,DSSAT_MPI_NAME,&
         self%inter_comm,status,ierr)
    call MPI_Recv(is_int,1,MPI_INTEGER,rank,DSSAT_MPI_IS_INT,self%inter_comm,&
         status,ierr)
    if(is_int == 0)then
       if(allocated(r_var%r_val)) deallocate(r_var%r_val)
       call MPI_Recv(val_size,1,MPI_INTEGER,rank,&
            DSSAT_MPI_VALUE_SIZE,self%inter_comm,status,ierr)
       if(val_size > 0)then
          allocate(r_var%r_val(val_size))
          r_var%curr_ind = val_size
          call MPI_Recv(r_var%r_val,val_size,MPI_REAL,rank,DSSAT_MPI_VALUE,&
               self%inter_comm,status,ierr)
       else
          r_var%curr_ind = 0
       end if
    else
       if(allocated(r_var%i_val)) deallocate(r_var%i_val)
       call MPI_Recv(val_size,1,MPI_INTEGER,rank,&
            DSSAT_MPI_VALUE_SIZE,self%inter_comm,status,ierr)
       if(val_size > 0)then
          allocate(r_var%i_val(val_size))
          r_var%curr_ind = val_size
          call MPI_Recv(r_var%i_val,val_size,MPI_REAL,rank,DSSAT_MPI_VALUE,&
               self%inter_comm,status,ierr)
       else
          r_var%curr_ind = 0
       end if
    end if

  end subroutine receive_registered_variable

  subroutine receive_registries(self)

    implicit none

    class(mpi_parent_type) self
    integer i,ierr,child,ind
    integer trtno,count,rank
    integer,dimension(MPI_STATUS_SIZE) :: status

    allocate(self%requests(size(self%trtno_range,2),1))

    do child=1,size(self%children)
       do i=1,size(self%seasonal(child)%variables)
          call self%receive_variable(self%children(child),&
               self%seasonal(child)%variables(i))
       end do
    end do

  end subroutine receive_registries

  subroutine print_mpi_parent_registries(self)

    implicit none

    integer i,j,k,ntrt,p1,p2,dim_size

    class(mpi_parent_type) self

    do i=1,size(self%seasonal)
       ntrt = size(self%trtno)/size(self%children)
       if(i < modulo(size(self%trtno),size(self%children))) ntrt = ntrt + 1
       do j=1,ntrt
          print *,"For TRTNO = ",self%trtno_range(1,i)+j-1,":"
          do k=1,size(self%seasonal(i)%variables)
             dim_size = self%seasonal(i)%variables(k)%curr_ind/ntrt
             p1 = (j-1)*dim_size + 1
             p2 = p1 + dim_size - 1
             if(allocated(self%seasonal(i)%variables(k)%r_val))then
                print *,self%seasonal(i)%variables(k)%name,' = ',&
                     self%seasonal(i)%variables(k)%r_val(p1:p2)
             else if(allocated(self%seasonal(i)%variables(k)%i_val))then
                print *,self%seasonal(i)%variables(k)%name,' = ',&
                     self%seasonal(i)%variables(k)%i_val(p1:p2)
             end if
          end do
       end do
    end do

  end subroutine print_mpi_parent_registries

  subroutine close_mpi_connection()

    use mpi

    implicit none

    integer ierr

    call MPI_Finalize(ierr)

  end subroutine close_mpi_connection

end module dssat_mpi
