program run_mpi_dssat

  use mpi
  use dssat_mpi
  use dssat_cmd_arg
  use dssat_netcdf
  use ordered_array

  implicit none

  integer i,ntrt

  ! Variables for MPI_Spawn_Multiple
  integer n_dssat,trt_start,trt_end,n_fields,sim,nyears
  character(len=1) :: rnmode
  character(len=2) :: crop_code
  character(len=3) :: rank_buff
  character(len=100) :: cmd
  character(len=1000):: dssat_args
  character(len=120) :: filex_path
  character(len=120) :: out_file_name
  character(len=120) :: work_dir

  real,dimension(:),allocatable :: xcrd,ycrd
  integer,dimension(:),allocatable :: xcrd_i,ycrd_i
  type(real_ordered_array)      :: latitude,longitude

  character(len=:),allocatable :: varlist

  type(nf90_file) nf90_output

  if(cmd_arg_present('--help'))then
     print *,"Example call:"
     print *,"mpirun --oversubscribe -n 1 ./run_mpi_dssat --trt_start=1"//&
          " --trt_end=7208 --n_dssat=3 --varlist=fvar,ivar"//&
          " --call=./test_dssat_mpi --nc_filex=ASA.nc --dssat_args="//&
          '"'//"<list of arguments>"//'"'
     stop
  end if

  call get_dssat_arg('--trt_start',trt_start)

  call get_dssat_arg('--trt_end',trt_end)

  ntrt = trt_end - trt_start + 1

  call get_dssat_arg('--n_dssat',n_dssat)

  call nc_filex%set_file_from_cmd_arg('--nc_filex')

  call get_dssat_arg('--nc_out',out_file_name)

  call get_dssat_arg('--work_dir',work_dir)

  allocate(character(len=1000)::varlist)
  call get_dssat_arg('--varlist',varlist)
  mpi_parent%varlist = trim(varlist)
  deallocate(varlist)

  call get_dssat_arg('--call',cmd)

  call get_dssat_arg('--dssat_args',dssat_args)

  call get_dssat_arg('--rnmode',rnmode)
  if(rnmode == ' ') rnmode = 'B'

  call mpi_parent%init()

  call nc_filex%open()
  call nc_filex%read('CR',trt_start,crop_code)

  write(*,fmt="(a)",advance="no") "   Spawning DSSAT worker processes..."
  call mpi_parent%spawn_dssat_children(n_dssat,trt_start,trt_end,rnmode,&
       crop_code,cmd,dssat_args,work_dir)
  write(*,fmt="(a)") "done."

  call nc_filex%get_dim_size('FIELDS',n_fields)

  allocate(xcrd(ntrt),ycrd(ntrt),xcrd_i(ntrt),ycrd_i(ntrt))

  call nc_filex%read('XCRD',trt_start,xcrd)
  call nc_filex%read('YCRD',trt_start,ycrd)

  call nc_filex%read('SM',trt_start,sim)
  call nc_filex%read('NYERS',sim,nyears)

  do i=1,size(xcrd)
     call longitude%insert(xcrd(i))
     call latitude%insert(ycrd(i))
  end do

  do i=1,size(xcrd)
     xcrd_i(i) = longitude%find(xcrd(i))
     ycrd_i(i) = latitude%find(ycrd(i))
  end do

  write(*,fmt="(a)",advance="no") "   Setting up output file..."
  call nf90_output%create(out_file_name,overwrite=.TRUE.)
  write(*,fmt="(a)") "done."

  call nf90_output%add_dim('lat',latitude%curr_end)
  call nf90_output%add_dim('lon',longitude%curr_end)
  call nf90_output%add_dim('season',nyears)

  call nf90_output%add_var('lat',(/'lat'/),nf90_float)
  call nf90_output%add_var('lon',(/'lon'/),nf90_float)
  call nf90_output%add_var('season',(/'season'/),nf90_int)

  call nf90_output%write_variable('lat',(/1/),(/latitude%curr_end/),&
       latitude%values)
  call nf90_output%write_variable('lon',(/1/),(/longitude%curr_end/),&
       longitude%values)
  call nf90_output%write_variable('season',(/1/),(/nyears/),&
       (/(i,i=1,nyears)/))

  write(*,fmt="(a)",advance="no") "   Receiving data from DSSAT worker processes..."
  call mpi_parent%receive_registries()
  write(*,fmt="(a)") "done."

  write(*,fmt="(a)",advance="no") "   Closing MPI connections..."
  call mpi_parent%close()
  write(*,fmt="(a)") "done."

  if(.not.cmd_arg_present('--no_cleanup'))then
    do i=1,n_dssat
       write(rank_buff,'(i3)') i-1
      call system("rm -r "//trim(adjustl(work_dir))//"/dssat_"//trim(adjustl(rank_buff))) ! Clean up running directories, Linux specific
    end do
  end if

  write(*,fmt="(a)",advance="no") "   Writing data to output file..."
  call nf90_output%write_netcdf(mpi_parent,nyears,xcrd_i,ycrd_i)
  write(*,fmt="(a)") "done."

  write(*,fmt="(a)") "   Simulations complete."
  call nf90_output%close()

end program run_mpi_dssat
