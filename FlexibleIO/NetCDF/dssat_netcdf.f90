module dssat_netcdf

  use dssat_cmd_arg
  use nf90_file_module

  implicit none

  type,extends(nf90_file) :: nf90_batch_file
     integer current_run
     character(len=2) crop
     character(len=120),dimension(:),allocatable :: filex
     integer,dimension(:),allocatable :: TRTNO
     integer,dimension(:),allocatable :: RP
     integer,dimension(:),allocatable :: SQ
     integer,dimension(:),allocatable :: OP
     integer,dimension(:),allocatable :: CO
   contains
     procedure :: read_batch => read_nf90_batch_file
     procedure :: free => free_nf90_batch_file
  end type nf90_batch_file

  type,extends(nf90_file) :: nf90_genotype
     integer eco_i,cul_i
   contains
     procedure :: read_cul_real
     procedure :: read_cul_int
     procedure :: read_cul_char
     generic   :: read_cul => &
          read_cul_real,&
          read_cul_int,&
          read_cul_char
     procedure :: read_eco_real
     procedure :: read_eco_int
     procedure :: read_eco_char
     generic   :: read_eco => &
          read_eco_real,&
          read_eco_int,&
          read_eco_char
     procedure :: read_spe_real
     procedure :: read_spe_int
     procedure :: read_spe_char
     procedure :: read_spe_real_vec
     procedure :: read_spe_int_vec
     procedure :: read_spe_char_vec
     generic   :: read_spe => &
          read_spe_real,&
          read_spe_int,&
          read_spe_char,&
          read_spe_real_vec,&
          read_spe_int_vec,&
          read_spe_char_vec
     procedure :: set_cul_eco
  end type nf90_genotype

  type(nf90_file) :: nc_filex
  type(nf90_file) :: nc_wth
  type(nf90_file) :: nc_soil
  type(nf90_genotype) :: nc_gen
  type(nf90_batch_file) :: nc_batch

  contains

    subroutine read_nf90_batch_file(self)

      implicit none

      class(nf90_batch_file) :: self

      integer ncid,nruns,dimid,varid,run,nchar
      integer,dimension(2) :: dimids

      self%current_run = 0

      call self%open()

      call self%get_dim_size('nruns',nruns)

      allocate(self%filex(nruns),&
               self%TRTNO(nruns),&
               self%RP(nruns),&
               self%SQ(nruns),&
               self%OP(nruns),&
               self%CO(nruns))

      call self%read('FILEX',1,self%FILEX)
      call self%read('TRTNO',1,self%TRTNO)
      call self%read('RP',1,self%RP)
      call self%read('SQ',1,self%SQ)
      call self%read('OP',1,self%OP)
      call self%read('CO',1,self%CO)

    end subroutine read_nf90_batch_file

    subroutine free_nf90_batch_file(self)

      implicit none

      class(nf90_batch_file) :: self

      deallocate(self%filex,&
               self%TRTNO,&
               self%RP,&
               self%SQ,&
               self%OP,&
               self%CO)

    end subroutine free_nf90_batch_file

    subroutine init_nc_soil()

      use csm_io

      implicit none

      real               :: lat,lon
      integer            :: i,nargs,fpos1,fpos2

      call csminp%get('*FIELDS','XCRD',lon)
      call csminp%get('*FIELDS','YCRD',lat)

      call nc_soil%open()

      call nc_soil%set_lat_lon(lat,lon)

    end subroutine init_nc_soil

    subroutine read_nc_wth_sta(ELEV,TAV,TAMP,REFHT,WINDHT)

      use csm_io
      use iso_c_binding

      implicit none

      real(C_FLOAT)        :: lon,lat

      real(C_FLOAT)        :: elev,tav,tamp,refht,windht

      call nc_wth%read('ELEV',1,ELEV)
      call nc_wth%read('TAV',1,TAV)
      call nc_wth%read('AMP',1,TAMP)
      call nc_wth%read('REFHT',1,REFHT)
      call nc_wth%read('WNDHT',1,WINDHT)

      IF (REFHT <= 0.) REFHT = 1.5
      IF (WINDHT <= 0.) WINDHT = 2.0
      IF (TAV  .LE. 0.0) TAV = 20.0
      IF (TAMP .LE. 0.0) TAMP = 5.0

    end subroutine read_nc_wth_sta

    function get_nlayr() result(nlayr)

      implicit none

      integer          :: dimid,nlayr

      call nc_soil%get_dim_size('layer',nlayr)

    end function get_nlayr

    subroutine read_cul_real(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*) :: name
      integer          :: status,var_id
      real             :: value
      logical,optional     :: optional

      call self%read(name,self%cul_i,value)

    end subroutine read_cul_real

    subroutine read_cul_int(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*) :: name
      integer          :: status,var_id
      integer          :: value
      logical,optional     :: optional

      call self%read(name,self%cul_i,value)

    end subroutine read_cul_int

    subroutine read_cul_char(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*) :: name
      integer          :: status,var_id
      character(len=*) :: value
      logical,optional     :: optional

      call self%read(name,self%cul_i,value)

    end subroutine read_cul_char

    subroutine read_eco_real(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*)     :: name
      integer              :: status,var_id
      real                 :: value
      logical,optional     :: optional

      call self%read(name,self%eco_i,value)

    end subroutine read_eco_real

    subroutine read_eco_int(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*)     :: name
      integer              :: status,var_id
      integer              :: value
      logical,optional     :: optional

      call self%read(name,self%eco_i,value)

    end subroutine read_eco_int

    subroutine read_eco_char(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*) :: name
      integer          :: status,var_id
      character(len=*) :: value
      logical,optional     :: optional

      call self%read(name,self%eco_i,value)

    end subroutine read_eco_char

    subroutine read_spe_real(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*) :: name
      integer          :: status,var_id
      real             :: value
      logical,optional     :: optional

      call self%read(name,1,value)

    end subroutine read_spe_real

    subroutine read_spe_int(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*) :: name
      integer          :: status,var_id
      integer          :: value
      logical,optional     :: optional

      call self%read(name,1,value)

    end subroutine read_spe_int

    subroutine read_spe_char(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*) :: name
      integer          :: status,var_id
      character(len=*) :: value
      logical,optional     :: optional

      call self%read(name,1,value)

    end subroutine read_spe_char

    subroutine read_spe_real_vec(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*)  :: name
      integer          :: status,var_id
      real,dimension(:) :: value
      logical,optional     :: optional


      call self%read(name,1,value)

    end subroutine read_spe_real_vec

    subroutine read_spe_int_vec(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*)     :: name
      integer          :: status,var_id
      integer,dimension(:) :: value
      logical,optional     :: optional

      call self%read(name,1,value)

    end subroutine read_spe_int_vec

    subroutine read_spe_char_vec(self,name,value,optional)

      implicit none

      class(nf90_genotype) :: self

      character(len=*)              :: name
      character(len=*),dimension(:) :: value
      logical,optional     :: optional

      call self%read(name,1,value)

    end subroutine read_spe_char_vec

    subroutine set_cul_eco(self,varnum)

      implicit none

      class(nf90_genotype) :: self

      character*6 econum,varnum

      call self%read('CUL'//varnum,1,self%cul_i)

      call self%read('ECONO',self%cul_i,econum)

      call self%read('ECO'//econum,1,self%eco_i)

    end subroutine set_cul_eco

end module dssat_netcdf
