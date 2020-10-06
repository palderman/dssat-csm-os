subroutine read_nc_trt_sec(TRTNUM,ENAME,TITLET,LNCU,LNFLD,LNSA,LNIC,&
     LNPLT,LNIR,LNFER,LNRES,LNCHE,LNTIL,LNENV,LNHAR,LNSIM)

  use dssat_netcdf

  implicit none

  character(len=60) :: ENAME
  character(len=25) :: TITLET
  integer TRTNUM,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,LNCHE,&
       LNTIL,LNENV,LNHAR,LNSIM

  call nc_filex%read('ENAME',1,ENAME)
  call nc_filex%read('TNAME',TRTNUM,TITLET)
  call nc_filex%read('CU',TRTNUM,LNCU)
  call nc_filex%read('FL',TRTNUM,LNFLD)
  call nc_filex%read('SA',TRTNUM,LNSA)
  call nc_filex%read('IC',TRTNUM,LNIC)
  call nc_filex%read('MP',TRTNUM,LNPLT)
  call nc_filex%read('MI',TRTNUM,LNIR)
  call nc_filex%read('MF',TRTNUM,LNFER)
  call nc_filex%read('MR',TRTNUM,LNRES)
  call nc_filex%read('MC',TRTNUM,LNCHE)
  call nc_filex%read('MT',TRTNUM,LNTIL)
  call nc_filex%read('ME',TRTNUM,LNENV)
  call nc_filex%read('MH',TRTNUM,LNHAR)
  call nc_filex%read('SM',TRTNUM,LNSIM)

end subroutine read_nc_trt_sec
