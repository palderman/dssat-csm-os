C=======================================================================
C  IPENV, Subroutine
C
C  Input environmental data and check for errors.
C-----------------------------------------------------------------------
C  Revision history
C
C  09/15/1991 NBP Written                             
C  05/28/1993 PWW Header revision and minor changes   
C  10/23/2002 CHP Modified for Y2K
C  02/03/2005 GH  Corrected error checking for missing levels
C-----------------------------------------------------------------------
C  INPUT  : FILEX,LNENV,LUNEXP
C
C  LOCAL  : ERRKEY,ERRNUM,FOUND,LINE,LN
C
C  OUTPUT : CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,
C           RADADJ,RADFAC,TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WNDADJ,WNDFAC
C
C  Fn/Sub : EOSECT,ERROR,FIND,IGNORE,VALSTR
C
C  Ifile  : *.SBX
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE read_nc_env_sec(LNENV,CO2ADJ,CO2FAC,DAYADJ,
     &  DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,RADADJ,RADFAC,
     &  TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WMODI,WNDADJ,WNDFAC,
     &  WTHADJ)

      USE ModuleDefs
      use dssat_netcdf
      IMPLICIT NONE

      CHARACTER   ERRKEY*6,LINE*80,FILEX*12,WMODI*1,FINDCH*6
      CHARACTER*1 DAYFAC(NAPPL),RADFAC(NAPPL),TXFAC (NAPPL)
      CHARACTER*1 TMFAC(NAPPL), WNDFAC(NAPPL)
      CHARACTER*1 PRCFAC(NAPPL),CO2FAC(NAPPL),DPTFAC(NAPPL)
      INTEGER     ERRNUM,FOUND,I,LN,LNENV,LUNEXP,LINEXP,NEV
      INTEGER     WMDATE(NAPPL),ENVlev(NAPPL)
      integer     start,count

      REAL        DAYADJ(NAPPL),RADADJ(NAPPL),TXADJ (NAPPL),TMADJ(NAPPL)
      REAL        PRCADJ(NAPPL)
      REAL        CO2ADJ(NAPPL),DPTADJ(NAPPL),WNDADJ(NAPPL),WTHADJ(2,8)

      PARAMETER  (ERRKEY = 'IPENV ')

      FINDCH = '*ENVIR'
C
C     Initialize
C
      DO I = 1, 8
        WTHADJ(1,I) = 0.0
        WTHADJ(2,I) = 1.0
      END DO

	DO I = 1, NAPPL
        WMDATE(I)  = 0
        DAYADJ(I)  = 0.0
        RADADJ(I)  = 0.0
        TXADJ(I)   = 0.0
        TMADJ(I)   = 0.0
        PRCADJ(I)  = 0.0
        CO2ADJ(I)  = 0.0
        DPTADJ(I)  = 0.0
        WNDADJ(I)  = 0.0
      END DO

      WMODI  = 'N'
      NEV    =  0
      LINEXP =  0

      IF (LNENV .EQ. 0) return
C
C     Find environmental section
C
      call nc_filex%get_ind_nvals('E',LNENV,I,NEV)
      if(NEV > NAPPL) NEV = NAPPL
      call nc_filex%read('WMDATE',I,WMDATE(1:NEV))
      call nc_filex%read('DAYFAC',I,DAYFAC(1:NEV))
      call nc_filex%read('DAYADJ',I,DAYADJ(1:NEV))
      call nc_filex%read('RADFAC',I,RADFAC(1:NEV))
      call nc_filex%read('RADADJ',I,RADADJ(1:NEV))
      call nc_filex%read('TXFAC',I,TXFAC(1:NEV))
      call nc_filex%read('TXADJ',I,TXADJ(1:NEV))
      call nc_filex%read('TMFAC',I,TMFAC(1:NEV))
      call nc_filex%read('TMADJ',I,TMADJ(1:NEV))
      call nc_filex%read('PRCFAC',I,PRCFAC(1:NEV))
      call nc_filex%read('PRCADJ',I,PRCADJ(1:NEV))
      call nc_filex%read('CO2FAC',I,CO2FAC(1:NEV))
      call nc_filex%read('CO2ADJ',I,CO2ADJ(1:NEV))
      call nc_filex%read('DPTFAC',I,DPTFAC(1:NEV))
      call nc_filex%read('DPTADJ',I,DPTADJ(1:NEV))
      call nc_filex%read('WNDFAC',I,WNDFAC(1:NEV))
      call nc_filex%read('WNDADJ',I,WNDADJ(1:NEV))
      do i=1,nev
         call y2k_doy(WMDATE(i))
         IF (DAYADJ(I) .LE. -90.) DAYADJ(I) = 0.0
         IF (RADADJ(I) .LE. -90.) RADADJ(I) = 0.0
         IF (TXADJ(I)  .LE. -90.) TXADJ(I)  = 0.0
         IF (TMADJ(I)  .LE. -90.) TMADJ(I)  = 0.0
         IF (PRCADJ(I) .LE. -90.) PRCADJ(I) = 0.0
         IF (CO2ADJ(I) .LE. -90.) CO2ADJ(I) = 0.0
         IF (DPTADJ(I) .LE. -90.) DPTADJ(I) = 0.0
         IF (WNDADJ(I) .LE. -90.) WNDADJ(I) = 0.0
      end do

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 1000 FORMAT (I3,I5,5(1X,A1,F4.0),1X,A1,F4.0,2(1X,A1,F4.0))

      END SUBROUTINE read_nc_env_sec
