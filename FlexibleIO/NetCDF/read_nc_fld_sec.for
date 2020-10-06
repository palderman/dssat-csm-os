C=======================================================================
C  IPFLD, Subroutine
C
C  Reads parameters related to field operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWJ Written
C  05/28/1993 PWW Header revision and minor changes            
C  02/21/2006 GH  Update 
!  07/26/2006 CHP Added previous management code for lookup in 
!       SOMFR045.SDA file to FIELDS section
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNFLD
C
C  LOCAL  : LN
C
C  OUTPUT : FLDNAM,WSTA,SLNO,SLOPE,DFDRN,FLDD,SFDRN,SLTX,FLST,FILEW,FLOB
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE read_nc_fld_sec(LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
     &           SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,
     &           XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS,FldHist, FHDUR)

      use dssat_netcdf
      IMPLICIT NONE

      CHARACTER*1  UPCASE
      CHARACTER*4  WSTA,WSTA1,HFNDCH
      CHARACTER*5  DFDRN,FLST,SLTX, FldHist
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*8  FLDNAM,wsta_temp
      CHARACTER*10 SLNO
      CHARACTER*12 FILEX
      CHARACTER*92 CHARTEST

      INTEGER LUNEXP,LNFLD,LN,LINEXP,ISECT,IFIND,ERRNUM,I, FHDUR

      REAL    FLDD,SFDRN,FLOB,SLDP,SLOPE
      REAL    XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS

      PARAMETER (ERRKEY='IPFLD ')

      call nc_filex%read('ID_FIELD',LNFLD,FLDNAM)
      call nc_filex%read('WSTA',LNFLD,wsta_temp)
      WSTA = wsta_temp(1:4)
      WSTA1 = wsta_temp(5:8)
      call nc_filex%read('FLSA',LNFLD,SLOPE)
      call nc_filex%read('FLOB',LNFLD,FLOB)
      call nc_filex%read('FLDT',LNFLD,DFDRN)
      call nc_filex%read('FLDD',LNFLD,FLDD)
      call nc_filex%read('FLDS',LNFLD,SFDRN)
      call nc_filex%read('FLST',LNFLD,FLST)
      call nc_filex%read('SLTX',LNFLD,SLTX)
      call nc_filex%read('SLDP',LNFLD,SLDP)
      call nc_filex%read('ID_SOIL',LNFLD,SLNO)

      DO I = 1, 4
        WSTA(I:I)  = UPCASE(WSTA(I:I))
        WSTA1(I:I) = UPCASE(WSTA1(I:I))
      END DO

      IF (WSTA .EQ. '    ') THEN
         CALL ERROR (ERRKEY,10,nc_filex%file_name,LINEXP)
      ENDIF
      IF (SLNO .EQ. '          ') THEN
         CALL ERROR(ERRKEY,11,nc_filex%file_name,LINEXP)
      ENDIF
      IF (SLOPE .LT. 0.0) THEN
         SLOPE = 0.0
      ENDIF
      IF (SFDRN .LE. 0.0) THEN
        SFDRN = 100.
      ENDIF

C
C    New section
C
C    Find header and read second line of field information
C
      call nc_filex%read('XCRD',LNFLD,XCRD)
      call nc_filex%read('YCRD',LNFLD,YCRD)
      call nc_filex%read('ELEV',LNFLD,ELEV)
      call nc_filex%read('AREA',LNFLD,AREA)
      call nc_filex%read('SLEN',LNFLD,SLEN)
      call nc_filex%read('FLWR',LNFLD,FLWR)
      call nc_filex%read('SLAS',LNFLD,SLAS)
      call nc_filex%read('FLHST',LNFLD,FldHist)
      call nc_filex%read('FHDUR',LNFLD,FHDUR)
      IF (AREA .LE. 0.0) AREA = 1.0
      IF (FLWR .LE. 0.0) FLWR = 1.0
      IF (SLEN .LE. 0.0) SLEN = SQRT(AREA*FLWR*10000.0)

      RETURN

      end subroutine read_nc_fld_sec
