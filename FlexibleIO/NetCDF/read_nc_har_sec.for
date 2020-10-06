C=======================================================================
C  IPHAR, Subroutine
C
C  Subroutine to read in harvest management
C 
C-----------------------------------------------------------------------
C  Revision history
C
C  05/08/1991 JWW Written for DSSAT v3 format
C  05/28/1993 PWW Header revision and minor changes
C  06/09/2002 GH  Modified for Y2K
C  02/03/2005 GH  Corrected error checking for missing levels
C
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNHAR,YEAR
C
C  LOCAL  : LN,ERRKEY,CHARTEST,ERRNUM,J,IFIND
C
C  OUTPUT : HDATE,HSTG,HCOM,HSIZ,HPC,NHAR
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND,IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE read_nc_har_sec(LNHAR,HDATE,HSTG,HCOM,HSIZ,HPC,
     &                  NHAR,IHARI,YRSIM,CROP,HBPC,FREQ,CUHT)!NEW FORAGE VARIABLES (DIEGO-2/14/2017)

      USE ModuleDefs
      use dssat_netcdf
      IMPLICIT     NONE

      CHARACTER*1  IHARI
      CHARACTER*2  CROP
      CHARACTER*5  HSTG(3),HCOM(3),HSIZ(3)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER      LNHAR,LUNEXP,ISECT,LINEXP,HDATE(3),NHAR
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM
      INTEGER      HYR, HDAY
      integer      i
      integer      start,count
      REAL         HPC(3),HBPC(3),FREQ,CUHT !NEW FORAGE VARIABLES (DIEGO-2/14/2017)

      PARAMETER   (ERRKEY='IPHAR ')

                   FINDCH='*HARVE'

      NHAR  = 0

      DO J = 1, 3
         HSTG(J)  = '     '
         HCOM(J)  = '     '
         HSIZ(J)  = '     '
         HPC(J)   = 100.0
         HDATE(J) = -99
      END DO

      IF (LNHAR .EQ. 0) return

      call nc_filex%get_ind_nvals('LNHAR',LNHAR,I,NHAR)
      if(NHAR > 3) NHAR = 3
      call nc_filex%read('HDATE',I,HDATE(1:NHAR))
      call nc_filex%read('HSTG',I,HSTG(1:NHAR))
      call nc_filex%read('HCOM',I,HCOM(1:NHAR))
      call nc_filex%read('HSIZ',I,HSIZ(1:NHAR))
      call nc_filex%read('HPC',I,HPC(1:NHAR))
      call nc_filex%read('HBPC',I,HBPC(1:NHAR))

      do i=1,3
C
C        Read several lines of harvest details
C
            IF ((HDATE(i) .LT.  0) .OR.
     &           (IHARI .EQ. 'R' .AND. MOD(HDATE(I),1000) .GT. 366))
     &           THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            IF (IHARI .EQ. 'R') THEN
               CALL Y2K_DOY(HDATE(I))
               IF (INT(HDATE(I)/100.) < INT(YRSIM/100.)) THEN
!     Increment harvest century (can't be less than simulation century)
                  CALL YR_DOY(HDATE(I), HYR, HDAY)
                  HDATE(I) = (HYR + 100) * 1000 + HDAY
               ENDIF
            ENDIF
            IF (HPC(I) .LT. 0.0) THEN
               HPC(I) = 100.0
            ENDIF
            IF (HBPC(I) .LT. 0.0) THEN
               HBPC(I) = 0.0
            ENDIF
            IF (HSTG(I) .EQ. '     ') THEN
               HSTG(I) = '  -99'
            ENDIF
            IF (HCOM(I) .EQ. '     ') THEN
               HCOM(I) = '  -99'
            ENDIF
            IF (HSIZ(I) .EQ. '     ') THEN
               HSIZ(I) = '  -99'
            ENDIF
      end do

      IF ((INDEX('CSPT',CROP)) .GT. 0) THEN
        IF (HDATE(1) .LT. 0) THEN
           CALL ERROR (ERRKEY,13,FILEX,LINEXP)
        ENDIF
        IF (IHARI .EQ. 'A') THEN
           CALL ERROR (ERRKEY,14,FILEX,LINEXP)
        ENDIF
      ENDIF

      NHAR = MAX (0,NHAR)
      IF (LNHAR .EQ. 0 .AND. IHARI .NE. 'M' .AND. IHARI .NE. 'A') THEN
         CALL ERROR (ERRKEY,1,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'G' .AND. HSTG(1) .EQ. '     ') THEN
         CALL ERROR (ERRKEY,3,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'R' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,4,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'D' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,5,FILEX,LINEXP)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,3(1X,A5),2(1X,F5.0),6X,F5.0,F5.0) !editted to read forage variables (Diego-2/14/2017)

      END SUBROUTINE read_nc_har_sec
