C=======================================================================
C  IPTILL, Subroutine
C
C  Determines tillage operations for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  07/18/1995 GPF Written 
C  04/01/1996 GH  Modified and included in DSSAT v3.1
C  08/19/2002 GH  Modified for Y2K
C  08/23/2002 GH  Expanded array tillage applications to 200
C  02/03/2005 GH  Corrected error checking for missing levels
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNCHE,CDATE,CHCOD,CHAMT,CHMET,CHDEP,CHT
C           YRSIM,ISWWAT,NCHEM,FOLFR,SOLFR
C
C  LOCAL  : ERRKEY,CHARTEST,ISECT,LINEXP,ERRNUM,J,IFIND,LN
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C=======================================================================

      SUBROUTINE read_nc_till_sec(LNTIL,YRSIM,ISWTIL,NTIL,TDATE,
     &    TIMPL,TDEP,LNSIM)

      USE ModuleDefs
      use dssat_netcdf
      IMPLICIT     NONE

      CHARACTER*5  TIMPL(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*1  ISWTIL
      CHARACTER*80 CHARTEST

      INTEGER      LNTIL,LUNEXP,ISECT,LINEXP,TDATE(NAPPL),NTIL
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM,LNSIM
      REAL         TDEP(NAPPL)
      integer      start

      PARAMETER   (ERRKEY ='IPTILL')
      FINDCH ='*TILLA'

      DO J = 1, NAPPL
         TIMPL(J)  = '     '
         TDATE(J)  = 0
         TDEP(J) = 0.0
      END DO

      NTIL = 0
      IF (LNTIL .GT. 0) THEN
         IF (ISWTIL .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            ISWTIL = 'Y'
         ENDIF
         call nc_filex%get_ind_nvals('LNTIL',LNTIL,J,NTIL)
         if(NTIL > NAPPL) NTIL = NAPPL
         call nc_filex%read('TDATE',J,TDATE(1:NTIL))
         call nc_filex%read('TIMPL',J,TIMPL(1:NTIL))
         call nc_filex%read('TDEP',J,TDEP(1:NTIL))
         do J=1,NTIL
            IF ((TDATE(J) .LT. 1)  .OR.
     &           (MOD(TDATE(J),1000) .GT. 366)) THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF

            CALL Y2K_DOY(TDATE(J))

            IF (TDATE(J) .LT. YRSIM) THEN
               CALL ERROR (ERRKEY,3,FILEX,LINEXP)
            ENDIF

            IF (TDEP(J) .LT. 0) CALL ERROR (ERRKEY,11,FILEX,LINEXP)
         end do
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,F5.0)

      END SUBROUTINE read_nc_till_sec

