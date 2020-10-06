C=======================================================================
C  IPCHEM, Subroutine
C
C  Determines chemical application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1995 GPF Written                                    
C  01/01/1996 GH  Accepted and included in DSSAT v3.1        
C  08/19/2002 GH  Modified for Y2K
C  08/23/2002 GH  Expanded array for chemical applications to 200
C  02/03/2005 GH  Corrected error checking for missing levels
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNCHE,CDATE,CHCOD,CHAMT,CHMET,CHDEP,CHT
C           YRSIM,ISWWAT,NCHEM
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
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE  read_nc_chem_sec(LNCHE,YRSIM,ISWWAT,NCHEM,CDATE,
     &    CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,LNSIM,CHEXTR)

      USE ModuleDefs
      use dssat_netcdf
      IMPLICIT     NONE

      CHARACTER*1  ISWWAT,ISWCHE
      CHARACTER*5  CHCOD(NAPPL),CHMET(NAPPL),CHT(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*42 CHEXTR(NAPPL)
      CHARACTER*80 CHARTEST

      INTEGER      LNCHE,LUNEXP,ISECT,LINEXP,CDATE(NAPPL),NCHEM
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM,LNSIM
      REAL         CHAMT(NAPPL),CHDEP(NAPPL)
      integer      count,start

      PARAMETER   (ERRKEY ='IPCHEM')
      FINDCH ='*CHEMI'


      NCHEM = 0
      IF (ISWCHE .EQ. 'N') RETURN

      DO J = 1, NAPPL
         CHCOD(J)  = '     '
         CDATE(J)  = 0
         CHAMT(J)  = 0.0
         CHDEP(J)  = 0.0
      END DO

      IF ( ISWWAT .NE. 'N' .AND. LNCHE .GT. 0) THEN
         IF (ISWCHE .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            ISWCHE = 'Y'
         ENDIF
         call nc_filex%get_ind_nvals('LNCHE',LNCHE,J,NCHEM)
         if(NCHEM > NAPPL) NCHEM = NAPPL
         call nc_filex%read('CDATE',J,CDATE(1:NCHEM))
         call nc_filex%read('CHCOD',J,CHCOD(1:NCHEM))
         call nc_filex%read('CHAMT',J,CHAMT(1:NCHEM))
         call nc_filex%read('CHMET',J,CHMET(1:NCHEM))
         call nc_filex%read('CHDEP',J,CHDEP(1:NCHEM))
         call nc_filex%read('CHT',J,CHT(1:NCHEM))
         call nc_filex%read('CHEXTR',J,CHEXTR(1:NCHEM))
         do J=1,NCHEM
            IF ((CDATE(J) .LT. 1)  .OR.
     &           (MOD(CDATE(J),1000) .GT. 366)) THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF

            CALL Y2K_DOY (CDATE(J))
            IF (CDATE(J) .LT. YRSIM) THEN
               CALL ERROR (ERRKEY,3,FILEX,LINEXP)
            ENDIF
            IF ((CHAMT(J) .LT. 0.0) .OR.
     &           (CHAMT(J) .GT. 9999999.)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
         end do
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,F5.0,1X,A5,1X,F5.0,1X,A5,A42)
      END SUBROUTINE read_nc_chem_sec
