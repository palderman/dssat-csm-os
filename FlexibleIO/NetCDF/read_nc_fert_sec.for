C=======================================================================
C  IPFERT, Subroutine
C
C  Subroutine to read in fertilizer applications during season
C  To read *FERTILIZER section in the V3.5 input files
C-----------------------------------------------------------------------
C  Revision history
C
C  05/08/1991 JWW Written for DSSAT v3 format
C  05/28/1993 PWW Header revision and minor changes
C  08/23/2002 GH  Expanded array for fertilizer applications to NAPPL
C  02/03/2005 GH  Corrected error checking for missing levels
C
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNFER,YRSIM,ISWNIT
C
C  LOCAL  : ISECT,LINEXP,LN,ERRKEY,CHARTEST,ERRNUM,J,IFIND
C
C  OUTPUT : NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER
C           ACFER,AOFER,FOCOD,ANFER
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND,IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE read_nc_fert_sec(LNFER,YRSIM,ISWNIT,ISWPHO,ISWPOT,
     &     NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER,ACFER,
     &     AOFER,FOCOD,TOTNAP,IFERI,ISWWAT,LNSIM)

      USE ModuleDefs
      use dssat_netcdf
      IMPLICIT     NONE

      CHARACTER*1  ISWNIT,ISWPHO,ISWPOT,IFERI,ISWWAT
      CHARACTER*5  FERCOD(NAPPL),FOCOD(NAPPL),IFTYPE(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER      LUNEXP,LNFER,YRSIM,NFERT,FDAY(NAPPL),IFFTYP,IFFCOD
      INTEGER      ISECT,LINEXP,ERRNUM,J,IFIND,LN,LNSIM
      REAL         DFERT(NAPPL),ANFER(NAPPL),APFER(NAPPL),AKFER(NAPPL)
      REAL         ACFER(NAPPL),AOFER(NAPPL),TOTNAP
      integer      count,start

      PARAMETER   (ERRKEY ='IPFERT')

                   FINDCH ='*FERTI'

      NFERT  = 0
      TOTNAP = 0.0

      DO J = 1, NAPPL
         DFERT(J) = 0.0
         ANFER(J) = 0.0
         APFER(J) = 0.0
         AKFER(J) = 0.0
         ACFER(J) = 0.0
         AOFER(J) = 0.0
      END DO

      IF (LNFER .GT. 0) THEN
         IF (ISWNIT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            ISWNIT = 'Y'
            IFERI  = 'R'
         ENDIF
         IF (ISWWAT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWWAT = 'Y'
        ENDIF
        call nc_filex%get_ind_nvals('LNFER',LNFER,J,NFERT)
        if(NFERT > NAPPL) NFERT = NAPPL
        call nc_filex%read('FDATE',J,FDAY(1:NFERT))
        call nc_filex%read('FMCD',J,IFTYPE(1:NFERT))
        call nc_filex%read('FACD',J,FERCOD(1:NFERT))
        call nc_filex%read('FDEP',J,DFERT(1:NFERT))
        call nc_filex%read('FAMN',J,ANFER(1:NFERT))
        call nc_filex%read('FAMP',J,APFER(1:NFERT))
        call nc_filex%read('FAMK',J,AKFER(1:NFERT))
        call nc_filex%read('FAMC',J,ACFER(1:NFERT))
        call nc_filex%read('FAMO',J,AOFER(1:NFERT))
        call nc_filex%read('FOCD',J,FOCOD(1:NFERT))
         do J=1,NFERT
            IF ((FDAY(J) .LT. 0) .OR.
     &           (IFERI .EQ. 'R' .AND. MOD(FDAY(J),1000) .GT. 366))
     &           THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            IF (IFERI .EQ. 'R') THEN
               CALL Y2K_DOY(FDAY(J))
            ENDIF
            IF (IFERI .EQ. 'R' .AND. FDAY(J) .LT. YRSIM)  THEN
               CALL ERROR (ERRKEY,3,FILEX,LINEXP)
            ENDIF
            IF ((DFERT(J) .LT. 0) .OR.
     &           (DFERT(J) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF ((ANFER(J) .LT. 0) .OR.
     &           (ANFER(J) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,12,FILEX,LINEXP)
            ENDIF
            READ (IFTYPE(J)(3:5),'(I3)',IOSTAT=ERRNUM) IFFTYP
            IF (IFFTYP .LT. 1 .OR. IFFTYP .GE. 60 .OR.
     &           ERRNUM .NE. 0) THEN
               CALL ERROR (ERRKEY,14,FILEX,LINEXP)
            ENDIF
            READ (FERCOD(J)(3:5),'(I3)',IOSTAT=ERRNUM) IFFCOD
!     CHP 2/1/11 IF (IFFCOD .LT. 1 .OR. IFFCOD .GT. 18 .OR.
            IF (IFFCOD .LT. 1 .OR. IFFCOD .GT. 20 .OR.
     &           ERRNUM .NE. 0) THEN
               WRITE(FERCOD(J)(3:5),'(A3)') '001'
            ENDIF
            IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN !CHP 3/29/05
               IF ((APFER(J) .LT. 0) .OR.
     &              (APFER(J) .GT. 99999.)) THEN
!     CHP 5/16/08     CALL ERROR (ERRKEY,13,FILEX,LINEXP)
                  APFER(J) = 0.0
               ENDIF
            ENDIF
            IF (ISWPOT .EQ. 'Y') THEN !CHP 3/29/05
               IF ((AKFER(J) .LT. 0) .OR.
     &              (AKFER(J) .GT. 99999.)) THEN
!     CHP 5/16/08     CALL ERROR (ERRKEY,13,FILEX,LINEXP)
                  AKFER(J) = 0.0
               ENDIF
            ENDIF
            TOTNAP = TOTNAP + ANFER(J)
         end do
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,A5,6(1X,F5.0),1X,A5)

      END SUBROUTINE read_nc_fert_sec
