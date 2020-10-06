C=======================================================================
C  IPRES, Subroutine
C
C  Determines residue application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  06/19/2002 GH  Modified for Y2K
C  05/28/1993 PWW Header revision and minor changes
C  08/23/2002 GH  Expanded array for organic material applications to NAPPL
C  02/03/2005 GH  Corrected error checking for missing levels
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,RINP,DEPRES,
C           RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,ISWPHO,ISWPOT
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

      SUBROUTINE read_nc_res_sec(LNRES,RESDAY,RESCOD,RESIDUE,
     &     RINP,DEPRES,RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,
     &     ISWPHO,ISWPOT,IRESI,ISWWAT,RMET,LNSIM)

      USE ModuleDefs
      use dssat_netcdf
      IMPLICIT     NONE

      CHARACTER*1  ISWNIT,ISWPHO,ISWPOT,IRESI,ISWWAT
      CHARACTER*5  RESCOD(NAPPL),RMET(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*78 MSG(3)
      CHARACTER*80 CHARTEST

      INTEGER      LNRES,LUNEXP,ISECT,LINEXP,RESDAY(NAPPL),NRESAP
      INTEGER      ERRNUM,J,IFIND,LN,NARES,YRSIM,LNSIM

      REAL         RESN(NAPPL),RESP(NAPPL),RESK(NAPPL),RESIDUE(NAPPL),
     &             RINP(NAPPL),DEPRES(NAPPL),RESAMT
      integer      start

      PARAMETER   (ERRKEY ='IPRES ')

                   FINDCH ='*RESID'

      NRESAP = 0
      NARES  = 0
      RESAMT = 0.0

      DO J = 1, NAPPL
         RESCOD(J)  = '     '
         RMET(J)    = '     '
         RESDAY(J)  = 0
         RESIDUE(J) = 0.0
!        RINP(J)    = 100.0
         RINP(J)    = 0.0
         DEPRES(J)  = 0.0
         RESN(J)    = 0.0
         RESP(J)    = 0.0
         RESK(J)    = 0.0
      END DO

      IF (LNRES .GT. 0) THEN
         IF (ISWNIT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWNIT = 'Y'
         ENDIF
         IF (ISWNIT .EQ. 'Y' .AND. LNSIM .EQ. 0) THEN
           IRESI  = 'R'
         ENDIF
         IF (ISWWAT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWWAT = 'Y'
         ENDIF
         call nc_filex%get_ind_nvals('LNRES',LNRES,J,NARES)
         if(NARES > NAPPL) NARES = NAPPL
         call nc_filex%read('RDATE',J,RESDAY(1:NARES))
         call nc_filex%read('RCOD',J,RESCOD(1:NARES))
         call nc_filex%read('RAMT',J,RESIDUE(1:NARES))
         call nc_filex%read('RESN',J,RESN(1:NARES))
         call nc_filex%read('RESP',J,RESP(1:NARES))
         call nc_filex%read('RESK',J,RESK(1:NARES))
         call nc_filex%read('RINP',J,RINP(1:NARES))
         call nc_filex%read('RDEP',J,DEPRES(1:NARES))
         call nc_filex%read('RMET',J,RMET(1:NARES))
         do J=1,NARES
            RESN(J)    = MAX (RESN(J),0.00)
            RESP(J)    = MAX (RESP(J),0.00)
            RESK(J)    = MAX (RESK(J),0.00)
            RESIDUE(J) = MAX (RESIDUE(J),0.0)
            IF ((RESDAY(J) .LT. 0) .OR.
     &           (IRESI .EQ. 'R' .AND. MOD(RESDAY(J),1000)
     &           .GT. 366)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF (IRESI .EQ. 'R') THEN
               CALL Y2K_DOY (RESDAY(J))
            ENDIF
            IF (IRESI .EQ. 'R' .AND. RESDAY(J) .LT. YRSIM) THEN
               CALL ERROR (ERRKEY,3,FILEX,LINEXP)
            ENDIF
            IF (RESIDUE(J) .LT. 0.0 .OR. RESIDUE(J)
     &           .GT. 99999.) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF ((RESN(J) .LT. 0) .OR.
     &           (RESN(J) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,12,FILEX,LINEXP)
            ENDIF
            IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN !CHP 3/29/05
               IF ((RESP(J) .LT.  0) .OR.
     &              (RESP(J) .GT. 99999.)) THEN
                  CALL ERROR (ERRKEY,13,FILEX,LINEXP)
               ENDIF
            ENDIF
            IF (ISWPOT .EQ. 'Y') THEN
               IF ((RESK(J) .LT.  0) .OR.
     &              (RESK(J) .GT. 99999.)) THEN
                  CALL ERROR (ERRKEY,13,FILEX,LINEXP)
               ENDIF
            ENDIF

            IF (RINP(J) < 0.0) THEN
               IF (DEPRES(J) > 0.0) THEN
                  RINP(J) = 100.0
                  DEPRES(J) = MAX (DEPRES (J),15.0)
                  WRITE(MSG(1),'("Residue application ",I3)') J
                  WRITE(MSG(2),'(A,A)')"Incorporation percentage not ",
     &                 "specified, and incorporation depth > 0."
                WRITE(MSG(3),'(A,A,F5.1,A)')"Applied residues will be ",
     &              "incorporated to a depth of ", DEPRES(J)," cm."
                CALL WARNING(3, ERRKEY, MSG)
             ELSE
                RINP(J) = 0.0
                WRITE(MSG(1),'("Residue application ",I3)') J
                MSG(2) = "Neither residue incorporation percentage " //
     &                "nor depth specified. Applied residues"
                MSG(3) = "will remain on surface until a tillage " //
     &              "event is specified."
                CALL WARNING(3, ERRKEY, MSG)
             ENDIF
          ENDIF
          IF (RINP(J) > 0.0 .AND. DEPRES(J) < 15.0) THEN
             DEPRES(J) = MAX (DEPRES (J),15.0)
             WRITE(MSG(1),'("Residue application ",I3)') J
             WRITE(MSG(2),'(A,A,F5.1,A)')"Applied residues will be ",
     &              "incorporated to a depth of ", DEPRES(J)," cm."
             CALL WARNING(2, ERRKEY, MSG)
          ENDIF
          RESAMT = RESAMT + RESIDUE(J)
        end do
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,F5.0,3(1X,F5.0),2(1X,F5.0),1X,A5)

      END SUBROUTINE read_nc_res_sec
