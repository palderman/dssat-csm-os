C=======================================================================
C  IPIRR, Subroutine
C
C  Determines irrigation application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  04/16/2002 GH  Modified logic for reading planting date
C  06/19/2002 GH  Modified for Y2K
C  08/23/2002 GH  Expanded array for irrigation applications to NAPPL
C
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,NIRR,EFFIRX,DSOILX,THETCX
C           IEPTX,IOFFX,IAMEX,NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT
C
C  LOCAL  : IDLAPL,ISECT,LINEXP,IFIND,LN,J,ERRNUM,AMT,ERRKEY,IRRCOD
C           CHARTEST
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND ERROR IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE read_nc_irr_sec(LNIR,YRSIM,ISWWAT,
     &           NIRR,EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,IAMEX,LNSIM,
     &           NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT,IIRV,IIRRI)

      USE ModuleDefs
      use dssat_netcdf
      IMPLICIT NONE

      INTEGER      LNIR,NIRR,LUNEXP,IDLAPL(NAPPL),ISECT,LINEXP,LNSIM
      INTEGER      YRSIM,IFIND,LN,J,ERRNUM,NAPW,IIRV(NAPPL),IRRCD

      REAL         AMT(NAPPL),DSOILX,THETCX,IEPTX,EFFIRX,TOTAPW,AIRAMX

      CHARACTER*1  ISWWAT,IIRRI
      CHARACTER*5  IRRCOD(NAPPL),IOFFX,IAMEX
      CHARACTER*6  FINDCH,ERRKEY
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER I, PERM, PERMDOY,K

      PARAMETER (ERRKEY='IPIRR ')

                 FINDCH='*IRRIG'
C
C     Set default values in case section or data are missing from file EXP
C
      EFFIRX = 1.00
      NIRR   = 0
      NAPW   = 0
      TOTAPW = 0.0
      THETCX = 0.0
      DSOILX = 0.0
      AIRAMX = 0.0
      IOFFX  = 'GS000'
      IAMEX  = 'IR001'

      idlapl = -99
      amt = -99
      irrcod = ' '

      IF (LNIR .GT. 0) THEN

         call nc_filex%get_ind_nvals('LNIR',LNIR,K,NIRR)
         if(NIRR > NAPPL) NIRR = NAPPL
         call nc_filex%read('EFIR',K,EFFIRX)
         call nc_filex%read('IDEP',K,DSOILX)
         call nc_filex%read('ITHR',K,THETCX)
         call nc_filex%read('IEPT',K,IEPTX)
         call nc_filex%read('IOFF',K,IOFFX)
         call nc_filex%read('IAME',K,IAMEX)
         call nc_filex%read('IAMT',K,AIRAMX)
C
C        Read different IRR amounts for the selected IRR level
C
         call nc_filex%read('IDATE',K,IDLAPL(1:NIRR))
         call nc_filex%read('IROP',K,IRRCOD(1:NIRR))
         call nc_filex%read('IRVAL',K,AMT(1:NIRR))
         call nc_filex%read('IIRV',K,IIRV(1:NIRR))
         do K=1,nirr
            if( IDLAPL(K) < 0 .and.
     &           (len(trim(IRRCOD(K)))==0 .or.
     &                index(IRRCOD(K),'-99')>0) .and.
     &           AMT(K) < 0 .and.
     &           IIRV(K) < 0)then
               j = K - 1
               exit
            end if

              IF ((IDLAPL(K) .LT.  0) .OR.
     &           (IIRRI .EQ. 'R' .AND. MOD(IDLAPL(K),1000) .GT. 366))
     &             CALL ERROR (ERRKEY,10,FILEX,LINEXP)
              IF (IIRRI .NE. 'D') CALL Y2K_DOY (IDLAPL(K))
              IF (IIRRI .EQ. 'R' .AND. IDLAPL(K) .LT. YRSIM)
     &             CALL ERROR (ERRKEY,3,FILEX,LINEXP)
!              IF (IIRRI .EQ. 'D' .AND. IDLAPL(K) .LT. 0) GO TO 70
              READ (IRRCOD(K)(3:5),'(I3)',IOSTAT=ERRNUM) IRRCD
              IF (IRRCD .LT. 1 .OR. IRRCD .GT. 11 .OR. !CHP changed to 11
     &             ERRNUM .NE. 0) CALL ERROR (ERRKEY,12,FILEX,LINEXP)

              IF ((IRRCD .EQ. 3 .OR. IRRCD .EQ. 9) .AND.
     &             IIRV(K) .GT. 0) THEN
                 SELECT CASE (IIRV(K))
                 CASE (6);       PERM = 1
                 CASE (7);       PERM = 2
                 CASE (8);       PERM = 3
                 CASE DEFAULT;   PERM = 0
                 END SELECT
                 IF (PERM > 0 .AND. K .GT. 1) THEN
                    PERMDOY = IDLAPL(K)
                    DO I = K-1, 1, -1
                       IF (IDLAPL(I) .EQ. PERMDOY .AND.
     &                      IRRCOD(I)(3:5) .NE. '007' .AND.
     &                      IRRCOD(I)(3:5) .NE. '008' .AND.
     &                      IRRCOD(I)(3:5) .NE. '009' .AND.
     &                      IRRCOD(I)(3:5) .NE. '010') THEN

                    !Found irrigation event corresponding to today's
                    !  bund record.  Change the irrigation code.
                          SELECT CASE (PERM)
                          CASE (1)
                             WRITE (IRRCOD(I), '(A5)') IRRCOD(I)(1:2) // '006'
                          CASE (2,3)
                             WRITE (IRRCOD(I), '(A5)') IRRCOD(I)(1:2) // '011'
                          END SELECT
                       ENDIF    !End of previous record found block
                    ENDDO       !End of loop thru previous records
                 ENDIF          !End of PERM > 0 block
              ENDIF             !End of IRRCOD = 9 (old-style bund record)
            !CHP end *************************************************

              IF ((IRRCOD(K)(3:5)) .NE. '007' .AND.
     &             (IRRCOD(K)(3:5)) .NE. '008' .AND.
     &             (IRRCOD(K)(3:5)) .NE. '009' .AND.
     &             (IRRCOD(K)(3:5)) .NE. '010') THEN !CHP added '010'
                 NAPW = NAPW + 1
                 IF (AMT(NAPW) .GT. 0.0) THEN
                    TOTAPW = TOTAPW + AMT(NAPW)
                 ENDIF

            !CHP start ***********************************************
            !When PERM > 0, change irrigation events to type 6 (PERM =1)
            !  or type 11 (PERM = 2 or 3).
                 IF (PERM > 0) THEN
                    SELECT CASE (PERM)
                    CASE (1)
                       WRITE(IRRCOD(K), '(A5)') IRRCOD(K)(1:2) // '006'
                    CASE (2,3)
                       WRITE(IRRCOD(K), '(A5)') IRRCOD(K)(1:2) // '011'
                    END SELECT
                 ENDIF
              ENDIF
            !CHP end *************************************************
           end do
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,F5.0,3(1X,F5.0),2(1X,A5),1X,F5.0)
 60   FORMAT (I3,I5,1X,A5,1X,F5.0,4X,I2)
      END SUBROUTINE read_nc_irr_sec
