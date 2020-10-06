C=======================================================================
C  IPSLIN, Subroutine
C
C  Reads soil initial conditions from FILEX file
C  Read initial values of rapidly changing soil variables,
C  and convert values to standard soil depths
C-----------------------------------------------------------------------
C  Revision history
C
C  06/21/1991 JWJ Written
C  05/28/1993 PWW Header revision and minor changes
C  12/01/1993 WTB Modifed to read soil test P
C  08/19/2002 GH  Modified for Y2K
C  02/07/2007 GH  Add path to FileX
C-----------------------------------------------------------------------
C  INPUT  : FILEX,LNIC,NLAYR,DUL,SWINIT,PEDON,SLNO
C
C  LOCAL  : LN,NLAYRI,NLAYR,DS,DLAYRI,LINEXP,ISECT,CHARTEST,LUNEXP
C
C  OUTPUT : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,INO3,INH4,SWINIT
C-----------------------------------------------------------------------
C  Called : SESOIL INPUT
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE read_nc_ic_sec(
     &     FILEX,FILEX_P,LNIC,NLAYR,DUL,YRIC,PRCROP,WRESR,
     &        WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,SWINIT,INH4,INO3,
     &        ISWITCH,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID,YRSIM) 
!     &        SOM1I, SOM2I, SOM3I)

      USE ModuleDefs
      USE csm_io
      USE dssat_netcdf
      IMPLICIT     NONE

      CHARACTER*2  PRCROP
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*10 PEDON,SLNO
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST
      CHARACTER*92 FILEX_P
      character*10 initsw_char

      INTEGER      L,LN,LUNEXP,NLAYRI,NLAYR,LINEXP,ISECT,LNIC,
     &             YRIC,ERRNUM,IFIND,YRSIM
      integer      i
      real         initsw_pct
      real         ll(NL)
      REAL         DS(NL),DLAYRI(NL),SWINIT(NL)
      REAL         DUL(NL),WRESR,WRESND,EFINOC,EFNFIX,INO3(NL),INH4(NL)
      REAL         ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID !, TOTSOM
!      REAL, DIMENSION(NL) :: SOM1I, SOM2I, SOM3I

      TYPE (SwitchType) ISWITCH

      integer      start

      PARAMETER   (LUNEXP = 16)
      PARAMETER   (ERRKEY = 'NCSLIN')

      FINDCH = '*INITI'
C
C     Set default initial conditions in case they are missing
C
      YRIC = 0
      PRCROP = '  '
      WRESR  = 0.0
C-GH  WRESR  = 1.0
      WRESND = 0.0
      EFINOC = 1.0
      EFNFIX = 1.0
      ICWD   = 0.0
      ICRES  = 0.0
      ICREN  = 0.0
      ICREP  = 0.0
      ICRIP  = 100.0
      ICRID  = 0.0
C-GH  ICRID  = 15.0
C

      SWINIT = -99.
      call find_cmd_arg('initsw_pct',initsw_char)
      if(len(trim(initsw_char))>0)then
         call csminp%get('*SOIL','LL',ll)
         read(initsw_char,'(f10.0)') initsw_pct
         do i=1,NL
            if(dul(i)>0.and.ll(i)>0)
     &           swinit(i) = (dul(i) - ll(i)) * initsw_pct/100 + ll(i)
         end do
      end if
      INO3   = -99.
      INH4   = -99.

      IF (PEDON .NE. SLNO) RETURN
      IF (LNIC  .LE. 0)    RETURN

      call nc_filex%read('PCR',LNIC,PRCROP)
      call nc_filex%read('ICDAT',LNIC,YRIC)
      call nc_filex%read('ICRT',LNIC,WRESR)
      call nc_filex%read('ICND',LNIC,WRESND)
      call nc_filex%read('ICRN',LNIC,EFINOC)
      call nc_filex%read('ICRE',LNIC,EFNFIX)
      call nc_filex%read('ICWD',LNIC,ICWD)
      call nc_filex%read('ICRES',LNIC,ICRES)
      call nc_filex%read('ICREN',LNIC,ICREN)
      call nc_filex%read('ICREP',LNIC,ICREP)
      call nc_filex%read('ICRIP',LNIC,ICRIP)
      call nc_filex%read('ICRID',LNIC,ICRID)

      IF (YRIC .LT. 0) THEN
        YRIC = YRSIM
      ENDIF
      CALL  Y2K_DOY (YRIC)
      IF (ISWITCH%ISWNIT .EQ. 'Y') THEN
         WRESR = MAX(WRESR,0.0)
C-GH     IF (WRESR  .LT. 1.0) WRESR  = 1.0
         IF (WRESND .LT. 0.0) WRESND = 0.0
         IF (EFINOC .LT. 0.0) EFINOC = 1.0
         IF (EFNFIX .LT. 0.0) EFNFIX = 1.0
         ICWD  = MAX(ICWD,0.0)
C-PW     ICRES = MAX(ICRES,10.0)
         ICRES = MAX(ICRES,0.0)
         ICREN = MAX(ICREN,0.0)
         ICREP = MAX(ICREP,0.0)
         ICRID = MAX(ICRID,0.0)
C-GH     ICRID = MAX(ICRID,15.0)
         IF (ICRIP  .LT. 0.0) ICRIP  = 100.0
      ENDIF

      NLAYRI = 1
C
C     Read layer information for the correct IC level number
C
      start = (LNIC - 1)*NL + 1
      call nc_filex%read('ICBL',start,DLAYRI(1:NL))
      call nc_filex%read('SH2O',start,SWINIT(1:NL))
      call nc_filex%read('SNH4',start,INH4(1:NL))
      call nc_filex%read('SNO3',start,INO3(1:NL))

      NLAYRI = 0
      do i=1,NL
         if(DLAYRI(i) < 0) exit
         NLAYRI = NLAYRI + 1
      end do

!     Match layers first!
      CALL LMATCH (NLAYRI,DLAYRI,SWINIT,NLAYR,DS)
      CALL LMATCH (NLAYRI,DLAYRI,INH4,  NLAYR,DS)
      CALL LMATCH (NLAYRI,DLAYRI,INO3,  NLAYR,DS)

!     Then check for bounds
      DO L = 1, NLAYR
        IF (ISWITCH%ISWWAT .NE. 'N') THEN
           IF (SWINIT(L) .GT. 0.75) THEN
              CALL ERROR (ERRKEY,10,FILEX,LINEXP)
           ENDIF
           IF (SWINIT(L) .LT. 0.00) THEN
               SWINIT(L) = DUL(L)
           ENDIF
        ENDIF

        IF (ISWITCH%ISWNIT .EQ. 'Y') THEN
           IF (INH4(L) .LT.   0.0) THEN
              INH4(L) = 0.01
           ENDIF
           IF (INH4(L) .GT. 100.0) THEN
              CALL ERROR (ERRKEY,11,FILEX,LINEXP)
           ENDIF
           IF (INO3(L) .LT.   0.0) THEN
              INO3(L) = 0.01
           ENDIF
           IF (INO3(L) .GT. 100.0) THEN
              CALL ERROR (ERRKEY,12,FILEX,LINEXP)
           ENDIF
        ENDIF
      ENDDO


!      CALL LMATCH (NLAYRI,DLAYRI,SOM1I, NLAYR,DS)
!      CALL LMATCH (NLAYRI,DLAYRI,SOM2I, NLAYR,DS)
!      CALL LMATCH (NLAYRI,DLAYRI,SOM3I, NLAYR,DS)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,3X,A2,1X,I5,10(1X,F5.0))
 60   FORMAT (I3,F5.0,3(1X,F5.0))
 65   FORMAT (26X,3F6.0)

      END SUBROUTINE read_nc_ic_sec
