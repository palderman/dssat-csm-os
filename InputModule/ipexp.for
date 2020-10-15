C=======================================================================
C  IPEXP, Subroutine
C
C  Determines experiment and treatment selection
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWW Written
C  05/28/1993 PWW Header revision and minor changes            
C  12/10/1995 NBP Soil top 2 layers=10 cm, error check on MEEVP & MEEVP
C  09/30/1997 GH  Added cotton                                 
C  01/19/1998 PWW Added species file for Ceres and OilCrop 
C  05/06/1998 GH  Changed date and version number to v3.5, May 15, 1998  
C  08/17/1999 GH  Added cabbage  
C  09/20/2000 GH  Replaced G9 with BR for Brachiaria decumbens
C  09/20/2000 GH  Changed MESOM for 'Parton and Godwin options
C  09/20/2000 GH  Changed to version 3.90 (990)
C  11/03/2001 GH  Add CASUPRO
C  12/12/2001 GH  Extract model information
C  01/30/2002 GH  Modify inputs for the wheat model
C  06/07/2002 GH  Modify dates for Y2K
C  11/21/2002 GH  Modify rotation options and reps
C  02/20/2006 GH  Added RNMODE=G for GENCALC
C  02/21/2006 GH  Moved logic for crop model selection to DSSATPRO.V45
!  07/26/2006 CHP Added previous management code for lookup in 
!       SOMFR045.SDA file to FIELDS section
!  08/25/2006 CHP Added FILEX method codes MESOL, MESEV, METMP
!                 MESOL = alternate soil layer distribution
!                 MESEV = soil evaporation method (S=Sulieman (default), 
!                                                  R=Ritchie (old))
!                 METMP = soil temperature options
!  01/12/2007 CHP Rotation number used for sequence runs.
!  02/05/2007 CHP Reverse location of MESEV and METMP in FILEX
C  02/09/2007 GH  Allow for improved path selection for input files
!  03/26/2007 CHP MESOL = 2 is default (see LYRSET2 in LMATCH.for)
!  07/04/2007 CHP MESEV = 2 is default (Sulieman-Ritchie soil evap)
!  07/05/2007 CHP Default simulation controls file added.
!  09/18/2007 CHP Added codes for IXIM maize model 
!  04/21/2008 CHP Added MESOL = 3, user-specified soil layer dist.
!  12/09/2008 CHP Remove METMP
!  04/16/2013 CHP/KD Added SALUS model
C-----------------------------------------------------------------------
C  INPUT  : MODEL,RUN,DS,SLNO,LNIC,LNSA,NYRS,VARNO,CROP,PATHMO,WMODI
C           FROP,SLTX
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR IGNORE VERIFY CLEAR FIND IPCUL PATH IPPLNT IPFLD IPSIM
C           YR_DOY IPENV IPHAR IPIRR IPRES IPFERT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPEXP (MODEL,RUN,RNMODE,FILEX,PATHEX,FILEX_P, FILECTL,
     &           SLNO,NYRS,VARNO, 
     &           CROP,WMODI,FROP,TRTN,EXPP,EXPN,TITLET,TRTALL,
     &           TRTNUM,ROTNUM, IIRV,FTYPEN,CHEXTR,
     &           NFORC,PLTFOR,NDOF,PMTYPE,
     &           LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
     &           LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &           CONTROL, ISWITCH, UseSimCtr, MODELARG)

      USE ModuleDefs
      USE ModuleData    
      use csm_io
      use dssat_netcdf
      Use CsvOutput   ! VSH
      IMPLICIT NONE
      SAVE

      CHARACTER* 1 LINE(80),BLANK, RNMODE
      CHARACTER* 1 WMODI,ANS
      CHARACTER* 2 CROP
      CHARACTER* 3 PROCOD,ALN(13),ALLN
      CHARACTER* 4 WSTA1
      CHARACTER* 6 VARNO,ERRKEY,FINDCH
      CHARACTER* 7 FILELS
      CHARACTER* 8 FILES_a, FILES_b, MODEL, MODELARG, FILEW4
      CHARACTER*10 SLNO
      CHARACTER*12 NAMEF, FILEX
      CHARACTER*25 TITLET
      CHARACTER*42 CHEXTR(NAPPL)
      CHARACTER*78 MSG(4)
      CHARACTER*80 CHARTEST,PATHEX
      CHARACTER*92 FILEX_P,FILETMP
      CHARACTER*120 FILECTL, WTHSTR

      INTEGER I,L,NLOOP,LINF,ISECT,LUNEXP,LUNLST
      INTEGER LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,LNCHE,LNCU
      INTEGER LNHAR,LNENV,LNTIL,LNSIM,LINEXP
      INTEGER NYRS,FROP,EXPN,EXPP,TRTN,ERRNUM,IFIND,FTYPEN
      INTEGER PATHL,RUN,ISIM,TRTALL,IIRV(NAPPL)   !,CRID
      INTEGER NFORC,NDOF,PMTYPE,YR,ROTN
      INTEGER TRTNUM, ROTNUM!,FREQ(3),CUHT(3) !NEW FORAGE VARIABLES (DIEGO-2/14/2017)

      REAL    FLAG,EXP,TRT,PLTFOR,FREQ,CUHT !NEW FORAGE VARIABLES (DIEGO-2/14/2017)

      LOGICAL FEXIST, UseSimCtr, SimLevel

      CHARACTER*1 ISIMI
      CHARACTER*1 DAYFAC(NAPPL),RADFAC(NAPPL),TXFAC(NAPPL),TMFAC(NAPPL)
      CHARACTER*1 PRCFAC(NAPPL),CO2FAC(NAPPL),DPTFAC(NAPPL)
      CHARACTER*1 WNDFAC(NAPPL)
      CHARACTER*1 PLME,PLDS
      CHARACTER*4 WSTA
      CHARACTER*5 DFDRN,FLST,IAME,IOFF,SLTX
      CHARACTER*5 NCODE,NEND,IOFFX,IAMEX, FldHist
      CHARACTER*5 HSTG(3),HCOM(3),HSIZ(3),IRRCOD(NAPPL)
      CHARACTER*5 RESCOD(NAPPL),RMET(NAPPL)
      CHARACTER*5 FERCOD(NAPPL),FOCOD(NAPPL),IFTYPE(NAPPL)
      CHARACTER*5 CHCOD(NAPPL),CHMET(NAPPL),CHT(NAPPL),TIMPL(NAPPL)
      CHARACTER*8 FLDNAM,EXPER
      CHARACTER*16 CROPD
      CHARACTER*25 TITSIM
      INTEGER YRSIM,YRPLT,IEMRG,TRTNO,ROTNO,ROTOPT,CRPNO,RSEED1
      INTEGER PWDINF,PWDINL,NRESDL,HDLAY,HLATE,NHAR,HDATE(3)
      INTEGER NEV,WMDATE(NAPPL),NIRR,IDLAPL(NAPPL),RESDAY(NAPPL)
      INTEGER FDAY(NAPPL),NFERT,YEAR,NARES,NAPW,NREPSQ
      INTEGER NCHEM,NTIL,CDATE(NAPPL),TDATE(NAPPL)
      INTEGER FHDur
      integer HSTG_int(3)
      REAL PLANTS,PLTPOP,ROWSPC,AZIR,SDEPTH,SDWTPL
      REAL SDAGE,ATEMP,PLPH,FLOB,FLDD,SFDRN,SWPLTL,SWPLTH
      REAL SWPLTD,PTX,PTTN,DSOIL,THETAC,IEPT,AIRAMT,EFFIRR
      REAL DSOILN,SOILNC,SOILNX,RIP,DRESMG,HPP,HRP
      REAL DAYADJ(NAPPL),RADADJ(NAPPL),TXADJ(NAPPL),TMADJ(NAPPL)
      REAL PRCADJ(NAPPL),RESK(NAPPL),ANFER(NAPPL)
      REAL CO2ADJ(NAPPL),DPTADJ(NAPPL),WNDADJ(NAPPL),DSOILX,THETCX
      REAL IEPTX,AIRAMX,EFFIRX,AMT(NAPPL),RESN(NAPPL),RESP(NAPPL)
      REAL RESIDUE(NAPPL),RINP(NAPPL),DEPRES(NAPPL),DFERT(NAPPL)
      REAL APFER(NAPPL),AKFER(NAPPL),ACFER(NAPPL),AOFER(NAPPL),TOTNAP
      REAL RESAMT,TOTAPW,WTHADJ(2,8),SLDP,SPRLAP
      REAL SLOPE,XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS
      REAL HPC(3),HBPC(3)
      REAL CHAMT(NAPPL),CHDEP(NAPPL),TDEP(NAPPL)
      CHARACTER*1 ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,MESIC
      CHARACTER*1 ICO2
      CHARACTER*1 MELI,MEEVP,MEINF,MEPHO,IPLTI,IIRRI,IFERI,IRESI,IHARI
      CHARACTER*1 ISWCHE,ISWTIL,MEHYD,MESOM, MESOL, MESEV, METMP
      CHARACTER*1 IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,IOX
      CHARACTER*1 IDETH,IDETR
      CHARACTER*1 IDETL
      CHARACTER*2 CG
      CHARACTER*12 FILEA,FILEC,FILEE,FILEG,FILEP,FILES,FILET,FILEW
      CHARACTER*12 OUTO
      CHARACTER*25 TITLER
      CHARACTER*102 DSSATP
      CHARACTER*60 ENAME
      CHARACTER*80 PATHSL,PATHWT,PATHCR,PATHGE,PATHPE,PATHEC
      INTEGER NSWITCH

      integer tmp_int

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

C-----------------------------------------------------------------------

      PARAMETER (LUNEXP = 16)
      PARAMETER (LUNLST = 17)
      PARAMETER (ERRKEY = 'IPEXP ')
      PARAMETER (BLANK = ' ')
                 FINDCH = '*TREAT'

C-----------------------------------------------------------------------
      FILELS = 'EXP.LST'
      
C-----------------------------------------------------------------------
C     Set depths of individual soil layers
C-----------------------------------------------------------------------
C     This subroutine assumes that DS(L) values are depths to the bottom
C     of layer L
C
C     DS(L) can be interactively modified in the sensitivity analysis

!     M = Soil layer at which profile transitions from 
!             30 cm to 60 cm thickness.
!      M = 18  !soil layers 18, 19, 20 with thickness of 60 cm.

!      DS(1) =  5.
!      DS(2) = 15.
!      DS(3) = 30.
!      DS(4) = 45.
!      DS(5) = 60.
!
!      DO L = 6, M-1
!         DS(L) = DS(L - 1) + 30.
!      END DO
!
!      DO L = M, NL
!         DS(L) = DS(L - 1) + 60.
!      END DO

C-----------------------------------------------------------------------
      NLOOP = 0
      IF (RUN .EQ. 1) THEN
         EXPN   = 1
         EXPP   = 0
         TRTN   = 1
         TRTALL = 999
       ELSE
         EXPN   = EXPP
      ENDIF

      IF (RNMODE .EQ. 'I') THEN
         OPEN (LUNLST, FILE = FILELS,STATUS = 'OLD',IOSTAT=ERRNUM)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,0)

         WRITE (*,200)
         I = 0
  300    CONTINUE
         I = I + 1
         LINF = 0
  350    CONTINUE
         CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
         IF (ISECT .EQ. 2) GO TO 350

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,410,IOSTAT=ERRNUM) EXPER,CG,ENAME
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,LINEXP)
            IF (MOD(I,16) .EQ. 0) THEN
               WRITE (*,600)
               READ (5,'(A1)') ANS
            ENDIF
            if(index(' 0123456789',exper(5:5))/=0 .and.
     &         index(' 0123456789',exper(6:6))/=0) then
               READ(EXPER(5:6),'(I2)',IOSTAT=ERRNUM) YR
               IF (YR .GE. 10) THEN
                 WRITE (*,500) I,CG,ENAME(1:45),EXPER(1:2),EXPER(3:4),
     &                    EXPER(5:6),EXPER(7:8)
               ELSE
                 WRITE (*,501) I,CG,ENAME(1:45),EXPER(1:2),EXPER(3:4),
     &                      EXPER(5:6),EXPER(7:8)
               ENDIF
            end if

          ELSE
            GO TO 800
         ENDIF

         GO TO 300
  800    CONTINUE
         REWIND (LUNLST)

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

  850    CONTINUE
         LINE(1) = ' '
         NLOOP = NLOOP + 1
         IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,2,FILELS,0)
         WRITE (*,1000) EXPN
         READ  (5,1100) LINE
         CALL VERIFY (LINE,EXP,FLAG)

         IF (EXP .LE. 0.0) THEN
            EXP = EXPN
          ELSEIF ((FLAG .GT. 0) .OR. (EXP .GT. (I-1))) THEN
            WRITE (*,1101) (I-1)
            GO TO 850
          ELSEIF (EXP .NE. NINT(EXP)) THEN
            WRITE (*,1102)
            GO TO 850
          ELSEIF (EXP .GT. 0.0) THEN
            EXPN = NINT(EXP)
          ELSE
            CALL ERROR (ERRKEY,2,FILELS,0)
         ENDIF
C
C     Establish the name of the experiment input file
C
        I = 0
 950    CONTINUE
        I = I + 1
 975    CONTINUE
        CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
        IF (ISECT .EQ. 2) GO TO 975
        READ (CHARTEST,410,IOSTAT=ERRNUM) EXPER,CG,ENAME
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,LINEXP)
        IF (I .LT. EXPN) GO TO 950
        CLOSE (LUNLST)
        FILEX(1:12) = EXPER//'.'//CG//'X'
        FILEX_P(1:12) = FILEX 
      ELSE
        READ(FILEX(10:11),'(A2)') CG
        READ(FILEX(1:8),'(A8)') EXPER
      ENDIF

!     VSH
      EXPNAME = EXPER
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        
      FILEA(1:12) = EXPER//'.'//CG//'A'
      FILET(1:12) = EXPER//'.'//CG//'T'

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      NLOOP = 0
      I     = 0
      if(nc_filex%yes)then

         call read_nc_trt_sec(TRTNUM,ENAME,TITLET,LNCU,LNFLD,LNSA,LNIC,
     &        LNPLT,LNIR,LNFER,LNRES,LNCHE,LNTIL,LNENV,LNHAR,LNSIM)

         SimLevel = lnsim > 0
         TRTNO = TRTNUM

      else ! use ascii text file X
      OPEN (LUNEXP,FILE = FILEX_P,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) THEN
        MSG(1) = "File not found:"
        MSG(2) = FILEX_P
        CALL WARNING(2,ERRKEY,MSG)
        CALL ERROR (ERRKEY,29,FILEX_P,0)
      ENDIF
      READ(LUNEXP,1500) ENAME
      control % ename = ename
 1500 FORMAT(25X,A60)
      IF (RNMODE .EQ. 'I') CALL CLEAR
      IF (EXPN .NE. EXPP) THEN
         TRTN = 1
      ENDIF
      IF (RNMODE .EQ. 'I' .OR. RNMODE .EQ. 'A' .AND.
     &   TRTALL .EQ. 999) THEN
         IF (RNMODE .EQ. 'I') WRITE (*,2300) ENAME(1:40)
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF(IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 2400    CONTINUE
         I = I + 1
         CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,54,IOSTAT=ERRNUM) (ALN(L),L=1,13)
   54       FORMAT (34X,13A3)
            DO L=1,13
              ALLN = ALN(L)
              IF (ALLN(3:3) .EQ. '?') THEN
                 ERRNUM = 99
                 CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
              ENDIF
            ENDDO
            IF (RNMODE .EQ. 'Q') THEN
              READ (CHARTEST,56,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,
     &              TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &              LNCHE,LNTIL,LNENV,LNHAR,LNSIM
            ELSE 
              READ (CHARTEST,55,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,
     &              TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &              LNCHE,LNTIL,LNENV,LNHAR,LNSIM
            ENDIF
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (MOD(I,16) .EQ. 0 .AND. RNMODE .EQ. 'I') THEN
               WRITE (*,600)
               READ (5,'(A1)') ANS
            ENDIF
            if(index(' 0123456789',exper(5:5))/=0.and.
     &         index(' 0123456789',exper(6:6))/=0)then
                READ(EXPER(5:6),'(I2)') YR
                IF (YR .GE. 10) THEN
                IF (RNMODE .EQ. 'I') WRITE (*,2600) I,TITLET,
     &              EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8),TRTNO
              ELSE
                  IF (RNMODE .EQ. 'I') WRITE (*,2601) I,TITLET,
     &                EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8),TRTNO
              ENDIF
            end if
          ELSE
            GO TO 2700
         ENDIF
         GO TO 2400
 2700    CONTINUE
         TRTALL = I - 1
         IF (RNMODE .EQ. 'A') TRTN = 1
C-GH     IF (RNMODE .EQ. 'I') 
C-GH     &   WRITE (*,2650) I,EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8)
         
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
 2750    CONTINUE
         NLOOP = NLOOP + 1
         LINE(1) = ' '
         IF (NLOOP .GT. 25) CALL ERROR(ERRKEY,3,FILEX,LINEXP)
         IF (RNMODE .EQ. 'I') THEN
           WRITE (*,2900) TRTN
C
C        Read the correct treatment number
C
           READ (5,1100) LINE
           CALL VERIFY (LINE,TRT,FLAG)
         ENDIF
         IF (TRT .LE. 0.0) THEN
            TRT = TRTN
C-GH      ELSEIF (TRT .EQ. (TRTALL+1) .AND. RNMODE .EQ. 'I') THEN
C-GH        RNMODE = 'A'
C-GH        TRTN   = 1
          ELSEIF ((FLAG .GT. 0) .OR. (TRT .GT. I)) THEN
            WRITE (*,2751) (I-1)
            GO TO 2750
          ELSEIF (TRT .NE. NINT(TRT)) THEN
            WRITE(*,2752)
            GO TO 2750
          ELSEIF (TRT .GT. 0.) THEN
            TRTN = NINT(TRT)
          ELSE
            CALL ERROR (ERRKEY,3,FILEX,LINEXP)
         ENDIF
       ELSEIF (INDEX ('Q',RNMODE) .GT. 0) THEN
         !READ (TRNARG(1:6),'(I6)') TRTN
         !READ (ROTNARG(1:6),'(I6)') ROTN
         TRTN = TRTNUM
         ROTN = ROTNUM
         I = 999
       ELSEIF (INDEX ('NQGSFBECT',RNMODE) .GT. 0) THEN
!         READ (TRNARG(1:6),'(I6)') TRTN
         TRTN = TRTNUM
         I = 999
       ELSEIF (INDEX ('A',RNMODE) .GT. 0) THEN
         TRTN = TRTN + 1
      ENDIF
      
C-----------------------------------------------------------------------
C     Find treatment number and appropriate levels
C-----------------------------------------------------------------------
      REWIND (LUNEXP)
      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)

!     Read @Line, check that 13th column = "SM"
      CALL IGNORE2(LUNEXP,LINEXP,ISECT,CHARTEST)
      SimLevel = .TRUE.
      IF (ISECT == 3) THEN
        IF (CHARTEST(72:73) /= 'SM') THEN
          SimLevel = .FALSE.
        ENDIF
      ENDIF

      I = 0
 50   CONTINUE
      I = I + 1
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
      IF (RNMODE .EQ. 'Q') THEN
        READ (CHARTEST,56,IOSTAT=ERRNUM) TRTNO,ROTNO,ROTOPT,CRPNO,
     &     TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &     LNCHE,LNTIL,LNENV,LNHAR,LNSIM
      ELSE
        READ (CHARTEST,55,IOSTAT=ERRNUM) TRTNO,ROTNO,ROTOPT,CRPNO,
     &     TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &     LNCHE,LNTIL,LNENV,LNHAR,LNSIM
      ENDIF
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)

C     IF (I .LT. TRTN) GO TO 50
      IF ((INDEX('BEDNSGFCT',RNMODE) .GT. 0 .AND. TRTN .NE. TRTNO) .OR.
     &    (INDEX('Q',RNMODE) .GT. 0 .AND. 
     &                     (TRTN .NE. TRTNO .OR. ROTN .NE. ROTNO)) .OR. 
     &    (INDEX('AI',RNMODE) .GT. 0 .AND. I .LT. TRTN))
     &    GO TO 50
      end if ! use ascii text file X?

!     Generate header information for Warnings or Errors in input module
      CALL OPHEAD (RUNINIT,99,0.0,0.0,"                ",0.0,0.0, 
     &     "      ",RUN,"        ",TITLET,WTHSTR, RNMODE,
     &     CONTROL, ISWITCH, UseSimCtr, PATHEX)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

c      IF (RNMODE .EQ. 'I' .OR. RNMODE .EQ. 'A' .AND. TRTN .EQ. 1) THEN
c         CALL CLEAR
c         WRITE(*,3450)
c      ENDIF

      call csminp%add_sec('*EXP.DETAILS',ntiers=1)

      call csminp%add_var('*EXP.DETAILS',tier=1,
     &     char_name=(/'EXPER','CG   ','ENAME'/),
     &     int_name=(/'EXPN  ','TRTN  ','TRTALL'/))

      call csminp%put('*EXP.DETAILS','EXPN',EXPN)
      call csminp%put('*EXP.DETAILS','EXPER',EXPER)
      call csminp%put('*EXP.DETAILS','CG',CG)
      call csminp%put('*EXP.DETAILS','ENAME',ENAME)
      call csminp%put('*EXP.DETAILS','TRTN',TRTN)
      call csminp%put('*EXP.DETAILS','TRTALL',TRTALL)

C-----------------------------------------------------------------------
C     Call input section for cultivar selection
C-----------------------------------------------------------------------
      if(nc_filex%yes)then
         call nc_filex%read('CR',LNCU,CROP)
         call nc_filex%read('INGENO',LNCU,VARNO)
      else
         CALL IPCUL (LUNEXP,FILEX,LNCU,CROP,VARNO)
         IF (CROP   .EQ. '  ') CALL ERROR (ERRKEY,10,FILEX,LINEXP)
         IF (VARNO  .EQ. '  ') CALL ERROR (ERRKEY,11,FILEX,LINEXP)
      end if
      CONTROL % CROP = CROP 

      call csminp%add_sec('*CULTIVARS')

      call csminp%add_var('*CULTIVARS',
     &                char_name=(/'CROP ','VARNO'/))

      call csminp%put('*CULTIVARS','CROP',CROP)
      call csminp%put('*CULTIVARS','VARNO',VARNO)

!     CHP 10/25/2006 Move this to IPSIM 
!C-----------------------------------------------------------------------
!C    Select Model Name and Path
!C-----------------------------------------------------------------------
!
!      PROCOD = 'M' // CROP  
!      CALL MODEL_NAME (PROCOD,DSSATP,MODEL)

C-----------------------------------------------------------------------
C  12/12/2008 Move planting date read to above simulation controls -- 
!     needed for Tony's generic simulation controls.
C-----------------------------------------------------------------------

      if(nc_filex%yes)then
         CALL read_nc_plt_sec (LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,
     &        MODEL,SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &        YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)
      else
         CALL IPPLNT_Inp (LUNEXP,FILEX,LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,
     &        MODEL,SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &        YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)
      end if
C-----------------------------------------------------------------------
C     Call IPSIM
C-----------------------------------------------------------------------
      IF (.NOT. SimLevel) THEN
        LNSIM = 0
        YRSIM = YRPLT
      ENDIF

      if(nc_filex%yes)then
         CALL read_nc_sim_sec(LNSIM,TITSIM,NYRS,RUN,NREPSQ,ISIMI,PWDINF,
     &     PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,PTX,PTTN,DSOIL,THETAC,
     &        IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,SOILNX,NEND,RIP,NRESDL,
     &        DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,RSEED1,LINEXP,AIRAMT,
     &        EFFIRR,CROP,FROP,MODEL,RNMODE,FILEX,
     &     CONTROL, ISWITCH, UseSimCtr, FILECTL, MODELARG, YRPLT)
      else
         CALL IPSIM (LUNEXP,LNSIM,TITSIM,NYRS,RUN,NREPSQ,ISIMI,PWDINF,
     &     PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,PTX,PTTN,DSOIL,THETAC,
     &        IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,SOILNX,NEND,RIP,NRESDL,
     &        DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,RSEED1,LINEXP,AIRAMT,
     &        EFFIRR,CROP,FROP,MODEL,RNMODE,FILEX,
     &        CONTROL, ISWITCH, UseSimCtr, FILECTL, MODELARG, YRPLT)
      end if

      call csminp%get('*SIMULATION CONTROL','ISWWAT',ISWWAT)
      call csminp%get('*SIMULATION CONTROL','ISWNIT',ISWNIT)
      call csminp%get('*SIMULATION CONTROL','ISWPHO',ISWPHO)
      call csminp%get('*SIMULATION CONTROL','ISWPOT',ISWPOT)
      call csminp%get('*SIMULATION CONTROL','MEWTH',MEWTH)
      call csminp%get('*SIMULATION CONTROL','IPLTI',IPLTI)
      call csminp%get('*SIMULATION CONTROL','IIRRI',IIRRI)
      call csminp%get('*SIMULATION CONTROL','IFERI',IFERI)
      call csminp%get('*SIMULATION CONTROL','IRESI',IRESI)
      call csminp%get('*SIMULATION CONTROL','IHARI',IHARI)
      call csminp%get('*SIMULATION CONTROL','ISWCHE',ISWCHE)
      call csminp%get('*SIMULATION CONTROL','ISWTIL',ISWTIL)
      call csminp%get('*SIMULATION CONTROL','IOX',IOX)

C-----------------------------------------------------------------------
C        Select crop parameter input file
C-----------------------------------------------------------------------

      IF (CROP .NE. 'FA' ) THEN
        ! IF ((INDEX('GROSIM',MODEL(3:5)) .GT. 0) .OR.
     &  !    (INDEX('ALOCERSUBOIL',MODEL(3:5)) .GT. 0) .OR.
     &  !    (INDEX('CSPCAN',MODEL(3:5)) .GT.0) .OR. 
     &  !    (INDEX('CRP',MODEL(3:5)) .GT.0) .OR.  !Cassava
     &  !    (INDEX('IXM',MODEL(3:5)) .GT.0) .OR.  !IXIM MAIZE
     &  !    (INDEX('CSM',MODEL(3:5)) .GT.0)) THEN
           FILEC(1:12) = CROP//MODEL(3:8)//'.SPE'
           INQUIRE (FILE = FILEC,EXIST = FEXIST)
           IF (.NOT. FEXIST) THEN
              CALL PATH('CRD',control%DSSATP,PATHCR,1,NAMEF)
            ELSE
              PATHCR = BLANK
           ENDIF
        ! ENDIF
C-----------------------------------------------------------------------
C        Select genetic parameter input file
C
C        READ GENCALC2.CUL if RNMODE = G & T
C        READ ???????0.CUL for other modes
C-----------------------------------------------------------------------
         FILEG(1:12) = CROP//MODEL(3:8)//'.CUL'
!        09/21/2009
!        CHP/KD Generic SALUS model used for several crops with single  
!          cultivar file. Do not include crop code in file name.
         IF (MODEL(1:5) == 'SALUS') THEN
            FILEG(1:12) = MODEL(1:8)//'.CUL'
         ENDIF
         IF (INDEX('GT',RNMODE) .GT. 0) THEN
            WRITE(FILEG(1:8),'(A8)') 'GENCALC2'
         ENDIF
         INQUIRE (FILE = FILEG,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            FILETMP = TRIM(PATHEX)//FILEG
            INQUIRE (FILE = FILETMP,EXIST = FEXIST)
            IF (.NOT. FEXIST) THEN
               CALL PATH('CRD',control%DSSATP,PATHGE,1,NAMEF)
            ELSE 
               PATHGE = TRIM(PATHEX)
            ENDIF
         ELSE
            PATHGE = BLANK
         ENDIF

C-----------------------------------------------------------------------
C        Select ecotype parameter input file
C
C        READ GENCALC2.ECO if RNMODE = G & T
C        READ ???????0.ECO for other modes;
C-----------------------------------------------------------------------
         FILEE(1:12) = CROP//MODEL(3:8)//'.ECO'
         IF (INDEX('GT',RNMODE) .GT. 0) THEN
            WRITE(FILEE(1:8),'(A8)') 'GENCALC2'
         ENDIF
         INQUIRE (FILE = FILEE,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            FILETMP = TRIM(PATHEX)//FILEE
            INQUIRE (FILE = FILETMP,EXIST = FEXIST)
            IF (.NOT. FEXIST) THEN
               CALL PATH ('CRD',control%DSSATP,PATHEC,1,NAMEF)
            ELSE
               PATHEC = TRIM(PATHEX)
            ENDIF
         ELSE
              PATHEC = BLANK
         ENDIF
        
C-----------------------------------------------------------------------
C        Select pest parameter input file
C-----------------------------------------------------------------------

         FILEP(1:12) = CROP//MODEL(3:8)//'.PST'
         INQUIRE (FILE = FILEP,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            CALL PATH('PSD',control%DSSATP,PATHPE,1,NAMEF)
          ELSE
            PATHPE = BLANK
         ENDIF

C-----------------------------------------------------------------------
C        End of IF NOT fallow
C-----------------------------------------------------------------------
      ENDIF

      CALL GET_CROPD(CROP, CROPD)

!      REWIND(LUNEXP)

!     Regen short headers now that MODEL is known.
      CALL OPHEAD (RUNINIT,99,0.0,0.0,"                ",0.0,0.0, 
     &     "      ",RUN,MODEL,TITLET,WTHSTR, RNMODE,
     &     CONTROL, ISWITCH, UseSimCtr, PATHEX)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
!     Skip soils field and soils input for sequence mode
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1 .or. nc_batch%yes)THEN

         if(nc_filex%yes)then
            CALL read_nc_fld_sec(LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
     &           SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,
     &           XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS,FldHist, FHDur)
         else
            CALL IPFLD (LUNEXP,FILEX,LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
     &           SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,
     &           XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS,FldHist, FHDur)
         end if
C-----------------------------------------------------------------------
C     Select soil profile input file
C       1. SOIL.SOL
C       2. ??.SOL where ?? = Institute ID from Soil Profile Number
C       3. From C:\DSSAT45\DSSATPRO.V45  SOIL.SOL
C       4. From C:\DSSAT45\DSSATPRO.V45  ??.SOL
C-----------------------------------------------------------------------

        if(.not.nc_soil%yes)then
        FILES_a = 'SOIL.SOL'
        FILES_b = SLNO(1:2)//'.SOL  '

        INQUIRE (FILE = FILES_a,EXIST = FEXIST)
        IF (FEXIST) THEN
!          SOIL.SOL in current directory
           FILES = FILES_a
           PATHSL = BLANK

        ELSE
           INQUIRE (FILE = FILES_b,EXIST = FEXIST)
           IF (FEXIST) THEN
!             Alt soil name in current directory
              FILES = FILES_b
              PATHSL = BLANK 

           ELSE
              FILETMP = TRIM(PATHEX)//FILES_a
              INQUIRE (FILE = FILETMP,EXIST = FEXIST)
              IF (FEXIST) THEN
!                SOIL.SOL in experiment directory
                 FILES = FILES_a
                 PATHSL = TRIM(PATHEX)

              ELSE
                 FILETMP = TRIM(PATHEX)//FILES_b
                 INQUIRE (FILE = FILETMP,EXIST = FEXIST)
                 IF (FEXIST) THEN
!                   Alt soil name in experiment directory
                    FILES = FILES_b
                    PATHSL = TRIM(PATHEX)

                 ELSE
                    PROCOD = 'SLD'
                    CALL PATH (PROCOD,control%DSSATP,PATHSL,1,NAMEF)
                    PATHL  = INDEX(PATHSL,BLANK)
                    FILETMP = PATHSL(1:(PATHL-1)) // FILES_a
                    INQUIRE (FILE = FILETMP,EXIST = FEXIST)
                    IF (FEXIST) THEN
!                      SOIL.SOL in DSSAT soil directory
                       FILES = FILES_a

                    ELSE
                       FILETMP = PATHSL(1:(PATHL-1)) // FILES_b
                       INQUIRE (FILE = FILETMP,EXIST = FEXIST)
                       IF (FEXIST) THEN
!                         Alt soil name in DSSAT soil directory
                          FILES = FILES_b
                       ELSE
!                         No valid soils file found
                          WRITE(MSG(1),5000) FILES_a, FILES_b 
                          WRITE(MSG(2),5010) 
                          WRITE(MSG(3),5020) PATHEX(1:76) 
                          WRITE(MSG(4),5030) PATHSL(1:76)
                          CALL WARNING(4,ERRKEY,MSG) 
                          CALL ERROR(ERRKEY,80,FILES_a,0)
                       ENDIF
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF
        ENDIF
      end if

      ENDIF   !Skip for sequence

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (ISIMI .EQ. 'S') THEN
         IF (YRSIM .LT. 0) THEN
           YRSIM = YRPLT
         ENDIF
      ELSE IF (ISIMI .EQ. 'P') THEN
           YRSIM = YRPLT
      ELSE IF (ISIMI .EQ. 'E') THEN
           YRSIM = IEMRG
           YRPLT = IEMRG
      ENDIF
      IF (CROP .EQ. 'FA' .AND. YRPLT .EQ. YRSIM) THEN
         YRSIM = YRSIM - 1
      ENDIF
      CALL YR_DOY (YRSIM,YEAR,ISIM)
      CONTROL % YRSIM = YRSIM

C-----------------------------------------------------------------------
C     Now establish the weather file FILEW as WSTA + .WT?  where ? :
C
C          M = observed data
C          G = generated data
C          S = interactively generated
C-----------------------------------------------------------------------
      call csminp%get('*SIMULATION CONTROL','MEWTH',MEWTH)

      if(.not.nc_wth%yes)then

      IF (MEWTH .EQ. 'G') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
            IF (YEAR .LT. 2000) THEN
              YR = YEAR - 1900
            ELSE IF (YEAR .LT. 3000) THEN
              YR = YEAR - 2000
            ENDIF
            WRITE (FILEW(1:12),75) WSTA,YR,'01.WTG'
         ELSE
            WRITE (FILEW(1:12),76) WSTA,WSTA1,'.WTG'
         ENDIF
         PROCOD = 'WGD'
      ELSEIF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
         WRITE (FILEW(1:12),77) WSTA,'.CLI    '
         PROCOD = 'CLD'
      ELSEIF (MEWTH .EQ. 'M') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
           IF (YEAR .LT. 2000) THEN
             YR = YEAR - 1900
           ELSE IF (YEAR .LT. 3000) THEN
             YR = YEAR - 2000
           ENDIF
           WRITE (FILEW(1:12),75) WSTA,YR,'01.WTH'
         ELSE
            WRITE(FILEW(1:12),76) WSTA,WSTA1,'.WTH'
         ENDIF
         PROCOD = 'WED'
      ELSE
         CALL ERROR (ERRKEY,22,FILEX,LINEXP)
      ENDIF

      end if

!--------------------------------------------------
!       Write treatment data to csm_input structure
!--------------------------------------------------

      call csminp%add_sec('*TREATMENTS',ntiers=1)

      call csminp%add_var('*TREATMENTS',tier=1,
     &     char_name=(/'TITLET','TITLER'/),
     &     int_name=(/'TRTNO ','ROTNO ','ROTOPT','CRPNO ','LNCU  ',
     &                'LNFLD ','LNSA  ','LNIC  ','LNPLT ','LNIR  ',
     &                'LNFER ','LNRES ','LNCHE ','LNTIL ','LNENV ',
     &                'LNHAR ','LNSIM '/))

      call csminp%put('*TREATMENTS','TRTNO',TRTNO)
      call csminp%put('*TREATMENTS','ROTNO',ROTNO)
      call csminp%put('*TREATMENTS','ROTOPT',ROTOPT)
      call csminp%put('*TREATMENTS','TITLET',TITLET)
      call csminp%put('*TREATMENTS','CRPNO',CRPNO)
      call csminp%put('*TREATMENTS','LNCU',LNCU)
      call csminp%put('*TREATMENTS','LNFLD',LNFLD)
      call csminp%put('*TREATMENTS','LNSA',LNSA)
      call csminp%put('*TREATMENTS','LNIC',LNIC)
      call csminp%put('*TREATMENTS','LNPLT',LNPLT)
      call csminp%put('*TREATMENTS','LNIR',LNIR)
      call csminp%put('*TREATMENTS','LNFER',LNFER)
      call csminp%put('*TREATMENTS','LNRES',LNRES)
      call csminp%put('*TREATMENTS','LNCHE',LNCHE)
      call csminp%put('*TREATMENTS','LNTIL',LNTIL)
      call csminp%put('*TREATMENTS','LNENV',LNENV)
      call csminp%put('*TREATMENTS','LNHAR',LNHAR)
      call csminp%put('*TREATMENTS','LNSIM',LNSIM)

!--------------------------------------------------
!       Write field data to csm_input structure
!--------------------------------------------------

      call csminp%add_sec('*FIELDS',ntiers=1)

      call csminp%add_var('*FIELDS',tier=1,
     &     char_name=(/'FLDNAM ','FILEW  ','DFDRN  ','FLST   ',
     &                 'SLTX   ', 'FldHist','SLNO   ','WSTA   '/),
     &     int_name=(/'LNFLD','FHDur'/),
     &     real_name=(/'SLOPE  ','FLOB   ','FLDD   ','SFDRN  ',
     &                 'XCRD   ','YCRD   ','ELEV   ','SLDP   ',
     &                 'AREA   ','SLEN   ','FLWR   ','SLAS   '/))

      call csminp%put('*FIELDS','WSTA',WSTA)
      call csminp%put('*FIELDS','LNFLD',LNFLD)
      call csminp%put('*FIELDS','FLDNAM',FLDNAM)
      call csminp%put('*FIELDS','FILEW',FILEW)
      call csminp%put('*FIELDS','SLOPE',SLOPE)
      call csminp%put('*FIELDS','FLOB',FLOB)
      call csminp%put('*FIELDS','DFDRN',DFDRN)
      call csminp%put('*FIELDS','FLDD',FLDD)
      call csminp%put('*FIELDS','SFDRN',SFDRN)
      call csminp%put('*FIELDS','FLST',FLST)
      call csminp%put('*FIELDS','SLTX',SLTX)
      call csminp%put('*FIELDS','SLDP',SLDP)
      call csminp%put('*FIELDS','SLNO',SLNO)
      call csminp%put('*FIELDS','XCRD',XCRD)
      call csminp%put('*FIELDS','YCRD',YCRD)
      call csminp%put('*FIELDS','ELEV',ELEV)
      call csminp%put('*FIELDS','AREA',AREA)
      call csminp%put('*FIELDS','SLEN',SLEN)
      call csminp%put('*FIELDS','FLWR',FLWR)
      call csminp%put('*FIELDS','SLAS',SLAS)
      call csminp%put('*FIELDS','FldHist',FldHist)
      call csminp%put('*FIELDS','FHDur',FHDur)

      call csminp%add_sec('*PLANTING DETAILS',ntiers=1)

      call csminp%add_var('*PLANTING DETAILS',tier=1,
     &     char_name=(/'PLME','PLDS'/),
     &     int_name=(/'LNPLT ','YRPLT ','IEMRG ','PMTYPE','NFORC ',
     &                'NDOF  '/),
     &     real_name=(/'PLTFOR','PLANTS','PLTPOP','ROWSPC','AZIR  ',
     &                 'SDEPTH','SDWTPL','SDAGE ','ATEMP ',
     &                 'PLPH  ','SPRLAP'/))

      call csminp%put('*PLANTING DETAILS','PLME',PLME)
      call csminp%put('*PLANTING DETAILS','PLDS',PLDS)
      call csminp%put('*PLANTING DETAILS','LNPLT',LNPLT)
      call csminp%put('*PLANTING DETAILS','YRPLT',YRPLT)
      call csminp%put('*PLANTING DETAILS','IEMRG',IEMRG)
      call csminp%put('*PLANTING DETAILS','PLTFOR',PLTFOR)
      call csminp%put('*PLANTING DETAILS','PMTYPE',PMTYPE)
      call csminp%put('*PLANTING DETAILS','PLANTS',PLANTS)
      call csminp%put('*PLANTING DETAILS','PLTPOP',PLTPOP)
      call csminp%put('*PLANTING DETAILS','ROWSPC',ROWSPC)
      call csminp%put('*PLANTING DETAILS','AZIR',AZIR)
      call csminp%put('*PLANTING DETAILS','SDEPTH',SDEPTH)
      call csminp%put('*PLANTING DETAILS','SDWTPL',SDWTPL)
      call csminp%put('*PLANTING DETAILS','SDAGE',SDAGE)
      call csminp%put('*PLANTING DETAILS','ATEMP',ATEMP)
      call csminp%put('*PLANTING DETAILS','PLPH',PLPH)
      call csminp%put('*PLANTING DETAILS','SPRLAP',SPRLAP)
      call csminp%put('*PLANTING DETAILS','NFORC',NFORC)
      call csminp%put('*PLANTING DETAILS','NDOF',NDOF)

      if(.not.nc_wth%yes)then
!     Check weather filename in current directory
      INQUIRE (FILE = FILEW,EXIST = FEXIST)
      IF (FEXIST) THEN
        PATHWT = BLANK
!     Check weather filename in data directory
      ELSE
        FILETMP = TRIM(PATHEX)//FILEW
        INQUIRE (FILE = FILETMP,EXIST = FEXIST)
        IF (FEXIST) THEN
          PATHWT = TRIM(PATHEX)
!       Check weather filename in default DSSAT directory
        ELSE
          CALL PATH(PROCOD,control%DSSATP,PATHWT,1,NAMEF)
          FILETMP = TRIM(PATHWT) // FILEW
          INQUIRE (FILE=FILETMP, EXIST = FEXIST)
          IF (FEXIST) THEN
            PATHWT = PATHWT
!         Check 4-character file name in data directory
          ELSE
            FILEW4 = FILEW(1:4) // ".WTH"
            FILETMP = TRIM(PATHEX) // FILEW4
            INQUIRE (FILE=FILETMP, EXIST = FEXIST)
            IF (FEXIST) THEN
              PATHWT = TRIM(PATHEX)
              FILEW = FILEW4
!           Check 4-character filename in default DSSAT directory
            ELSE
              FILETMP = TRIM(PATHWT) // FILEW
              INQUIRE (FILE=FILETMP, EXIST = FEXIST)
              IF (FEXIST) THEN
                PATHWT = PATHWT
                FILEW = FILEW4
              ELSE
                MSG(1) = "Weather file not found."
                MSG(2) = "  Neither " // FILEW // " nor " // FILEW4
                MSG(3) = 
     &            "  were found in weather or experiment directories."
                MSG(4) = "Simulation will end."
                CONTROL % ErrCode = 29
                CALL PUT(CONTROL)
                CALL WARNING(4,ERRKEY,MSG)
!               CALL ERROR(ERRKEY,29,FILEW,0)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      end if

C-----------------------------------------------------------------------
C     Build output files.
C
C     Generic output file names with extension 'OUT' are overwritten
C     at the start of each simulation.
C
C     IOX = 'Y' creates experiment specific output file names
C-----------------------------------------------------------------------
      IF (IOX .EQ. 'Y') THEN
         WRITE (OUTO(1:12),80) EXPER,'.',CG,'O'
       ELSE
         OUTO  = 'OVERVIEW.OUT'
      ENDIF

C-----------------------------------------------------------------------
C     Call IPENV
C-----------------------------------------------------------------------
      if(nc_filex%yes)then
         CALL read_nc_env_sec(LNENV,CO2ADJ,CO2FAC,DAYADJ,
     &        DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,RADADJ,RADFAC,
     &        TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WMODI,WNDADJ,WNDFAC,
     &        WTHADJ)
      else
         CALL IPENV (FILEX,LNENV,LUNEXP,CO2ADJ,CO2FAC,DAYADJ,
     &        DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,RADADJ,RADFAC,
     &        TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WMODI,WNDADJ,WNDFAC,
     &        WTHADJ)
      end if

      if(nev>0)then

         call csminp%add_sec('*ENVIRONMENT')

         call csminp%add_var('*ENVIRONMENT',
     &     char_name=(/'DAYFAC','RADFAC','TXFAC ','TMFAC ',
     &                 'PRCFAC','CO2FAC','DPTFAC','WNDFAC'/),
     &     int_name=(/'LNENV ','WMDATE'/),
     &     real_name=(/'DAYADJ','RADADJ','TXADJ ','TMADJ ',
     &                 'PRCADJ','CO2ADJ','DPTADJ','WNDADJ'/),
     &     nrow=nev)

         do i=1,nev
            call csminp%put('*ENVIRONMENT','LNENV',LNENV,ind=i)
         end do
         call csminp%put('*ENVIRONMENT','DAYFAC',DAYFAC(1:nev))
         call csminp%put('*ENVIRONMENT','RADFAC',RADFAC(1:nev))
         call csminp%put('*ENVIRONMENT','TXFAC',TXFAC(1:nev))
         call csminp%put('*ENVIRONMENT','TMFAC',TMFAC(1:nev))
         call csminp%put('*ENVIRONMENT','PRCFAC',PRCFAC(1:nev))
         call csminp%put('*ENVIRONMENT','CO2FAC',CO2FAC(1:nev))
         call csminp%put('*ENVIRONMENT','DPTFAC',DPTFAC(1:nev))
         call csminp%put('*ENVIRONMENT','WNDFAC',WNDFAC(1:nev))
         call csminp%put('*ENVIRONMENT','WMDATE',WMDATE(1:nev))
         call csminp%put('*ENVIRONMENT','DAYADJ',DAYADJ(1:nev))
         call csminp%put('*ENVIRONMENT','RADADJ',RADADJ(1:nev))
         call csminp%put('*ENVIRONMENT','TXADJ',TXADJ(1:nev))
         call csminp%put('*ENVIRONMENT','TMADJ',TMADJ(1:nev))
         call csminp%put('*ENVIRONMENT','PRCADJ',PRCADJ(1:nev))
         call csminp%put('*ENVIRONMENT','CO2ADJ',CO2ADJ(1:nev))
         call csminp%put('*ENVIRONMENT','DPTADJ',DPTADJ(1:nev))
         call csminp%put('*ENVIRONMENT','WNDADJ',WNDADJ(1:nev))
      end if

C-----------------------------------------------------------------------
C     Call IPHAR
C-----------------------------------------------------------------------
      call csminp%get('*SIMULATION CONTROL','IHARI',IHARI)

      if(nc_filex%yes)then
         CALL read_nc_har_sec(LNHAR,HDATE,HSTG,HCOM,HSIZ,HPC,
     &        NHAR,IHARI,YRSIM,CROP,HBPC,FREQ,CUHT) !NEW FORAGE VARIABLES (DIEGO-
      else
         CALL IPHAR (LUNEXP,FILEX,LNHAR,HDATE,HSTG,HCOM,HSIZ,HPC,
     &        NHAR,IHARI,YRSIM,CROP,HBPC,FREQ,CUHT) !NEW FORAGE VARIABLES (DIEGO-2/14/2017)
      end if
      if(nhar>0)then
         call csminp%add_sec('*HARVEST')

         call csminp%add_var('*HARVEST',
     &     char_name=(/'HCOM','HSIZ'/),
     &     int_name=(/' HSTG','LNHAR','HDATE'/),
     &     real_name=(/'HPC ','HBPC'/),
     &     nrow=nhar)

         do i=1,nhar
            call csminp%put('*HARVEST','LNHAR',LNHAR,ind=i)
         end do
         do i=1,3
            read(HSTG(i)(4:5),'(I2)') HSTG_int(i)
         end do
         call csminp%put('*HARVEST','HSTG',HSTG_int)
         call csminp%put('*HARVEST','HCOM',HCOM)
         call csminp%put('*HARVEST','HSIZ',HSIZ)
         call csminp%put('*HARVEST','HDATE',HDATE)
         call csminp%put('*HARVEST','HPC',HPC)
         call csminp%put('*HARVEST','HBPC',HBPC)
      end if

C-----------------------------------------------------------------------
C     Call IPIRR
C-----------------------------------------------------------------------
      if(nc_filex%yes)then
         CALL read_nc_irr_sec(LNIR,YRSIM,ISWWAT,
     &        NIRR,EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,IAMEX,LNSIM,
     &        NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT,IIRV,IIRRI)
      else
         CALL IPIRR (LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,
     &        NIRR,EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,IAMEX,LNSIM,
     &        NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT,IIRV,IIRRI)
      end if
      if(nirr>0)then

         call csminp%add_sec('*IRRIGATION',ntiers=2)

         call csminp%add_var('*IRRIGATION',tier=1,
     &     char_name=(/'IOFFX','IAMEX'/),
     &     int_name=(/'LNIR'/),
     &     real_name=(/'EFFIRX','DSOILX','THETCX','IEPTX ','AIRAMX',
     &        'TOTAPW'/))

         call csminp%put('*IRRIGATION','IOFFX',IOFFX)
         call csminp%put('*IRRIGATION','IAMEX',IAMEX)
         call csminp%put('*IRRIGATION','LNIR',LNIR)
         call csminp%put('*IRRIGATION','TOTAPW',TOTAPW)
         call csminp%put('*IRRIGATION','EFFIRX',EFFIRX)
         call csminp%put('*IRRIGATION','DSOILX',DSOILX)
         call csminp%put('*IRRIGATION','THETCX',THETCX)
         call csminp%put('*IRRIGATION','IEPTX',IEPTX)
         call csminp%put('*IRRIGATION','AIRAMX',AIRAMX)

         call csminp%add_var('*IRRIGATION',tier=2,
     &        int_name=(/'LNIR  ','IRRCOD','IDLAPL'/),
     &        real_name=(/'AMT'/),
     &        nrow = nirr)
         do i=1,nirr
            call csminp%put('*IRRIGATION','LNIR',LNIR,ind=i,tier=2)
            read(irrcod(i)(3:5),'(I3)') tmp_int
            call csminp%put('*IRRIGATION','IRRCOD',tmp_int,ind=i,
     &        tier=2)
         end do
         call csminp%put('*IRRIGATION','IDLAPL',IDLAPL,
     &        tier=2)
         call csminp%put('*IRRIGATION','AMT',AMT,tier=2)
      end if
C-----------------------------------------------------------------------
C     Call IPFERT
C-----------------------------------------------------------------------
      if(nc_filex%yes)then
         CALL read_nc_fert_sec(LNFER,YRSIM,ISWNIT,ISWPHO,ISWPOT,
     &        NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER,ACFER,
     &        AOFER,FOCOD,TOTNAP,IFERI,ISWWAT,LNSIM)
      else
         CALL IPFERT (LUNEXP,FILEX,LNFER,YRSIM,ISWNIT,ISWPHO,ISWPOT,
     &        NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER,ACFER,
     &        AOFER,FOCOD,TOTNAP,IFERI,ISWWAT,LNSIM)
      end if

      if(nfert>0)then
         call csminp%add_sec('*FERTILIZERS',ntiers=2)

         call csminp%add_var('*FERTILIZERS',tier=1,
     &     int_name=(/'LNFER','NFERT'/))

         call csminp%put('*FERTILIZERS','LNFER',LNFER)
         call csminp%put('*FERTILIZERS','NFERT',NFERT)

         call csminp%add_var('*FERTILIZERS',tier=2,
     &     char_name=(/'IFTYPE','FERCOD','FOCOD '/),
     &     int_name=(/'FDAY'/),
     &     real_name=(/'DFERT ','ANFER ','APFER ','AKFER ','ACFER ',
     &     'AOFER ','TOTNAP'/))

         call csminp%put('*FERTILIZERS','TOTNAP',TOTNAP)
         call csminp%put('*FERTILIZERS','IFTYPE',IFTYPE)
         call csminp%put('*FERTILIZERS','FERCOD',FERCOD)
         call csminp%put('*FERTILIZERS','FOCOD',FOCOD)
         call csminp%put('*FERTILIZERS','FDAY',FDAY)
         call csminp%put('*FERTILIZERS','DFERT',DFERT)
         call csminp%put('*FERTILIZERS','ANFER',ANFER)
         call csminp%put('*FERTILIZERS','APFER',APFER)
         call csminp%put('*FERTILIZERS','AKFER',AKFER)
         call csminp%put('*FERTILIZERS','ACFER',ACFER)
         call csminp%put('*FERTILIZERS','AOFER',AOFER)

      end if
C-----------------------------------------------------------------------
C     Call IPRES
C-----------------------------------------------------------------------
      if(nc_filex%yes)then
         CALL read_nc_res_sec(LNRES,RESDAY,RESCOD,RESIDUE,
     &        RINP,DEPRES,RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,
     &        ISWPHO,ISWPOT,IRESI,ISWWAT,RMET,LNSIM)
      else
         CALL IPRES (LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,
     &        RINP,DEPRES,RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,
     &        ISWPHO,ISWPOT,IRESI,ISWWAT,RMET,LNSIM)
      end if

      if(nares>0)then
         call csminp%add_sec('*RESIDUES',ntiers=1)

         call csminp%add_var('*RESIDUES',
     &        char_name=(/'RESCOD','RMET  '/),
     &        int_name=(/'LNRES ','RESDAY'/),
     &        real_name=(/'RESIDUE','RESN   ','RESP   ',
     &        'RESK   ','RINP   ','DEPRES ','RESAMT '/),
     &        nrow = nares)

         do i=1,nares
            call csminp%put('*RESIDUES','LNRES',LNRES,ind=i)
         end do
         call csminp%put('*RESIDUES','RESCOD',RESCOD)
         call csminp%put('*RESIDUES','RMET',RMET)
         call csminp%put('*RESIDUES','RESDAY',RESDAY)
         call csminp%put('*RESIDUES','RESIDUE',RESIDUE)
         call csminp%put('*RESIDUES','RESAMT',RESAMT)
         call csminp%put('*RESIDUES','RESN',RESN)
         call csminp%put('*RESIDUES','RESP',RESP)
         call csminp%put('*RESIDUES','RESK',RESK)
         call csminp%put('*RESIDUES','RINP',RINP)
         call csminp%put('*RESIDUES','DEPRES',DEPRES)
      end if
C-----------------------------------------------------------------------
C     Call IPCHEM - Chemical applications
C-----------------------------------------------------------------------
      if(nc_filex%yes)then
         CALL read_nc_chem_sec(LNCHE,YRSIM,ISWWAT,NCHEM,CDATE,
     &        CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,LNSIM,CHEXTR)
      else
         CALL IPCHEM (LUNEXP,FILEX,LNCHE,YRSIM,ISWWAT,NCHEM,CDATE,
     &        CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,LNSIM,CHEXTR)
      end if

      if(nchem>0)then
         call csminp%add_sec('*CHEMICALS',ntiers=1)

         call csminp%add_var('*CHEMICALS',
     &     char_name=(/'CHCOD ','CHMET ','CHT   ','CHEXTR'/),
     &     int_name=(/'LNCHE','CDATE'/),
     &     real_name=(/'CHAMT','CHDEP'/),
     &     nrow = nchem)

         do i=1,nchem
            call csminp%put('*CHEMICALS','LNCHE',LNCHE,ind=i)
         end do
         call csminp%put('*CHEMICALS','CHCOD',CHCOD)
         call csminp%put('*CHEMICALS','CHMET',CHMET)
         call csminp%put('*CHEMICALS','CHT',CHT)
         call csminp%put('*CHEMICALS','CHEXTR',CHEXTR)
         call csminp%put('*CHEMICALS','CDATE',CDATE)
         call csminp%put('*CHEMICALS','CHAMT',CHAMT)
         call csminp%put('*CHEMICALS','CHDEP',CHDEP)
      end if

C-----------------------------------------------------------------------
C     Call IPTILL - Tillage operations
C-----------------------------------------------------------------------
      if(nc_filex%yes)then
         CALL read_nc_till_sec(LNTIL,YRSIM,ISWTIL,NTIL,TDATE,
     &        TIMPL,TDEP,LNSIM)
      else
         CALL IPTILL (LUNEXP,FILEX,LNTIL,YRSIM,ISWTIL,NTIL,TDATE,
     &        TIMPL,TDEP,LNSIM)
      end if

      if(ntil>0)then
         call csminp%add_sec('*TILLAGE',ntiers=1)

         call csminp%add_var('*TILLAGE',
     &     char_name=(/'TIMPL'/),
     &     int_name=(/'LNTIL','TDATE'/),
     &     real_name=(/'TDEP'/),
     &     nrow = ntil)

         do i=1,ntil
            call csminp%put('*TILLAGE','LNTIL',LNTIL,ind=i)
         end do
         call csminp%put('*TILLAGE','TIMPL',TIMPL)
         call csminp%put('*TILLAGE','TDATE',TDATE)
         call csminp%put('*TILLAGE','TDEP',TDEP)
      end if

!--------------------------------------------------
!       Write file path data to csm_input structure
!--------------------------------------------------

      call csminp%add_sec('*FILES',ntiers=1)

      call csminp%add_var('*FILES',tier=1,
     &     char_name=(/'FILEX ','PATHEX','FILEA ','FILET ',
     &                 'FILEC ','PATHCR',
     &                 'FILEE ','PATHEC','FILEG ','PATHGE',
     &                 'FILEP ','PATHPE',
     &                 'FILES ','PATHSL','FILEW ','PATHWT',
     &                 'OUTO  '/),
     &     int_name=(/'LNSIM','ISENS'/))

      call csminp%put('*FILES','FILEX',FILEX)
      call csminp%put('*FILES','PATHEX',PATHEX)
      call csminp%put('*FILES','FILEA',FILEA)
      call csminp%put('*FILES','FILET',FILET)
      call csminp%put('*FILES','FILEC',FILEC)
      call csminp%put('*FILES','PATHCR',PATHCR)
      call csminp%put('*FILES','FILEE',FILEE)
      call csminp%put('*FILES','PATHEC',PATHEC)
      call csminp%put('*FILES','FILEG',FILEG)
      call csminp%put('*FILES','PATHGE',PATHGE)
      call csminp%put('*FILES','FILEP',FILEP)
      call csminp%put('*FILES','PATHPE',PATHPE)
      call csminp%put('*FILES','FILES',FILES)
      call csminp%put('*FILES','PATHSL',PATHSL)
      call csminp%put('*FILES','FILEW',FILEW)
      call csminp%put('*FILES','PATHWT',PATHWT)
      call csminp%put('*FILES','LNSIM',LNSIM)
      call csminp%put('*FILES','OUTO',OUTO)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

   55 FORMAT (I3,I1,2(1X,I1),1X,A25,14I3)
   56 FORMAT (2I2,2(1X,I1),1X,A25,14I3)

   75 FORMAT (A4,I2.2,A6)
   76 FORMAT (3A4)
   77 FORMAT (A4,A8)
   80 FORMAT (A8,A1,A2,A1)
  200 FORMAT (T57,'INST.',T64,'SITE',T70,'YEAR',T75,'EXPT.',
     &  /,T7,'CROP EXPERIMENTAL CASE STUDIES',T58,'ID',T65,'ID',
     &  T76,'NO',/T7,4('-'),1X,31('-'),T57,'----',
     &    T64,'----',T70,'----',T75,'----')
  410 FORMAT (3X,A8,1X,A2,2X,A60)
  500 FORMAT (1X,I3,'.',2X,A2,2X,A45,1X,A2,5X,A2,3X,'19',A2,2X,A2)
  501 FORMAT (1X,I3,'.',2X,A2,2X,A45,1X,A2,5X,A2,3X,'20',A2,2X,A2)
  600 FORMAT (/,'  More.... press < ENTER > key',$)
 1000 FORMAT (/,6X,'EXPERIMENT SELECTED ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?     --->',2X,' ',$)
 1100 FORMAT (80A1)
 1101 FORMAT (10X,'ERROR! Experiment Selection must be between 1',
     &            ' and ',I3,/)
 1102 FORMAT (10X,'ERROR! Experiment Selection must be an',
     &            ' INTEGER value',/)
 2300 FORMAT (T47,'INST.',T54,'SITE',T60,'YEAR',T66,'EXPT.',
     &  T72,'TRT.',/,T7,A40,T48,'ID',T55,'ID',T67,'NO',
     &  T73,'NO', /,T7,37('-'),T47,'----',
     &  T54,'----',T60,'----',T66,'----',T72,'----')
 2600 FORMAT (1X,I3,'.',1X,A25,16X,A2,5X,A2,3X,'19',A2,3X,A2,3X,I3)
 2601 FORMAT (1X,I3,'.',1X,A25,16X,A2,5X,A2,3X,'20',A2,3X,A2,3X,I3)
 2650 FORMAT (1X,I3,'.',1X,'RUN ALL TREATMENTS',23X,
     &        A2,5X,A2,3X,'19',A2,3X,A2,4X,I2)
 2751 FORMAT (10X,'ERROR! Treatment Selection must be between 1',
     &            ' and ',I3,/)
 2752 FORMAT (10X,'ERROR! Treatment Selection must be an INTEGER',/)
 2900 FORMAT (/,6X,'TREATMENT SELECTED ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?    --->',2X,' ',$)
 3450 FORMAT (//////,15X,' Reading Data.  Please be patient.',/,
     &               15X,' Do not touch the keyboard !',/,16X,33('='))
 5000 FORMAT("Soil files not found: ",A,", ",A)
 5010 FORMAT("Searched current directory and the following:")
 5020 FORMAT(2X,A76)
 5030 FORMAT(2X,A76)

      END SUBROUTINE IPEXP

C=======================================================================
C  IPPLNT, Subroutine
C
C  Reads parameters related to planting operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWJ Written
C  05/28/1993 PWW Header revision and minor changes            
C  02/21/2006 GH  Update 
C  04/26/2013 GH  Update planting method for cassava
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNPLT
C
C  LOCAL  : LN
C
C  OUTPUT : IPLT,PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,PLTPOP,PLANTS,
C           SDAGE,ATEMP,PLPH,IEMRG
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPPLNT_Inp (
     &     LUNEXP,FILEX,LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,MODEL,
     &     SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &     YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)

      use dssat_netcdf
      IMPLICIT NONE

      CHARACTER*1   PLME,PLDS,IPLTI
      CHARACTER*2   CROP
      CHARACTER*6   ERRKEY,FINDCH
      CHARACTER*8   MODEL
      CHARACTER*12  FILEX
      CHARACTER*78  MSG(2)
      CHARACTER*110 CHARTEST

      INTEGER   LUNEXP,LNPLT,IEMRG,LN,LINEXP,ISECT,IFIND,ERRNUM
      INTEGER   IPLT,YRPLT,YR,NFORC,NDOF,PMTYPE

      REAL      ROWSPC,AZIR,SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP
      REAL      PLPH,SPRLAP,PLTFOR

      PARAMETER (ERRKEY='IPPLNT')
                 FINDCH='*PLANT'
      LINEXP = 0
      IF (LNPLT .GT. 0) THEN
         REWIND(LUNEXP)
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
C
C           Actual read statement for Planting inputs
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,YRPLT,IEMRG,PLANTS,
     &      PLTPOP,PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,
     &      PLPH,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE
C New variables for pineapple
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
            CALL Y2K_DOY(YRPLT)
            CALL Y2K_DOY(IEMRG)
            CALL YR_DOY (YRPLT,YR,IPLT)
          ELSE
            CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         IF (LN .NE. LNPLT) GO TO 50

         IF (IPLTI .EQ. 'R') THEN
           IF ((YRPLT .LT. 1 .OR. YRPLT .GT. 9999999)
     &       .AND. IEMRG .LT. 1) THEN
              CALL ERROR (ERRKEY,10,FILEX,LINEXP)
           ENDIF
         ENDIF
         IF (PLTPOP .LE. 0.0 .AND. PLANTS .GT. 0.0) THEN
            PLTPOP = PLANTS
         ENDIF
         IF (PLTPOP .LE. 0.0 .OR. PLTPOP .GT. 999.) THEN
            IF (CROP /= 'SC') CALL ERROR (ERRKEY,11,FILEX,LINEXP)
         ENDIF

!        chp 5/17/2011
!        Need to handle missing row spacing data
!         IF ((ROWSPC .GT. -90. .AND. ROWSPC .LE. 0.0)
!     &      .OR. ROWSPC .GT. 99999.) THEN
!            CALL ERROR (ERRKEY,12,FILEX,LINEXP)
!         ENDIF
         IF (ROWSPC <= 0.0) THEN
            SELECT CASE (MODEL(1:5))
            CASE ('SCCAN'); ROWSPC = 0.0  !Canegro does not need this
            CASE DEFAULT
              ROWSPC = 1.0 / SQRT(PLTPOP) * 100.
              MSG(1) = "Missing row spacing in experiment file."
              WRITE(MSG(2),'(A,F8.1,A)') "Value set to ", ROWSPC," cm"
              CALL WARNING(2,ERRKEY,MSG)
            END SELECT
         ENDIF

         IF ((AZIR .GT. -90. .AND. AZIR .LT. 0.0)
     &      .OR. AZIR .GT. 99999.) THEN
            CALL ERROR (ERRKEY,13,FILEX,LINEXP)
         ENDIF
         IF (SDEPTH .LE. 0.0 .OR. SDEPTH .GT. 100.0) THEN
            IF (CROP /= 'SC') CALL ERROR (ERRKEY,14,FILEX,LINEXP)
         ENDIF
         IF ((INDEX('PT',CROP)) .GT. 0) THEN
           IF (SPRLAP .LE. 0.0) THEN
              CALL ERROR (ERRKEY,16,FILEX,LINEXP)
           ENDIF
           IF (SDWTPL .LE. 0.0) THEN
              CALL ERROR (ERRKEY,17,FILEX,LINEXP)
           ENDIF
         ENDIF

         IF (INDEX('TSPNRCBHIV',PLME) .LE. 0) 
     &                    CALL ERROR (ERRKEY,19,FILEX,LINEXP)
C-GH
         IF (INDEX('CS',CROP) .GT. 0) THEN
           IF (INDEX('VHI',PLME) .LE. 0)
     &         CALL ERROR (ERRKEY,19,FILEX,LINEXP)
         ENDIF
         IF (INDEX('RI',CROP) .LE. 0) THEN    !CHP ADDED
           IF ((INDEX('T',PLME)) .GT. 0) THEN
             IF (SDWTPL .LT. 0.0) THEN
               CALL ERROR (ERRKEY,18,FILEX,LINEXP)
             ENDIF
           ENDIF
         ENDIF
      ENDIF

      REWIND (LUNEXP)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,I5,2(1X,F5.0),2(5X,A1),8(1X,F5.0),I6,F6.0,2I6)

      END SUBROUTINE IPPLNT_Inp

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

      SUBROUTINE IPFLD (LUNEXP,FILEX,LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
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
                 FINDCH='*FIELD'
      LINEXP = 0
      REWIND (LUNEXP)
      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50   CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,FLDNAM,WSTA,WSTA1,SLOPE,
     &                     FLOB,DFDRN,FLDD,SFDRN,FLST,SLTX,SLDP,SLNO
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
       ELSE
         CALL ERROR (ERRKEY,2,FILEX,LINEXP)
      ENDIF
      IF (LN .NE. LNFLD) GO TO 50
      DO I = 1, 4
        WSTA(I:I)  = UPCASE(WSTA(I:I))
        WSTA1(I:I) = UPCASE(WSTA1(I:I))
      END DO
      IF (WSTA(1:3) .EQ. '-99' .AND. SLNO(1:3) .EQ. '-99') THEN
        CLOSE(LUNEXP)
        STOP
      ENDIF

      IF (WSTA .EQ. '    ') THEN
         CALL ERROR (ERRKEY,10,FILEX,LINEXP)
      ENDIF
      IF (SLNO .EQ. '          ') THEN
         CALL ERROR(ERRKEY,11,FILEX,LINEXP)
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
      HFNDCH='SLAS'
      CALL HFIND(LUNEXP,HFNDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 1) THEN
 70     CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
        IF (ISECT .EQ. 1) THEN
           READ (CHARTEST,80,IOSTAT=ERRNUM) LN,
     &                XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS, FldHist, FHDUR
           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         ELSE
           CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         IF (LN .NE. LNFLD) GO TO 70
      ENDIF

      IF (AREA .LE. 0.0) AREA = 1.0
      IF (FLWR .LE. 0.0) FLWR = 1.0
      IF (SLEN .LE. 0.0) SLEN = SQRT(AREA*FLWR*10000.0)

C
C    End New section

      REWIND(LUNEXP)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,A8,1X,2A4,1X,F5.0,1X,F5.0,1X,A5,2(1X,F5.0),
     &         2(1X,A5),1X,F5.0,1X,A10)
!     chp 7/26/2006
! 80   FORMAT (I3,2(F15.0,1X),F9.0,1X,F17.0,3(1X,F5.0))
 80   FORMAT (I3,2(F15.0,1X),F9.0,1X,F17.0,3(1X,F5.0),1X,A5,I6)

      END SUBROUTINE IPFLD

