C=======================================================================
C  IPSOIL_Inp, Subroutine
C
C  Soil selection
C-----------------------------------------------------------------------
C  Revision history
C
C  06/21/1991 JWJ 
C  05/28/1993 PWW Header revision and minor changes
C  12/01/1993 WTB Modified for soil P
C  12/12/2000 GH  Modified format
C  07/01/2003 GH  Add error checking codes for surface values
!  04/07/2005 CHP Added EXCA, exchangable calcium (cmol/kg) 
!  06/13/2005 CHP Free format read for soils parameters.  Format is 
!                 defined by location of headers.
!  08/23/2005 CHP Fixed some error checking for soil water parameters.
!  05/18/2006 CHP Modifications for input subroutine module
!  09/01/2011 CHP Added Van Genuchten parameters in optional 3rd tier of data
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,FILES,PATHSL,ISWWAT
C
C  LOCAL  :
C
C  OUTPUT : NSENS
C-----------------------------------------------------------------------
C  Called : SESOIL INPUT
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE read_nc_soil(RNMODE,NSENS,ISWITCH)

      USE ModuleDefs
      use csm_io
      use dssat_netcdf
      IMPLICIT NONE

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS,UPCASE
      CHARACTER*5   MH(NL)
      CHARACTER*6   ERRKEY
      CHARACTER*12  FILES
      CHARACTER*78  MSG(25)
      CHARACTER*80  PATHSL
      CHARACTER*92  FILESS
      CHARACTER*255 C255
      CHARACTER*5 SLTXS,SMHB,SMPX,SMKE,SGRP,SCOM
      CHARACTER*10 PEDON,SLNO
      CHARACTER*11 SCOUNT
      CHARACTER*12 SLSOUR
      CHARACTER*16 SSITE
      CHARACTER*50 SLDESC,TAXON

      INTEGER I,J,P1,NLAYRI,LINSOL,ISECT
      INTEGER NSENS,NLSOIL,NLOOP,ERR,LUNSL,PATHL, LINSOL_1
      INTEGER NLAYR

!     05/27/2004 CHP Added these variables to COMSOI.blk
!      REAL    PTERMA(NL),PTERMB(NL),EXK(NL),EXMG(NL),EXNA(NL),EXTS(NL)
!      REAL    SLEC(NL),ZLYR(NL),ZZLYR(NL)
      REAL    ZLYR(NL)
      REAL    FLAG,SL,SLDP
      REAL U,SWCON,CN2,SALB,DEPMAX,LL(NL),DUL(NL),SAT(NL),TOTN(NL)
      REAL SHF(NL),SWCN(NL),BD(NL),OC(NL),PH(NL),DLAYR(NL)
      REAL EXTP(NL)
      REAL SASC(NL)
      REAL TOTP(NL),ORGP(NL),SLNF,SLPF,DS(NL),CEC(NL),ADCOEF(NL)
      REAL STONES(NL),CLAY(NL),SILT(NL),PHKCL(NL),SLAT,SLONG
      REAL CACO(NL),EXTAL(NL),EXTFE(NL),EXTMN(NL),TOTBAS(NL)
      REAL PTERMA(NL),PTERMB(NL),EXK(NL),EXMG(NL)
      REAL EXNA(NL),EXTS(NL),SLEC(NL), EXCA(NL) !4/07/05 CHP added EXCA
      REAL alphaVG(NL), mVG(NL), nVG(NL), WCR(NL)

!     Up to MAXCOL headers per line, up to 10 characters long each
      INTEGER, PARAMETER :: MAXCOL = 50
      CHARACTER*15  HEADER(MAXCOL)
!     COL keeps beginning and ending column for each header
      INTEGER COL(MAXCOL,2), C1, C2, COUNT, L

      TYPE (SwitchType) ISWITCH

      real round_real

      PARAMETER (ERRKEY = 'NCIPSL')
      PARAMETER (LUNSL  = 12)
      PARAMETER (BLANK = ' ')

      NLSOIL = 0

      call init_nc_soil()

!-----------------------------------------------------------------------
!     Sensitivity Analysis - soil selection
         IF (NSENS .EQ. 1 ) THEN
            NLOOP  = 0
            NLSOIL = 0
            DO I = 1, 80
               LINE(I) = ' '
            END DO
            I  = 1
            IF (INDEX('IE',RNMODE) .GT. 0) THEN
               CALL CLEAR
               WRITE (*, 5130)
            ENDIF
 10         CONTINUE
            CALL IGNORE(LUNSL, LINSOL, ISECT, C255)
            IF ( ISECT .EQ. 0 ) GO TO 111
            IF (C255(1:1) .NE. '*') GO TO 10
            IF (C255(2:5) .EQ. 'SOIL') GO TO 10

            READ (C255,5030,IOSTAT=ERR) PEDON,SLSOUR,SLTXS,
     &             SLDP,SLDESC
            IF (ERR .NE. 0) THEN
               CALL ERROR (ERRKEY,ERR,FILES,LINSOL)
            ENDIF

            DO P1 = 1, 10
              PEDON(P1:P1) = UPCASE(PEDON(P1:P1))
              SLNO(P1:P1)  = UPCASE(SLNO(P1:P1))
            END DO
            IF (INDEX('IE',RNMODE) .GT. 0) WRITE(*, 5140) I,SLDESC,PEDON
            IF (PEDON  .EQ. SLNO) NLSOIL = I
C
C           Write out pause statement every 15 lines
C
            IF ((MOD(I,15) .EQ. 0).AND.(INDEX('IE',RNMODE) .GT. 0)) THEN
               WRITE (*,5000)
               READ  (5,'(A1)') ANS
            END IF
            I  = I + 1

            GOTO 10
 111        CONTINUE

            NLOOP = NLOOP + 1
            IF (NLOOP .GT. 25) THEN
               CALL ERROR (ERRKEY,1,FILES,LINSOL)
            ENDIF
            LINE(1) = ' '

            IF ((INDEX('IE',RNMODE) .GT. 0) .AND. NLSOIL .EQ. 0) THEN
               WRITE (*,950) SLNO, FILESS
            ENDIF
            IF (INDEX('IE',RNMODE) .GT. 0) WRITE(*, 5160) NLSOIL
            READ (5,'(80A1)') LINE
            CALL VERIFY (LINE,SL,FLAG)

            IF (SL .LE. 0) THEN
               SL  = NLSOIL
             ELSEIF ((FLAG .GT. 0) .OR. (SL .GT. (I-1))) THEN
               WRITE (*,5101) (I-1)
               GO TO 111
             ELSEIF (SL .NE. NINT(SL)) THEN
               WRITE (*,5102)
               GO TO 111
             ELSEIF (SL .GT. 0.) THEN
               NLSOIL = NINT(SL)
             ELSE
               CALL ERROR(ERRKEY,3,FILES,LINSOL)
            ENDIF

            REWIND (LUNSL)
         ENDIF     !End of sensitivity selection

!-----------------------------------------------------------------------
!     Set default values for soil parameters
      SLTXS  = '-99  '
      SLSOUR = '-99        '
      SLDESC = '-99                                              '
      TAXON  = '-99                                              '
      SSITE  = '-99        '
      SCOUNT = '-99        '
      SCOM   = '-99  '
      SMHB   = '-99  '
      SMPX   = '-99  '
      SMKE   = '-99  '
      SGRP   = '-99  '
      
      SLAT   = -99.
      SLONG  = -99.
      SLDP   = -99.
      SALB   = -99.
      SLNF   = -99.
      SLPF   = -99.
      CLAY   = -99.
      SILT   = -99.
      STONES = -99.
      OC     = -99.
      PH     = -99.
      BD     = -99.
      LL     = -99.
      DUL    = -99.
      SAT    = -99.
      SWCN   = -99.
      PHKCL  = -99.
      CEC    = -99.

      U      = -99.
      SWCON  = -99.
      CN2    = -99.
      SHF    = -99.
      TOTN   = -99.
      ADCOEF = 0.
      
!     2ND TIER
      EXTP   = -99.
      TOTP   = -99.
      ORGP   = -99.
      CACO   = -99.
      EXTAL  = -99.
      EXTFE  = -99.
      EXTMN  = -99.
      TOTBAS = -99.
      PTERMA = -99.
      PTERMB = -99.
      EXK    = -99.
      EXMG   = -99.
      EXNA   = -99.
      EXTS   = -99.
      SLEC   = -99.
      EXCA   = -99.

      alphaVG= -99.
      mVG = -99.
      nVG = -99.
      WCR = -99.

!     Stable organic C read from soil analysis section only
!     Output with 2nd tier soil data to INP file.
      SASC   = -99.

!        Initialize
      HEADER = '-99'

      call nc_soil%read('SLNO',1,SLNO)
      call nc_soil%read('SLSOUR',1,SLSOUR)
      call nc_soil%read('SLTXS',1,SLTXS)
      call nc_soil%read('SLDP',1,SLDP)
      call nc_soil%read('SLDESC',1,SLDESC)

      call nc_soil%read('TAXON',1,TAXON)

      call nc_soil%read('SALB',1,SALB)
      call nc_soil%read('SLU1',1,U)
      call nc_soil%read('SLDR',1,SWCON)
      call nc_soil%read('SLRO',1,CN2)
      call nc_soil%read('SLNF',1,SLNF)
      call nc_soil%read('SLPF',1,SLPF)
      call nc_soil%read('SMPX',1,SMPX)

      call nc_soil%read('SBDM',1,BD)
      call nc_soil%read('SLOC',1,OC)
      call nc_soil%read('SLHW',1,PH)
      call nc_soil%read('SLTXS',1,SLTXS)
      call nc_soil%read('TOTN',1,TOTN)

      call nc_soil%read('SLB',1,DS)
      ZLYR = DS
      call nc_soil%read('SLLL',1,LL)
      call nc_soil%read('SDUL',1,DUL)
      call nc_soil%read('SSAT',1,SAT)
      call nc_soil%read('SRGF',1,SHF)
      call nc_soil%read('SSKS',1,SWCN)
      call nc_soil%read('SLCL',1,CLAY)
      call nc_soil%read('SLSI',1,SILT)
      call nc_soil%read('SLCF',1,STONES)
      call nc_soil%read('PHKCL',1,PHKCL)
      call nc_soil%read('SCEC',1,CEC)
      call nc_soil%read('SADC',1,ADCOEF)

      call nc_soil%read('EXTP',1,EXTP)
      call nc_soil%read('TOTP',1,TOTP)
      call nc_soil%read('ORGP',1,ORGP)
      call nc_soil%read('CACO',1,CACO)
      call nc_soil%read('EXTAL',1,EXTAL)
      call nc_soil%read('EXTFE',1,EXTFE)
      call nc_soil%read('EXTMN',1,EXTMN)
      call nc_soil%read('TOTBAS',1,TOTBAS)
      call nc_soil%read('PTERMA',1,PTERMA)
      call nc_soil%read('PTERMB',1,PTERMB)
      call nc_soil%read('EXK',1,EXK)
      call nc_soil%read('EXMG',1,EXMG)
      call nc_soil%read('EXNA',1,EXNA)
      call nc_soil%read('EXTS',1,EXTS)
      call nc_soil%read('SLEC',1,SLEC)
      call nc_soil%read('EXCA',1,EXCA)
      call nc_soil%read('SASC',1,SASC)

      call nc_soil%read('alphaVG',1,alphaVG)
      call nc_soil%read('mVG',1,mVG)
      call nc_soil%read('nVG',1,nVG)
      call nc_soil%read('WCR',1,WCR)

      do i=1,size(ds)
          if(ds(i) < 0)then
              nlayri = i - 1
              exit
          end if
      end do

!        Check validity of soil values 
      IF (SWCON .LT. 0.0) CALL ERROR (ERRKEY,10,FILES,LINSOL)
      IF (CN2 .LE. 0.0)   CALL ERROR (ERRKEY,11,FILES,LINSOL)
      IF (SALB .LE. 0.0) THEN
!           SALB = 0.13
!           MSG(1) = "Soil albedo not specified. " //
!     &        "Default value of 0.13 will be used."
!           CALL WARNING(1,ERRKEY,MSG)
         CALL ERROR (ERRKEY,12,FILES,LINSOL)
      ENDIF

      IF (SLNF > 1.0 .OR. SLNF < 0.0) THEN
         SLNF = 1.0
         IF (ISWITCH%ISWNIT .NE. 'N') THEN
            MSG(1) = "Invalid value for SLNF in soil file."
            MSG(2) = "Value changed to 1.0."
            CALL WARNING(2,ERRKEY,MSG)
         ENDIF
      ENDIF

      IF (ISWITCH%ISWWAT .NE. 'N') THEN
         ERR = 0
         DO J = 1, NLAYRI
            IF ((DUL(J) - SAT(J)) .GT. 1.E-4) THEN
               CALL ERROR (ERRKEY,7,FILES,LINSOL_1+J-1)
            ENDIF
            IF ((LL(J) - DUL(J)) .GT. 1.E-4) THEN
               CALL ERROR (ERRKEY,8,FILES,LINSOL_1+J-1)
            ENDIF
            IF (DUL(J) .LT. 1.E-3) THEN
               CALL ERROR (ERRKEY,13,FILES,LINSOL_1+J-1)
            ENDIF
            IF (ABS(SAT(J) - DUL(J)) .LT. 1.E-2) THEN
               SAT(J) = DUL(J) + 0.01
            ENDIF
            IF (ABS(DUL(J) -  LL(J)) .LT. 1.E-2) THEN
               LL(J) = DUL(J) - 0.01
            ENDIF  
            IF (SHF(J) .LT. -1.E-6) THEN
               WRITE(MSG(1),'(A,A72)') 'File: ',FILESS
               
               WRITE(MSG(2),'(A,I4,2X,A,I2)') 
     &              'Line number:',LINSOL_1+J-1, 'Soil layer: ',J
               MSG(3) = 'Root growth factor is missing.  '
               MSG(4) = 'Model requires value between 0 and 1.'
               MSG(5) = 'Program will stop.'
               CALL WARNING(5,ERRKEY,MSG)
               CALL ERROR(ERRKEY,14,FILES,LINSOL_1+J-1)
            ENDIF

            IF (SWCN(J) < 1.E-4) THEN
               SWCN(J) = -99.
               ERR = ERR + 1
            ENDIF
         ENDDO

         IF (ERR > 0) THEN
            MSG(1) = "Saturated hydraulic conductivity equal to " // 
     &           "zero for one or more soil layers."
            MSG(2) = "Data will be treated as missing."
            CALL WARNING(2,ERRKEY,MSG)
         ENDIF
      ENDIF

      SELECT CASE (ISWITCH % MESOL)
      CASE('1'); CALL LYRSET (NLAYRI, ZLYR, NLAYR, DS, DLAYR, DEPMAX)
      CASE('3'); CALL LYRSET3(NLAYRI, ZLYR, DS, NLAYR, DLAYR, DEPMAX)
      CASE DEFAULT
         CALL LYRSET2(NLAYRI, ZLYR, DS, NLAYR, DLAYR, DEPMAX)
      END SELECT

!        Check for top layer too thick (could happen for MESOL='3')
      IF (DLAYR(1) > 5) THEN
         MSG(1) = "Soil layer 1 thicker than 5 cm."
         MSG(2) = "Soil water algorithms may become unstable."
         CALL WARNING(2,ERRKEY,MSG)
      ENDIF

!        CHP 10/2/2009 No evidence of instabilities with thin layers - remove message
!         DO L = 1, NLAYR
!           IF (DLAYR(L) < 5.) THEN
!             WRITE(MSG(1),
!     &     '("Soil layer",I3,": thickness =",F4.1," cm")') L, DLAYR(L)
!             MSG(2)="Thin layers may result in instabilities."
!             MSG(3)="See INFO.OUT for more detail on soil layers."
!             CALL WARNING(3,ERRKEY,MSG)
!           ENDIF
!         ENDDO

!!        Print soil layer data to INFO.OUT
!         WRITE(MSG(1),'(A,A)') "Soil layer distribution method ", 
!     &            ISWITCH%MESOL
!         MSG(2) = "            Thick-"
!         MSG(3) = "      Depth  ness"
!         MSG(4) = "   L   (cm)   (cm)"
!         DO L = 1, NLAYR
!           WRITE(MSG(L+4),'(I4,2F7.0)') L, DS(L), DLAYR(L)
!         ENDDO
!         CALL INFO(NLAYR+4, "LYRSET", MSG)
!
!        First tier soils data
      CALL LMATCH (NLAYRI, ZLYR, LL,    NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, DUL,   NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, SAT,   NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, SHF,   NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, SWCN,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, BD,    NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, OC,    NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, CLAY,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, SILT,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, STONES,NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, TOTN,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, PH,    NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, PHKCL, NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, CEC,   NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, ADCOEF,NLAYR, DS)
      
!     Second tier soils data
      CALL LMATCH (NLAYRI, ZLYR, EXTP,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, TOTP  ,NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, ORGP,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, CACO,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, EXTAL, NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, EXTFE, NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, EXTMN, NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, TOTBAS,NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, PTERMA,NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, PTERMB,NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, EXK,   NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, EXMG,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, EXNA,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, EXTS,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, SLEC,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, EXCA,  NLAYR, DS)

      CALL LMATCH (NLAYRI, ZLYR, alphaVG,NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, mVG,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, nVG,  NLAYR, DS)
      CALL LMATCH (NLAYRI, ZLYR, WCR,  NLAYR, DS)

         call csminp%add_sec('*SOIL',ntiers=2)
         call csminp%add_var('*SOIL',tier=1,int_name=(/'NLAYR'/),
     &        char_name=(/'SLNO  ','SLSOUR','SLTXS ','SLDESC',
     &           'SSITE ','SCOUNT','TAXON ','SCOM  ','SMHB  ',
     &           'SMPX  ','SMKE  ','SGRP  '/))
         call csminp%add_var('*SOIL',tier=2,
     &        real_name=(/'SLDP   ','SALB   ','U      ','SWCON  ',
     &          'CN2    ','SLNF   ','SLPF   ',
     &          'DS     ','DLAYR  ','SLAT   ','SLONG  ',
     &          'LL     ','DUL    ','SAT    ','SHF    ','SWCN   ',
     &          'BD     ','OC     ','CLAY   ','SILT   ','STONES ',
     &          'TOTN   ','PH     ','PHKCL  ','CEC    ','ADCOEF ',
     &          'EXTP   ','TOTP   ','ORGP   ','CACO   ','EXTAL  ',
     &          'EXTFE  ','EXTMN  ','TOTBAS ','PTERMA ','PTERMB ',
     &          'EXK    ','EXMG   ','EXNA   ','EXTS   ','SLEC   ',
     &          'EXCA   ','SASC   ','alphaVG','mVG    ','nVG    ',
     &          'WCR    '/))

      if(cmd_arg_present('--mimic_inp'))then
         
         SALB = round_real(SALB,6,2)
         U = round_real(U,6,1)
         SWCON = round_real(SWCON,6,2)
         CN2 = round_real(CN2,6,0)
         SLNF = round_real(SLNF,6,2)
         SLPF = round_real(SLPF,6,2)

         ! Begin code adapted from OPTEMPY2K
        DO I = 1, NLAYR
!         Construct format string depending on info avail.
!     DS(I),LL(I),DUL(I),SAT(I),SHF(I)
           DS(I) = round_real(DS(I),6,0)
           if(I==1)then
              DLAYR(I) = DS(I)
           else
              DLAYR(I) = DS(I) - DS(I-1)
           end if
           DLAYR(I) = round_real(DLAYR(I),6,0)
           LL(I) = round_real(LL(I),6,3)
           DUL(I) = round_real(DUL(I),6,3)
           SAT(I) = round_real(SAT(I),6,3)
           SHF(I) = round_real(SHF(I),6,3)

           IF (SWCN(I) < 1.E-6) THEN
              SWCN(I) = round_real(SWCN(I),6,0)
           ELSEIF (SWCN(I) < 0.1) THEN                       
              SWCN(I) = round_real(SWCN(I),6,4)
           ELSEIF (SWCN(I) < 1.0) THEN                       
              SWCN(I) = round_real(SWCN(I),6,3)
           ELSEIF (SWCN(I) < 10.) THEN
              SWCN(I) = round_real(SWCN(I),6,2)
           ELSEIF (SWCN(I) < 100.) THEN
              SWCN(I) = round_real(SWCN(I),6,1)
           ELSE
              SWCN(I) = round_real(SWCN(I),6,0)
           ENDIF
           BD(I) = round_real(BD(I),6,2)
           IF (OC(I) > 0.0 .AND. OC(I) < 9.99) THEN
              OC(I) = round_real(OC(I),6,3)
           ELSE
              OC(I) = round_real(OC(I),6,1)
           ENDIF
           CLAY(I) = round_real(CLAY(I),6,1)
           SILT(I) = round_real(SILT(I),6,1)
           STONES(I) = round_real(STONES(I),6,1)
           IF (TOTN(I) > 10.0) THEN
              TOTN(I) = round_real(TOTN(I),6,2)
           ELSEIF (TOTN(I) > 0.0) THEN
              TOTN(I) = round_real(TOTN(I),6,3)
           ELSE
              TOTN(I) = round_real(TOTN(I),6,1)
           ENDIF
           PH(I) = round_real(PH(I),6,2)
           PHKCL(I) = round_real(PHKCL(I),6,2)
           CEC(I) = round_real(CEC(I),6,2)
           ADCOEF(I) = round_real(ADCOEF(I),6,2)

        END DO
        ! End code adapted from OPTEMPY2K

      end if

         call csminp%put('*SOIL','NLAYR',NLAYR)
         call csminp%put('*SOIL','SLNO',PEDON)
         call csminp%put('*SOIL','SLSOUR',SLSOUR)
         call csminp%put('*SOIL','SLTXS',SLTXS)
         call csminp%put('*SOIL','SLDP',SLDP)
         call csminp%put('*SOIL','SLDESC',SLDESC)

         call csminp%put('*SOIL','SSITE',SSITE)
         call csminp%put('*SOIL','SCOUNT',SCOUNT)
         call csminp%put('*SOIL','SLAT',SLAT)
         call csminp%put('*SOIL','SLONG',SLONG)
         call csminp%put('*SOIL','TAXON',TAXON)

         call csminp%put('*SOIL','SCOM',SCOM)
         call csminp%put('*SOIL','SALB',SALB)
         call csminp%put('*SOIL','U',U)
         call csminp%put('*SOIL','SWCON',SWCON)
         call csminp%put('*SOIL','CN2',CN2)
         call csminp%put('*SOIL','SLNF',SLNF)
         call csminp%put('*SOIL','SLPF',SLPF)
         call csminp%put('*SOIL','SMHB',SMHB)
         call csminp%put('*SOIL','SMPX',SMPX)
         call csminp%put('*SOIL','SMKE',SMKE)
         call csminp%put('*SOIL','SGRP',SGRP)

         call csminp%put('*SOIL','DS',DS(1:nlayr))
         call csminp%put('*SOIL','DLAYR',DLAYR(1:nlayr))
         call csminp%put('*SOIL','LL',LL(1:nlayr))
         call csminp%put('*SOIL','DUL',DUL(1:nlayr))
         call csminp%put('*SOIL','SAT',SAT(1:nlayr))
         call csminp%put('*SOIL','SHF',SHF(1:nlayr))
         call csminp%put('*SOIL','SWCN',SWCN(1:nlayr))
         call csminp%put('*SOIL','BD',BD(1:nlayr))
         call csminp%put('*SOIL','OC',OC(1:nlayr))
         call csminp%put('*SOIL','CLAY',CLAY(1:nlayr))
         call csminp%put('*SOIL','SILT',SILT(1:nlayr))
         call csminp%put('*SOIL','STONES',STONES(1:nlayr))
         call csminp%put('*SOIL','TOTN',TOTN(1:nlayr))
         call csminp%put('*SOIL','PH',PH(1:nlayr))
         call csminp%put('*SOIL','PHKCL',PHKCL(1:nlayr))
         call csminp%put('*SOIL','CEC',CEC(1:nlayr))
         call csminp%put('*SOIL','ADCOEF',ADCOEF(1:nlayr))

         call csminp%put('*SOIL','EXTP',EXTP(1:nlayr))
         call csminp%put('*SOIL','TOTP',TOTP(1:nlayr))
         call csminp%put('*SOIL','ORGP',ORGP(1:nlayr))
         call csminp%put('*SOIL','CACO',CACO(1:nlayr))
         call csminp%put('*SOIL','EXTAL',EXTAL(1:nlayr))
         call csminp%put('*SOIL','EXTFE',EXTFE(1:nlayr))
         call csminp%put('*SOIL','EXTMN',EXTMN(1:nlayr))
         call csminp%put('*SOIL','TOTBAS',TOTBAS(1:nlayr))
         call csminp%put('*SOIL','PTERMA',PTERMA(1:nlayr))
         call csminp%put('*SOIL','PTERMB',PTERMB(1:nlayr))
         call csminp%put('*SOIL','EXK',EXK(1:nlayr))
         call csminp%put('*SOIL','EXMG',EXMG(1:nlayr))
         call csminp%put('*SOIL','EXNA',EXNA(1:nlayr))
         call csminp%put('*SOIL','EXTS',EXTS(1:nlayr))
         call csminp%put('*SOIL','SLEC',SLEC(1:nlayr))
         call csminp%put('*SOIL','EXCA',EXCA(1:nlayr))
         call csminp%put('*SOIL','SASC',SASC(1:nlayr))

         call csminp%put('*SOIL','alphaVG',alphaVG(1:nlayr))
         call csminp%put('*SOIL','mVG',mVG(1:nlayr))
         call csminp%put('*SOIL','nVG',nVG(1:nlayr))
         call csminp%put('*SOIL','WCR',WCR(1:nlayr))

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

!  950 FORMAT (/,5X,' SOIL ',A10,' is not in the SOIL file SOIL.SOL !')
  950 FORMAT (/,5X,' SOIL ',A10,' is not in the SOIL file:',/,A92)
 5000 FORMAT (/, '  More.... press < ENTER > key ')
 5030 FORMAT (1X,A10, 2X, A11,1X,A5,1X,F5.0,1X,A50)
 5035 FORMAT (2(1X,A11),2(1X,F8.3),1X,A50)
! 5040 FORMAT (1X,A5,6(1X,F5.0),4(1X,A5))
! 5040 FORMAT (1X,A5,6(1X,F5.0),4(1X,A5),1X,F5.0)
! 5040 FORMAT (1X,A5,6(1X,F5.0),3(1X,A5),F6.0) !McNair's 5/13/2005
! 5080 FORMAT (1X,F5.0,1X,A5,29(1X,F5.0))
! 5090 FORMAT (1X,F5.0,18(1X,F5.0))
 5101 FORMAT (10X,'ERROR! Soil Selection must be between 1 and ',I3,/)
 5102 FORMAT (10X,'ERROR! Soil Selection must be an INTEGER value',/)
 5130 FORMAT (T25, 'SOILS IN THE DATA BASE', /,T3, 'REF', T25,
     &        22('='),/,T3, 'NO.', T7, 'TAXONOMY NAME', T67,
     &        'PEDON NUMBER', /T2, 4('-'), 1X, 50('-'), T67, 12('-'))
 5140 FORMAT (I4,') ',A50, T67, A10)
 5160 FORMAT (/,6X,'SELECTED SOIL TYPE ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?    --->',2X,' ',$)

      END SUBROUTINE read_nc_soil

      function round_real(unrounded,width,digits) result(rounded)

        implicit none

        integer,intent(in) :: width,digits
        real,intent(in) :: unrounded
        real :: rounded
        character(len=width) :: char_tmp
        character(len=12) :: fmt

        fmt = ' '

        write(fmt(1:3),'(i3)') width
        write(fmt(4:6),'(i3)') digits

        fmt = '(f'//trim(adjustl(fmt(1:3)))//'.'//
     &       trim(adjustl(fmt(4:6)))//')'

        write(char_tmp,fmt) unrounded

        read(char_tmp,fmt) rounded

      end function
