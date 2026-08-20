C=======================================================================
C  IPVAR, Subroutine
C
C  Reads in genetic information for crop
C-----------------------------------------------------------------------
C  Revision       History
C  01/01/1990 GH  Written
C  05/28/1993 PWW Header revision and minor changes  
C  12/14/2000 GH  Version correction
C  12/14/2005 CHP/PST Added new sorghum cultivar coefficients (optional)
C  02/06/2007 CHP Added alternate sugarcane parameters for CASUPRO
C  04/21/2007 GH  Added P3 and P4 coefficients for sorghum 
C  09/16/2007 JIL New inputs for IXIM
!  11/08/2007 CHP Added 6X field for quality (= number of experiments used
!                 in estimation of parameters).  This field is read here,
!                 but not written to INP or INH files.
C  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
C  01/16/2008 GH  Added P2 and PANTH coefficients for sorghum
C  08/03/2009 FSR Added numerous variables for CASUPRO
C  06/30/2010 FSR Added PLF2 variable for CASUPRO
C  05/19/2011 GH  Updated for sorghum
C  02/25/2012 JZW add the PHINT data reading from *.cul for RICER
C  08/09/2012 GH  Updated for cassava
!  04/16/2013 CHP/KAD Added SALUS model
!  05/09/2013 CHP/FR/JZW Added N-wheat module
C-----------------------------------------------------------------------
C  INPUT  : FILEG,NSENS,VARNO,VARTY,VRNAME,PATHGE,ECONO
C
C  LOCAL  : LINE,BLANK,ANS,ERRKEY,C360,FILEGG,ERRNUM,LINVAR,LUNVAR,I,ISECT,
C           PATHL,NLOOP,NLVAR,FLAG,VAR
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEVAR SENS INPUT
C
C  Calls  : ERROR CLEAR IGNORE VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      SUBROUTINE read_nc_gen(NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &                  PATHGE,ECONO, MODEL, ATLINE,CROP)

      use csm_io
      use dssat_netcdf

      IMPLICIT NONE

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS
      CHARACTER*2   CROP
      CHARACTER*6   VARTY,VARNO,ERRKEY,ECONO
      CHARACTER*8   MODEL
      CHARACTER*12  FILEG
      CHARACTER*16  VRNAME
      CHARACTER*78  MSG(2)
      CHARACTER*80  PATHGE
      CHARACTER*92  FILEGG
      CHARACTER*1000 C360,ATLINE

      INTEGER       I,NSENS,NLVAR,LUNVAR,LINVAR,ISECT,NLOOP
      INTEGER       ERRNUM,PATHL
      REAL          FLAG,VAR
      REAL P1,P1V,P1D,P2,P2O,P2R,P3,P4,P5,G1,G2,G3,G4,G5,G0
      REAL PHINT,PD,TC,AX,LX,PANTH
      INTEGER LFN
      REAL PPS1, B01ND, B12ND, B23ND, B34ND, B45ND, B56ND
      REAL SRNWT, SRFR, HMPC, LA1S, LAXS
      REAL LAXND, LAXN2, LAFS, LAFND, SLASS, LLIFA, LPEFR
      REAL STFR
      REAL VREQ, VBASE, VEFF, PPS2
      REAL P6, P7, P8
      REAL GNOWT, GWTS, SHWTS, LAFV, LAFR
      REAL PHTMAX,empty,PLF1,PLF2,Gamma,StkB,StkM
      REAL LIsun,LIshd,TB(3),TO1(3),TO2(3),TM(3)
      REAL LI1,TELOM, LSFAC
      REAL Ph1P,Ph1R,Ph2,Ph3,Ph4,StkHrNO,RTNFAC,MinGr
      REAL RES30C,RLF30C,R30C2
      REAL StkH2OFac,SuH2OFac
      REAL MaxPARCE, APFMX, STKPFMAX, SUCA, TBFT, Tthalfo, TBase
      REAL MXLFAREA, MXLFARNO, PSWITCH, TTPLNTEM
      REAL TTRATNEM, CHUPIBASE, TT_POPGROWTH, MAX_POP, POPTT16
      REAL LG_AMBASE
      REAL CSDVAR,PHTHRS(20),SDPDVR,SLAVAR,LFMAX,XFRUIT,WTPSD
      REAL SFDUR,PODUR,PPSEN,PH2T5,SIZELF
      REAL PI1,PI2
      REAL PCINT,PCGRD
      REAL DTPI,SIZLF !SC
      REAL PBASE, PSAT !Sorghum
      REAL THRESH, SDPRO, SDLIP
      CHARACTER*360 PLAINTXT
      REAL          PHYL1,PHYL2,FRSUG,DRCER

!     For CSYCA-cassava
      REAL          LNSLP, NODWT, NODLT

! 	  For APSIM-wheat (WHAPS)
      REAL  VSEN,GRNO,MXFIL
      REAL  STMMX,SLAP1,SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2
      REAL  P2AF,P3AF,P4AF,P5AF,P6AF
      REAL  ADLAI,ADTIL,ADPHO,STEMN,MXNUP,MXNCR,WFNU
      REAL  PNUPR,EXNO3,MNNO3,EXNH4,MNNH4,INGWT,INGNC,FREAR
      REAL  MNNCR,GPPSS,GPPES,MXGWT,MNRTN,NOMOB,RTDP1,RTDP2

      PARAMETER (LUNVAR = 19)
      PARAMETER (ERRKEY = 'NCGEN ')
      PARAMETER (BLANK  = ' ')

      DATA NLVAR /0/

C-----------------------------------------------------------------------
C    Read Cultivar Specific Genetics/Cultivar Parameter File
C-----------------------------------------------------------------------

!     ------------------------------------------------------------------
      SELECT CASE (MODEL(1:5))

!     ** Formats now include space (6X) for quality parameter between 
!     variety name and ecotype name

!     Generic SALUS simple crop models
!     09/21/2009 CHP/KAD
      CASE ('SALUS')
         call nc_gen%read_cul('VARNO',VARNO)
         call nc_gen%read_cul('VARNAME',VRNAME,optional=.true.)
         if(vrname(1:3) == '-99')then
            call nc_gen%read_cul('VRNAME',VRNAME)
         end if

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME'/))

        call csminp%put('*CULTIVAR','VARNO',VARNO)
        call csminp%put('*CULTIVAR','VRNAME',VRNAME)

!     CROPGRO crops **
      CASE ('CRGRO')
         call nc_gen%read_cul('VARNAME',VRNAME,optional=.true.)
         if(vrname(1:3) == '-99')then
            call nc_gen%read_cul('VRNAME',VRNAME)
         end if
         call nc_gen%read_cul('ECONO',ECONO)
         call nc_gen%read_cul('CSDL',CSDVAR)
         call nc_gen%read_cul('PPSEN',PPSEN)
         call nc_gen%read_cul('EM-FL',PH2T5)
         call nc_gen%read_cul('FL-SH',PHTHRS(6))
         call nc_gen%read_cul('FL-SD',PHTHRS(8))
         call nc_gen%read_cul('SD-PM',PHTHRS(10))
         call nc_gen%read_cul('FL-LF',PHTHRS(13))
         call nc_gen%read_cul('LFMAX',LFMAX)
         call nc_gen%read_cul('SLAVR',SLAVAR)
         call nc_gen%read_cul('SIZLF',SIZELF)
         call nc_gen%read_cul('XFRT',XFRUIT)
         call nc_gen%read_cul('WTPSD',WTPSD)
         call nc_gen%read_cul('SFDUR',SFDUR)
         call nc_gen%read_cul('SDPDV',SDPDVR)
         call nc_gen%read_cul('PODUR',PODUR)
         call nc_gen%read_cul('THRSH',THRESH)
         call nc_gen%read_cul('SDPRO',SDPRO)
         call nc_gen%read_cul('SDLIP',SDLIP)

        IF (LFMAX  .LE. 0) CALL ERROR (ERRKEY,22,FILEG,LINVAR)
        IF (SLAVAR .LE. 0) CALL ERROR (ERRKEY,23,FILEG,LINVAR)
        IF (SIZELF .LE. 0) CALL ERROR (ERRKEY,23,FILEG,LINVAR)
        IF (XFRUIT .LE. 0) CALL ERROR (ERRKEY,24,FILEG,LINVAR)
        IF (WTPSD  .LE. 0) CALL ERROR (ERRKEY,25,FILEG,LINVAR)
        IF (SDPDVR .LE. 0) CALL ERROR (ERRKEY,26,FILEG,LINVAR)
        IF (SFDUR  .LE. 0) CALL ERROR (ERRKEY,27,FILEG,LINVAR)
        IF (PODUR  .LE. 0) CALL ERROR (ERRKEY,28,FILEG,LINVAR)
        IF (THRESH .LE. 0) CALL ERROR (ERRKEY,50,FILEG,LINVAR)
!        IF (SDPRO  .LE. 0) CALL ERROR (ERRKEY,51,FILEG,LINVAR)
!        IF (SDLIP  .LE. 0) CALL ERROR (ERRKEY,52,FILEG,LINVAR)

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'CSDVAR    ','PPSEN     ','PH2T5     ',
     &                 'PHTHRS(6) ','PHTHRS(8) ','PHTHRS(10)',
     &                 'PHTHRS(13)','LFMAX     ','SLAVAR    ',
     &                 'SIZELF    ','XFRUIT    ','WTPSD     ',
     &                 'SFDUR     ','SDPDVR    ','PODUR     ',
     &                 'THRESH    ','SDPRO     ','SDLIP     '/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','CSDVAR',CSDVAR)
        call csminp%put('*CULTIVAR','PPSEN',PPSEN)
        call csminp%put('*CULTIVAR','PH2T5',PH2T5)
        call csminp%put('*CULTIVAR','PHTHRS(6)',PHTHRS(6))
        call csminp%put('*CULTIVAR','PHTHRS(8)',PHTHRS(8))
        call csminp%put('*CULTIVAR','PHTHRS(10)',PHTHRS(10))
        call csminp%put('*CULTIVAR','PHTHRS(13)',PHTHRS(13))
        call csminp%put('*CULTIVAR','LFMAX',LFMAX)
        call csminp%put('*CULTIVAR','SLAVAR',SLAVAR)
        call csminp%put('*CULTIVAR','SIZELF',SIZELF)
        call csminp%put('*CULTIVAR','XFRUIT',XFRUIT)
        call csminp%put('*CULTIVAR','WTPSD',WTPSD)
        call csminp%put('*CULTIVAR','SFDUR',SFDUR)
        call csminp%put('*CULTIVAR','SDPDVR',SDPDVR)
        call csminp%put('*CULTIVAR','PODUR',PODUR)
        call csminp%put('*CULTIVAR','THRESH',THRESH)
        call csminp%put('*CULTIVAR','SDPRO',SDPRO)
        call csminp%put('*CULTIVAR','SDLIP',SDLIP)

C-GH Remove cassava
!     CropSim: wheat, barley
      CASE ('CSCRP')
!       For now, CropSim will just have a text string for cultivar info

!WHEAT & BARLEY
!@VAR#  VAR-NAME........  EXP#   ECO#  VREQ  PPS1    P8 G#WTS  GWTS SHWTS PHINT    P1    P2    P3    P4    P5    P6    P7  LA1S  LAFV  LAFR VBASE  VEFF  PPS2
!DFAULT DEFAULTS             . DFAULT     0     0   500    25    40   2.5    80   380    70   200   200    60    25   150   3.0   0.1   0.5     0     0     0

         call nc_gen%read_cul('VARNAME',VRNAME,optional=.true.)
         if(vrname(1:3) == '-99')then
            call nc_gen%read_cul('VRNAME',VRNAME)
         end if
         call nc_gen%read_cul('ECONO',ECONO)
         call nc_gen%read_cul('P1',P1)
         call nc_gen%read_cul('P2',P2)
         call nc_gen%read_cul('P3',P3)
         call nc_gen%read_cul('P4',P4)
         call nc_gen%read_cul('P5',P5)
         call nc_gen%read_cul('P6',P6)
         call nc_gen%read_cul('P7',P7)
         call nc_gen%read_cul('P8',P8)
         call nc_gen%read_cul('VREQ',VREQ)
         call nc_gen%read_cul('VBASE',VBASE)
         call nc_gen%read_cul('VEFF',VEFF)
         call nc_gen%read_cul('PPS1',PPS1)
         call nc_gen%read_cul('PPS2',PPS2)
         call nc_gen%read_cul('PHINT',PHINT)
         call nc_gen%read_cul('LA1S',LA1S)
         call nc_gen%read_cul('LAFV',LAFV)
         call nc_gen%read_cul('LAFR',LAFR)
         call nc_gen%read_cul('SHWTS',SHWTS)
         call nc_gen%read_cul('GNOWT',GNOWT)
         call nc_gen%read_cul('GWTS',GWTS)

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO   ','VRNAME  ','ECONO   ','PLAINTXT'/),
     &     real_name=(/'P1   ','P2   ','P3   ','P4   ','P5   ',
     &                 'P6   ','P7   ','P8   ','VREQ ','VBASE',
     &                 'VEFF ','PPS1 ','PPS2 ','PHINT','LA1S ',
     &                 'LAFV ','LAFR ','SHWTS','GNOWT','GWTS '/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','PLAINTXT',PLAINTXT)
        call csminp%put('*CULTIVAR','P1',P1)
        call csminp%put('*CULTIVAR','P2',P2)
        call csminp%put('*CULTIVAR','P3',P3)
        call csminp%put('*CULTIVAR','P4',P4)
        call csminp%put('*CULTIVAR','P5',P5)
        call csminp%put('*CULTIVAR','P6',P6)
        call csminp%put('*CULTIVAR','P7',P7)
        call csminp%put('*CULTIVAR','P8',P8)
        call csminp%put('*CULTIVAR','VREQ',VREQ)
        call csminp%put('*CULTIVAR','VBASE',VBASE)
        call csminp%put('*CULTIVAR','VEFF',VEFF)
        call csminp%put('*CULTIVAR','PPS1',PPS1)
        call csminp%put('*CULTIVAR','PPS2',PPS2)
        call csminp%put('*CULTIVAR','PHINT',PHINT)
        call csminp%put('*CULTIVAR','LA1S',LA1S)
        call csminp%put('*CULTIVAR','LAFV',LAFV)
        call csminp%put('*CULTIVAR','LAFR',LAFR)
        call csminp%put('*CULTIVAR','SHWTS',SHWTS)
        call csminp%put('*CULTIVAR','GNOWT',GNOWT)
        call csminp%put('*CULTIVAR','GWTS',GWTS)

C-GH Tony update February, 2014
!     &      VREQ, PPS1, P8, GNOWT, GWTS, SHWTS, PHINT, 
!     &      P1, P2, P3, P4, P5, P6, P7, 
!     &      LA1S, LAFV, LAFR, VBASE, VEFF, PPS2, PLAINTXT


C-GH  Add cassava model
!     CASSAVA: cassava **
      CASE ('CSCAS')
          READ (C360,820,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, 
     &      PPS1, B01ND, B12ND, B23ND, B34ND, B45ND, B56ND,
C-GH &      SNFX, SRNWT, SRFR, HMPC, PHINT, LA1S, LAXS, LAXND, LAXN2,
     &      SRNWT, SRFR, HMPC, PHINT, LA1S, LAXS, LAXND, LAXN2,
     &      LAFS, LAFND, SLASS, LLIFA, LPEFR, STFR, PLAINTXT

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO   ','VRNAME  ','ECONO   ','PLAINTXT'/),
     &     real_name=(/'PPS1 ','B01ND','B12ND','B23ND','B34ND',
     &                 'B45ND','B56ND','SRNWT','SRFR ','HMPC ',
     &                 'PHINT','LA1S ','LAXS ','LAXND','LAXN2',
     &                 'LAFS ','LAFND','SLASS','LLIFA','LPEFR',
     &                 'STFR '/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','PLAINTXT',PLAINTXT)
        call csminp%put('*CULTIVAR','PPS1',PPS1)
        call csminp%put('*CULTIVAR','B01ND',B01ND)
        call csminp%put('*CULTIVAR','B12ND',B12ND)
        call csminp%put('*CULTIVAR','B23ND',B23ND)
        call csminp%put('*CULTIVAR','B34ND',B34ND)
        call csminp%put('*CULTIVAR','B45ND',B45ND)
        call csminp%put('*CULTIVAR','B56ND',B56ND)
        call csminp%put('*CULTIVAR','SRNWT',SRNWT)
        call csminp%put('*CULTIVAR','SRFR',SRFR)
        call csminp%put('*CULTIVAR','HMPC',HMPC)
        call csminp%put('*CULTIVAR','PHINT',PHINT)
        call csminp%put('*CULTIVAR','LA1S',LA1S)
        call csminp%put('*CULTIVAR','LAXS',LAXS)
        call csminp%put('*CULTIVAR','LAXND',LAXND)
        call csminp%put('*CULTIVAR','LAXN2',LAXN2)
        call csminp%put('*CULTIVAR','LAFS',LAFS)
        call csminp%put('*CULTIVAR','LAFND',LAFND)
        call csminp%put('*CULTIVAR','SLASS',SLASS)
        call csminp%put('*CULTIVAR','LLIFA',LLIFA)
        call csminp%put('*CULTIVAR','LPEFR',LPEFR)
        call csminp%put('*CULTIVAR','STFR',STFR)

!     Ceres-wheat: wheat, barley **
      CASE ('CSCER')

         call nc_gen%read_cul('VARNAME',VRNAME,optional=.true.)
         if(vrname(1:3) == '-99')then
            call nc_gen%read_cul('VRNAME',VRNAME)
         end if
         call nc_gen%read_cul('ECONO',ECONO)
         call nc_gen%read_cul('P1V',P1V)
         call nc_gen%read_cul('P1D',P1D)
         call nc_gen%read_cul('P5',P5)
         call nc_gen%read_cul('G1',G1)
         call nc_gen%read_cul('G2',G2)
         call nc_gen%read_cul('G3',G3)
         call nc_gen%read_cul('PHINT',PHINT)

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'P1V  ','P1D  ','P5   ','G1   ','G2   ',
     &                 'G3   ','PHINT'/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','P1V',P1V)
        call csminp%put('*CULTIVAR','P1D',P1D)
        call csminp%put('*CULTIVAR','P5',P5)
        call csminp%put('*CULTIVAR','G1',G1)
        call csminp%put('*CULTIVAR','G2',G2)
        call csminp%put('*CULTIVAR','G3',G3)
        call csminp%put('*CULTIVAR','PHINT',PHINT)

!     Ceres Maize: maize, sweet corn **
      CASE ('MZCER','SWCER')

         call nc_gen%read_cul('VARNAME',VRNAME,optional=.true.)
         if(vrname(1:3) == '-99')then
            call nc_gen%read_cul('VRNAME',VRNAME)
         end if
         call nc_gen%read_cul('ECONO',ECONO)
         call nc_gen%read_cul('P1',P1)
         call nc_gen%read_cul('P2',P2)
         call nc_gen%read_cul('P5',P5)
         call nc_gen%read_cul('G2',G2)
         call nc_gen%read_cul('G3',G3)
         call nc_gen%read_cul('PHINT',PHINT)

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO   ','VRNAME  ','ECONO   ','PLAINTXT'/),
     &       real_name=(/'P1       ','P2       ','P5       ',
     &       'G2       ','G3       ','PHINT    ','PHTHRS(8)'/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','P1',P1)
        call csminp%put('*CULTIVAR','P2',P2)
        call csminp%put('*CULTIVAR','P5',P5)
        call csminp%put('*CULTIVAR','G2',G2)
        call csminp%put('*CULTIVAR','G3',G3)
        call csminp%put('*CULTIVAR','PHINT',PHINT)
        call csminp%put('*CULTIVAR','PHTHRS(8)',0.)

C-LPM  Add CIAT cassava model
!     CASSAVA: cassava **
      CASE ('CSYCA')

!DA 04OCT2016 Removing LA1S variable, is not used according to LPM 07MAR15          
!         READ (C360,821,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, 
!     &      PPS1, B01ND, B12ND, SRNWT, HMPC, LA1S, LAXS, 
!     &      SLASS, LLIFA, LPEFR, LNSLP, NODWT, NODLT, PLAINTXT 

          READ (C360,821,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, 
     &      PPS1, B01ND, B12ND, SRNWT, HMPC, LAXS, 
     &      SLASS, LLIFA, LPEFR, LNSLP, NODWT, NODLT, PLAINTXT 


!     APSIM-NWheat wheat  **
      CASE ('WHAPS')
!*!        READ (C360,'(A6,1X,A16,7X,A6,6F6.0)',IOSTAT=ERRNUM)
        READ (C360,850,IOSTAT=ERRNUM) 
     &            VARTY,VRNAME,ECONO,VSEN,PPSEN,P2,P5,PHINT,GRNO,MXFIL,
     &            STMMX,SLAP1,SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2,
     &            P2AF,P3AF,P4AF,P5AF,P6AF,
     &            ADLAI,ADTIL,ADPHO,STEMN,MXNUP,MXNCR,WFNU,
     &            PNUPR,EXNO3,MNNO3,EXNH4,MNNH4,INGWT,INGNC,FREAR,
     &            MNNCR,GPPSS,GPPES,MXGWT,MNRTN,NOMOB,RTDP1,RTDP2

!WDB 7/2016 Added cultivar coefficients for sugar beet model
      CASE ('BSCER')
          READ (C360,'(A6,1X,A16,7X,A6,9F6.0)',IOSTAT=ERRNUM)
     &        VARTY,VRNAME,ECONO,P1,P2,P5,G2,G3,PHYL1,PHYL2,FRSUG,DRCER
!WDB** end changes

!     Ixim maize **
      CASE ('MZIXM')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P5,G2,G3,PHINT,AX,LX
C ** Use default values if inputs not available
        LFN = ((1.4*P1)/(PHINT*0.5))+(10.55-0.0216*P1)
	IF (AX .EQ. 0.0) THEN
	  AX = 1000.0*EXP(-1.17 + (0.047*LFN))  !From Birch et al, 1998
	ENDIF
        IF (LX .EQ. 0.0) THEN
	  LX = 1.1138 * AX                      !From regression, JIL 
	ENDIF

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO   ','VRNAME  ','ECONO   ','PLAINTXT'/),
     &     real_name=(/'P1   ','P2   ','P5   ','G2   ','G3   ','PHINT',
     &                 'AX   ','LX   ','LFN  '/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','PLAINTXT',PLAINTXT)
        call csminp%put('*CULTIVAR','P1',P1)
        call csminp%put('*CULTIVAR','P2',P2)
        call csminp%put('*CULTIVAR','P5',P5)
        call csminp%put('*CULTIVAR','G2',G2)
        call csminp%put('*CULTIVAR','G3',G3)
        call csminp%put('*CULTIVAR','PHINT',PHINT)
        call csminp%put('*CULTIVAR','AX',AX)
        call csminp%put('*CULTIVAR','LX',LX)
        call csminp%put('*CULTIVAR','LFN',LFN)

!     Ceres Sorghum **
      CASE ('SGCER')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P2O,P2R,PANTH,P3,P4,P5,PHINT,G1,G2
C-GH &            P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4,P2,PANTH
C-GH &            P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4
!           Read optional PBASE and PSAT parameters
!           If these parameters are non-zero, then optional 
!             method is used
     &            ,PBASE, PSAT
!       Optional cultivar parameters
        IF (PBASE < 1.E-2) PBASE = -99.
        IF (PSAT  < 1.E-2) PSAT  = -99.

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'P1   ','P2   ','P2O  ','P2R  ','PANTH',
     &                 'P3   ','P4   ','P5   ','PHINT','G1   ',
     &                 'G2   ','PBASE','PSAT '/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','P1',P1)
        call csminp%put('*CULTIVAR','P2',P2)
        call csminp%put('*CULTIVAR','P2O',P2O)
        call csminp%put('*CULTIVAR','P2R',P2R)
        call csminp%put('*CULTIVAR','PANTH',PANTH)
        call csminp%put('*CULTIVAR','P3',P3)
        call csminp%put('*CULTIVAR','P4',P4)
        call csminp%put('*CULTIVAR','P5',P5)
        call csminp%put('*CULTIVAR','PHINT',PHINT)
        call csminp%put('*CULTIVAR','G1',G1)
        call csminp%put('*CULTIVAR','G2',G2)
        call csminp%put('*CULTIVAR','PBASE',PBASE)
        call csminp%put('*CULTIVAR','PSAT',PSAT)

!     Ceres Millet **
      CASE ('MLCER')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G4,PHINT,G0,G5,PHINT
        IF (G4 > 1.2) THEN
          MSG(1)="G4 is fraction partitioning and should not exceed 1.2"
          MSG(2)=
     &  "G4 (from cultivar file) set equal to 1.2 for this simulation."
          CALL WARNING(2, ERRKEY, MSG)
          G4 = 1.2
        ENDIF

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'P1   ','P2O  ','P2R  ','P5   ','G1   ',
     &                 'G4   ','PHINT'/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','P1',P1)
        call csminp%put('*CULTIVAR','P2O',P2O)
        call csminp%put('*CULTIVAR','P2R',P2R)
        call csminp%put('*CULTIVAR','P5',P5)
        call csminp%put('*CULTIVAR','G1',G1)
        call csminp%put('*CULTIVAR','G4',G4)
        call csminp%put('*CULTIVAR','PHINT',PHINT)

!     Substor Potato **
      CASE ('PTSUB')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            G2,G3,PD,P2,TC
!     &            G2,G3,G4,PD,P2,TC

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'G2   ','G3   ','PD   ','P2   ','TC   '/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','G2',G2)
        call csminp%put('*CULTIVAR','G3',G3)
        call csminp%put('*CULTIVAR','PD',PD)
        call csminp%put('*CULTIVAR','P2',P2)
        call csminp%put('*CULTIVAR','TC',TC)

!     Ceres Rice **
      CASE ('RICER')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2R,P5,P2O,G1,G2,G3,G4, PHINT,G5
!       For backwards compatibility for cultivar files with no G5.
        IF (ERRNUM /= 0) THEN
          READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2R,P5,P2O,G1,G2,G3,G4, PHINT
          G5 = 1.0
        ENDIF
        IF (G5 < 0.0) G5 = 1.0

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'P1   ','P2R  ','P5   ','P2O  ','G1   ',
     &                 'G2   ','G3   ','G4   ','PHINT'/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','P1',P1)
        call csminp%put('*CULTIVAR','P2R',P2R)
        call csminp%put('*CULTIVAR','P5',P5)
        call csminp%put('*CULTIVAR','P2O',P2O)
        call csminp%put('*CULTIVAR','G1',G1)
        call csminp%put('*CULTIVAR','G2',G2)
        call csminp%put('*CULTIVAR','G3',G3)
        call csminp%put('*CULTIVAR','G4',G4)
        call csminp%put('*CULTIVAR','PHINT',PHINT)

!     ORYZA Rice **
!     Read name of OYRZA crop file
      CASE ('RIORZ')
        READ (C360,'(A6,1X,A16,7X,A80)',IOSTAT=ERRNUM) VARTY,VRNAME,
     &            PLAINTXT
        ECONO = '      '

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO   ','VRNAME  ','ECONO   ','PLAINTXT'/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','PLAINTXT',PLAINTXT)

!     CaneGro: South African Sugarcane model **
      CASE ('SCCAN')
        READ (C360,1060,IOSTAT=ERRNUM) VARTY, VRNAME, ECONO,
     &      MaxPARCE, APFMX, STKPFMAX, SUCA, TBFT, Tthalfo, TBase, 
     &      LFMAX, MXLFAREA, MXLFARNO, PI1, PI2, PSWITCH, TTPLNTEM, 
     &      TTRATNEM, CHUPIBASE, TT_POPGROWTH, MAX_POP, POPTT16, 
     &      LG_AMBASE 

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'MaxPARCE    ','APFMX       ','STKPFMAX    ',
     &                 'SUCA        ','TBFT        ','Tthalfo     ',
     &                 'TBase       ','LFMAX       ','MXLFAREA    ',
     &                 'MXLFARNO    ','PI1         ','PI2         ',
     &                 'PSWITCH     ','TTPLNTEM    ','TTRATNEM    ',
     &                 'CHUPIBASE   ','TT_POPGROWTH','MAX_POP     ',
     &                 'POPTT16     ','LG_AMBASE   '/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','MaxPARCE',MaxPARCE)
        call csminp%put('*CULTIVAR','APFMX',APFMX)
        call csminp%put('*CULTIVAR','STKPFMAX',STKPFMAX)
        call csminp%put('*CULTIVAR','SUCA',SUCA)
        call csminp%put('*CULTIVAR','TBFT',TBFT)
        call csminp%put('*CULTIVAR','Tthalfo',Tthalfo)
        call csminp%put('*CULTIVAR','TBase',TBase)
        call csminp%put('*CULTIVAR','LFMAX',LFMAX)
        call csminp%put('*CULTIVAR','MXLFAREA',MXLFAREA)
        call csminp%put('*CULTIVAR','MXLFARNO',MXLFARNO)
        call csminp%put('*CULTIVAR','PI1',PI1)
        call csminp%put('*CULTIVAR','PI2',PI2)
        call csminp%put('*CULTIVAR','PSWITCH',PSWITCH)
        call csminp%put('*CULTIVAR','TTPLNTEM',TTPLNTEM)
        call csminp%put('*CULTIVAR','TTRATNEM',TTRATNEM)
        call csminp%put('*CULTIVAR','CHUPIBASE',CHUPIBASE)
        call csminp%put('*CULTIVAR','TT_POPGROWTH',TT_POPGROWTH)
        call csminp%put('*CULTIVAR','MAX_POP',MAX_POP)
        call csminp%put('*CULTIVAR','POPTT16',POPTT16)
        call csminp%put('*CULTIVAR','LG_AMBASE',LG_AMBASE)

!     Casupro: Florida-Colombia Sugarcane model **
      CASE ('SCCSP')
        READ (C360,1055,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &          LFMAX,PHTMAX,StkH2OFac,SuH2OFac,empty,PLF1,
     &          PLF2,Gamma,StkB,StkM,empty,
     &          SIZLF,LIsun,LIshd,empty,TB(1),TO1(1),TO2(1),TM(1),
     &          PI1,PI2,DTPI,LSFAC,empty,LI1,TELOM,TB(2),TO1(2),
     &          TO2(2),TM(2),Ph1P,Ph1R,Ph2,Ph3,Ph4,StkHrNO,RTNFAC,
     &          MinGr,empty,RES30C,RLF30C,R30C2,empty,empty 

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'LFMAX    ','PHTMAX   ','StkH2OFac','SuH2OFac ',
     &                 'PLF1     ','PLF2     ','Gamma    ','StkB     ',
     &                 'StkM     ','SIZLF    ','LIsun    ','LIshd    ',
     &                 'TB(1)    ','TO1(1)   ','TO2(1)   ','TM(1)    ',
     &                 'PI1      ','PI2      ','DTPI     ','LSFAC    ',
     &                 'LI1      ','TELOM    ','TB(2)    ','TO1(2)   ',
     &                 'TO2(2)   ','TM(2)    ','Ph1P     ','Ph1R     ',
     &                 'Ph2      ','Ph3      ','Ph4      ','StkHrNO  ',
     &                 'RTNFAC   ','MinGr    ','RES30C   ','RLF30C   ',
     &                 'R30C2    '/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','LFMAX',LFMAX)
        call csminp%put('*CULTIVAR','PHTMAX',PHTMAX)
        call csminp%put('*CULTIVAR','StkH2OFac',StkH2OFac)
        call csminp%put('*CULTIVAR','SuH2OFac',SuH2OFac)
        call csminp%put('*CULTIVAR','PLF1',PLF1)
        call csminp%put('*CULTIVAR','PLF2',PLF2)
        call csminp%put('*CULTIVAR','Gamma',Gamma)
        call csminp%put('*CULTIVAR','StkB',StkB)
        call csminp%put('*CULTIVAR','StkM',StkM)
        call csminp%put('*CULTIVAR','SIZLF',SIZLF)
        call csminp%put('*CULTIVAR','LIsun',LIsun)
        call csminp%put('*CULTIVAR','LIshd',LIshd)
        call csminp%put('*CULTIVAR','TB(1)',TB(1))
        call csminp%put('*CULTIVAR','TO1(1)',TO1(1))
        call csminp%put('*CULTIVAR','TO2(1)',TO2(1))
        call csminp%put('*CULTIVAR','TM(1)',TM(1))
        call csminp%put('*CULTIVAR','PI1',PI1)
        call csminp%put('*CULTIVAR','PI2',PI2)
        call csminp%put('*CULTIVAR','DTPI',DTPI)
        call csminp%put('*CULTIVAR','LSFAC',LSFAC)
        call csminp%put('*CULTIVAR','LI1',LI1)
        call csminp%put('*CULTIVAR','TELOM',TELOM)
        call csminp%put('*CULTIVAR','TB(2)',TB(2))
        call csminp%put('*CULTIVAR','TO1(2)',TO1(2))
        call csminp%put('*CULTIVAR','TO2(2)',TO2(2))
        call csminp%put('*CULTIVAR','TM(2)',TM(2))
        call csminp%put('*CULTIVAR','Ph1P',Ph1P)
        call csminp%put('*CULTIVAR','Ph1R',Ph1R)
        call csminp%put('*CULTIVAR','Ph2',Ph2)
        call csminp%put('*CULTIVAR','Ph3',Ph3)
        call csminp%put('*CULTIVAR','Ph4',Ph4)
        call csminp%put('*CULTIVAR','StkHrNO',StkHrNO)
        call csminp%put('*CULTIVAR','RTNFAC',RTNFAC)
        call csminp%put('*CULTIVAR','MinGr',MinGr)
        call csminp%put('*CULTIVAR','RES30C',RES30C)
        call csminp%put('*CULTIVAR','RLF30C',RLF30C)
        call csminp%put('*CULTIVAR','R30C2',R30C2)

!     Taro, tanier **
      CASE ('TRARO','TNARO')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           P1,P3,P4,P5,G3,G4,PHINT,PCINT,PCGRD

        call csminp%add_sec('*CULTIVAR')

        call csminp%add_var('*CULTIVAR',
     &     char_name=(/'VARNO ','VRNAME','ECONO '/),
     &     real_name=(/'P1   ','P3   ','P4   ','P5   ','G3   ',
     &                 'G4   ','PHINT','PCINT','PCGRD'/))

        call csminp%put('*CULTIVAR','VRNAME',VRNAME)
        call csminp%put('*CULTIVAR','ECONO',ECONO)
        call csminp%put('*CULTIVAR','P1',P1)
        call csminp%put('*CULTIVAR','P3',P3)
        call csminp%put('*CULTIVAR','P4',P4)
        call csminp%put('*CULTIVAR','P5',P5)
        call csminp%put('*CULTIVAR','G3',G3)
        call csminp%put('*CULTIVAR','G4',G4)
        call csminp%put('*CULTIVAR','PHINT',PHINT)
        call csminp%put('*CULTIVAR','PCINT',PCINT)
        call csminp%put('*CULTIVAR','PCGRD',PCGRD)

!!     Sunflower **
!      CASE ('SUOIL')
!        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!     &           P1,P2,P5,G2,G3,O1
!
!!    Pineapple **
!     CASE ('PIALO')
!       READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!    &           P1,P2,P3,P4,P5,P6,G2,G3,PHINT
!
      END SELECT

      VARNO = VARTY

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT (T30,'VARIETY SELECTION',/,T30,'=================',
     &     //,T43,'ECOTYPE',2X,'MATURITY',
     &      /,2X,'NO.',1X,'ENTRY',3X,'VARIETY',22X,'GROUP',5X,'GROUP',
     &      /,2X,'---',1X,'------',2X,20('-'),8X,'-------',2X,
     &           '--------')
! 110 FORMAT (A6,1X,A16,1X,A6,6X,A2)
  110 FORMAT (A6,1X,A16,7X,A6)
  120 FORMAT (I4,') ',A6,2X,A16,13X,A6,6X,A2)
  300 FORMAT (/,'  More.... press < ENTER > key')
  350 FORMAT (/,6X,'VARIETY SELECTED ===>',1X,I4,
     &        /,6X,'NEW SELECTION ?  --->',3X,' ',$)
  400 FORMAT (80A1)
  500 FORMAT (6X,'ERROR! Variety Selection must be between 1 & ',I3,/)
  510 FORMAT (6X,'ERROR! Variety Selection must be an INTEGER value',/)

! 800 FORMAT (A6,1X,A16,1X,A6,15(F6.0))
  800 FORMAT (A6,1X,A16,7X,A6,21F6.0)                     !11/8/07


  810 FORMAT (A6,1X,A16,7X,A6,20F6.0,A)         !WHCRP, BACRP 03/16/2010
C 820 FORMAT (A6,1X,A16,7X,A6,22F6.0,A)         !CSCAS        04/25/2013
  820 FORMAT (A6,1X,A16,7X,A6,21F6.0,A)         !CSCAS        02/18/2014
     
!  821 FORMAT (A6,1X,A16,7X,A6,13F6.0,A)         !CSYCA        06/05/2015 !DA 04OCT2016 Changed since LA1S variable is removed, is not used according to LPM 07MAR15 
  821 FORMAT (A6,1X,A16,7X,A6,12F6.0,A)         !CSYCA        06/05/2015 

  830 FORMAT (A6,1X,A16,7X,A6,7F6.0,A)          !WHCER, BACER 03/16/2010
  850 FORMAT (A6,1X,A16,7X,A6,43F6.0,A)
! 1050 FORMAT (A6,1X,A16,7X,A6,9F6.0,1X,I5,3F6.0)          !11/8/07
 1055 FORMAT (A6,1X,A16,7X,A6,44F6.0)                   !02/10/2009 
!!! 1055 FORMAT (A6,1X,A16,7X,A6,37F6.0,G8.0,4F6.1)       !02/10/2009 
 1060 FORMAT (A6,1X,A16,7X,A6,44F15.0)                   !02/10/2009 

!1500 FORMAT (A6,1X,A16,1X,A255)
 1500 FORMAT (A6,1X,A16,7X,A)                             !11/8/07

      END SUBROUTINE read_nc_gen
