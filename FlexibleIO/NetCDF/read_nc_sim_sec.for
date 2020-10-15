C=======================================================================
C  IPSIM, Subroutine
C
C  Reads parameters related to field operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C  01/01/1990 JWJ Written
C  05/28/1993 PWW Header revision and minor changes            
C  11/19/2003 CHP Added check for MEPHO and incompatible models.
C  02/21/2006 GH  Removed crop model selection
!  10/25/2006 CHP CRMODEL from FILEX overrides MODEL in DSSATPRO 
!  05/09/2007 CHP Make Sulieman-Ritchie the default soil evaporation method
!  04/28/2008 CHP Added switch for CO2 from file (ICO2)
!  12/09/2009 CHP IPSIM separate file.  
!  02/11/2010 CHP Added checks for P model linked with crop models.

C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNSIM
C
C  LOCAL  : LN
C
C  OUTPUT : NYRS,NREPSQ,ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,
C           MESIC,MELI,MEEVP,MEINF,MEPHO,ISIMI,ISIM,IPLTI,IIRRI,IFERI,
C           IRESI,IHARI,IOX,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
C           PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,DSOILX,THETACX,
C           IEPTX,IOFFX,IAMEX,DSOILN,SOILNC,SOILNX,NEND,RIP,NRESDL,
C           DRESMG,HDLAY,HLATE
!           MESOM, METMP, MESOL, MESEV, MEGHG
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE read_nc_sim_sec(LNSIM,TITSIM,NYRS,RUN,NREPSQ,
     & ISIMI,PWDINF,PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,
     & PTX,PTTN,DSOIL,THETAC,IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,
     & SOILNX,NEND,RIP,NRESDL,DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,
     & RSEED1,LINEXP,AIRAMT,EFFIRR,CROP,FROP,MODEL,RNMODE,FILEX,
     & CONTROL, ISWITCH, UseSimCtr, FILECTL, MODELARG, YRPLT)

      USE ModuleDefs
      USE ModuleData
      USE CsvOutput
      use csm_io
      use dssat_netcdf
      IMPLICIT NONE
      SAVE

      CHARACTER*1   UPCASE,ISIMI, RNMODE
      CHARACTER*2   CROP
      CHARACTER*5   NEND,NCODE,IOFF,IAME, TEXT
      CHARACTER*6   ERRKEY,FINDCH
      CHARACTER*8   MODEL, MODELARG, CRMODEL, TRY_MODEL, Try_MODELARG
      CHARACTER*12  FILEX
      CHARACTER*16  CROPD
      CHARACTER*25  TITSIM
      CHARACTER*78  MSG(7)
      CHARACTER*120 FILECTL
      CHARACTER*128 CHARTEST

      INTEGER LNSIM,LUNEXP,ISECT,LINEXP,ISIM,NYRS,NREPSQ,FROP
      INTEGER PLDATE,PWDINF,PWDINL,HLATE,HDLAY,NRESDL
      INTEGER IFIND,LN,ERRNUM,FTYPEN,YRSIM,YEAR,RUN,RSEED1,RRSEED1
      INTEGER YRPLT

      REAL DSOIL,THETAC,DSOILN,SOILNC,SOILNX,SWPLTL,SWPLTH,SWPLTD
      REAL PTX,PTTN,DRESMG,RIP,IEPT,HPP,HRP,AIRAMT,EFFIRR, AVWAT
      REAL V_AVWAT(20)    ! Create vectors to save growth stage based irrigation
      REAL V_IMDEP(20)
      REAL V_ITHRL(20)
      REAL V_ITHRU(20), IFREQ
      INTEGER V_IRON(20), V_IFREQ(20)
      CHARACTER*5 V_IRONC(20)
      CHARACTER*5 V_IMETH(20)
      REAL V_IRAMT(20)
      REAL V_IREFF(20)
      INTEGER GSIRRIG, I, STAT, CHARLEN

      LOGICAL UseSimCtr, MulchWarn

      TYPE (SwitchType)  ISWITCH
      TYPE (ControlType) CONTROL

      CHARACTER*1  ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,MESIC
      CHARACTER*1  ICO2
      CHARACTER*1  MELI,MEEVP,MEINF,MEPHO,IPLTI,IIRRI,IFERI,IRESI,IHARI
      CHARACTER*1  ISWCHE,ISWTIL,MEHYD,MESOM, MESOL, MESEV, METMP, MEGHG
      CHARACTER*1  IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,IOX
      CHARACTER*1  IDETH,IDETR
      CHARACTER*1  IDETL    !,RNMODE
      CHARACTER*102 DSSATP
      INTEGER      NSWITCH

      PARAMETER (ERRKEY='IPSIM ')
                 FINDCH='*SIMUL'

      DATA MulchWarn /.FALSE./

      dssatp = control%dssatp

      IF (LNSIM .EQ. 0) THEN
         LNSIM   = 0
         NYRS    = 1
         NREPSQ  = 1
         ISIMI   = 'S'
         YRSIM   = -99
         RSEED1  = 2150
         ISWWAT  = 'Y'
         ISWNIT  = 'Y'
         ISWSYM  = 'Y'
         ISWPHO  = 'N'
         ISWPOT  = 'N'
         ISWDIS  = 'N'
         ISWCHE  = 'N'
         ISWTIL  = 'Y'

         IF (INDEX('FNQS',RNMODE) > 0) THEN
           ICO2 = 'D' !Default CO2 from CO2???.WDA file
         ELSE
           ICO2 = 'M' !Measured CO2 from CO2???.WDA file
         ENDIF

         MEWTH   = 'M'
         MESIC   = 'M'
         MELI    = 'E'
         MEEVP   = 'R'
         MEINF   = 'S'
         MEPHO   = 'L'
         MEHYD   = 'R'
         NSWITCH =  1
         MESOM   = 'G'
         MESOL   = '2'    !was '1'
         MESEV   = 'S'    !new Sulieman-Ritchie (2006)
         METMP   = 'D'    !DSSAT original soil temperature
!        METMP   = 'E'    ! EPIC soil temp routine.
         MEGHG   = '0'
!                   0  => DSSAT original denitrification routine
!                   1  => DayCent N2O calculation

         IPLTI   = 'R'
         IIRRI   = 'R'
         IFERI   = 'R'
         IRESI   = 'R'
         IHARI   = 'M'
         IOX     = 'N'
         FROP    =  3
         IDETO   = 'Y'
         IDETS   = 'Y'
         IDETG   = 'Y'
         IDETN   = 'N'
         IDETC   = 'N'
         IDETW   = 'N'
         IDETP   = 'N'
         IDETD   = 'N'
         IDETL   = 'N'
         IDETH   = 'N'
         IDETR   = 'Y'
         EFFIRR  = 1.00
         AVWAT  = -99.
         THETAC  = 75.0
         IEPT    = 100.0
         DSOIL   = 30.0
         DSOILN  = 30.0
         AIRAMT  = 10.0
         IOFF    = 'GS000'
         IAME    = 'IR001'
         CRMODEL = '        '
         NCODE = "-99  "
         NEND  = "-99  "
       ELSE
         call nc_filex%read('NYERS',LNSIM,NYRS)
         call nc_filex%read('NREPS',LNSIM,NREPSQ)
         call nc_filex%read('START',LNSIM,ISIMI)
         call nc_filex%read('SDATE',LNSIM,YRSIM)
         call nc_filex%read('RSEED',LNSIM,RRSEED1)
         call nc_filex%read('SNAME',LNSIM,TITSIM)
         call nc_filex%read('CRMODEL',LNSIM,CRMODEL)
         IF (INDEX('G',RNMODE) .GT. 0) NYRS = 1
         IF ((RNMODE .NE. 'Q') .OR. (RNMODE .EQ. 'Q'
     &        .AND. RUN .EQ. 1)) THEN
            RSEED1 = RRSEED1
            IF (RSEED1 .LE. 0) THEN
               RSEED1 = 2150
            ENDIF
         ENDIF
         CALL Y2K_DOY (YRSIM)
         CALL YR_DOY (YRSIM,YEAR,ISIM)

C        Read SECOND line of simulation control
C
         call nc_filex%read('WATER',LNSIM,ISWWAT)
         call nc_filex%read('NITRO',LNSIM,ISWNIT)
         call nc_filex%read('SYMBI',LNSIM,ISWSYM)
         call nc_filex%read('PHOSP',LNSIM,ISWPHO)
         call nc_filex%read('POTAS',LNSIM,ISWPOT)
         call nc_filex%read('DISES',LNSIM,ISWDIS)
         call nc_filex%read('CHEM',LNSIM,ISWCHE)
         call nc_filex%read('TILL',LNSIM,ISWTIL)
         call nc_filex%read('CTEMP',LNSIM,ICO2)

         ISWWAT = UPCASE(ISWWAT)
         ISWNIT = UPCASE(ISWNIT)
         ISWSYM = UPCASE(ISWSYM)
         ISWPHO = UPCASE(ISWPHO)
         ISWPOT = UPCASE(ISWPOT)
         ISWDIS = UPCASE(ISWDIS)
         ISWCHE = UPCASE(ISWCHE)
         ISWTIL = UPCASE(ISWTIL)
         ICO2   = UPCASE(ICO2)

!        IF (INDEX ('BNSBPNPECHPPVBCPCBFB',CROP) .EQ. 0) THEN
         SELECT CASE (CROP)
         CASE ('BN','SB','PN','PE','CH','PP',
     &          'VB','CP','CB','FB','GB','LT','AL')
C     &          'VB','CP','CB','FB','GB','LT')
C  KJB, ADDED AL TO THIS, SO N-FIXATION WORKS FOR ALFALFA
!          Do nothing -- these crops fix N and can have Y or N
         CASE DEFAULT; ISWSYM = 'N'  !other crops don't have a choice
         END SELECT
!        ENDIF
         IF (ISWCHE .EQ. ' ') THEN
            ISWCHE = 'N'
         ENDIF
         IF (ISWTIL .EQ. ' ') THEN
            ISWTIL = 'N'
         ENDIF
         IF (ISWWAT .EQ. 'N') THEN
            ISWNIT = 'N'
            ISWPHO = 'N'
!            ISWCHE = 'N'
         ENDIF

         IF (INDEX('FNQS',RNMODE) > 0) THEN
!          For sequence, seasonal runs, default CO2 uses static value
           IF (INDEX ('WMD', ICO2) < 1) ICO2 = 'D'
         ELSE
!          For experimental runs, default CO2 uses measured values
           IF (INDEX ('WMD', ICO2) < 1) ICO2 = 'M'
         ENDIF

C        Read THIRD line of simulation control
C
         call nc_filex%read('WTHER',LNSIM,MEWTH)
         call nc_filex%read('INCON',LNSIM,MESIC)
         call nc_filex%read('LIGHT',LNSIM,MELI)
         call nc_filex%read('EVAPO',LNSIM,MEEVP)
         call nc_filex%read('INFIL',LNSIM,MEINF)
         call nc_filex%read('PHOTO',LNSIM,MEPHO)
         call nc_filex%read('HYDRO',LNSIM,MEHYD)
         call nc_filex%read('NSWIT',LNSIM,NSWITCH)
         call nc_filex%read('MESOM',LNSIM,MESOM)
         call nc_filex%read('MESEV',LNSIM,MESEV)
         call nc_filex%read('MESOL',LNSIM,MESOL)
         call nc_filex%read('METMP',LNSIM,METMP)
         call nc_filex%read('MEGHG',LNSIM,MEGHG)

         MEWTH = UPCASE(MEWTH)
         MESIC = UPCASE(MESIC)
         MELI  = UPCASE(MELI)
         MEEVP = UPCASE(MEEVP)
         MEINF = UPCASE(MEINF)
         MEPHO = UPCASE(MEPHO)
         MESOM = UPCASE(MESOM)
         MEHYD = UPCASE(MEHYD)
         MESEV = UPCASE(MESEV)
         METMP = UPCASE(METMP)
         MEGHG = UPCASE(MEGHG)

         IF (INDEX('PG',MESOM) .EQ. 0) THEN
            MESOM = 'G'
         ENDIF
         
         IF (INDEX('G',MESOM)   > 0 .AND. 
     &       INDEX('FQ',RNMODE) > 0 .AND. 
     &       INDEX('N',MEINF)  == 0) THEN
           MEINF = 'N'
           IF (.NOT. MulchWarn) THEN
             MSG(1)=
     &  "Long-term simulation of surface residues may not be accurate"
             MSG(2)=
     &  "when using Godwin soil organic matter module.  The effects of"
             MSG(3)=
     &  "a surface mulch layer on runoff and evaporation will " //
     &       "not be modeled."  
             MSG(4)=
     &  "Simulation Options/Methods/Infiltration = 'No mulch effects'"
             MSG(5)=
     &  "You may want to consider using the Parton (CENTURY) method of"
             MSG(6)= "modeling soil organic matter."
             CALL WARNING(6,ERRKEY,MSG)
             MulchWarn = .TRUE.
           ENDIF
         ENDIF

! ** DEFAULT MESOL = 2 ** 3/26/2007
!  MESOL = '1' Original soil layer distribution. Calls LYRSET.
!  MESOL = '2' New soil layer distribution. Calls LYRSET2.
!  MESOL = '3' User specified soil layer distribution. Calls LYRSET3.
         IF (INDEX('123',MESOL) < 1) THEN
            MESOL = '2'
         ENDIF

!        3/27/2016 chp Default soil temperature method is EPIC
!        7/21/2016 chp Default soil temperature method is DSSAT, per GH
         IF (INDEX('ED',METMP) < 1) METMP = 'D'
!        IF (INDEX('ED',METMP) < 1) METMP = 'E'

!        Default greenhouse gas method is DSSAT
         IF (INDEX('01',MEGHG) < 1) MEGHG = '0'

         SELECT CASE(MESEV)
         CASE('R','r'); MESEV = 'R'
         CASE DEFAULT;  MESEV = 'S'   !Default method -- use NEW
         END SELECT

         IF (MEEVP == 'Z' .AND. MEPHO /= 'L') CALL ERROR(ERRKEY,3,' ',0)

         IF (MEHYD .EQ. ' ') THEN
            MEHYD = 'R'
         ENDIF

         IF (NSWITCH .LE. 0 .AND. ISWNIT .EQ. 'Y') THEN
           NSWITCH = 1
         ENDIF
C
C        Read FOURTH line of simulation control
C
         call nc_filex%read('PLANT',LNSIM,IPLTI)
         call nc_filex%read('IRRIG',LNSIM,IIRRI)
         call nc_filex%read('FERTI',LNSIM,IFERI)
         call nc_filex%read('RESID',LNSIM,IRESI)
         call nc_filex%read('HARVS',LNSIM,IHARI)

         IPLTI = UPCASE(IPLTI)
         IIRRI = UPCASE(IIRRI)
         IFERI = UPCASE(IFERI)
         IRESI = UPCASE(IRESI)
         IHARI = UPCASE(IHARI)

         IF ((INDEX('CSPT',CROP)) .GT. 0) THEN
           IF (IHARI .EQ. 'A') THEN
              CALL ERROR (ERRKEY,4,FILEX,LINEXP)
           ENDIF
         ENDIF

         IF ((INDEX('PT',CROP)) .GT. 0) THEN
           IF (IPLTI .EQ. 'A') THEN
              CALL ERROR (ERRKEY,5,FILEX,LINEXP)
           ENDIF
         ENDIF
C
C        Read FIFTH line of simulation control
C
         IF (INDEX('FQ',RNMODE) < 1 .OR. RUN == 1) THEN
            call nc_filex%read('FNAME',LNSIM,IOX)
            call nc_filex%read('OVVEW',LNSIM,IDETO)
            call nc_filex%read('SUMRY',LNSIM,IDETS)
            call nc_filex%read('FROPT',LNSIM,FROP)
            call nc_filex%read('GROUT',LNSIM,IDETG)
            call nc_filex%read('CAOUT',LNSIM,IDETC)
            call nc_filex%read('WAOUT',LNSIM,IDETW)
            call nc_filex%read('NIOUT',LNSIM,IDETN)
            call nc_filex%read('MIOUT',LNSIM,IDETP)
            call nc_filex%read('DIOUT',LNSIM,IDETD)
            call nc_filex%read('LONG',LNSIM,IDETL)
            if(IDETL .eq. ' ')
     &          call nc_filex%read('VBOSE',LNSIM,IDETL)
            call nc_filex%read('CHOUT',LNSIM,IDETH)
            call nc_filex%read('OPOUT',LNSIM,IDETR)
            call nc_filex%read('FMOPT',LNSIM,FMOPT)

            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IOX   = UPCASE(IOX)
            IDETO = UPCASE(IDETO)
            IDETS = UPCASE(IDETS)
            IDETG = UPCASE(IDETG)
            IDETC = UPCASE(IDETC)
            IDETW = UPCASE(IDETW)
            IDETN = UPCASE(IDETN)
            IDETP = UPCASE(IDETP)
            IDETD = UPCASE(IDETD)    
            FMOPT = UPCASE(FMOPT) ! VSH
!           FMOPT = 'A': ASCII format output
!           FMOPT = 'C': CSV format output
!           By default, use ASCII outputs
            IF (INDEX('CA',FMOPT) < 1) FMOPT = 'A'

!           IDETL = VBOSE.
!             0  Only Summary.OUT
!             N  Minimal output
!             Y  Normal output
!             D  Detailed output
!             A  All outputs
            IF (IDETL .EQ. ' ') THEN
               IDETL = 'N'
            ENDIF
            IDETL = UPCASE(IDETL)
            IF (IDETH .EQ. ' ') THEN
               IDETH = 'N'
            ENDIF
            IDETH = UPCASE(IDETH)
            IF (IDETR .EQ. ' ') THEN
               IDETR = 'Y'
            ENDIF
            IDETR = UPCASE(IDETR)

            IDETL = '0'
!           Verbose output switch
            IF (IDETL == '0') THEN
!             VBOSE = zero, suppress all output except Summary and Evaluate
              IDETS = 'Y'
              IDETG = 'N' 
              IDETC = 'N' 
              IDETW = 'N' 
              IDETN = 'N' 
              IDETP = 'N' 
              IDETD = 'N' 
              IDETH = 'N' 
              IDETR = 'N' 
              IDETO = 'E'
!             Seasonal and spatial runs do not get evaluate file when IDETL=0
              IF (INDEX('SN',RNMODE) > 0) IDETO = 'N'
            ELSEIF (IDETL == 'A') THEN
!             VBOSE = 'A', generate all output
              IDETS = 'A'
              IDETO = 'Y'
              IDETG = 'Y' 
              IDETC = 'Y' 
              IDETW = 'Y' 
              IDETN = 'Y' 
              IDETP = 'Y' 
              IDETD = 'Y' 
              IDETH = 'Y' 
              IDETR = 'Y' 
!             Set IDETL back to "D" so no need for changes elsewhere
!             IDETL = 'D' 
              FROP  = 1
            ENDIF

            IF (FROP .LE. 0) FROP = 10
         ENDIF
C
C        Read SIXTH line of simulation control
C
         call nc_filex%read('PFRST',LNSIM,PWDINF)
         call nc_filex%read('PLAST',LNSIM,PWDINL)
         call nc_filex%read('PH2OL',LNSIM,SWPLTL)
         call nc_filex%read('PH2OU',LNSIM,SWPLTH)
         call nc_filex%read('PH2OD',LNSIM,SWPLTD)
         call nc_filex%read('PSTMX',LNSIM,PTX)
         call nc_filex%read('PSTMN',LNSIM,PTTN)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (PWDINF .LT. 1000) PWDINF = YEAR * 1000 + PWDINF
            IF (PWDINL .LT. 1000) PWDINL = YEAR * 1000 + PWDINL
            CALL Y2K_DOY (PWDINF)
            CALL Y2K_DOY (PWDINL)
C
C           Read SEVENTH line of simulation control
C
         V_IMDEP = -99      ! Assighn default values to variable
         V_ITHRL = -99
         V_ITHRU = -99
         V_IRON = -99
!     V_IMETH (I) = -99
         V_IRAMT = -99
         V_IREFF = -99
         V_AVWAT = -99

         call nc_filex%read('IMDEP',LNSIM,V_IMDEP(1))
         do i=1,size(V_IMDEP)
             if(V_IMDEP(i)<0)then
                 GSIRRIG = i - 1
                 exit
             end if
         end do
         call nc_filex%read('ITHRL',LNSIM,V_ITHRL(1))
         call nc_filex%read('ITHRU',LNSIM,V_ITHRU(1))
         call nc_filex%read('IROFF',LNSIM,V_IRONC(1))
         do i=1,GSIRRIG
            READ(V_IRONC(i)(4:5), *, IOSTAT = STAT) V_IRON(i)
         end do
         call nc_filex%read('IMETH',LNSIM,V_IMETH(1))
         call nc_filex%read('IRAMT',LNSIM,V_IRAMT(1))
         call nc_filex%read('IREFF',LNSIM,V_IREFF(1))

           DSOIL  = V_IMDEP(1)                         ! Save value of first line as default for compatibility with old files
           THETAC = V_ITHRL(1)
           IEPT   = V_ITHRU(1)
           IOFF   = V_IRONC(1)
           IAME   = V_IMETH(1)
           AIRAMT = V_IRAMT(1)
           EFFIRR = V_IREFF(1)
           AVWAT  = V_AVWAT(1)
           IFREQ  = FLOAT(V_IFREQ(1))

           SAVE_data % MGMT % V_IMDEP = V_IMDEP
           SAVE_data % MGMT % V_ITHRL = V_ITHRL
           SAVE_data % MGMT % V_ITHRU = V_ITHRU
           SAVE_data % MGMT % V_IRONC = V_IRONC
           SAVE_data % MGMT % V_IRON  = V_IRON
           SAVE_data % MGMT % V_IRAMT = V_IRAMT
           SAVE_data % MGMT % V_IREFF = V_IREFF
           SAVE_data % MGMT % V_AVWAT = V_AVWAT
           SAVE_data % MGMT % V_IFREQ = V_IFREQ
           SAVE_data % MGMT % GSIRRIG = GSIRRIG

C
C           Read EIGHTH line of simulation control
C
           call nc_filex%read('NMDEP',LNSIM,DSOILN)
           call nc_filex%read('NMTHR',LNSIM,SOILNC)
           call nc_filex%read('NAMNT',LNSIM,SOILNX)
           call nc_filex%read('NCODE',LNSIM,NCODE)
           call nc_filex%read('NAOFF',LNSIM,NEND)
           READ (NCODE,70,IOSTAT=ERRNUM) FTYPEN

C
C           Read NINTH line of simulation control
C
           call nc_filex%read('RIPCN',LNSIM,RIP)
           call nc_filex%read('RTIME',LNSIM,NRESDL)
           call nc_filex%read('RIDEP',LNSIM,DRESMG)
C
C           Read TENTH line of simulation control
C
           call nc_filex%read('HFRST',LNSIM,HDLAY)
           call nc_filex%read('HLAST',LNSIM,HLATE)
           call nc_filex%read('HPCNP',LNSIM,HPP)
           call nc_filex%read('HPCNR',LNSIM,HRP)
            CALL Y2K_DOY (HLATE)
            IF (HPP   .LT. 0.0)  HPP   = 100.
            IF (HRP   .LT. 0.0)  HRP   = 0.0

            if(PWDINF < 0) PWDINF  =   1
            if(PWDINL < 0) PWDINL  =   366
            if(SWPLTL < 0) SWPLTL  =   1.0
            if(SWPLTH < 0) SWPLTH =   100.0
            if(SWPLTD < 0) SWPLTD =   200.0
            if(PTX < 0)    PTX    =   50.0
            if(PTTN < 0) PTTN   =   1.0
            if(DSOIL < 0) DSOIL   =   200.0
            if(THETAC < 0) THETAC  =   10.0
            if(IEPT < 0) IEPT   =   100.0
         END IF

C-----------------------------------------------------------------------
C    Select Model Name and Path -- order of priority:
!     CTRMODEL is value from control file override -- this is used
!         over all other values if valid. (Done in Default_SimControls)
!     CRMODEL is read from FILEX.  Use this if no control file.  
!     MODELARG is from command line argument list. Third priority. 
!     Last, use value from DSSATPRO.v??.
C-----------------------------------------------------------------------
!     First check model name from FILEX
      TRY_MODEL = CRMODEL
      CALL MODEL_NAME (CROP, DSSATP, TRY_MODEL, MODEL)

!     If FILEX model name was not acceptable, then try the 
!       model name read from command line.  If this is not OK, 
!       MODEL contains value from DSSATPRO file
      IF (TRY_MODEL /= MODEL) THEN
!       Fallow must be associated with CRGRO model (for now)
        IF (CROP == 'FA') THEN
          Try_MODELARG(1:5) = "CRGRO"
        ELSE
          Try_MODELARG = MODELARG
        ENDIF
        CALL MODEL_NAME (CROP, DSSATP, Try_MODELARG, MODEL)
      ENDIF

      IF (MEPHO .EQ. 'L' .AND. MODEL(1:5) .NE. 'CRGRO' 
     &  .and. model(1:5) .ne. 'PRFRM' ) THEN
        MEPHO = 'C'
        WRITE(MSG(1),80)
        WRITE (MSG(2),81) MODEL(1:5)
        CALL WARNING(2, "IPEXP ", MSG)

   80 FORMAT('Photosynthesis method (PHOTO in FILEX) has been changed')
   81 FORMAT('from "L" to "C" for compatibility with crop model, '
     &            ,A5,'.') 
      ENDIF


!-----------------------------------------------------------
!       Write simulation control data to csm_input structure
!-----------------------------------------------------------

      call csminp%add_sec('*SIMULATION CONTROL')

      call csminp%add_var('*SIMULATION CONTROL',
     &     char_name=(/'ISIMI  ','TITSIM ','MODEL  ','ISWWAT ',
     &       'ISWNIT ','ISWSYM ','ISWPHO ','ISWPOT ','ISWDIS ',
     &       'ISWCHE ','ISWTIL ','ICO2   ','MEWTH  ','MESIC  ',
     &       'MELI   ','MEEVP  ','MEINF  ','MEPHO  ','MEHYD  ',
     &       'MESOM  ','MESEV  ','MESOL  ','METMP  ','IPLTI  ',
     &       'IIRRI  ','IFERI  ','IRESI  ','IHARI  ','IOX    ',
     &       'IDETO  ','IDETS  ','IDETG  ','IDETC  ','IDETW  ',
     &       'IDETN  ','IDETP  ','IDETD  ','IDETL  ','IDETH  ',
     &       'IDETR  ','IOFF   ','IAME   ','NCODE  ','NEND   '/),
     &     int_name=(/'LNSIM  ','NYRS   ','NREPSQ ','YRSIM  ',
     &      'RSEED1 ','NSWITCH','FROP   ','FTYPEN ','PWDINF ',
     &      'PWDINL ','NRESDL ','HDLAY  ','HLATE  '/),
     &     real_name=(/'SWPLTL','SWPLTH','SWPLTD','PTX   ','PTTN  ',
     &        'DSOIL ','THETAC','IEPT  ','AIRAMT','EFFIRR','DSOILN',
     &     'SOILNC','SOILNX','RIP   ','DRESMG','HPP   ','HRP   '/))

      call csminp%put('*SIMULATION CONTROL','LNSIM',LN)
      call csminp%put('*SIMULATION CONTROL','NYRS',NYRS)
      call csminp%put('*SIMULATION CONTROL','NREPSQ',NREPSQ)
      call csminp%put('*SIMULATION CONTROL','ISIMI',ISIMI)
      call csminp%put('*SIMULATION CONTROL','YRSIM',YRSIM)
      call csminp%put('*SIMULATION CONTROL','RSEED1',RSEED1)
      call csminp%put('*SIMULATION CONTROL','TITSIM',TITSIM)
      call csminp%put('*SIMULATION CONTROL','MODEL',MODEL)
      call csminp%put('*SIMULATION CONTROL','ISWWAT',ISWWAT)
      call csminp%put('*SIMULATION CONTROL','ISWNIT',ISWNIT)
      call csminp%put('*SIMULATION CONTROL','ISWSYM',ISWSYM)
      call csminp%put('*SIMULATION CONTROL','ISWPHO',ISWPHO)
      call csminp%put('*SIMULATION CONTROL','ISWPOT',ISWPOT)
      call csminp%put('*SIMULATION CONTROL','ISWDIS',ISWDIS)
      call csminp%put('*SIMULATION CONTROL','ISWCHE',ISWCHE)
      call csminp%put('*SIMULATION CONTROL','ISWTIL',ISWTIL)
      call csminp%put('*SIMULATION CONTROL','ICO2',ICO2)
      call csminp%put('*SIMULATION CONTROL','MEWTH',MEWTH)
      call csminp%put('*SIMULATION CONTROL','MESIC',MESIC)
      call csminp%put('*SIMULATION CONTROL','MELI',MELI)
      call csminp%put('*SIMULATION CONTROL','MEEVP',MEEVP)
      call csminp%put('*SIMULATION CONTROL','MEINF',MEINF)
      call csminp%put('*SIMULATION CONTROL','MEPHO',MEPHO)
      call csminp%put('*SIMULATION CONTROL','MEHYD',MEHYD)
      call csminp%put('*SIMULATION CONTROL','MESOM',MESOM)
      call csminp%put('*SIMULATION CONTROL','MESEV',MESEV)
      call csminp%put('*SIMULATION CONTROL','MESOL',MESOL)
      call csminp%put('*SIMULATION CONTROL','METMP',METMP)
      call csminp%put('*SIMULATION CONTROL','NSWITCH',NSWITCH)
      call csminp%put('*SIMULATION CONTROL','IPLTI',IPLTI)
      call csminp%put('*SIMULATION CONTROL','IIRRI',IIRRI)
      call csminp%put('*SIMULATION CONTROL','IFERI',IFERI)
      call csminp%put('*SIMULATION CONTROL','IRESI',IRESI)
      call csminp%put('*SIMULATION CONTROL','IHARI',IHARI)
      call csminp%put('*SIMULATION CONTROL','IOX',IOX)
      call csminp%put('*SIMULATION CONTROL','IDETO',IDETO)
      call csminp%put('*SIMULATION CONTROL','IDETS',IDETS)
      call csminp%put('*SIMULATION CONTROL','FROP',FROP)
      call csminp%put('*SIMULATION CONTROL','IDETG',IDETG)
      call csminp%put('*SIMULATION CONTROL','IDETC',IDETC)
      call csminp%put('*SIMULATION CONTROL','IDETW',IDETW)
      call csminp%put('*SIMULATION CONTROL','IDETN',IDETN)
      call csminp%put('*SIMULATION CONTROL','IDETP',IDETP)
      call csminp%put('*SIMULATION CONTROL','IDETD',IDETD)
      call csminp%put('*SIMULATION CONTROL','IDETL',IDETL)
      call csminp%put('*SIMULATION CONTROL','IDETH',IDETH)
      call csminp%put('*SIMULATION CONTROL','IDETR',IDETR)
      call csminp%put('*SIMULATION CONTROL','PWDINF',PWDINF)
      call csminp%put('*SIMULATION CONTROL','PWDINL',PWDINL)
      call csminp%put('*SIMULATION CONTROL','SWPLTL',SWPLTL)
      call csminp%put('*SIMULATION CONTROL','SWPLTH',SWPLTH)
      call csminp%put('*SIMULATION CONTROL','SWPLTD',SWPLTD)
      call csminp%put('*SIMULATION CONTROL','PTX',PTX)
      call csminp%put('*SIMULATION CONTROL','PTTN',PTTN)
      call csminp%put('*SIMULATION CONTROL','DSOIL',DSOIL)
      call csminp%put('*SIMULATION CONTROL','THETAC',THETAC)
      call csminp%put('*SIMULATION CONTROL','IEPT',IEPT)
      call csminp%put('*SIMULATION CONTROL','IOFF',IOFF)
      call csminp%put('*SIMULATION CONTROL','IAME',IAME)
      call csminp%put('*SIMULATION CONTROL','AIRAMT',AIRAMT)
      call csminp%put('*SIMULATION CONTROL','EFFIRR',EFFIRR)
      call csminp%put('*SIMULATION CONTROL','NCODE',NCODE)
      call csminp%put('*SIMULATION CONTROL','NEND',NEND)
      call csminp%put('*SIMULATION CONTROL','FTYPEN',FTYPEN)
      call csminp%put('*SIMULATION CONTROL','DSOILN',DSOILN)
      call csminp%put('*SIMULATION CONTROL','SOILNC',SOILNC)
      call csminp%put('*SIMULATION CONTROL','SOILNX',SOILNX)
      call csminp%put('*SIMULATION CONTROL','RIP',RIP)
      call csminp%put('*SIMULATION CONTROL','NRESDL',NRESDL)
      call csminp%put('*SIMULATION CONTROL','DRESMG',DRESMG)
      call csminp%put('*SIMULATION CONTROL','HDLAY',HDLAY)
      call csminp%put('*SIMULATION CONTROL','HLATE',HLATE)
      call csminp%put('*SIMULATION CONTROL','HPP',HPP)
      call csminp%put('*SIMULATION CONTROL','HRP',HRP)

      CALL FILL_ISWITCH(
     &      CONTROL, ISWITCH, FROP, MODEL, NYRS, RNMODE)

!     Planting date needed for generic start of simulation
      SELECT CASE(IPLTI)
      CASE('R'); PLDATE = YRPLT
      CASE('A'); PLDATE = PWDINF
      END SELECT

!     Check Simulation control file for control overrides 
      CALL Default_SimControls(
     &    CONTROL, CRMODEL, DSSATP, FILECTL, ISWITCH,     !Input
     &    MODELARG, PLDATE,                               !Input
     &    UseSimCtr, MODEL)                               !Output

      IF (UseSimCtr) THEN
        IOX     = ISWITCH % FNAME 
        ISIMI   = ISWITCH % ISIMI 
        ISWWAT  = ISWITCH % ISWWAT
        ISWNIT  = ISWITCH % ISWNIT
        ISWSYM  = ISWITCH % ISWSYM
        ISWPHO  = ISWITCH % ISWPHO
        ISWPOT  = ISWITCH % ISWPOT
        ISWDIS  = ISWITCH % ISWDIS
        ISWCHE  = ISWITCH % ISWCHE
        ISWTIL  = ISWITCH % ISWTIL
        ICO2    = ISWITCH % ICO2
        MEWTH   = ISWITCH % MEWTH 
        MESOM   = ISWITCH % MESOM 
        MELI    = ISWITCH % MELI  
        MEEVP   = ISWITCH % MEEVP 
        MEINF   = ISWITCH % MEINF 
        MEPHO   = ISWITCH % MEPHO 
        MEHYD   = ISWITCH % MEHYD 
        MESEV   = ISWITCH % MESEV 
        MESOL   = ISWITCH % MESOL 
        METMP   = ISWITCH % METMP 
        MEGHG   = ISWITCH % MEGHG 
        IPLTI   = ISWITCH % IPLTI 
        IIRRI   = ISWITCH % IIRRI 
        IFERI   = ISWITCH % IFERI 
        IRESI   = ISWITCH % IRESI 
        IHARI   = ISWITCH % IHARI 
        IDETO   = ISWITCH % IDETO 
        IDETS   = ISWITCH % IDETS 
        IDETG   = ISWITCH % IDETG 
        IDETC   = ISWITCH % IDETC 
        IDETW   = ISWITCH % IDETW 
        IDETN   = ISWITCH % IDETN 
        IDETP   = ISWITCH % IDETP 
        IDETD   = ISWITCH % IDETD 
        IDETL   = ISWITCH % IDETL 
        IDETH   = ISWITCH % IDETH 
        IDETR   = ISWITCH % IDETR 
        NSWITCH = ISWITCH % NSWI  
        FMOPT   = ISWITCH % FMOPT   ! VSH   
      
        NYRS  = CONTROL % NYRS  
        YRSIM = CONTROL % YRSIM 
        MODEL = CONTROL % MODEL 
!       MESIC = CONTROL % MESIC     
        FROP  = CONTROL % FROP

        call csminp%put('*SIMULATION CONTROL','NYRS',NYRS)
        call csminp%put('*SIMULATION CONTROL','ISIMI',ISIMI)
        call csminp%put('*SIMULATION CONTROL','YRSIM',YRSIM)
        call csminp%put('*SIMULATION CONTROL','MODEL',MODEL)
        call csminp%put('*SIMULATION CONTROL','ISWWAT',ISWWAT)
        call csminp%put('*SIMULATION CONTROL','ISWNIT',ISWNIT)
        call csminp%put('*SIMULATION CONTROL','ISWSYM',ISWSYM)
        call csminp%put('*SIMULATION CONTROL','ISWPHO',ISWPHO)
        call csminp%put('*SIMULATION CONTROL','ISWPOT',ISWPOT)
        call csminp%put('*SIMULATION CONTROL','ISWDIS',ISWDIS)
        call csminp%put('*SIMULATION CONTROL','ISWCHE',ISWCHE)
        call csminp%put('*SIMULATION CONTROL','ISWTIL',ISWTIL)
        call csminp%put('*SIMULATION CONTROL','ICO2',ICO2)
        call csminp%put('*SIMULATION CONTROL','MEWTH',MEWTH)
!        call csminp%put('*SIMULATION CONTROL','MESIC',MESIC)
        call csminp%put('*SIMULATION CONTROL','MELI',MELI)
        call csminp%put('*SIMULATION CONTROL','MEEVP',MEEVP)
        call csminp%put('*SIMULATION CONTROL','MEINF',MEINF)
        call csminp%put('*SIMULATION CONTROL','MEPHO',MEPHO)
        call csminp%put('*SIMULATION CONTROL','MEHYD',MEHYD)
        call csminp%put('*SIMULATION CONTROL','MESOM',MESOM)
        call csminp%put('*SIMULATION CONTROL','MESEV',MESEV)
        call csminp%put('*SIMULATION CONTROL','MESOL',MESOL)
        call csminp%put('*SIMULATION CONTROL','METMP',METMP)
        call csminp%put('*SIMULATION CONTROL','NSWITCH',NSWITCH)
        call csminp%put('*SIMULATION CONTROL','IPLTI',IPLTI)
        call csminp%put('*SIMULATION CONTROL','IIRRI',IIRRI)
        call csminp%put('*SIMULATION CONTROL','IFERI',IFERI)
        call csminp%put('*SIMULATION CONTROL','IRESI',IRESI)
        call csminp%put('*SIMULATION CONTROL','IHARI',IHARI)
        call csminp%put('*SIMULATION CONTROL','IOX',IOX)
        call csminp%put('*SIMULATION CONTROL','IDETO',IDETO)
        call csminp%put('*SIMULATION CONTROL','IDETS',IDETS)
        call csminp%put('*SIMULATION CONTROL','FROP',FROP)
        call csminp%put('*SIMULATION CONTROL','IDETG',IDETG)
        call csminp%put('*SIMULATION CONTROL','IDETC',IDETC)
        call csminp%put('*SIMULATION CONTROL','IDETW',IDETW)
        call csminp%put('*SIMULATION CONTROL','IDETN',IDETN)
        call csminp%put('*SIMULATION CONTROL','IDETP',IDETP)
        call csminp%put('*SIMULATION CONTROL','IDETD',IDETD)
        call csminp%put('*SIMULATION CONTROL','IDETL',IDETL)
        call csminp%put('*SIMULATION CONTROL','IDETH',IDETH)
        call csminp%put('*SIMULATION CONTROL','IDETR',IDETR)
        call csminp%put('*SIMULATION CONTROL','RIP',RIP)
        call csminp%put('*SIMULATION CONTROL','NRESDL',NRESDL)
        call csminp%put('*SIMULATION CONTROL','DRESMG',DRESMG)
        call csminp%put('*SIMULATION CONTROL','HDLAY',HDLAY)
        call csminp%put('*SIMULATION CONTROL','HLATE',HLATE)
        call csminp%put('*SIMULATION CONTROL','HPP',HPP)
        call csminp%put('*SIMULATION CONTROL','HRP',HRP)

      ENDIF

      CALL PUT(CONTROL)  
      CALL PUT(ISWITCH)

!     --------------------------------------------------------------------
!     Check for N model compatible with crop model
      IF (ISWNIT /= 'N') THEN
        SELECT CASE(MODEL(1:5))
        CASE ('SALUS', 'SCCAN', 'SCCSP')
!           N model has NOT been linked for these models
!           Print a warning message.
            CALL GET_CROPD(CROP, CROPD)
            CROPD = ADJUSTL(CROPD)

            WRITE(MSG(1),
     &         '("Nitrogen dynamics model has not been developed for "
     &         ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
!            MSG(2)="Model will run if soils and species P data" //
!     &         " are supplied."
!            MSG(3)="User must verify validity of crop response."
            MSG(2)="Please contact the CSM development team if " // 
     &         "you wish to "
            WRITE(MSG(3),'("contribute to development of a N model for "
     &          ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
            MSG(4) = "N simulation will be switched off."
            CALL WARNING(4,ERRKEY,MSG)
            ISWNIT = 'N'
            ISWPHO = 'N'
            ISWPOT = 'N'
!           CALL ERROR('IPSIM', 6, "", 0)
        END SELECT
      ENDIF

!     --------------------------------------------------------------------
!     Check for phosphorus model compatible with crop model
!      IF (ISWPHO /= 'N') THEN
!       Check for validity of P model for this crop
!        SELECT CASE(MODEL(1:5))
        !CASE('CRGRO','MZCER','RICER')
        !  SELECT CASE(CONTROL % CROP)
        !  CASE('SB','FA','MZ','RI','PN') 
!           Phosphorus model has been enabled and tested for these crops, do nothing
         
! MA (19dec2013) to test P coupling to SG ceres 
       IF (ISWPHO /= 'N') THEN
        SELECT CASE(MODEL(1:5))
        CASE('CRGRO','MZCER','RICER','SGCER')
          SELECT CASE(CONTROL % CROP)
          CASE('SB','FA','MZ','RI','PN','SG') 
!           Phosphorus model has been enabled and tested for these crops, do nothing

          CASE DEFAULT
!           P model has NOT been tested for the remainder of the CROPGRO crops
!           Print a warning message.
            CALL GET_CROPD(CROP, CROPD)
            CROPD = ADJUSTL(CROPD)

            WRITE(MSG(1),
     &         '("Phosphorus model has not been tested for "
     &         ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
!            MSG(2)="Model will run if soils and species P data" //
!     &         " are supplied."
!            MSG(3)="User must verify validity of crop response."
            MSG(2)="Please contact the CSM development team if " // 
     &         "you wish to "
            WRITE(MSG(3),'("contribute to development of a P model for "
     &          ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
            CALL WARNING(3,ERRKEY,MSG)
            CALL ERROR('IPSIM', 6, "", 0)
          END SELECT

        CASE DEFAULT
!         Crop model has not been linked to P model.
!         Print a warning message. stop the run.
          WRITE(MSG(1),
     &       '("Phosphorus model has not been enabled for ",
     &       A5," model.")') MODEL(1:5)
          MSG(2)="Please contact the CSM development team if you " //
     &          "wish to contribute to "
          WRITE(MSG(3),'("development of a P model for ",A5,".")')
     &        MODEL(1:5)
          CALL WARNING(3,ERRKEY,MSG)
          CALL ERROR('IPSIM', 6, "", 0)
        END SELECT
      ENDIF

!     --------------------------------------------------------------------
!     Check for potassium model compatible with crop model
      IF (ISWPOT /= 'N') THEN
!       Check for validity of K model for this crop
        SELECT CASE(MODEL(1:5))
        CASE('MZCER','RICER')
!          SELECT CASE(CONTROL % CROP)
!          CASE('MZ','RI') 
!!           Potassium model has been enabled and tested for these crops, do nothing
!
!          CASE DEFAULT
!           K model has NOT been tested for the remainder of the CROPGRO crops
!           Print a warning message, but allow the user to continue.
            CALL GET_CROPD(CROP, CROPD)
            CROPD = ADJUSTL(CROPD)

            WRITE(MSG(1),
     &         '("Potassium model has not been tested for "
     &         ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
!            MSG(2)="Model will run if soils and species K data" //
!     &         " are supplied."
!            MSG(3)="User must verify validity of crop response."
            MSG(2)="Please contact the CSM development team if " // 
     &         "you wish to "
            WRITE(MSG(3),'("contribute to development of a K model for "
     &          ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
            CALL WARNING(3,ERRKEY,MSG)
            CALL ERROR('IPSIM', 7, "", 0)
!          END SELECT

        CASE DEFAULT
!         Crop model has not been linked to K model.
!         Print a warning message. stop the run.
          WRITE(MSG(1),
     &       '("Potassium model has not been enabled for ",
     &       A5," model.")') MODEL(1:5)
          MSG(2)="Please contact the CSM development team if you " //
     &          "wish to contribute to "
          WRITE(MSG(3),'("development of a K model for ",A5,".")')
     &        MODEL(1:5)
          CALL WARNING(3,ERRKEY,MSG)
          CALL ERROR('IPSIM', 7, "", 0)
        END SELECT
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  70  FORMAT (3X,I2)

      END SUBROUTINE read_nc_sim_sec
