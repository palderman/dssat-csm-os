!=======================================================================
!  read_nc_wth, Subroutine, P.D. Alderman, 04/27/2018
!  Read weather records into arrays
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  04/27/2018 PDA Adapted from IpWRec
!-----------------------------------------------------------------------
!  Called by: IPWTH_alt
!  Calls:     None
!=======================================================================

      subroutine read_nc_wth(CONTROL, MaxRecords, YRDOYWY,!Input
     &    FirstWeatherDay, LastWeatherDay, DCO2_A, PAR_A, !Output
     &    RAIN_A, RHUM_A, SRAD_A, TDEW_A, TMAX_A,         !Output
     &    TMIN_A, VAPR_A, WINDSP_A, YRDOY_A, NRecords)    !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      use csm_io
      use dssat_netcdf
      IMPLICIT NONE
      SAVE

      INTEGER MaxRecords

      TYPE (ControlType) CONTROL
      CHARACTER*1  RNMODE,mewth
      CHARACTER*6, PARAMETER :: ERRKEY = "IPWTH "
      CHARACTER*78 MSG(2)
      CHARACTER*120 LINE  
      CHARACTER*92 FILEWW

      INTEGER CENTURY, ERR, ErrCode, FOUND, LINWTH, LUNWTH, MULTI, RUN  
      INTEGER YRDOY, YRDOYW, YRDOYWY, YRDOY_start, YREND, YRSIM
      INTEGER YRDOYW_SAVE,incyd

      REAL PAR, RAIN, SRAD, TDEW, TMAX, TMIN, WINDSP, RHUM, VAPR, DCO2

      LOGICAL LongFile

!     Arrays of weather data -- up to one year stored.
      INTEGER, DIMENSION(MaxRecords) :: YRDOY_A, LineNumber
      REAL, DIMENSION(MaxRecords) :: SRAD_A, TMAX_A, TMIN_A, DCO2_A,
     &          RAIN_A, TDEW_A, WINDSP_A, PAR_A, RHUM_A, VAPR_A
      INTEGER LastRec, LastWeatherDay, NRecords
      INTEGER FirstWeatherDay

!     For free-format reads
!     Up to MAXCOL headers per line, up to 10 characters long each
      INTEGER, PARAMETER :: MAXCOL = 25
      CHARACTER*15  HEADER(MAXCOL)
!     COL keeps beginning and ending column for each header
      INTEGER COL(MAXCOL,2), ICOUNT, C1, C2, I
      real time(MaxRecords)
      real                 :: lon,lat
      integer              :: lat_i,lon_i,time_i
      integer,dimension(3) :: start,count
      integer dimid, t_size

      real round_real

      MULTI  = CONTROL % MULTI
      RNMODE = CONTROL % RNMODE
      RUN    = CONTROL % RUN
      YRDOY  = CONTROL % YRDOY
      YRSIM  = CONTROL % YRSIM

!-----------------------------------------------------------------------
!     LongFile = .FALSE.
      YRDOY_start = CONTROL % YRDOY
!-----------------------------------------------------------------------
!     Begin reading daily weather data
      NRecords = 0

!     zero out arrays

      CENTURY = INT(YRSIM / 100000.)

      SRAD_A  = -99
      TMAX_A  = -99
      TMIN_A  = -99
      RAIN_A  = -99
      TDEW_A  = -99
      WINDSP_A= -99
      PAR_A   = -99
      RHUM_A  = -99
      VAPR_A  = -99
      DCO2_A  = -99

      call nc_wth%open()

      call nc_wth%set_date(yrdoywy)
! Passing address of first element in array, but function will fill entire array
      t_size=size(YRDOY_A)
      time_i = nc_wth%z_i
      call nc_wth%read('DATE',time_i,YRDOY_A)
      call nc_wth%read('SRAD',time_i,SRAD_A)
      call nc_wth%read('TMAX',time_i,TMAX_A)
      call nc_wth%read('TMIN',time_i,TMIN_A)
      call nc_wth%read('RAIN',time_i,RAIN_A)
      call nc_wth%read('TDEW',time_i,TDEW_A)
      call nc_wth%read('WIND',time_i,WINDSP_A)
      call nc_wth%read('PAR',time_i,PAR_A)
      call nc_wth%read('RHUM',time_i,RHUM_A)
      call nc_wth%read('VAPR',time_i,VAPR_A)
      call nc_wth%read('DCO2',time_i,DCO2_A)

      NRecords = 0
      do i=1,MaxRecords
         if(YRDOY_A(i)>0)then
            NRecords = NRecords + 1
         end if
      end do

      if(cmd_arg_present('--mimic_inp'))then
         do i=1,NRecords
            
            SRAD_A(i) = round_real(SRAD_A(i),6,1)
            TMAX_A(i) = round_real(TMAX_A(i),6,1)
            TMIN_A(i) = round_real(TMIN_A(i),6,1)
            RAIN_A(i) = round_real(RAIN_A(i),6,1)
            TDEW_A(i) = round_real(TDEW_A(i),6,1)
            WINDSP_A(i) = round_real(WINDSP_A(i),6,0)
            PAR_A(i) = round_real(PAR_A(i),6,1)
            RHUM_A(i) = round_real(RHUM_A(i),6,1)
            VAPR_A(i) = round_real(VAPR_A(i),6,2)
            DCO2_A(i) = round_real(DCO2_A(i),6,1)

         end do
      end if
      
      FirstWeatherDay = YRDOY_A(1)
      LastWeatherDay = YRDOY_A(NRecords)
      lastrec = 0
      end subroutine 
!=======================================================================
!=======================================================================
! IPWTH Variables

!-----------------------------------------------------------------------
! BLANK   blank character 
! CALC_TDEW Function that calculates dew point temperature from min and
!           max temperatures and relative humidity.
! CCO2    Atmospheric CO2 concentration read from input file (ppm)
! ERRKEY  Subroutine name for error file 
! ERR  Error number for input 
! FILEIO  Filename for input file (e.g., IBSNAT35.INP) 
! FILEW   Weather data file 
! FILEWW  Pathname plus filename for weather file (e.g. UFGA7801.WTH) 
! FIRST   Indicates first call to subroutine (true or false) 
! FOUND   Indicator that good data was read from file by subroutine FIND  
!           (0- End-of-file encountered, 1 - NAME was found) 
! INCYD   Function subroutine increases/decreases date (YRDOY) 
!           based on variable INC. 
! INSI    Location code for weather data 
! ISIM    Day portion of Julian date 
! LINE    Record of data read from file 
! LINWTH  Current line read from weather file 
! LNUM    Current line number of input file 
! LUNIO   Logical unit number for FILEIO 
! LUNWTH  Unit number for weather file 
! MEWTH   Switch for method of obtaining weather data-- 'G' or 'M'- read 
!           weather data file 'S'- read SIMMETEO inputs and generate 
!           weather data 'W'- read WGEN inputs and generate weather data 
! MULTI   Current simulation year (=1 for first or single simulation, =NYRS 
!           for last seasonal simulation) 
! NYEAR   Numeric value of 7th and 8th characters in FILEW, i.e.,  
! PAR     Daily photosynthetically active radiation or photon flux density
!           (moles[quanta]/m2-d)
! PATHL   Number of characters in path name (path plus filename for FILEC) 
! PATHWT  Directory path for weather file 
! RAIN    Precipitation depth for current day (mm)
! REFHT   Reference height for wind speed (m)
! RHUM    Relative humidity (%)
! RSEED1  Random number generator seed- user input 
! SECTION Section name in input file 
! SRAD    Solar radiation (MJ/m2-d)
! TAMP    Amplitude of temperature function used to calculate soil 
!           temperatures (°C)
! TAV     Average annual soil temperature, used with TAMP to calculate soil 
!           temperature. (°C)
! TDEW    Dewpoint temperature (°C)
! TIMDIF  Integer function which calculates the number of days between two 
!           Julian dates (da)
! TMAX    Maximum daily temperature (°C)
! TMIN    Minimum daily temperature (°C)
! WINDHT  Reference height for wind speed (m)
! WINDSP  Wind speed (km/d)
! WYEAR   Weather year; current year for weather data 
! XELEV   Field elevation (not used) (m)
! XLAT    Latitude (deg.)
! XLONG   Longitude (deg.)
! YR      Year portion of date 
! YR_DOY  Function subroutoine converts date in YYDDD format to integer 
!           year (YY) and day (DDD). 
! YRDOY   Current day of simulation (YYDDD)
! YRDOYW  Julian date read from weather file (YYDDD)
! YRDOYY  Date read from weather file yesterday (YYDDD)
! YRSIM   Start of simulation date (YYDDD)
! YRSIMM  Beginning date of simulation (YYDDD)
! YRSIMMY Day before simulation date (YYDDD format) 
! YRSIMY  Day before simulation date (YYDDD format) 
!=======================================================================

