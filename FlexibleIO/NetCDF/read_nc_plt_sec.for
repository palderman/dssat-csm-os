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

      SUBROUTINE read_nc_plt_sec (
     &     LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,MODEL,
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
         call nc_filex%read('PDATE',LNPLT,YRPLT)
         call nc_filex%read('EDATE',LNPLT,IEMRG)
         call nc_filex%read('PPOP',LNPLT,PLANTS)
         call nc_filex%read('PPOE',LNPLT,PLTPOP)
         call nc_filex%read('PLME',LNPLT,PLME)
         call nc_filex%read('PLDS',LNPLT,PLDS)
         call nc_filex%read('PLRS',LNPLT,ROWSPC)
         call nc_filex%read('PLRD',LNPLT,AZIR)
         call nc_filex%read('PLDP',LNPLT,SDEPTH)
         call nc_filex%read('PLWT',LNPLT,SDWTPL)
         call nc_filex%read('PAGE',LNPLT,SDAGE)
         call nc_filex%read('PENV',LNPLT,ATEMP)
         call nc_filex%read('PLPH',LNPLT,PLPH)
         call nc_filex%read('SPRL',LNPLT,SPRLAP)
         call nc_filex%read('NFORC',LNPLT,NFORC)
         call nc_filex%read('PLTFOR',LNPLT,PLTFOR)
         call nc_filex%read('NDOF',LNPLT,NDOF)
         call nc_filex%read('PMTYPE',LNPLT,PMTYPE)
         CALL Y2K_DOY(YRPLT)
         CALL Y2K_DOY(IEMRG)
         CALL YR_DOY (YRPLT,YR,IPLT)

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

      RETURN

      END SUBROUTINE read_nc_plt_sec
