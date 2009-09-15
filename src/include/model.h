C This header relates to high level model entities in ESP-r. It is
C dependant on building.h and should follow building.h so that
C parameters will have been defined.

C The title and summary of the model
      character modeltitle*72  ! title of the model (to replace LSNAM)
      character modeldocblock*248   ! text block of documentation for model
      common/modeltd/modeltitle,modeldocblock

      integer nccode  ! zone code.
      integer indutl  ! zone utilities file index: 0= none 1= used (deprediated)
      character LPROJ*72 ! zone scheduling (operation files)
      character LGEOM*72 ! zone geometry file
      character LSHAD*72 ! zone temporal shading and insolation file
      character LTHRM*72 ! zone construction file
      character LUTIL*72 ! zone utilities file name (legacy file)
      COMMON/C2/NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

C Indicators for locations or existance of zone supplimental files.
      integer IVF  ! zone view factor file zero is not used, one exists 
      integer ISI  ! zone shading/insolation file zero is not used, one exists
      integer IHC  ! zone hc coef file zero is not used, one exists
      integer ITW  ! zone optical properties file zero is not used, one exists
      integer ICGC ! zone casual gain control file zero is not used, one exists
      integer IOBS ! zone obstructions zero is not used, one exists in a
                   ! separate file, two included in zone geometry file
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)

C File names for zone supplimental files.
      character LVIEW*72  ! zone view factor file
      character LHCCO*72  ! zone hc coef file
      character LTWIN*72  ! zone optical properties file
      character LCGCIN*72 ! zone casual gain control file
      character ZOBS*72   ! zone obstructions file (if separate from geo file)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
