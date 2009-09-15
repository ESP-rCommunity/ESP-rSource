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
