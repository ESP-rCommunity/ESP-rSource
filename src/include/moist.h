
      integer LEGACY,OTTO

      PARAMETER (LEGACY=1)
      PARAMETER (OTTO=2)

C Moisture model common for file version and active
C model storage
      common/moist85/iMoistModel(MCOM),iMoistVer(MCOM)

      integer iMoistModel  !  Moisture model used:
                           !  LEGACY = 1
                           !  OTTO = 2

      integer iMoistVer    !  File version

