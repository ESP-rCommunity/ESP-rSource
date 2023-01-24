C Mass flow network common blocks. Assumes that subroutines have
C an include net_flow.h prior to include net_flow_data.h

C Type declarations:
      integer NNOD  ! number of flow nodes
      integer NCMP  ! number of fluid flow components
      integer NCNN  ! number of fluid flow interconnections
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      character NDNAM*12  ! - flow node name
      COMMON/MFLOW2/NDNAM(0:MNOD)

      integer NDFLD  ! - fluid type (1=air, 2=water)
      Integer NDTYP  ! - type (0=internal; unknown pressure
C  1=internal; known total pressure, 2=boundary; known total pressure
C  3=boundary; wind induced pressure; implies NDFLD=1)
      REAL    HNOD   ! - XYZ position (m)
      REAL    SUPNOD ! - supplementary data items (max. MNDS)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD,3),
     &              SUPNOD(MNOD,MNDS)
      character NODASSOC*12  ! associated zone & surface names
      COMMON/MFLOW3A/NODASSOC(MNOD,2)

      integer ITND !  node temperature index; zero=constant T
                   !  non-zero then take zone DBT
      real    TNOD ! - node fluid temperature (C)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      real RHON    !  node fluid density (kg/m^3)
      COMMON/MFLOW5/RHON(MNOD)

      integer IVALCM ! array of mass flow component type numbers
      COMMON/MFLOW6/IVALCM(MCMV)
      character LVALCM*60  ! short description of each flow component type
      character LTVALCM*32 ! terse description of each flow component
      COMMON/MFLOW7/LVALCM(MCMV),LTVALCM(MCMV)

      character CMNAM*12  ! - flow component identifier/name
      character LTPCMP*60 ! - short description of that component type
      character LTTCMP*32 ! - terse description of component tpe
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP),LTTCMP(MCMP)
      character CMPASSOC*12  ! associated zone & surface names
      COMMON/MFLOW8A/CMPASSOC(MCMP,2)

      integer ITPCMP ! component type number
      integer ISDCMP ! number of supplementary data items (max. MCMS)
      integer ISDCNN ! number of connection level supplementary items (max. MCNS)
      real   SUPCMP  ! component supplementary data items (1st item = fluid type)
      integer NWPCMP ! 0=start-end same, 1 start end different positions, >1 waypoints
      real   HCMP    ! XYZ position of component start point with optional way
                     ! points and end point (as in NWPCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS),NWPCMP(MCMP),HCMP(MCMP,5,3)

      integer NODPS  ! node number on positive side of connection
      real    HGTPS  ! height of +ve linkage point relative to NODPS (m)
      integer NODNE  ! node number on negative side of connection
      Real    HGTNE  ! height of -ve linkage point relative to NODNE (m)
      integer ITPCON ! number of linking fluid flow component
      integer NDSCNN ! connection level component supplementary node numbers
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

