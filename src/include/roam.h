C ESP-r header file for parameters associated with changing orientation
C as a function of time

C Array sizing variables
      INTEGER MPDR, MLOC
      PARAMETER (MPDR=365) ! max. periods for roaming
      PARAMETER (MLOC=30)  ! max. locations for roaming


C Common block variables: Integers
      INTEGER IROAM ! 1 signifies that model can change orientation 
      INTEGER NPDR  ! roaming periods in the model
      INTEGER NLOC  ! number of locations in the model
      INTEGER LHRS  ! start hour for each period
      INTEGER LDYS  ! start day for each period
      INTEGER LMTS  ! start month for each period
      INTEGER LORNT ! orientation (clockwise from North) for each period
      INTEGER LDTP  ! day type to look up in opr file for each period
      INTEGER LLOCT ! location index number for each period (0 for cruising)

      COMMON/ROAMINT/IROAM,NPDR,NLOC,
     &LHRS(MPDR),LDYS(MPDR),LMTS(MPDR),
     &LORNT(MPDR),LDTP(MPDR),LLOCT(MPDR)

C Common block variables: Real numbers
      REAL VERS   ! roaming file version number
      REAL LOCLT  ! latitude
      REAL LOCLG  ! longitude

      COMMON/ROAMREL/VERS,LOCLT(MLOC),LOCLG(MLOC)

C Common block variables: Strings
      CHARACTER LROAM*72 ! roaming file name
      CHARACTER LOCRM*30 ! location names

      COMMON/ROAMSTR/LROAM,LOCRM(MLOC)
