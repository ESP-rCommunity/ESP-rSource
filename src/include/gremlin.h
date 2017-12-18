C *** PARAMETERS

C Maximum number of gremlins.
      INTEGER, PARAMETER :: MGRM=10

C *** ENUMERATORS

C Gremlin mischief.
C 1 - boiler failure (basic control heating capacity in all zones = 0)
C 2 - heat wave (external dry bulb temperature + 10 deg. C)
C 3 - cold snap (external dry bulb temperature - 10 deg. C)

C *** COMMONS

C NGRM    - number of gremlins
C GRMTYP  - gremlin type
C GRMZON  - zone that the gremlin is in (0 = omnipresent)
C GRMMCF  - mischief index (0 if not causing mischief)
      common/gremlin/NGRM,GRMTYP(MGRM),GRMZON(MGRM),GRMMCF(MGRM)
      integer NGRM,GRMTYP,GRMZON,GRMMCF

C NMCF   - number of gremlins currently causing mischief
C MCFGRM - indices of gremlins currently causing mischief
C MCFTYP - mischief type
C MCFSTS - mischief start time step
      common/grmschf/NMCF,MCFGRM(MGRM),MCFTYP(MGRM),MCFSTS(MGRM)
      integer NMCF,MCFGRM,MCFTYP,MCFSTS

C PROB1PS - probability such that an event should occur once per simulation.
      common/grmprob/PROB1PS
      real PROB1PS


